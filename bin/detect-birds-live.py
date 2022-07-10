# From https://towardsdatascience.com/implementing-real-time-object-detection-system-using-pytorch-and-opencv-70bac41148f7

# UBUNTU:
# $ sudo apt update
# $ sudo apt install python3-pip
# $ sudo apt install ffmpeg

# If running Python 3.10, downgrade to Python 3.9 for compatibility with
# onnxruntime, and use `/home/ubuntu/.local/bin/pip3.9` instead of `pip3`, and
# `python3.9` instead of `python3`:
#
# $ sudo apt install software-properties-common
# $ sudo add-apt-repository ppa:deadsnakes/ppa
# $ sudo apt install python3.9 python3.9-dev python3.9-venv
# $ python3.9 -m pip install --upgrade pip --user

# macOS:
# $ brew install ffmpeg

# $ pip3 install opencv-python torch torchvision yolov5 onnx onnxruntime ffmpeg-python av

import cv2
import torch
import yolov5
import time
import numpy as np
import random
import subprocess
import datetime
import av
import ffmpeg
import queue
import threading
import signal

# device = 'cuda' if torch.cuda.is_available() else 'cpu'
# print(f'Using device {device}')

# Generate the ONNX model as follows:
# git clone git@github.com:ultralytics/yolov5.git
# cd yolov5
# pip3 install -r requirements.txt coremltools onnx onnx-simplifier onnxruntime openvino-dev tensorflow-cpu
# python3 export.py --weights yolov5s.pt --include onnx
model = yolov5.load('repos/yolov5/yolov5n.onnx')
# model = yolov5.load('/Users/ankur.dave/exp12/weights/best.pt')
model.conf = 0.5
# model.to(device)

target_classes = ['bird', 'cat']

def score_frame(frame_rgb):
    results = model(frame_rgb)
    # results.print()
    labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()

    has_bird = False
    n = len(labels)
    x_shape, y_shape = frame_rgb.shape[1], frame_rgb.shape[0]
    for i in range(n):
        row = cord[i]
        score = row[4]
        x1 = int(row[0]*x_shape)
        y1 = int(row[1]*y_shape)
        x2 = int(row[2]*x_shape)
        y2 = int(row[3]*y_shape)
        classes = model.names # Get the name of label index
        has_bird = has_bird or classes[int(labels[i])] in target_classes

    return has_bird

in_filename = "foo"

width = 2304
height = 1296

container = av.open(in_filename)
in_stream = container.streams.video[0]

now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
output_file = f"live-{now}.mp4"
output = av.open(output_file, "w")
out_stream = output.add_stream(template=in_stream)
print(f"Writing to {output}")

target_ts_delta_per_frame = int(1 / (in_stream.time_base * in_stream.guessed_rate))
print(f"Stream time_base={in_stream.time_base}, guessed_rate={in_stream.guessed_rate}, target_delta = {target_ts_delta_per_frame}")

def stop_threads():
    detect_queue.put(None)

default_sigint_handler = signal.getsignal(signal.SIGINT)
def sigint_handler(sig, frame):
    stop_threads()
    default_sigint_handler()
signal.signal(signal.SIGINT, sigint_handler)

max_queue_size = 50
detect_queue = queue.Queue(maxsize=max_queue_size)
def detector():
    chunk = []
    # Can have 3 values: [None, True, False].
    # - None: need to continue detecting on subsequent frames.
    # - True: already detected in a past frame.
    # - False: decided to skip detection for all remaining frames in this chunk.
    should_write_chunk = None
    last_written_frame_dts = 0
    last_written_frame_pts = 0
    while True:
        packet = detect_queue.get()
        if packet is None: return
        qsize = detect_queue.qsize()
        if packet.is_keyframe:
            # Flush the previous chunk to the output if necessary.
            if should_write_chunk:
                for packet2 in chunk:
                    packet2.stream = out_stream
                    last_written_frame_dts += target_ts_delta_per_frame
                    last_written_frame_pts += target_ts_delta_per_frame
                    packet2.dts = last_written_frame_dts
                    packet2.pts = last_written_frame_pts
                    output.mux(packet2)

            # Start a new chunk.
            chunk = []
            should_write_chunk = None

        chunk.append(packet)

        if should_write_chunk:
            print(f"packet pts={packet.pts}: skipping detection, bird already detected. qsize={qsize}")
            continue

        if (qsize > random.randint(0, max_queue_size - 1) and not packet.is_keyframe) or should_write_chunk is False:
            print(f"packet pts={packet.pts}: skipping detection, need to catch up. qsize={qsize}")
            should_write_chunk = False
            continue

        start_time = time.time()
        packet_has_bird = False
        frames = packet.decode()
        if len(frames) == 0: continue
        for frame in frames:
            frame_arr = frame.to_ndarray(format='rgb24')
            assert frame_arr.shape == (height, width, 3), frame_arr.shape
            has_bird = score_frame(frame_arr)
            if has_bird:
                packet_has_bird = True
                should_write_chunk = True
                break
        infer_s = time.time() - start_time
        print(f"packet pts={packet.pts}: has_bird={'Y' if packet_has_bird else 'n'}, infer_s={infer_s:.2f}. qsize={qsize}")

detect_thread = threading.Thread(target=detector, name='detect')
detect_thread.start()

try:
    for packet in container.demux(in_stream):
        # We need to skip the "flushing" packets that `demux` generates.
        if packet.dts is None:
            continue
        try:
            detect_queue.put(packet, block=False)
        except queue.Full:
            print(f"Dropping packet {packet}, detect queue is full")
finally:
    stop_threads()
    detect_thread.join()
