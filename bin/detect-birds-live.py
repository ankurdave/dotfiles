# $ brew install ffmpeg
# $ pip3 install opencv-python torch torchvision yolov5 onnx onnxruntime ffmpeg-python av coremltools

import av
import coremltools
import datetime
import numpy as np
import queue
import random
import signal
import threading
import time
import yolov5
from PIL import Image, ImageOps

# CoreML model downloaded from https://github.com/john-rocky/CoreML-Models#yolov5s
model_coreml = coremltools.models.MLModel('yolov5s.mlmodel')

model_onnx = yolov5.load('repos/yolov5/yolov5n.onnx')
model_onnx.conf = 0.6

def score_frame_coreml(frame_rgb):
    img = Image.fromarray(frame_rgb)
    img = ImageOps.fit(img, (640, 640), Image.Resampling.BILINEAR)
    results = model_coreml.predict({"image": img})
    confidence = results['confidence']
    bird_confidences = confidence[:, 14] # class 14 = bird
    bird_confidence = np.amax(bird_confidences, initial=0.0)
    return bird_confidence >= 0.6

def score_frame_onnx(frame_rgb):
    results = model_onnx(frame_rgb)
    labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()
    has_bird = False
    n = len(labels)
    for i in range(n):
        row = cord[i]
        classes = model.names
        if classes[int(labels[i])] in ['bird']:
            return True
    return False

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

def stop_threads():
    detect_queue.put(None, block=False)

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
    packet_idx = 0
    while True:
        packet = detect_queue.get()
        packet_idx += 1
        if packet is None: return
        qsize = detect_queue.qsize()
        if packet.is_keyframe:
            # Flush the previous chunk to the output if necessary.
            if should_write_chunk:
                # Default to 1/25 s per frame.
                ts_delta_per_frame = 3600
                if len(chunk) > 1:
                    ts_delta_per_frame = chunk[1].pts - chunk[0].pts
                    assert ts_delta_per_frame > 0
                for packet2 in chunk:
                    packet2.stream = out_stream
                    last_written_frame_dts += ts_delta_per_frame
                    last_written_frame_pts += ts_delta_per_frame
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
        # We need to decode every frame after a keyframe, even if we don't want
        # to run detection on it. Otherwise we will get incorrect results.
        frames = packet.decode()
        if len(frames) == 0: continue
        # Only run inference on a subset of frames to save energy.
        if packet_idx % 10 != 0: continue
        for frame in frames:
            frame_arr = frame.to_ndarray(format='rgb24')
            assert frame_arr.shape == (height, width, 3), frame_arr.shape
            has_bird = score_frame_coreml(frame_arr)
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
