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

# $ pip3 install opencv-python torch torchvision yolov5

import cv2
import torch
import yolov5
import time
import numpy as np
import random
import subprocess
import datetime

# device = 'cuda' if torch.cuda.is_available() else 'cpu'
# print(f'Using device {device}')

# Generate the ONNX model as follows:
# git clone git@github.com:ultralytics/yolov5.git
# cd yolov5
# pip3 install -r requirements.txt coremltools onnx onnx-simplifier onnxruntime openvino-dev tensorflow-cpu
# python3 export.py --weights yolov5s.pt --include onnx
model = yolov5.load('repos/yolov5/yolov5s.onnx')
model.conf = 0.6
# model.to(device)

target_classes = ['bird', 'cat']

def score_frame(frame):
    frame_rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    results = model(frame_rgb)
    # results.print()
    labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()

    has_bird = False
    confidence = 0.0
    n = len(labels)
    x_shape, y_shape = frame.shape[1], frame.shape[0]
    for i in range(n):
        row = cord[i]
        score = row[4]
        x1 = int(row[0]*x_shape)
        y1 = int(row[1]*y_shape)
        x2 = int(row[2]*x_shape)
        y2 = int(row[3]*y_shape)
        bgr = (0, 255, 0) # color of the box
        classes = model.names # Get the name of label index
        label_font = cv2.FONT_HERSHEY_SIMPLEX #Font for the label.
        # print(classes[int(labels[i])] + ' ' + f'{score:.2f}')
        cv2.rectangle(frame, \
                      (x1, y1), (x2, y2), \
                       bgr, 2) #Plot the boxes
        cv2.putText(frame,\
                    classes[int(labels[i])] + ' ' + f'{score:.2f}', \
                    (x1, y1), \
                    label_font, 0.9, bgr, 2) #Put a label over box.
        has_bird = has_bird or classes[int(labels[i])] in target_classes
        if classes[int(labels[i])] in target_classes:
            confidence = max(confidence, score)

    return frame, has_bird, confidence

NOTIFY_CMD = '''
on run argv
  display notification (item 2 of argv) with title (item 1 of argv)
end run
'''

last_bird_time = 0
def notify(title, text):
    cur_time = time.time()
    global last_bird_time
    if cur_time >= last_bird_time + 30:
        subprocess.call(['osascript', '-e', NOTIFY_CMD, title, text])
    last_bird_time = cur_time

now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
output_file = f"live-{now}.mp4"
print(f"Writing to {output_file}")
out = cv2.VideoWriter(output_file, cv2.VideoWriter_fourcc(*"mp4v"), 20, \
                      (1080, 1920))

files = ["http://192.168.1.143:8080/shot.jpg"]

for f in files:
    frame_idx = 0
    last_seen_frame_idx = -10000

    while True:
        frame_idx += 1

        start_time = time.time()
        stream = cv2.VideoCapture(f)
        if not stream.isOpened():
            print(f"Could not open {f}")
            time.sleep(5)
            continue
        ret, frame = stream.read()
        stream.release()
        if not ret: break

        _, has_bird, _ = score_frame(frame)
        if has_bird:
            notify('Bird detected', '')
            last_seen_frame_idx = frame_idx
        if frame_idx - last_seen_frame_idx < 30:
            out.write(frame)

        end_time = time.time()
        fps = 1/np.round(end_time - start_time, 3)
        print(f"file={f}, frame={frame_idx:08d}: has_bird={'Y' if has_bird else 'n'}, fps={fps:.1f}")

        cv2.imshow('frame', frame)
        if cv2.waitKey(1 if frame_idx - last_seen_frame_idx < 300 else 5000) == ord('q'):
            quit()
