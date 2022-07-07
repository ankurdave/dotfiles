# From https://towardsdatascience.com/implementing-real-time-object-detection-system-using-pytorch-and-opencv-70bac41148f7

# UBUNTU:
# $ sudo apt update
# $ sudo apt install python3-pip
# $ sudo apt install ffmpeg

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
# pip install -r requirements.txt coremltools onnx onnx-simplifier onnxruntime openvino-dev tensorflow-cpu
# python3 export.py --weights yolov5s.pt --include onnx
model = yolov5.load('repos/yolov5/yolov5n.onnx')
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

now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
output_file = f"live-{now}.mp4"
print(f"Writing to {output_file}")
out = cv2.VideoWriter(output_file, cv2.VideoWriter_fourcc(*"mp4v"), 20, \
                      (1080, 1920))

mjpeg = "http://192.168.1.143:8080/video"
jpeg = "http://192.168.1.143:8080/shot.jpg"
use_mjpeg = True

frame_idx = 0
last_seen_frame_idx = -10000

if use_mjpeg:
    stream = cv2.VideoCapture(mjpeg)
    if not stream.isOpened():
        print(f"Could not open {f}")
        quit()

while True:
    frame_idx += 1

    start_time = time.time()
    if not use_mjpeg:
        stream = cv2.VideoCapture(jpeg)
        if not stream.isOpened():
            print(f"Could not open {f}")
            time.sleep(5)
            continue
    ret, frame = stream.read()
    if not ret:
        print(f"Could not read frame from {f}")
        continue
    fetch_s = time.time() - start_time

    start_time = time.time()
    # To reduce resource usage, check for bird only every 30 frames.
    run_inference = frame_idx % 30 == 0
    if run_inference:
        _, has_bird, _ = score_frame(frame)
        if has_bird:
            last_seen_frame_idx = frame_idx
    infer_s = time.time() - start_time

    # After seeing a bird, record the next 60 frames.
    start_time = time.time()
    if frame_idx - last_seen_frame_idx < 60:
        out.write(frame)
    write_s = time.time() - start_time

    print(f"frame={frame_idx:08d}: has_bird={'?' if not run_inference else ('Y' if has_bird else 'n')}, fetch_s={fetch_s:.2f}, infer_s={infer_s:.2f}, write_s={write_s:.2f}")

    if not use_mjpeg:
        # When no bird, throttle framerate to reduce resource usage.
        if frame_idx - last_seen_frame_idx >= 120:
            time.sleep(2)
