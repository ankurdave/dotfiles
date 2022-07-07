# From https://towardsdatascience.com/implementing-real-time-object-detection-system-using-pytorch-and-opencv-70bac41148f7

# pip3 install opencv-python torch torchvision yolov5
# brew install ffmpeg

import cv2
import torch
import yolov5
import time
import numpy as np
import random

# device = 'cuda' if torch.cuda.is_available() else 'cpu'
# print(f'Using device {device}')

# Generate the ONNX model as follows:
# git clone git@github.com:ultralytics/yolov5.git
# pip install -r requirements.txt coremltools onnx onnx-simplifier onnxruntime openvino-dev tensorflow-cpu
# python3 export.py --weights yolov5s.pt --include onnx
model = yolov5.load('/Users/ankur.dave/exp12/weights/best.onnx')
model.conf = 0.6
# model.to(device)

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
        bgr = (0, 0, 255) # color of the box
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
        has_bird = has_bird or classes[int(labels[i])] == 'bird'
        if classes[int(labels[i])] == 'bird':
            confidence = max(confidence, score)

    return frame, has_bird, confidence

files = [
    'live-2022-07-05T152807.mp4',
]

for f in files:
    stream = cv2.VideoCapture(f)
    if not stream.isOpened():
        print(f"Could not open {f}")
        continue

    frame_idx = 0

    while True:
        frame_idx += 1

        start_time = time.time()
        ret, frame = stream.read()
        if not ret: break
        fetch_s = time.time() - start_time

        start_time = time.time()
        _, has_bird, _ = score_frame(frame)
        infer_s = time.time() - start_time

        print(f"frame={frame_idx:08d}: has_bird={'Y' if has_bird else 'n'}, fetch_s={fetch_s:.2f}, infer_s={infer_s:.2f}")

        cv2.imshow('frame', frame)
        if cv2.waitKey(1) == ord('q'):
            quit()
