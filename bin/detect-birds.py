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

model = yolov5.load('yolov5s.pt')
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
        has_bird = has_bird or classes[int(labels[i])] == 'bird'
        if classes[int(labels[i])] == 'bird':
            confidence = max(confidence, score)

    return frame, has_bird, confidence

out = cv2.VideoWriter("labeled.mp4", cv2.VideoWriter_fourcc(*"mp4v"), 20, \
                          (1920, 1080))

files = [
    'Downloads/modet_2022-06-01_13-04.mp4',
    'Downloads/modet_2022-06-01_13-06.mp4',
    'Downloads/modet_2022-06-01_13-07.mp4',
    'Downloads/modet_2022-06-01_13-08.mp4',
    'Downloads/modet_2022-06-01_13-09.mp4',
    'Downloads/modet_2022-06-01_13-10.mp4',
    'Downloads/modet_2022-06-01_13-13.mp4',
    'Downloads/modet_2022-06-01_13-17.mp4',
    'Downloads/modet_2022-06-01_13-21.mp4',
    'Downloads/modet_2022-06-01_13-22.mp4',
    'Downloads/modet_2022-06-01_13-25.mp4',
    'Downloads/modet_2022-06-01_13-29.mp4',
    'Downloads/modet_2022-06-01_13-30.mp4',
    'Downloads/modet_2022-06-01_13-31.mp4',
    'Downloads/modet_2022-06-01_13-33.mp4',
    'Downloads/modet_2022-06-01_13-36.mp4',
    'Downloads/modet_2022-06-01_13-39.mp4',
    'Downloads/modet_2022-06-01_13-41.mp4',
    'Downloads/modet_2022-06-01_13-43.mp4',
    'Downloads/modet_2022-06-01_13-46.mp4',
    'Downloads/modet_2022-06-01_13-48.mp4',
    'Downloads/modet_2022-06-01_13-50.mp4',
    'Downloads/modet_2022-06-01_13-54.mp4',
    'Downloads/modet_2022-06-01_14-04.mp4',
    'Downloads/modet_2022-06-01_14-05.mp4',
    'Downloads/modet_2022-06-01_14-07.mp4',
    'Downloads/modet_2022-06-01_14-10.mp4',
    'Downloads/modet_2022-06-01_14-13.mp4',
    'Downloads/modet_2022-06-01_14-17.mp4',
    'Downloads/modet_2022-06-01_14-18.mp4',
    'Downloads/modet_2022-06-01_14-19.mp4',
    'Downloads/modet_2022-06-01_14-21.mp4',
    'Downloads/modet_2022-06-01_14-24.mp4',
    'Downloads/modet_2022-06-01_14-26.mp4',
]

files = ['Downloads/modet_2022-06-01_11-42.mp4']

# files = ["http://192.168.1.176:8080/video"]

for f in files:
    stream = cv2.VideoCapture(f)
    assert stream.isOpened()

    frame_idx = 0

    while True:
        start_time = time.time()
        frames = []
        for i in range(30):
            ret, frame = stream.read()
            if not ret: break
            frames.append(frame)
            frame_idx += 1
        if len(frames) == 0: break

        sample_frames = random.sample(frames, min(len(frames), 10))
        false_negative_sample_frames = sample_frames[0:2]
        false_positive_sample_frames = sample_frames[2:]

        # Check 2 arbitrary frames. If at least one shows a bird, proceed.
        num_with_bird = 0
        for frame in false_negative_sample_frames:
            _, sample_has_bird, _ = score_frame(frame)
            if sample_has_bird:
                num_with_bird += 1

        if num_with_bird > 0:
            # Check 8 more buffered frames to reduce false positives. If at
            # least half show a bird, proceed.
            for frame in false_positive_sample_frames:
                _, sample_has_bird, _ = score_frame(frame)
                if sample_has_bird:
                    num_with_bird += 1

        has_bird = num_with_bird >= 5
        if has_bird:
            for frame in frames:
                out.write(frame)

        end_time = time.time()
        fps = len(frames)/np.round(end_time - start_time, 3)
        print(f"file={f}, frame={frame_idx:08d}: has_bird={'Y' if has_bird else 'n'}, num_with_bird={num_with_bird}, fps={fps:.1f}")
