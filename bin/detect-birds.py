# From https://towardsdatascience.com/implementing-real-time-object-detection-system-using-pytorch-and-opencv-70bac41148f7

# pip3 install opencv-python torch torchvision yolov5
# brew install ffmpeg

import coremltools
import cv2
import numpy as np
import time
import yolov5
from PIL import Image, ImageOps

# CoreML model downloaded from https://github.com/john-rocky/CoreML-Models#yolov5s
#
# TODO: use Metal Performance Shaders backend. Requires torch 1.12.0 and macOS 12.3+.
# model = yolov5.load('repos/yolov5/yolov5n.pt')
# model.to('mps')

model = coremltools.models.MLModel('yolov5s.mlmodel')

def score_frame(frame):
    frame_rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    img = Image.fromarray(frame_rgb)
    img = ImageOps.fit(img, (640, 640), Image.BILINEAR)
    results = model.predict({"image": img})
    confidence = results['confidence']
    bird_confidences = confidence[:, 14] # class 14 = bird
    bird_confidence = np.amax(bird_confidences, initial=0.0)
    return bird_confidence >= 0.6

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
        has_bird = score_frame(frame)
        infer_s = time.time() - start_time

        print(f"frame={frame_idx:08d}: has_bird={'Y' if has_bird else 'n'}, fetch_s={fetch_s:.2f}, infer_s={infer_s:.2f}")
