# From https://towardsdatascience.com/implementing-real-time-object-detection-system-using-pytorch-and-opencv-70bac41148f7

# pip3 install opencv-python torch torchvision yolov5
# brew install ffmpeg

import cv2
import torch
import yolov5
import time
import numpy as np

# camera_ip = "http://192.168.1.176:8080/video"
camera_ip = "Downloads/modet_2022-06-01_12-20.mp4"
stream = cv2.VideoCapture(camera_ip)

model = yolov5.load('yolov5s.pt')
model.conf = 0.5
device = 'cuda' if torch.cuda.is_available() else 'cpu'
print(f'Using device {device}')
model.to(device)

def score_frame(frame):
    frame_rgb = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    results = model(frame_rgb)
    # results.print()
    labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()

    has_bird = False
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

    return frame, has_bird

assert stream.isOpened()
x_shape = int(stream.get(cv2.CAP_PROP_FRAME_WIDTH))
y_shape = int(stream.get(cv2.CAP_PROP_FRAME_HEIGHT))
out = cv2.VideoWriter("labeled.mp4", cv2.VideoWriter_fourcc(*"mp4v"), 20, \
                          (x_shape, y_shape))

# cv2.startWindowThread()
# cv2.namedWindow("preview")

detected_bird_frames_ago = 1000
frame_idx = 0

while True:
    detected_bird_frames_ago += 1
    frame_idx += 1

    start_time = time.time()
    ret, frame = stream.read()
    if not ret: break
    frame, has_bird = score_frame(frame)
    if has_bird:
        detected_bird_frames_ago = 0
    if detected_bird_frames_ago < 150:
        out.write(frame)

    cv2.imshow('preview', frame)
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break

    end_time = time.time()
    fps = 1/np.round(end_time - start_time, 3)
    print(f"{frame_idx}: has_bird={has_bird}, fps={fps}")
