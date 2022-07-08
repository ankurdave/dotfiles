import ctypes
import cv2
import datetime
import numpy as np
import sys
import time
import vlc
import yolov5

from PIL import Image

pl = vlc.MediaPlayer('live-2022-07-07T195733.mp4')
(VIDEOWIDTH, VIDEOHEIGHT) = (2304, 1296)

print(f"Playing video of size ({VIDEOWIDTH}, {VIDEOHEIGHT})")

# size in bytes when RV32
size = VIDEOWIDTH * VIDEOHEIGHT * 4
# allocate buffer
buf = (ctypes.c_ubyte * size)()
# get pointer to buffer
buf_p = ctypes.cast(buf, ctypes.c_void_p)

# global frame (or actually displayed frame) counter
global framenr
framenr = 0

# vlc.CallbackDecorators.VideoLockCb is incorrect
CorrectVideoLockCb = ctypes.CFUNCTYPE(ctypes.c_void_p, ctypes.c_void_p, ctypes.POINTER(ctypes.c_void_p))


@CorrectVideoLockCb
def _lockcb(opaque, planes):
    planes[0] = buf_p

model = yolov5.load('repos/yolov5/yolov5n.onnx')
model.conf = 0.6

@vlc.CallbackDecorators.VideoDisplayCb
def _display(opaque, picture):
    global framenr
    framenr += 1


vlc.libvlc_video_set_callbacks(pl, _lockcb, None, _display, None)
pl.video_set_format("RV32", VIDEOWIDTH, VIDEOHEIGHT, VIDEOWIDTH * 4)

pl.play()

now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
output_file = f"live-{now}.mp4"
out = cv2.VideoWriter(output_file, cv2.VideoWriter_fourcc(*"mp4v"), 20, \
                      (VIDEOWIDTH, VIDEOHEIGHT))
print(f"Writing to {output_file}")

processed_framenr = -1
last_seen_frame_idx = -10000
while True:
    # Wait for VLC to give us a new frame.
    start_time = time.time()
    while processed_framenr == framenr:
        time.sleep(0.01)
    processed_framenr = framenr

    image = Image.frombuffer("RGBA", (VIDEOWIDTH, VIDEOHEIGHT), buf, "raw", "BGRA", 0, 1)
    assert image.size == (VIDEOWIDTH, VIDEOHEIGHT)

    array_rgb = np.asarray(image.convert('RGB'))
    # assert array_rgb.shape == (VIDEOHEIGHT, VIDEOWIDTH, 3), f"{array_rgb.shape} != ({VIDEOWIDTH}, {VIDEOHEIGHT}, 3)"
    array_bgr = array_rgb[:,:,::-1]
    # assert array_bgr.shape == (VIDEOHEIGHT, VIDEOWIDTH, 3), f"{array_bgr.shape} != ({VIDEOWIDTH}, {VIDEOHEIGHT}, 3)"
    fetch_s = time.time() - start_time

    start_time = time.time()
    results = model(array_rgb)
    labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()
    has_bird = False
    n = len(labels)
    for i in range(n):
        row = cord[i]
        x1 = int(row[0]*VIDEOWIDTH)
        y1 = int(row[1]*VIDEOHEIGHT)
        x2 = int(row[2]*VIDEOWIDTH)
        y2 = int(row[3]*VIDEOHEIGHT)
        classes = model.names
        has_bird = has_bird or classes[int(labels[i])] in ['bird']
    if has_bird:
        last_seen_frame_idx = processed_framenr
    infer_s = time.time() - start_time

    # After seeing a bird, record the next 60 frames.
    start_time = time.time()
    # if processed_framenr - last_seen_frame_idx < 60:
    out.write(array_bgr)
    write_s = time.time() - start_time

    print(f"frame={processed_framenr:08d}: has_bird={'Y' if has_bird else 'n'}, fetch_s={fetch_s:.2f}, infer_s={infer_s:.2f}, write_s={write_s:.2f}")

input('Decoding, press enter to quit')
