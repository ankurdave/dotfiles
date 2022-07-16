# $ brew install ffmpeg
# $ pip3 install opencv-python torch torchvision yolov5 onnx onnxruntime ffmpeg-python av coremltools

import av
import collections
import colorsys
# import coremltools
import cv2
import datetime
import numpy as np
import os
import queue
import random
import signal
import sys
import threading
import time
import uuid
import yolov5
from PIL import Image, ImageOps

class BoundedBlockingQueue(object):
    def __init__(self, max_queue_size):
        self.has_new_item = threading.Condition()
        # Guarded by `self.has_new_item`.
        self.deque = collections.deque(maxlen=max_queue_size)

    def put(self, elem):
        with self.has_new_item:
            self.deque.append(elem)
            self.has_new_item.notify()

    def get(self):
        with self.has_new_item:
            self.has_new_item.wait_for(lambda: len(self.deque) > 0)
            return self.deque.popleft()


class ChunkedVideoWriter(object):
    def __init__(self, output_file, out_stream_template):
        self.chunk = []

        self.last_written_frame_dts = 0
        self.last_written_frame_pts = 0

        self.output = av.open(output_file, "w")
        self.out_stream = self.output.add_stream(template=out_stream_template)
        print(f"Writing to {self.output}")

    def append(self, packet):
        self.chunk.append(packet)

    def flush(self, write_current_chunk_to_output):
        if write_current_chunk_to_output:
            # Use a hardcoded 1/25 s per frame.
            # TODO: support variable framerates.
            ts_delta_per_frame = 3600
            for packet2 in self.chunk:
                packet2.stream = self.out_stream
                self.last_written_frame_dts += ts_delta_per_frame
                self.last_written_frame_pts += ts_delta_per_frame
                packet2.dts = self.last_written_frame_dts
                packet2.pts = self.last_written_frame_pts
                self.output.mux(packet2)
        # Start a new chunk.
        self.chunk = []

class NullVideoWriter(object):
    def __init__(self):
        pass

    def append(self, packet):
        pass

    def flush(self, write_current_chunk_to_output):
        pass


class DetectorAndWriterThread(threading.Thread):
    def __init__(self, output_file, max_queue_size, out_stream_template, reader_supports_backpressure):
        super(DetectorAndWriterThread, self).__init__(name='DetectorAndWriterThread')

        # CoreML model downloaded from https://github.com/john-rocky/CoreML-Models#yolov5s
        # self.model_coreml = coremltools.models.MLModel('yolov5s.mlmodel')

        # From https://github.com/ankurdave/bird-models.
        self.model_onnx = yolov5.load('/Users/ankur.dave/repos/bird-models/yolov5s-birds-pittsburgh.onnx')
        self.model_onnx.conf = 0.6

        self.model = self.model_onnx

        self.detect_queue = queue.Queue(maxsize=max_queue_size)

        if output_file:
            self.chunked_writer = ChunkedVideoWriter(output_file, out_stream_template)
        else:
            self.chunked_writer = NullVideoWriter()

        self.display_queue = BoundedBlockingQueue(max_queue_size=1)

        def hsv_to_bgr(h, s, v):
            return 255 * np.array(colorsys.hsv_to_rgb(h, s, v)[::-1])
        self.prediction_colors_bgr = [hsv_to_bgr(h, 0.5, 0.8)
                                      for h in np.linspace(0, 1, len(self.model.names))]

        self.reader_supports_backpressure = reader_supports_backpressure

    def enqueue(self, packet_and_frames):
        try:
            self.detect_queue.put(packet_and_frames, block=self.reader_supports_backpressure)
        except queue.Full:
            print(f"Dropping packet {packet_and_frames}, detect queue is full")

    def get_frame_to_display_and_original_frame(self):
        return self.display_queue.get()

    def stop(self):
        self.detect_queue.put(None)

    def run(self):
        packet_idx = 0
        should_write_chunk = False
        last_frame_start_time = None
        last_frame_avg_s = None
        while True:
            elem = self.detect_queue.get()
            if elem is None:
                self.display_queue.put(None)
                return

            start_time = time.time()
            if last_frame_start_time is not None:
                last_frame_s = start_time - last_frame_start_time
                # Update the EWMA.
                if last_frame_avg_s is None:
                    last_frame_avg_s = last_frame_s
                else:
                    last_frame_avg_s = 0.5 * last_frame_s + 0.5 * last_frame_avg_s
            last_frame_start_time = start_time

            if last_frame_avg_s is not None and packet_idx % 100 == 0:
                print(f'{last_frame_avg_s * 1000:.0f} ms, {1/last_frame_avg_s:.0f} FPS')

            packet_idx += 1
            packet, frames = elem

            qsize = self.detect_queue.qsize()
            if packet.is_keyframe:
                self.chunked_writer.flush(should_write_chunk)
                should_write_chunk = False

            self.chunked_writer.append(packet)
            if len(frames) == 0: continue

            # When the queue starts to fill up, skip inference on some frames to catch up.
            if (not self.reader_supports_backpressure
                and (qsize > random.randint(0, max_queue_size - 1) and not packet.is_keyframe)):
                continue

            # TODO: Consider only running inference on a subset of frames to save energy.

            start_time = time.time()
            packet_has_bird = False
            for frame in frames:
                frame_arr = frame.to_ndarray(format='rgb24')
                has_bird = self.__score_frame_onnx(frame_arr)
                if has_bird:
                    packet_has_bird = True
                    should_write_chunk = True
                    break
            infer_s = time.time() - start_time
            print(f"packet pts={packet.pts}: has_bird={'Y' if packet_has_bird else 'n'}, infer_s={infer_s:.2f}. qsize={qsize}")
        self.display_queue.put(None)

    # def __score_frame_coreml(self, frame_rgb):
    #     img = Image.fromarray(frame_rgb)
    #     img = ImageOps.fit(img, (640, 640), Image.Resampling.BILINEAR)
    #     results = self.model_coreml.predict({"image": img})
    #     confidence = results['confidence']
    #     bird_confidences = confidence[:, 14] # class 14 = bird
    #     bird_confidence = np.amax(bird_confidences, initial=0.0)
    #     return bird_confidence >= 0.6

    def __score_frame_onnx(self, frame_rgb):
        start_time = time.time()
        results = self.model(frame_rgb)
        infer_ms = (time.time() - start_time) * 1000

        labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()
        objects = [(self.model.names[int(labels[i])], cord[i][4]) for i in range(len(labels))]
        if len(objects) > 0:
            print(f"{objects}, {infer_ms:.0f}")

        # Show the predictions as labeled boxes.
        frame_bgr = cv2.cvtColor(frame_rgb, cv2.COLOR_RGB2BGR)
        height, width, _ = frame_bgr.shape
        font_scale = 1.5
        thickness = 3
        for i in range(len(labels)):
            class_name = self.model.names[int(labels[i])]
            prediction = cord[i]
            x1, y1 = int(prediction[0] * width), int(prediction[1] * height)
            x2, y2 = int(prediction[2] * width), int(prediction[3] * height)
            score = prediction[4]
            prediction_color_bgr = self.prediction_colors_bgr[int(labels[i])]
            prediction_weight = int(np.interp(score, [self.model_onnx.conf, 1.0], [1, 20]))
            cv2.rectangle(frame_bgr, (x1, y1), (x2, y2), (0, 0, 0), thickness=prediction_weight)
            self.__put_text_with_background(frame_bgr, f"{class_name} {score:.2f}",
                                            x1, y1 - prediction_weight // 2,
                                            font_scale, (0, 0, 0), prediction_color_bgr, thickness)
        fps_color_bgr = (255, 255, 255)
        fps_background_color_bgr = (0, 0, 0)
        (infer_ms_text_w, infer_ms_text_h) = \
            self.__put_text_with_background(frame_bgr, f"{infer_ms:.0f} ms", width, height,
                                            font_scale, fps_color_bgr, fps_background_color_bgr,
                                            thickness, align='right')
        (fps_ms_text_w, fps_ms_text_h) = \
            self.__put_text_with_background(frame_bgr, f"{1000/infer_ms:.0f} FPS",
                                            width, height - infer_ms_text_h,
                                            font_scale, fps_color_bgr, fps_background_color_bgr,
                                            thickness, align='right')

        self.display_queue.put((frame_bgr, frame_rgb))

        return len(objects) > 0

    def __put_text_with_background(self, frame_bgr, text_str, x, y, font_scale,
                                   text_color_bgr, background_color_bgr, thickness, padding=5,
                                   align='left'):
        (text_w, text_h), _ = cv2.getTextSize(
            text=text_str,
            fontFace=cv2.FONT_HERSHEY_SIMPLEX,
            fontScale=font_scale,
            thickness=thickness,
        )
        if align == 'left':
            text_origin = (x + padding, y - padding)
            rect_origin = (x + text_w + 2 * padding, y - text_h - 2 * padding)
        elif align == 'right':
            text_origin = (x - text_w - padding, y - padding)
            rect_origin = (x - text_w - 2 * padding, y - text_h - 2 * padding)

        cv2.rectangle(frame_bgr,
                      (x, y),
                      rect_origin,
                      background_color_bgr,
                      thickness=-1)
        cv2.putText(
            img=frame_bgr,
            text=text_str,
            org=text_origin,
            fontFace=cv2.FONT_HERSHEY_SIMPLEX,
            fontScale=font_scale,
            color=text_color_bgr,
            thickness=thickness,
        )

        return (text_w + 2 * padding, text_h + 2 * padding)


class FrameDecoderThread(threading.Thread):
    def __init__(self, in_filename):
        super(FrameDecoderThread, self).__init__(name='FrameDecoderThread')

        self.container = av.open(in_filename)
        self.in_stream = self.container.streams.video[0]
        self.should_stop = threading.Event()

    def run(self):
        for packet in self.container.demux(self.in_stream):
            if self.should_stop.is_set(): return

            # We need to skip the "flushing" packets that `demux` generates.
            if packet.dts is None:
                continue

            frames = packet.decode()
            detector_and_writer_thread.enqueue((packet, frames))

        detector_and_writer_thread.stop()

    def stop(self):
        self.should_stop.set()


if __name__ == '__main__':
    in_file = sys.argv[1]
    should_write = sys.argv[2] == '--write'
    is_live = sys.argv[3] == '--live'
    screenshot_dir = sys.argv[4]
    os.makedirs(screenshot_dir, exist_ok=True)

    max_queue_size = 50

    threads = []

    frame_decoder_thread = FrameDecoderThread(in_file)
    threads.append(frame_decoder_thread)

    now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
    detector_and_writer_thread = DetectorAndWriterThread(
        f"live-{now}.mp4" if should_write else None,
        max_queue_size,
        frame_decoder_thread.in_stream,
        reader_supports_backpressure=not is_live,
    )
    threads.append(detector_and_writer_thread)

    default_sigint_handler = signal.getsignal(signal.SIGINT)
    def stop_threads_on_sigint(sig, frame):
        for thread in threads:
            thread.stop()
        default_sigint_handler()
    signal.signal(signal.SIGINT, stop_threads_on_sigint)

    for thread in threads:
        thread.start()

    try:
        while True:
            elem = detector_and_writer_thread.get_frame_to_display_and_original_frame()
            if elem is None:
                quit()
            frame_bgr, orig_frame_rgb = elem
            cv2.imshow('frame', frame_bgr)
            key = cv2.waitKey(1)
            if key == ord('q'):
                quit()
            elif key == ord('s'):
                screenshot_path = os.path.join(screenshot_dir, f"shot-{now}-{uuid.uuid4()}.jpg")
                cv2.imwrite(screenshot_path, cv2.cvtColor(orig_frame_rgb, cv2.COLOR_RGB2BGR))
                print(f"Saved frame to {screenshot_path}")

    finally:
        for thread in threads:
            thread.stop()
        for thread in threads:
            thread.join()
