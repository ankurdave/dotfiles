# $ brew install ffmpeg
# $ pip3 install opencv-python torch torchvision yolov5 onnx onnxruntime ffmpeg-python av coremltools

import av
import collections
# import coremltools
import cv2
import datetime
import numpy as np
import queue
import random
import signal
import sys
import threading
import time
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


class DetectorAndWriterThread(threading.Thread):
    def __init__(self, output_file, max_queue_size, out_stream_template):
        super(DetectorAndWriterThread, self).__init__(name='DetectorAndWriterThread')

        # CoreML model downloaded from https://github.com/john-rocky/CoreML-Models#yolov5s
        # self.model_coreml = coremltools.models.MLModel('yolov5s.mlmodel')

        # From https://github.com/ankurdave/bird-models.
        self.model_onnx = yolov5.load('/Users/ankur.dave/repos/bird-models/yolov5s-birds-pittsburgh.onnx')
        self.model_onnx.conf = 0.6

        self.model = self.model_onnx

        self.detect_queue = queue.Queue(maxsize=max_queue_size)

        self.chunked_writer = ChunkedVideoWriter(output_file, out_stream_template)

        self.display_queue = BoundedBlockingQueue(max_queue_size=1)

    def enqueue(self, packet):
        try:
            self.detect_queue.put_nowait(packet)
        except queue.Full:
            print(f"Dropping packet {packet}, detect queue is full")

    def get_frame_to_display(self):
        return self.display_queue.get()

    def stop(self):
        self.detect_queue.put_nowait(None)

    def run(self):
        packet_idx = 0
        should_write_chunk = False
        while True:
            packet = self.detect_queue.get()
            packet_idx += 1
            if packet is None:
                self.display_queue.put(None)
                return
            qsize = self.detect_queue.qsize()
            if packet.is_keyframe:
                self.chunked_writer.flush(should_write_chunk)
                should_write_chunk = False

            self.chunked_writer.append(packet)

            # We need to decode every frame after a keyframe, even if we don't want
            # to run detection on it. Otherwise we will get incorrect results for
            # subsequent detections.
            frames = packet.decode()
            if len(frames) == 0: continue

            if qsize > random.randint(0, max_queue_size - 1) and not packet.is_keyframe:
                print(f"packet pts={packet.pts}: skipping detection, need to catch up. qsize={qsize}")
                continue

            start_time = time.time()
            packet_has_bird = False
            # Only run inference on a subset of frames to save energy.
            if packet_idx % 10 != 0: continue
            for frame in frames:
                frame_arr = frame.to_ndarray(format='rgb24')
                has_bird = self.__score_frame_onnx(frame_arr)
                if has_bird:
                    packet_has_bird = True
                    should_write_chunk = True
                    break
            infer_s = time.time() - start_time
            print(f"packet pts={packet.pts}: has_bird={'Y' if packet_has_bird else 'n'}, infer_s={infer_s:.2f}. qsize={qsize}")

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
        labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()
        objects = [(self.model.names[int(labels[i])], cord[i][4]) for i in range(len(labels))]
        if len(objects) > 0:
            print(f"{objects}, {time.time() - start_time:.2f}")

        # Show the predictions as labeled boxes.
        frame_bgr = cv2.cvtColor(frame_rgb, cv2.COLOR_RGB2BGR)
        height, width, _ = frame_bgr.shape
        for i in range(len(labels)):
            class_name = self.model.names[int(labels[i])]
            prediction = cord[i]
            x1, y1 = int(prediction[0] * width), int(prediction[1] * height)
            x2, y2 = int(prediction[2] * width), int(prediction[3] * height)
            score = prediction[4]
            box_color_bgr = (0, 0, 255)
            cv2.rectangle(frame_bgr, (x1, y1), (x2, y2), box_color_bgr, 2)
            cv2.putText(frame_bgr,
                        f"{class_name} {score:.2f}",
                        (x1, y1),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.9, box_color_bgr, 2)
        self.display_queue.put(frame_bgr)

        return len(objects) > 0


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

            detector_and_writer_thread.enqueue(packet)

    def stop(self):
        self.should_stop.set()


if __name__ == '__main__':
    in_file = sys.argv[1]

    max_queue_size = 50

    threads = []

    frame_decoder_thread = FrameDecoderThread(in_file)
    threads.append(frame_decoder_thread)

    now = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")
    detector_and_writer_thread = DetectorAndWriterThread(
        f"live-{now}.mp4",
        max_queue_size,
        frame_decoder_thread.in_stream
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
            frame_bgr = detector_and_writer_thread.get_frame_to_display()
            if frame_bgr is None:
                quit()
            cv2.imshow('frame', frame_bgr)
            if cv2.waitKey(1) == ord('q'):
                quit()

    finally:
        for thread in threads:
            thread.stop()
        for thread in threads:
            thread.join()
