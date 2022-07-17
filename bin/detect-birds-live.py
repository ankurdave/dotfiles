# UBUNTU:
# $ sudo apt update
# $ sudo apt install python3-pip python3-opencv
# If running Python 3.10, downgrade to Python 3.9 for compatibility with
# onnxruntime:
# $ sudo apt install software-properties-common
# $ sudo add-apt-repository ppa:deadsnakes/ppa
# $ sudo apt install python3.9 python3.9-dev python3.9-venv

# macOS:
# $ brew install ffmpeg

# Create virtualenv:
# $ python3 -m venv ~/venv-birds
# $ source ~/venv-python3.9/bin/activate
# $ python -m pip install --upgrade pip

# Install requirements:
# $ git clone https://github.com/ultralytics/yolov5
# $ pip install -r yolov5/requirements.txt
# $ pip install yolov5 av

import argparse
import av
import collections
import colorsys
import cv2
import datetime
import inspect
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

class BoundedBlockingQueue(object):
    def __init__(self, max_queue_size):
        self.lock = threading.Lock()
        self.has_new_item = threading.Condition(self.lock)
        self.has_free_space = threading.Condition(self.lock)
        # Guarded by `self.lock`.
        self.deque = collections.deque(maxlen=max_queue_size)

    def qsize(self):
        with self.lock:
            return len(self.deque)

    def put(self, elem):
        with self.lock:
            self.has_free_space.wait_for(lambda: len(self.deque) < self.deque.maxlen)
            self.deque.append(elem)
            self.has_new_item.notify()

    def put_nowait(self, elem):
        with self.lock:
            self.deque.append(elem)
            self.has_new_item.notify()

    def put_at_front(self, elem):
        with self.lock:
            self.deque.appendleft(elem)
            self.has_new_item.notify()

    def get(self):
        with self.lock:
            self.has_new_item.wait_for(lambda: len(self.deque) > 0)
            elem = self.deque.popleft()
            self.has_free_space.notify()
            return elem

    def get_nowait(self):
        with self.lock:
            if len(self.deque) > 0:
                elem = self.deque.popleft()
                self.has_free_space.notify()
                return elem
            else:
                raise queue.Empty

    def peek(self):
        with self.lock:
            self.has_new_item.wait_for(lambda: len(self.deque) > 0)
            return self.deque[-1]


class Producer(object):
    """Base class for threads that produce output that can be subscribed to by Consumers. """
    def __init__(self):
        super(Producer, self).__init__()

        self.subscribers = []

    def add_subscriber(self, subscriber):
        self.subscribers.append(subscriber)


class Consumer(object):
    """Base class for threads that consume output from a Producer. """
    def __init__(self, max_queue_size):
        super(Consumer, self).__init__()

        self.input_queue = BoundedBlockingQueue(max_queue_size=max_queue_size)

    def enqueue(self, elem, blocking):
        """Adds `elem` as input to this thread. If `input` is None, shuts down this thread.

        If the input queue is full and `blocking` is true, blocks until space is available in the
        queue. If the input queue is false and `blocking` is false, drops the oldest input frame,
        enqueues the new input, and returns immediately.

        """
        if blocking:
            self.input_queue.put(elem)
        else:
            self.input_queue.put_nowait(elem)


interrupt = threading.Event()
def terminate_threads():
    print('Terminating threads...')
    global interrupt
    interrupt.set()


class FrameDecoderThread(threading.Thread, Producer):
    def __init__(self, args):
        threading.Thread.__init__(self, name='FrameDecoderThread')
        Producer.__init__(self)

        self.container = av.open(args.input_file)
        self.in_stream = self.container.streams.video[0]

        self.args = args

        self.shutting_down = threading.Event()

    def shutdown(self):
        """Ends decoding early, even if there is more input remaining."""
        print('Shutting down gracefully...')
        self.shutting_down.set()

    def run(self):
        global interrupt
        try:
            for packet in self.container.demux(self.in_stream):
                if interrupt.is_set(): return
                if self.shutting_down.is_set(): return

                # We need to skip the "flushing" packets that `demux` generates.
                if packet.dts is None:
                    continue

                frames = packet.decode()
                for t in self.subscribers:
                    t.enqueue(
                        (packet, frames),
                        blocking=self.args.input_supports_backpressure,
                    )
        finally:
            for t in self.subscribers:
                t.enqueue(None, blocking=False)


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


class DetectorAndWriterThread(threading.Thread, Consumer, Producer):
    def __init__(self, out_stream_template, args):
        threading.Thread.__init__(self, name='DetectorAndWriterThread')
        Consumer.__init__(self, max_queue_size=args.max_detect_queue_size)
        Producer.__init__(self)

        self.model = yolov5.load(args.model_file)
        if args.confidence_threshold is not None:
            self.model.conf = args.confidence_threshold

        if args.output_dir is not None:
            global start_time
            output_file = os.path.join(args.output_dir, f"live-{start_time}.mp4")
            self.chunked_writer = ChunkedVideoWriter(output_file, out_stream_template)
        else:
            self.chunked_writer = NullVideoWriter()

        def hsv_to_bgr(h, s, v):
            return 255 * np.array(colorsys.hsv_to_rgb(h, s, v)[::-1])
        self.prediction_colors_bgr = [hsv_to_bgr(h, 0.5, 0.8)
                                      for h in np.linspace(0, 1, len(self.model.names))]

        self.args = args

    def run(self):
        global interrupt
        packet_idx = 0
        should_write_chunk = False
        try:
            while True:
                if interrupt.is_set(): return

                elem = self.input_queue.get()
                if elem is None:
                    return

                packet_idx += 1
                packet, frames = elem

                qsize = self.input_queue.qsize()
                if packet.is_keyframe:
                    self.chunked_writer.flush(should_write_chunk)
                    should_write_chunk = False

                self.chunked_writer.append(packet)
                if len(frames) == 0: continue

                # When the queue starts to fill up, skip inference on some
                # frames to catch up.
                #
                # This is not necessary if the input supports backpressure. In
                # that case we can let the queue fill up, which will slow down
                # the rate of input.
                if (not args.input_supports_backpressure
                    and (qsize > random.randint(0, args.max_detect_queue_size - 1)
                         and not packet.is_keyframe)):
                    continue

                if packet_idx % self.args.infer_every_frames != 0: continue

                packet_has_bird = False
                for frame in frames:
                    frame_arr = frame.to_ndarray(format='rgb24')
                    has_bird = self.__score_frame(frame_arr)
                    if has_bird:
                        packet_has_bird = True
                        should_write_chunk = True
                        break
        finally:
            for t in self.subscribers:
                t.enqueue(None, blocking=False)

    def __score_frame(self, frame_rgb):
        start_time = time.time()
        results = self.model(frame_rgb)
        infer_ms = (time.time() - start_time) * 1000

        labels, cord = results.xyxyn[0][:, -1].numpy(), results.xyxyn[0][:, :-1].numpy()

        if not self.args.headless:
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
                prediction_weight = int(np.interp(score, [self.model.conf, 1.0], [1, 20]))
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

            for t in self.subscribers:
                t.enqueue((frame_bgr, frame_rgb), blocking=False)

        if not self.args.quiet and len(labels) > 0:
            objects = [(self.model.names[int(labels[i])], f'{cord[i][4]:.2f}') for i in range(len(labels))]
            print(f"{infer_ms:.0f} ms, {1000/infer_ms:.0f} FPS, {self.input_queue.qsize()} qsz: {objects}")

        return len(labels) > 0

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


class DisplayMainThread(Consumer):
    def __init__(self, args):
        Consumer.__init__(self, max_queue_size=1)
        self.args = args

        os.makedirs(args.screenshot_dir, exist_ok=True)

    def start(self):
        frame_bgr, orig_frame_rgb = None, None
        try:
            while True:
                if frame_bgr is None:
                    # We haven't received the first frame yet. Block while we wait for it.
                    elem = self.input_queue.get()
                else:
                    try:
                        elem = self.input_queue.get_nowait()
                    except queue.Empty:
                        # No new element. Keep showing the previous one.
                        pass
                if elem is None:
                    return
                frame_bgr, orig_frame_rgb = elem
                cv2.imshow('frame', frame_bgr)
                key = cv2.waitKey(16)
                if key == ord('q'):
                    frame_decoder_thread.shutdown()
                    return
                elif key == ord('s'):
                    global start_time
                    screenshot_path = os.path.join(
                        self.args.screenshot_dir,
                        f"shot-{start_time}-{uuid.uuid4()}.jpg",
                    )
                    cv2.imwrite(screenshot_path, cv2.cvtColor(orig_frame_rgb, cv2.COLOR_RGB2BGR))
                    print(f"Saved frame to {screenshot_path}")
        finally:
            cv2.destroyAllWindows()

    def join(self):
        pass


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=inspect.cleandoc(
        """
        Decode a video stream, run inference on each frame, display inference
        results on screen, and write matching frames to a file.
        """
    ))
    parser.add_argument('--input_file', type=str, help='input video stream', required=True)
    parser.add_argument('--input_supports_backpressure', action='store_true',
                        help='if we can rate-limit decoding in case inference cannot keep up')

    parser.add_argument('--model_file', type=str, help='model for inference', required=True)
    parser.add_argument('--confidence_threshold', type=float, default=0.6,
                        help='inference confidence threshold [0.0, 1.0]')
    parser.add_argument('--infer_every_frames', type=int, default=1,
                        help='run inference every N frames to save energy')
    parser.add_argument('--max_detect_queue_size', type=int, default=50,
                        help='max number of frames that can be buffered waiting for detection')

    parser.add_argument('--output_dir', type=str, help='output video directory')

    parser.add_argument('--screenshot_dir', type=str, default='.', help='screenshot image directory')

    parser.add_argument('--headless', action='store_true',
                        help='do not display inference results on screen')
    parser.add_argument('--quiet', action='store_true',
                        help='do not print inference results to stdout')
    args = parser.parse_args()

    threads = []

    frame_decoder_thread = FrameDecoderThread(args)
    threads.append(frame_decoder_thread)

    detector_and_writer_thread = DetectorAndWriterThread(
        frame_decoder_thread.in_stream,
        args
    )
    frame_decoder_thread.add_subscriber(detector_and_writer_thread)
    threads.append(detector_and_writer_thread)

    display_main_thread = DisplayMainThread(args)
    detector_and_writer_thread.add_subscriber(display_main_thread)
    threads.append(display_main_thread)

    sigint_count = 0
    default_sigint_handler = signal.getsignal(signal.SIGINT)
    def stop_threads_on_sigint(sig, frame):
        global sigint_count
        sigint_count += 1
        if sigint_count == 1:
            frame_decoder_thread.shutdown()
        else:
            terminate_threads()
            default_sigint_handler()
    signal.signal(signal.SIGINT, stop_threads_on_sigint)

    global start_time
    start_time = datetime.datetime.now().strftime("%Y-%m-%dT%H%M%S")

    # `display_main_thread` runs synchronously in the main thread, so we must
    # start it last to avoid a deadlock.
    for thread in threads:
        thread.start()

    for thread in threads:
        thread.join()
