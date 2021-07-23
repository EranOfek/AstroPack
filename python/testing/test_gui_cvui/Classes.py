# =============== OpSCi - IOP v.1.0 - Classes - Ido Abergel ===============
#  =============== Includes ===============
import time
import threading
from threading import Event, Thread
import cv2
from datetime import datetime
import numpy as np
import os

#  =============== Classes define ===============

class RepeatedTimer:

    def __init__(self, interval, function, *args, **kwargs):
        self.interval = interval
        self.function = function
        self.args = args
        self.kwargs = kwargs
        self.start = time.time()
        self.event = Event()
        self.thread = Thread(target=self._target)
        self.thread.start()

    def _target(self):
        while not self.event.wait(self._time):
            self.function(*self.args, **self.kwargs)

    @property
    def _time(self):
        return self.interval - ((time.time() - self.start) % self.interval)

    def stop(self):
        self.event.set()
        self.thread.join()

#============================================================================
# Camera object based on shared-memory
# Functions are compatible with CSI_Camera class for easier testing
class SM_Camera:

    def __init__(self, enabled = True):
        # Initialize instance variables
        self.sm = None
        # The last captured image from the camera
        self.frame = None
        self.grabbed = False
        # The thread where the video capture runs
        self.read_thread = None
        self.read_lock = threading.Lock()
        self.running = False
        self.enabled = enabled


    def open(self, pipeline_string):
        try:
            if not self.sm:
                self.sm = SharedMemory()
            if self.enabled:
                self.sm.open(pipeline_string)
        except RuntimeError:
            self.sm = None
            print("Unable to open camera shared-memory")
            print("Pipeline: " + pipeline_string)
            return


    def start(self):
        if self.running:
            print('Video capturing is already running')
            return None
        # create a thread to read the camera image
        if self.enabled and self.sm is not None:
            self.running = True
            self.read_thread = threading.Thread(target=self.updateCamera)
            self.read_thread.start()
        return self


    def stop(self):
        self.running = False
        if self.enabled:
            self.read_thread.join()


    # This is the thread to read images from the camera
    # Return tuple grabbed, frame as cv2.VideoCapture.read()
    # See: https://stackoverflow.com/questions/60729170/python-opencv-converting-planar-yuv-420-image-to-rgb-yuv-array-format
    def updateCamera(self):
        counter = 0
        while self.running:
            try:
                # Poll queue for next buffer
                if self.sm:
                    buf, put_counter, width, height, flags = self.sm.get_buf()
                else:
                    time.sleep(0.01)
                    continue

                if buf and width > 0 and height > 0:

                    if put_counter > counter+1:
                        print('counter gap: {}'.format(put_counter-counter))

                    counter = put_counter

                    # Raw UYVY buffer
                    if flags & self.sm.BUFH_UYVY != 0:

                        # Read the buffer into 1D byte array and shape the array as you please
                        # Use flags to specify additional data such as image format (@Ido)
                        frame = np.frombuffer(buf, dtype=np.uint8)

                        # Convert buffer to numpy 2d array
                        # Notes:
                        #   - When using YUV color spaces, each pixel is 16-bits
                        #   - When using RGB color space, each pixel is 24-bits
                        #   - When using ABGR32 color space, each pixel is 32-bit
                        frame.shape = (height, width, 2)

                        # Convert color space using cv2.cvtColor(...)
                        # Convert MUST be done BEFORE resize
                        frame = cv2.cvtColor(frame, cv2.COLOR_YUV2BGR_Y422)

                        # Optional: Resize image
                        Resize = True
                        if Resize:
                            w = 256
                            h = 144
                            if w != width or h != height:
                                dim = (w, h)
                                frame = cv2.resize(frame, dim, interpolation=cv2.INTER_NEAREST)

                    # RGB scaled image
                    elif flags & self.sm.BUFH_MAT_RGB != 0:

                        # Read the buffer into 1D byte array and shape the array as you please
                        # Use flags to specify additional data such as image format (@Ido)
                        frame = np.frombuffer(buf, dtype=np.uint8)

                        # Convert buffer to numpy 2d array of RGB (24-bits per pixel)
                        frame.shape = (height, width, 3)

                        # Optional: Resize image
                        Resize = True
                        if Resize:
                            w = 256
                            h = 144
                            if w != width or h != height:
                                dim = (w, h)
                                frame = cv2.resize(frame, dim, interpolation=cv2.INTER_NEAREST)


                    else:
                        print('Unknown buffer firmat specified by flags: %08X', flags)

                    with self.read_lock:
                        self.grabbed = True
                        self.frame = frame

                # No frame from queue, use last frame that we have
                else:
                    time.sleep(0.025)

            except RuntimeError:
                print("Could not read image from camera")
            except:
                time.sleep(0.01)

        # FIX ME - stop and cleanup thread
        # Something bad happened


    def read(self):
        with self.read_lock:
            if self.frame is not None:
                frame = self.frame.copy()
                grabbed = self.grabbed
            else:
                frame = None
                grabbed = False
        return grabbed, frame


    def release(self):
        if self.sm is not None:
            self.sm.close()
            self.sm = None
        # Now kill the thread
        if self.read_thread is not None:
            self.read_thread.join()

    # Prepare shared-memory file name in /tmp folder
    def gstreamer_preview_pipeline(self, sensor_id):
        return '/tmp/smem_camera_%d' % sensor_id


    # Just for compatibility with CSI_Camera
    def gstreamer_record_pipeline(self, filename, runtime, sensor_id=0, sensor_mode=1, capture_width=3264, capture_height=1848,
                                  display_width=576, display_height=288, framerate=28):
        return ""

