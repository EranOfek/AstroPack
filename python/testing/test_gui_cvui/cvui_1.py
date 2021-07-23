# =============== OpSCi - IOP v.1.1 - GUI - Ido Abergel ===============
#  =============== Includes ===============
import numpy as np
import cv2
import cvui
import Classes
import time
from datetime import datetime
import subprocess
import logging
import threading

WINDOW_NAME = 'My Windows Name'

def main():
    #  =============== variables and object Init. ===============

    main_gui = np.zeros((300, 500, 3), np.uint8)
    pres_start = [20]
    pres_end = [60]
    patient_cnt = [1]
    test_time = [25]  # @Chen - Original [25]
    cvui.init(WINDOW_NAME)
    # options = cvui.TRACKBAR_DISCRETE | cvui.TRACKBAR_HIDE_SEGMENT_LABELS

    # ============= Main Loop =============

    while True:
        main_gui[:] = (49, 52, 49)

        if cvui.button(main_gui, 40, 60, 'Cameras Initialize'):
            print('Open cameras preview and adjustment')
            #run_preview(left_camera, right_camera, test_parameters)
            cvui.init(WINDOW_NAME)

        cvui.text(main_gui, 40, 100, 'Pressure:', 0.4)
        cvui.text(main_gui, 40, 125, 'Start:', 0.4)

        if cvui.counter(main_gui, 90, 120, pres_start):
            pres_start = [1] if pres_start[0] < 1 else pres_start
            pres_start = [70] if pres_start[0] > 70 else pres_start

        cvui.text(main_gui, 40, 155, 'End:', 0.4)
        if cvui.counter(main_gui, 90, 150, pres_end):
            pres_end = [10] if pres_end[0] < 10 else pres_end
            pres_end = [80] if pres_end[0] > 80 else pres_end

        cvui.text(main_gui, 40, 180, 'Patient Number:', 0.4)
        if cvui.counter(main_gui, 90, 205, patient_cnt):
            patient_cnt = [1] if patient_cnt[0] < 1 else patient_cnt

        if cvui.button(main_gui, 300, 60, 'Start Record'):
            print('Test is starting..')
            #run_record(left_camera, right_camera, file_name, test_parameters.test_time, test_parameters)
            cvui.init(WINDOW_NAME)

        cvui.text(main_gui, 300, 100, 'Test Time:', 0.4)
        if cvui.counter(main_gui, 300, 125, test_time):
            test_time = [1] if test_time[0] < 1 else test_time
            #test_parameters.test_time = test_time[0]

        cvui.text(main_gui, 10, 300 - 15, 'IOP test v1.0', 0.3)

        if cvui.button(main_gui, 500 - 75, 300 - 35, "&Quit"):
            break

        cvui.update()
        cv2.imshow(WINDOW_NAME, main_gui)
        # Check if ESC key was pressed
        if cv2.waitKey(2) == 27:
            break


if __name__ == '__main__':
    main()
