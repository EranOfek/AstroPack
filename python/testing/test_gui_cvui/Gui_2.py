# =============== OpSCi - IOP v.1.1 - GUI - Ido Abergel ===============
#  =============== Includes ===============
import numpy as np
import cv2
import cvui

# from datetime import datetime
# import threading
# from threading import Event, Thread
import time
import onnxruntime as ort
from datetime import datetime
import subprocess
import logging
import threading

#  =============== Global variables ===============

WINDOW_NAME = 'Windows Name'

#  =============== Functions definitions ==============

def run_record(left_camera, right_camera, filename, runtime, test_parameters):

    cv2.namedWindow("CSI Cameras", cv2.WINDOW_AUTOSIZE)
    cv2.waitKey(5)
    options = cvui.TRACKBAR_DISCRETE | cvui.TRACKBAR_HIDE_SEGMENT_LABELS
    main_gui = np.zeros((350, 570, 3), np.uint8)
    cvui.init("Testing Window")

    # ============= pre-allocations =============
    blank_proc = np.zeros((144, 256), np.uint8)

    pupils_masks = np.zeros((144, 512, 3), np.uint8)
    cv2.circle(pupils_masks, (128, 130), 50, (0, 255, 0), 1)
    cv2.circle(pupils_masks, (128 + 256, 130), 50, (0, 255, 0), 1)



                # ============= Pupil - right =============
                pupilR = cv2.normalize(pupilR, None, alpha=0, beta=255, norm_type=cv2.NORM_MINMAX, dtype=cv2.CV_8UC1)
                close_pupilR = cv2.morphologyEx(pupilR, cv2.MORPH_CLOSE, test_parameters.kernel)
                _, pupil_threshR = cv2.threshold(close_pupilR, 127, 255, cv2.THRESH_BINARY)

                pupilR_center = cv2.moments(pupil_threshR)
                pupilR_center_x = int(pupilR_center["m10"] / pupilR_center["m00"])
                pupilR_center_y = int(pupilR_center["m01"] / pupilR_center["m00"])

                if ((test_parameters.proc_width / 2 + 20) > pupilR_center_x > (test_parameters.proc_width / 2 - 20)
                        and (45 + 20) > pupilR_center_y > (45 - 20)):
                    test_parameters.right_eye_ind = True
                else:
                    test_parameters.right_eye_ind = False

                # ============= Pupil - left =============
                pupilL = cv2.normalize(pupilL, None, alpha=0, beta=255, norm_type=cv2.NORM_MINMAX, dtype=cv2.CV_8UC1)
                close_pupilL = cv2.morphologyEx(pupilL, cv2.MORPH_CLOSE, test_parameters.kernel)
                _, pupil_threshL = cv2.threshold(close_pupilL, 127, 255, cv2.THRESH_BINARY)

                pupilL_center = cv2.moments(pupil_threshL)
                pupilL_center_x = int(pupilL_center["m10"] / pupilL_center["m00"])
                pupilL_center_y = int(pupilL_center["m01"] / pupilL_center["m00"])

                if ((test_parameters.proc_width / 2 + 20) > pupilL_center_x > (test_parameters.proc_width / 2 - 20)
                        and (45 + 20) > pupilL_center_y > (45 - 20)):
                    test_parameters.left_eye_ind = True
                else:
                    test_parameters.left_eye_ind = False

                if test_parameters.right_eye_ind:
                    pupilR_mask = np.stack((blank_proc, pupil_threshR, blank_proc), -1)
                    if ser is not None: ser.write(b'right 1\n')
                else:
                    pupilR_mask = np.stack((blank_proc, blank_proc, pupil_threshR), -1)
                    if ser is not None: ser.write(b'right 0\n')
                if test_parameters.left_eye_ind:
                    pupilL_mask = np.stack((blank_proc, pupil_threshL, blank_proc), -1)
                    if ser is not None: ser.write(b'left 1\n')
                else:
                    pupilL_mask = np.stack((blank_proc, blank_proc, pupil_threshL), -1)
                    if ser is not None: ser.write(b'left 0\n')

                pupils_masks = np.hstack((pupilL_mask, pupilR_mask))
                cv2.circle(pupils_masks, (128, 45), 20, (255, 255, 255), 1)
                cv2.circle(pupils_masks, (128 + 256, 45), 20, (255, 255, 255), 1)

            # ============= Pressure sensors reading. =============
            # if ser is not None:
            #     try:
            #         if not test_parameters.pressure_on:
            #             serial_port_interrupt = Classes.RepeatedTimer(0.03, serial_handler, test_parameters)
            #         # serial_line = ser.readline().decode("utf-8")
            #         # msg_split = serial_line.split()
            #         # msg_split = msg_split[0].split(",")
            #         # test_parameters.target_pres = int(msg_split[0])
            #         # test_parameters.curr_pres = int(msg_split[1])
            #     except Exception as e:
            #         print(e)

            # ============= Testing gui handler. =============
            main_gui[:] = (49, 52, 49)
            # cvui.text(main_gui, 20, 5, 'Focus:', 0.5)
            # cvui.text(main_gui, 20, 30, 'Left camera focus', 0.4)
            # if cvui.trackbar(main_gui, 20, 45, 150, test_parameters.focusing_val_left, 10, 990, 3, '%.0Lf', options,
            #                  10):
            #     test_parameters.focus_left_updated = 1
            #
            # cvui.text(main_gui, 20, 105, 'Right camera focus', 0.4)
            # if cvui.trackbar(main_gui, 20, 120, 150, test_parameters.focusing_val_right, 10, 990, 3, '%.0Lf', options,
            #                  10):
            #     test_parameters.focus_right_updated = 1

            cvui.text(main_gui, 245, 5, 'Left Eye:', 0.4)
            cvui.text(main_gui, 245, 20, 'Gaze:', 0.4)
            cvui.text(main_gui, 285, 20, 'Good' if test_parameters.left_eye_ind else 'Bad', 0.4,
                      0x00ff00 if test_parameters.left_eye_ind else 0xff0000)

            cvui.text(main_gui, 345, 5, 'Right Eye:', 0.4)
            cvui.text(main_gui, 345, 20, 'Gaze:', 0.4)
            cvui.text(main_gui, 385, 20, 'Good' if test_parameters.right_eye_ind else 'Bad', 0.4,
                      0x00ff00 if test_parameters.right_eye_ind else 0xff0000)

            if cvui.checkbox(main_gui, 245, 45, 'Show AI detection', with_AI):
                try:
                    main_gui[170:170 + 144, 275 - 256:275 + 256, :] = pupils_masks
                except:
                    cvui.text(main_gui, 165, 275, 'Problem with showing detection', 0.4, 0xff0000)

            # ============= insert informative text into image =============
            cvui.printf(main_gui, 245, 65, 0.4, 0x00ff00, 'Test timer: %.2f [sec]', (time.time() - video_time_start))
            cvui.printf(main_gui, 245, 85, 0.4, 0x00ff00, 'Processing rate: %.2f [fps]',
                        1.0 / (time.time() - full_time))

            cvui.printf(main_gui, 245, 105, 0.4, 0x00ff00, 'Target Pressure: %d [mmHg]', test_parameters.target_pres)
            cvui.printf(main_gui, 245, 125, 0.4, 0x00ff00, 'Current Pressure: %d [mmHg]', test_parameters.curr_pres)

            if cvui.button(main_gui, 570 - 75, 350 - 35, 'Quit'):
                break

            cvui.update()
            cv2.imshow("Testing Window", main_gui)

            # Hold for a key
            keyCode = cv2.waitKey(2) & 0xFF
            # Stop the program on the ESC key
            if keyCode == 27:
                break

        except Exception as e:
            print(e)
            left_grabbed = False
            right_grabbed = False

        # print('fps = %.2f' % (1.0 / (time.time() - full_time)))


# This function will terminate after the preview is done, according to test_params.
def run_preview(left_camera, right_camera, test_parameters):
    #  =============== variables and windows Init. ===============
    cv2.namedWindow("CSI Cameras", cv2.WINDOW_AUTOSIZE)
    cv2.waitKey(5)
    options = cvui.TRACKBAR_DISCRETE | cvui.TRACKBAR_HIDE_SEGMENT_LABELS
    main_gui = np.zeros((350, 570, 3), np.uint8)
    cvui.init("Calibration Window")
    with_AI = [True]
    with_PRESSURE = [False]
    const_pres = [30]
    serial_line = []
    sender_thread = threading.Thread(target=start_sender)


    # ============= Cameras Init. =============
    try:


    # ============= pre-allocations =============
    blank_proc = np.zeros((144, 256), np.uint8)
    # blank_for_info = np.stack((blank_proc, blank_proc, blank_proc), -1)
    pupils_masks = np.zeros((144, 512, 3), np.uint8)

    # ============= Time stamp =============
    # time.sleep(0.2)
    video_time_start = time.time()

    count = 0
    while (time.time()-video_time_start) < test_parameters.test_time+1:

        grabbed_left, left_frame = left_camera.read()
        grabbed_right, right_frame = right_camera.read()
        # right_frame = left_frame            # for testing with one camera
        # left_frame = right_frame            # for testing with one camera

        if not grabbed_left or not grabbed_right:
            time.sleep(0.1)
            continue


        camera_images = np.hstack((left_frame, right_frame))
        cv2.imshow("CSI Cameras", camera_images)
        cv2.waitKey(1)

        if with_AI[0]:
            # ============= CNN segmentation =============
            preprocess_time = time.time()
            left_frame_to_feed = pre_process(cv2.resize(left_frame, (256, 144),
                                                        interpolation=cv2.INTER_NEAREST), 144, 256, 3)
            right_frame_to_feed = pre_process(cv2.resize(right_frame, (256, 144),
                                                         interpolation=cv2.INTER_NEAREST), 144, 256, 3)

            preprocess_time = (1.0 / (time.time() - preprocess_time))
            seg_net_time = time.time()
            result_left = sess.run(["softmax_out_Transpose2"], {"imageinput": left_frame_to_feed})
            result_right = sess.run(["softmax_out_Transpose2"], {"imageinput": right_frame_to_feed})

            count += 1
            if count % 100 == 0:
                print('preprocess time : %.2f forward fps = %.2f' % (preprocess_time, 1.0 / (time.time() - seg_net_time)))

            classes_left = np.array(result_left)
            classes_right = np.array(result_right)

            pupilR = classes_right[0][0][0]
            pupilL = classes_left[0][0][0]

            # ============= Pupil - right =============

            # pupilR = ((pupilR - pupilR.min()) * (1 / (pupilR.max() - pupilR.min()) * 255)).astype('uint8')
            pupilR = cv2.normalize(pupilR, None, alpha=0, beta=255, norm_type=cv2.NORM_MINMAX, dtype=cv2.CV_8UC1)
            # pupilR_gpu.upload(pupilR)
            # cleaned_pupil = morphology.remove_small_objects(pupil, min_size=100, connectivity=2)
            close_pupilR = cv2.morphologyEx(pupilR, cv2.MORPH_CLOSE, test_parameters.kernel)
            _, pupil_threshR = cv2.threshold(close_pupilR, 127, 255, cv2.THRESH_BINARY)

            pupilR_center = cv2.moments(pupil_threshR)
            pupilR_center_x = int(pupilR_center["m10"] / pupilR_center["m00"])
            pupilR_center_y = int(pupilR_center["m01"] / pupilR_center["m00"])

            if ((test_parameters.proc_width / 2 + 20) > pupilR_center_x > (test_parameters.proc_width / 2 - 20)
                    and (45 + 20) > pupilR_center_y > (45 - 20)):
                test_parameters.right_eye_ind = True
            else:
                test_parameters.right_eye_ind = False

            # ============= Pupil - left =============
            # pupilL = ((pupilL - pupilL.min()) * (1 / (pupilL.max() - pupilL.min()) * 255)).astype('uint8')
            pupilL = cv2.normalize(pupilL, None, alpha=0, beta=255, norm_type=cv2.NORM_MINMAX, dtype=cv2.CV_8UC1)

            # cleaned_pupil = morphology.remove_small_objects(pupil, min_size=100, connectivity=2)
            close_pupilL = cv2.morphologyEx(pupilL, cv2.MORPH_CLOSE, test_parameters.kernel)
            _, pupil_threshL = cv2.threshold(close_pupilL, 127, 255, cv2.THRESH_BINARY)

            pupilL_center = cv2.moments(pupil_threshL)
            pupilL_center_x = int(pupilL_center["m10"] / pupilL_center["m00"])
            pupilL_center_y = int(pupilL_center["m01"] / pupilL_center["m00"])

            if ((test_parameters.proc_width / 2 + 20) > pupilL_center_x > (test_parameters.proc_width / 2 - 20)
                    and (45 + 20) > pupilL_center_y > (45 - 20)):
                test_parameters.left_eye_ind = True
            else:
                test_parameters.left_eye_ind = False

            pupils_masks = np.hstack((pupilL_mask, pupilR_mask))
            cv2.circle(pupils_masks, (128, 45), 20, (255, 255, 255), 1)
            cv2.circle(pupils_masks, (128 + 256, 45), 20, (255, 255, 255), 1)


        # ============= calibration gui handler. =============

        main_gui[:] = (49, 52, 49)


        cvui.text(main_gui, 245, 5, 'Left Eye:', 0.4)
        cvui.text(main_gui, 245, 20, 'Gaze:', 0.4)
        cvui.text(main_gui, 285, 20, 'Good' if test_parameters.left_eye_ind else 'Bad', 0.4,
                  0x00ff00 if test_parameters.left_eye_ind else 0xff0000)

        cvui.text(main_gui, 345, 5, 'Right Eye:', 0.4)
        cvui.text(main_gui, 345, 20, 'Gaze:', 0.4)
        cvui.text(main_gui, 385, 20, 'Good' if test_parameters.right_eye_ind else 'Bad', 0.4,
                  0x00ff00 if test_parameters.right_eye_ind else 0xff0000)

        if cvui.checkbox(main_gui, 245, 45, 'Show AI detection', with_AI):
            try:
                main_gui[170:170 + 144, 275 - 256:275 + 256, :] = pupils_masks
            except:
                cvui.text(main_gui, 165, 275, 'Problem with showing detection', 0.4, 0xff0000)

        if cvui.checkbox(main_gui, 245, 65, 'Pressure Test', with_PRESSURE):
            if cvui.counter(main_gui, 445, 75, const_pres):
                const_pres = [1] if const_pres[0] < 1 else const_pres
                test_parameters.target_pres = const_pres[0]
                cmd = 't_pres %d\n' % (const_pres[0])
                if ser is not None: ser.write(cmd.encode())
            cvui.printf(main_gui, 245, 85, 0.4, 0x00ff00, 'Target Pressure: %d [mmHg]', test_parameters.target_pres)
            cvui.printf(main_gui, 245, 105, 0.4, 0x00ff00, 'Current Pressure: %d [mmHg]', test_parameters.curr_pres)

        if cvui.button(main_gui, 570 - 75, 350 - 35, 'Quit'):
            test_parameters.focusing_val_right = test_parameters.focusing_val_right
            test_parameters.focusing_val_left = test_parameters.focusing_val_left
            break

        cvui.update()
        cv2.imshow("Calibration Window", main_gui)

        # This also acts as
        keyCode = cv2.waitKey(1) & 0xFF
        # Stop the program on the ESC key
        if keyCode == 27:
            break

    cv2.destroyWindow("CSI Cameras")
    cv2.destroyWindow("Calibration Window")




def start_sender():
    process = subprocess.Popen('./execute_2cam_4k.sh 20', shell=True)
    time.sleep(20)


def main():

    main_gui = np.zeros((300, 500, 3), np.uint8)
    cvui.init(WINDOW_NAME)

    while True:
        main_gui[:] = (49, 52, 49)

        if cvui.button(main_gui, 40, 60, 'Cameras Initialize'):
            print('Open cameras preview and adjustment')
            run_preview()
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

            run_record()
            cvui.init(WINDOW_NAME)

        # Update test_time
        cvui.text(main_gui, 300, 100, 'Test Time:', 0.4)
        if cvui.counter(main_gui, 300, 125, test_time):
            test_time = [1] if test_time[0] < 1 else test_time

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

