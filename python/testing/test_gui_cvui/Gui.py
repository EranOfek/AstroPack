# =============== OpSCi - IOP v.1.1 - GUI - Ido Abergel ===============
#  =============== Includes ===============
import numpy as np
import cv2
import cvui
import Classes
import serial
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

WINDOW_NAME = 'CvUi'

ser = None

# @Chen - True to use Shared-Memory camera
SHAREDMEM_CAMERA = True

#  =============== CNN loading. ===============

try:
    print('Loading Segmentation Network..')
    sess = ort.InferenceSession("segNet_5.1_opt.onnx")
    print('Segmentation Network Loaded')
    ser = serial.Serial("/dev/ttyACM0", 115200)
    print("connected to: " + ser.portstr)
    # print('Loading Vessels Network..')
    # sess_ves = ort.InferenceSession("ves_net_17.1_out.onnx")
    # print('Vessels Network Loaded')
except Exception as e:
    print(e)
    print('Problem loading networks')


#  =============== Functions definitions ==============

def run_record(left_camera, right_camera, filename, runtime, test_parameters):
    #  =============== variables and windows Init. ===============
    cv2.namedWindow("CSI Cameras", cv2.WINDOW_AUTOSIZE)
    cv2.waitKey(5)
    options = cvui.TRACKBAR_DISCRETE | cvui.TRACKBAR_HIDE_SEGMENT_LABELS
    main_gui = np.zeros((350, 570, 3), np.uint8)
    cvui.init("Testing Window")
    with_AI = [False]
    sender_thread = threading.Thread(target=start_sender)
    reciever_thread_left = threading.Thread(target=start_reciever_left, args=(left_camera,))
    reciever_thread_right = threading.Thread(target=start_reciever_right, args=(right_camera,))

    # ============= Cameras Init. =============
    try:

        # Run preview BEFORE starting the threads, so shared-memory will be created by C++ processes
        # before openning it from Python
        subprocess.Popen('./record.sh %d' %(test_parameters.test_time), shell=True)
        time.sleep(1)

        reciever_thread_left.start()
        time.sleep(1)
        reciever_thread_right.start()
        time.sleep(1)

        logging.info("Main    : Starting streaming thread")
        # sender_thread.start()

        logging.info("Main    : wait for the thread to finish")
        # sender_thread.join()
        logging.info("Main    : Live")
        time.sleep(1)
        left_camera.start()
        right_camera.start()

        grabbed_left = True
        grabbed_right = True

        # reciever_thread.join()

        # right_camera.open(
        #     Classes_agx.CSI_Camera.gstreamer_preview_pipeline(right_camera, sensor_id=1, sensor_mode=1,
        #                                                   flip_method=2, display_height=288, display_width=512))
        # right_camera.start()

    except Exception as e:
        print(e)
        print('Problem loading Cameras')
        return
    # ============= External sensors - arduino =============
    try:
        if ser is not None:
            ser.write(b'mode 1\n')  # write a string
            time.sleep(0.05)
            ser.write(b't_pres 60\n')  # write a string
            time.sleep(0.05)
            ser.write(b'e_pres 20\n')
            time.sleep(0.05)
            ser.write(b'on 1\n')
            time.sleep(0.05)
            test_parameters.pressure_on = True
    except Exception as e:
        print(e)

    # ser = serial.Serial("/dev/ttyUSB0", 115200)
    # pressure_file = open(filename + ".txt", "w+")
    # print("connected to: " + ser.portstr)

    # ============= Time stamp =============
    # time.sleep(0.2)
    video_time_start = time.time()

    # ============= additional check. =============

    # if not left_camera.video_capture.isOpened() or not right_camera.video_capture.isOpened():
    #     # Cameras did not open, or no camera attached
    #     print("Unable to open any cameras")
    #     # TODO: Proper Cleanup
    #     SystemExit(0)

    # ============= pre-allocations =============
    blank_proc = np.zeros((144, 256), np.uint8)
    # blank_for_info = np.stack((blank_proc, blank_proc, blank_proc), -1)
    pupils_masks = np.zeros((144, 512, 3), np.uint8)
    cv2.circle(pupils_masks, (128, 130), 50, (0, 255, 0), 1)
    cv2.circle(pupils_masks, (128 + 256, 130), 50, (0, 255, 0), 1)

    # ============= Interrupts timers Init. =============

    # focusing_interrupt = Classes_agx.RepeatedTimer(0.5, focusing, test_parameters)
    # pressure_interrupt = RepeatedTimer(0.5, focusing)
    # serial_port_interrupt = None
    serial_port_interrupt = Classes_agx.RepeatedTimer(0.035, serial_handler, test_parameters)

    # ============= Main Loop =============
    left_grabbed = True
    right_grabbed = left_grabbed
    # right_grabbed, _ = right_camera.read()
    time.sleep(0.1)
    count = 0
    while (time.time()-video_time_start) < test_parameters.test_time+3:

        grabbed_left, left_frame = left_camera.read()
        grabbed_right, right_frame = right_camera.read()

        full_time = time.time()
        try:
            grabbed_left, left_frame = left_camera.read()
            grabbed_right, right_frame = right_camera.read()

            if not grabbed_left or not grabbed_right:
                time.sleep(0.1)
                continue

            # right_frame = left_frame  # for testing with one camera

            camera_images = np.hstack((left_frame, right_frame))
            cv2.imshow("CSI Cameras", camera_images)

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
                if count % 10 == 0:
                    print('preprocess time : %.2f forward fps = %.2f' % (preprocess_time, 1.0 / (time.time() - seg_net_time)))

                classes_left = np.array(result_left)
                classes_right = np.array(result_right)

                pupilR = classes_right[0][0][0]
                pupilL = classes_left[0][0][0]


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

    # ============= stop all threads and close all windows =============
    if serial_port_interrupt is not None:
        serial_port_interrupt.stop()

    if ser is not None:
        ser.write(b'on 0\n')
        time.sleep(0.05)
        test_parameters.pressure_on = False

    # focusing_interrupt.stop()
    # left_camera.stop()
    # time.sleep(0.3)
    left_camera.release()
    time.sleep(0.3)
    # right_camera.stop()
    # time.sleep(0.3)
    right_camera.release()
    try:
        cv2.destroyWindow("CSI Cameras")
        # cv2.destroyWindow("Testing Window")
    except Exception as e:
        print(e)


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
    reciever_thread_left = threading.Thread(target=start_reciever_left, args=(left_camera,))
    reciever_thread_right = threading.Thread(target=start_reciever_right, args=(right_camera,))

    # ============= Cameras Init. =============
    try:

        # Run preview BEFORE starting the threads, so shared-memory will be created by C++ processes
        # before openning it from Python
        subprocess.Popen('./preview.sh 40', shell=True)
        time.sleep(1)

        # left_camera.open(
        #     Classes_agx.CSI_Camera.gstreamer_preview_pipeline(left_camera, sensor_id=0))
        #
        reciever_thread_left.start()
        time.sleep(1)
        reciever_thread_right.start()
        time.sleep(1)

        logging.info("Main    : Starting streaming thread")
        # sender_thread.start()

        logging.info("Main    : wait for the thread to finish")
        # sender_thread.join()
        logging.info("Main    : Live")
        time.sleep(1)
        left_camera.start()
        right_camera.start()

        grabbed_left = True
        grabbed_right = True

        # reciever_thread.join()

        # right_camera.open(
        #     Classes_agx.CSI_Camera.gstreamer_preview_pipeline(right_camera, sensor_id=1, sensor_mode=1,
        #                                                   flip_method=2, display_height=288, display_width=512))
        # right_camera.start()

    except Exception as e:
        print(e)
        print('Problem loading Cameras')
        return
    # ============= pre-allocations =============
    blank_proc = np.zeros((144, 256), np.uint8)
    # blank_for_info = np.stack((blank_proc, blank_proc, blank_proc), -1)
    pupils_masks = np.zeros((144, 512, 3), np.uint8)

    # ============= Time stamp =============
    # time.sleep(0.2)
    video_time_start = time.time()

    # ============= Interrupts timers Init. =============

    # focusing_interrupt = Classes_agx.RepeatedTimer(0.5, focusing, test_parameters)
    serial_port_interrupt = None

    # ============= External sensors - arduino =============
    try:
        ser.write(b'mode 0\n')  # write a string
        time.sleep(0.05)
        ser.write(b'on 1\n')
        time.sleep(0.05)
        test_parameters.pressure_on = True
    except Exception as e:
        print(e)

    # ============= additional check. =============
    #
    # if not left_camera.video_capture.isOpened() or not right_camera.video_capture.isOpened():
    #     # Cameras did not open, or no camera attached
    #     print("Unable to open any cameras")
    #     # TODO: Proper Cleanup
    #     SystemExit(0)

    # ============= Main Loop =============
    time.sleep(1)

    # while cv2.getWindowProperty("CSI Cameras", 0) >= 0:
    # @Chen - while condition modified
    count = 0
    while (time.time()-video_time_start) < test_parameters.test_time+3:

        grabbed_left, left_frame = left_camera.read()
        grabbed_right, right_frame = right_camera.read()
        # right_frame = left_frame            # for testing with one camera
        # left_frame = right_frame            # for testing with one camera

        if not grabbed_left or not grabbed_right:
            time.sleep(0.1)
            continue

        # @Chen
        '''
        if SHAREDMEM_CAMERA:            
            if not grabbed_left or not grabbed_right:
                time.sleep(0.05)
                continue
        else:
            break
        '''

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
            if count % 10 == 0:
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

            # ============= Gaze Indicators =============

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

        # if with_PRESSURE[0]:
        #     try:
        #         if not test_parameters.pressure_on:
        #             ser.write(b'on 1\n')
        #             time.sleep(0.05)
        #             test_parameters.pressure_on = True
        #             serial_port_interrupt = Classes.RepeatedTimer(0.035, serial_handler, test_parameters)
        #
        #     except Exception as e:
        #         print(e)
        #
        # else:
        #     if test_parameters.pressure_on:
        #         if serial_port_interrupt is not None:
        #             serial_port_interrupt.stop()
        #         try:
        #             ser.write(b'on 0\n')
        #             time.sleep(0.05)
        #             test_parameters.pressure_on = False
        #         except Exception as e:
        #             pass
                    # print('Port unavailable')
                # if serial_port_interrupt is not None:
                #     serial_port_interrupt.stop()
                #     time.sleep(0.05)
                #     serial_port_interrupt = None

        # ============= calibration gui handler. =============

        main_gui[:] = (49, 52, 49)
        # cvui.text(main_gui, 20, 5, 'Calibration:', 0.5)
        # cvui.text(main_gui, 20, 30, 'Left camera focus', 0.4)
        # if cvui.trackbar(main_gui, 20, 45, 150, test_parameters.focusing_val_left, 10, 990, 3, '%.0Lf', options, 10):
        #     test_parameters.focus_left_updated = 1
        #
        # cvui.text(main_gui, 20, 105, 'Right camera focus', 0.4)
        # if cvui.trackbar(main_gui, 20, 120, 150, test_parameters.focusing_val_right, 10, 990, 3, '%.0Lf', options, 10):
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

    # ============= stop and release all =============
    if serial_port_interrupt is not None:
        serial_port_interrupt.stop()
    if ser is not None:
        ser.write(b'on 0\n')
        time.sleep(0.05)
        test_parameters.pressure_on = False

    # focusing_interrupt.stop()
    # left_camera.stop()
    # time.sleep(0.3)
    left_camera.release()
    time.sleep(0.3)
    # right_camera.stop()
    # time.sleep(0.3)
    right_camera.release()
    cv2.destroyWindow("CSI Cameras")
    cv2.destroyWindow("Calibration Window")


def serial_handler(test_parameters):
    global ser
    try:
        serial_line = ser.readline().decode("utf-8")
        # serial_line = serial_line.decode("utf-8")  # ser.readline returns a binary, convert to string
        msg_split = serial_line.split()
        msg_split = msg_split[0].split(",")
        test_parameters.target_pres = int(msg_split[0])
        test_parameters.curr_pres = int(msg_split[1])
        # print(serial_line)
    except Exception as e:
        print(e)


def pre_process(image, height, width, depth):
    frame = global_standardization(image, depth)

    image_resized_norm = (
            (frame - frame.min()) * (1 / (frame.max() - frame.min()) * 255)).astype(
        'float32')

    if depth == 3:
        image_resized_norm = np.transpose(image_resized_norm, (2, 0, 1))

    img32n_reshape = np.reshape(image_resized_norm, (1, depth, height, width))

    return img32n_reshape


def global_standardization(image, depth):
    pixels = np.array(image)

    if depth == 3:
        pixels = cv2.cvtColor(pixels, cv2.COLOR_BGR2RGB)

    pixels = pixels.astype('float32')
    mean, std = pixels.mean(), pixels.std()

    pixels = (pixels - mean) / std

    return pixels


def start_sender():
    process = subprocess.Popen('./execute_2cam_4k.sh 20', shell=True)
    time.sleep(20)


def start_reciever_left(left_camera):

    if SHAREDMEM_CAMERA:
        left_camera.open(
            Classes_agx.SM_Camera.gstreamer_preview_pipeline(left_camera, sensor_id=0))

    else:
        left_camera.open(
            Classes_agx.CSI_Camera.gstreamer_preview_pipeline(left_camera, sensor_id=0))
    time.sleep(4)



def start_reciever_right(right_camera):

    if SHAREDMEM_CAMERA:
        right_camera.open(
            Classes_agx.SM_Camera.gstreamer_preview_pipeline(right_camera, sensor_id=1))
    else:
        right_camera.open(
            Classes_agx.CSI_Camera.gstreamer_preview_pipeline(right_camera, sensor_id=1))

    time.sleep(4)


def main():
    #  =============== variables and object Init. ===============
    global ser
    test_parameters = Classes_agx.TestingPar()
    main_gui = np.zeros((300, 500, 3), np.uint8)
    pres_start = [20]
    pres_end = [60]
    patient_cnt = [1]
    test_time = [25]  # @Chen - Original [25]
    cvui.init(WINDOW_NAME)
    # options = cvui.TRACKBAR_DISCRETE | cvui.TRACKBAR_HIDE_SEGMENT_LABELS

    # ============= Cameras Init. =============

    # @Chen
    if SHAREDMEM_CAMERA:
        left_camera = Classes_agx.SM_Camera()
        right_camera = Classes_agx.SM_Camera()

    else:
        left_camera = Classes_agx.CSI_Camera()
        right_camera = Classes_agx.CSI_Camera()

    # ============= Main Loop =============

    while True:
        main_gui[:] = (49, 52, 49)

        if cvui.button(main_gui, 40, 60, 'Cameras Initialize'):
            print('Open cameras preview and adjustment')
            run_preview(left_camera, right_camera, test_parameters)
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
            test_parameters.time_date = datetime.now().strftime("_%d.%m.%Y_%H:%M:%S")
            file_name = test_parameters.name + "_" + str(patient_cnt) + test_parameters.time_date
            run_record(left_camera, right_camera, file_name, test_parameters.test_time, test_parameters)
            cvui.init(WINDOW_NAME)

        cvui.text(main_gui, 300, 100, 'Test Time:', 0.4)
        if cvui.counter(main_gui, 300, 125, test_time):
            test_time = [1] if test_time[0] < 1 else test_time
            test_parameters.test_time = test_time[0]

        cvui.text(main_gui, 10, 300 - 15, 'IOP test v1.0', 0.3)

        if cvui.button(main_gui, 500 - 75, 300 - 35, "&Quit"):
            break

        cvui.update()
        cv2.imshow(WINDOW_NAME, main_gui)
        # Check if ESC key was pressed
        if cv2.waitKey(2) == 27:
            break

    if ser is not None:
        ser.close()
        ser = None


if __name__ == '__main__':
    main()
