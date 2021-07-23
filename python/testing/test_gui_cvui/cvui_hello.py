
import numpy as np
import cv2
import cvui

WINDOW_NAME = 'CVUI Hello World!'

# Create a frame where components will be rendered to.
frame = np.zeros((200, 500, 3), np.uint8)

# Init cvui and tell it to create a OpenCV window, i.e. cv2.namedWindow(WINDOW_NAME).
cvui.init(WINDOW_NAME)

while True:
	# Fill the frame with a nice color
	frame[:] = (49, 52, 49)

	# Render UI components to the frame
	cvui.text(frame, 110, 80, 'Hello, world!')
	cvui.text(frame, 110, 120, 'cvui is awesome!')

	# Update cvui stuff and show everything on the screen
	cvui.imshow(WINDOW_NAME, frame)

	if cv2.waitKey(20) == 27:
		break

