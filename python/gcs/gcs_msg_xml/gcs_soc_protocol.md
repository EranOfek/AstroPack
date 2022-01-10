
## 3-1 Command and Control Channel

This channel is dedicated for the messages between the SOC at Weizmann institute and
the GCS at IAI/MBT. All tasking and commands from the SOC to the GCS as well as
responses and maintenance activities requests will be provided using this channel.
Scheduling of non-imaging activities for the payload (decontamination, focusing, dark
current imaging, etc.) will be performed by e-mail and phone and not through this
channel. After a time slot is coordinated, the GCS will send a maintenance activity
request to the SOC as described in this section. Chapter 3-3 contains a short description
of these activities. Further details can be found in the MOCD.

## SOC -> GCS

The following is a list of the messages from the SOC to the GCS (to be thoroughly
detailed on the section 4):

1. "Acknowledge" automatic response to each GCS message representing the reception
of the message on the SOC side, and that the message is in the correct format, prior
to processing.

2. "Keep Alive" automatic and periodic message sent from the SOC to the GCS used
only to verify that the C&C channel is working.

3. Observational commands:
The SOC sends a plan containing all imaging operations. For each target, the plan
includes the targets coordinates, time of observation start, time of observation end or
number of images (TBD), exposure time, and which sensor tiles will be used.
The first imaging operation in the plan should start as close as possible to the time
the plan is sent to the GCS (TBF).
The last imaging operation in each plan should have no ending time in case a new
plan is not sent by the SOC (due to communication problems, for example)


4. OBRD commands:
By default, the OBRD is controlled autonomously by the DPSL, which manages
storage, downloading and erasing of images. The SOC can create OBRD commands
to alter the nominal operations as needed. There are several types of OBRD
commands that the SOC may send to the GCS.
a. Task for selecting between download modes (e.g., online, offline, and
priority).
b. Task for downloading specific images.
c. Task for selecting between image deletion modes (e.g. automatic or manual
deletion).
d. Task for selecting images to be kept on the OBRD (i.e., not to be deleted in
the automatic deletion mode).
e. Task for deletion of specific images from the OBRD in manual mode.

5. Retransmission of images from the GCS database:
Request for performing a retransmit of a specific set of images from the GCS
database to the SOC

6. Response to GCS request for S/C, GCS or payload maintenance activity:
If approved, the SOC sends a response to notify the GCS about the validity of the
selected time slot.

7. Request to receive from the GCS the current approved observing plan in the GCS
queue.


## GCS -> SOC

The following is a list of messages from the GCS to the SOC (GCS->SOC):

1. "Acknowledge" automatic response to each SOC message representing the reception
of the message on the GCS side, and that the message is in the correct format, prior
to processing. This includes response to SOC messages of the "keep alive" type.

2. Responses to SOC imaging tasks:
a. Accepted, possibly with warnings (e.g., no communication, negative power
consumption, neighboring satellites in line-of-site).
b. Rejected, with detailed errors (e.g., pointing limits, battery limits, priority
rejection – conflict with maintenance activities).

3. Responses to SOC OBRD commands (TBF):
a. Accepted
b. Rejected, with detailed errors

4. Approved observing plan currently in the GCS queue. (TBD)

5. Satellite, GCS and Payload scheduled operations window request (maintenance,
station keeping).

6. Response to SOC request for retransmission of images from the GCS database:
a. Accepted
b. Rejected, with detailed errors


The SOC is responsible to monitor the directory and pull new files to the SOC for further
processing. The SOC is also responsible for the deletion of imagery and telemetry files
from the Vector IT computer on its side, to avoid filling up the storage.
Future operational procedures will be agreed upon in order to decide regarding the
maintenance of the file system



## 4-1 Common Parameters

### 4-1.1 SOC to GCS messages

Every message created by the SOC will include the following parameters:

1. Date and Time of message creation
2. Source of message: should be SOC
3. SOC task ID: a unique ID for every task created by the SOC
4. To which GCS message ID this message is relevant (if applicable): if the message is
a response to a GCS request for a maintenance window, indicate which GCS request ID was the trigger
5. Type of task: Imaging task, OBRD task, non-imaging activity request, etc.


### 4-1.2 GCS to SOC messages

Every message created by the GCS will include the following parameters:
1. Date and Time of message creation
2. Source of message: should be GCS
3. GCS message ID: a unique ID for every message created by the GCS
4. To which SOC task ID this message is relevant (if applicable): if the message is a
response to a SOC request or task, indicate which SOC task ID was the trigger
5. Type of message: response to task, maintenance activity schedule, etc


## 4-2 Imaging Tasks Parameters

Imaging tasks created at the SOC should include the following parameters (in addition to the
common parameters mentioned in 4-1):

1. Time for beginning of imaging plan: Immediate (in case of ToO) or UTC time
2. Total number of scientific targets in task
3. For every scientific target:
- a. Start time of first exposure for current target
- b. Target coordinates (RA, Dec, Roll [optional, TBD])
- c. Image exposure duration: default is 300 seconds
- d. Number of exposures (from 1 to unlimited) – TBD
- e. Start time of last exposure of current target - TBD
- f. Camera tiles to be active: 1 and/or 2 and/or 3 and/or 4 (default – all 4 tiles) (TBD)


### 4-3 OBRD Tasks Parameters

OBRD tasks created at the SOC should include the following parameters (in addition to the
common parameters mentioned in 4-1):

1. Start time of operation

2. Type of OBRD task: download mode selection, specific images download, deletion
mode, images to keep in OBRD, images to delete from OBRD

3. For download mode selection, the options are:

a. Online (default mode)
b. Offline with priority to images being acquired at the moment
c. Offline with priority to images in the OBRD

4. For specific images download:
a. Total number of images to download
b. List of all images to be downloaded
c. Part of image to be downloaded: full image, first half only, second half only,
first and second half

5. For deletion mode selection, the options are:
a. Automatic (including inputs by SOC to delete specific images)
b. Fully manual

6. For images to keep in OBRD:
a. List of images to be added to the "not to be erased" list.
This list shall contain up to 128 images

7. For specific images to delete from OBRD:
a. List of images to be deleted.
One delete command shall contain up to 124 images to be deleted


## 4-4 GCS request for S/C, G/S or payload maintenance activity

GCS maintenance activities requests should include the following parameters (in addition to the
common parameters mentioned in 4-1):

1. Maintenance activity name: for example, orbit maneuver, RWA calibration, telescope
focusing, ground equipment maintenance, etc.
2. Start time of allowable time slot in which the activity can be performed
3. End time of time allowable slot in which the activity can be performed
4. Preferred start time of activity
5. Duration of activity
6. It is / is not possible to continue scientific imaging
If due to constrains, the maintenance activity has to be executed at a specific time, the start time
of the window and the preferred time shall be the same, and the end time should be equal to the
start time + the duration time.

## 4-5 Response to Imaging Tasks Parameters
Response to imaging tasks should include the following parameters (in addition to the common
parameters mentioned in 4-1):
1. Imaging plan status: Approved / Not Approved / Approved with warnings
2. Details (in case of status is "Not Approved"):

a. Time of mission rule violation (only the first violation will be logged)
b. Mission rule violated (for example, electricity, OBRD, telescope pointing, etc)
c. Violation parameters: TBD (probably: OBRD filling status, EPS parameters
violated, S/C angles with respect to celestial bodies)
3. Details (in case of status is "Approved with warnings"):
a. Number of warnings
b. Time of warning #1
c. Duration of warning #1
d. Type of warning #1 (for example, negative power balance during imaging, no
APM direct link to GCS, etc)
e. Warning #1 detailed parameters: TBD
f. …
g. Time of warning #N
h. Duration of warning #N
i. Type of warning #N (for example, negative power balance during imaging, no
APM direct link to GCS, etc)
j. Warning #N detailed parameters: TBD
4-6 Response to OBRD Tasks Parameters
Response to OBRD tasks should include the following parameters (in addition to the common
parameters mentioned in 4-1):
1. OBRD task status: Approved / Not Approved
2. Details (in case of status is "Not Approved"):
a. Time of mission rule violation (only the first violation will be logged)
b. Mission rule violated (TBD)
c. Violation parameters: TBD
4-7 Response to GCS maintenance activity request
Response to GCS maintenance activity request should include the following parameters (in
addition to the common parameters mentioned in 4-1):

1. GCS maintenance activity request status: Approved / Not Approved
2. Approved maintenance activity start time
4-8 Acknowledge response
All messages received at both sites shall be replied with an acknowledge message. This message
should contain the following parameters (in addition to the common parameters mentioned in 4-1):
1. Response: ACK / NACK
2. NACK Details
4-9 Keep alive periodic message
This message, originated in the SOC will have no additional information besides the common
parameters mentioned in 4-1.
4-10 Request for retransmission of images from GCS database
Requests for retransmission of images from the GCS database should include the following
parameters (in addition to the common parameters mentioned in 4-1):
1. Total number of images to transmit
2. List of all images to transmit
4-11 Request for current approved imaging plan in GCS queue
This message will have no additional information besides the common parameters mentioned in
4-1.
4-12 Approved imaging plan in the GCS queue
This message will contain the last imaging plan which was received at the GCS and approved.
4-13 Response to retransmission of images from GCS database
Response to retransmission of images from GCS database should include the following parameters
(in addition to the common parameters mentioned in 4-1):
1. Retransmission task status: Approved / Not Approved
2. Details (in case of status is "Not Approved")

5. S/C TELEMETRY FILES
S/C and Payload telemetry will be transferred to the SOC for use as input to the mission planning
system and for scientific use. The specific telemetry parameters which will be sent to the SOC are
still TBD.
Telemetry is constantly downloaded to the GCS. Each telemetry packet or parameter is collected
by the DPSL at a specific frequency, ranging from once every second to once every several minutes
or tens of minutes.
Every 5 minutes (TBR), the SOCM will create a text file containing the values received during the
last 5 minutes for every one of the selected telemetry parameters. This text file contains the
parameter code, time of sample (in UTC format) and value.
Parameter code format: x### or xx##, where "x" and "xx" are letters which indicate the subsystem
from which the parameter was collected, and "###" and "##" are numbers.
The telemetry text files will be transmitted through the data channel.




