
# GCS Message
class MsgMaintenanceTaskResponse(MsgBase):

    # Constructor
    def __init__(self):
        self.msg_id = ''
        self.rcv_time = 0
        self.send_time = 0
        self.process_time = 0
        self.source = ''
        self.msg_type = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Load from XML
    def load_from_xml(self):
        pass

    # Save to XML
    def save_to_xml(self):
        pass

    def to_str(self):
        pass

    def from_str(self):
        pass

    def to_html(selfself):
        pass

    

'''


5-1	Common Parameters

5-1.1	SOC to GCS messages
Every message created by the SOC will include the following parameters:
1.	Date and Time of message creation
2.	Source of message: should be SOC
3.	SOC task ID: a unique ID for every task created by the SOC
4.	To which GCS message ID this message is relevant (if applicable):if the message is a response to a GCS request for a maintenance window, indicate which GCS request ID was the trigger
5.	Type of task: Imaging task, OBRD task, non-imaging activity request, etc.

5-1.2	GCS to SOC messages
Every message created by the GCS will include the following parameters:
1.	Date and Time of message creation
2.	Source of message: should be GCS
3.	GCS message ID: a unique ID for every message created by the GCS
4.	To which SOC task ID this message is relevant (if applicable): if the message is a response to a SOC request or task, indicate which SOC task ID was the trigger
5.	Type of message: response to task, maintenance activity schedule, etc

5-2	Imaging Tasks Parameters
Imaging tasks created at the SOC should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Time for beginning of imaging plan: Immediate (in case of ToO) or UTC time
2.	Total number of scientific targets in task
3.	For every scientific target:
a.	Start time of first exposure for current target
b.	Target coordinates (RA, Dec, Roll [optional] )
c.	Image exposure duration: default is 300 seconds
d.	Number of exposures (from 1 to unlimited) – TBD
e.	Start time of last exposure of current target - TBD
f.	Camera tiles to be active: 1 and/or 2 and/or 3and/or 4 (default – all 4 tiles) (TBD)

5-3	OBRD Tasks Parameters
OBRD tasks created at the SOC should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Start time of operation
2.	Type of OBRD task: download mode selection, specific images download, deletion mode, images to keep in OBRD, images to delete from OBRD
3.	For download mode selection, the options are:
a.	Online (default mode)
b.	Offline with priority to images being acquired at the moment
c.	Offline with priority to images in the OBRD
4.	For specific images download:
a.	Total number of images to download
b.	List of all images to be downloaded
c.	Part of image to be downloaded: full image, first half only, second half only, first and second half
5.	For deletion mode selection, the options are:
a.	Automatic (including inputs by SOC to delete specific images)
b.	Fully manual
6.	For images to keep in OBRD:
a.	List of images to be added to the "not to be erased" list.

This list shall contain up to 128 images
7.	For specific images to delete from OBRD:
a.	List of images to be deleted.
One delete command shall contain up to 124 images to be deleted

5-4	GCS request for S/C, G/S or payload maintenance activity
GCS maintenance activities requests should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Maintenance activity name: for example, orbit maneuver, RWA calibration, telescope focusing, ground equipment maintenance, etc.
2.	Start time of allowable time slot in which the activity can be performed
3.	End time of time allowable slot in which the activity can be performed
4.	Preferred start time of activity
5.	Duration of activity
6.	It is / is not possible to continue scientific imaging
If due to constrains, the maintenance activity has to be executed at a specific time, the start time of the window and the preferred time shall be the same, and the end time should be equal to the start time + the duration time.

5-5	Response to Imaging Tasks Parameters
Response to imaging tasks should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Imaging plan status: Approved / Not Approved / Approved with warnings
2.	Details (in case of status is "Not Approved"):
a.	Time of mission rule violation (only the first violation will be logged)
b.	Mission rule violated (for example, electricity, OBRD, telescope pointing, etc)
c.	Violation parameters: TBD (probably: OBRD filling status, EPS parameters violated, S/C angles with respect to celestial bodies)
3.	Details (in case of status is "Approved with warnings"):
a.	Number of warnings
b.	Time of warning #1
c.	Duration of warning #1
d.	Type of warning #1 (for example, negative power balance during imaging, no APM direct link to GCS, etc)
e.	Warning #1 detailed parameters: TBD
f.	…
g.	Time of warning #N
h.	Duration of warning #N
i.	Type of warning #N (for example, negative power balance during imaging, no APM direct link to GCS, etc)
j.	Warning #N detailed parameters: TBD

5-6	Response to OBRD Tasks Parameters
Response to OBRD tasks should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	OBRD task status: Approved / Not Approved
2.	Details (in case of status is "Not Approved"):
a.	Time of mission rule violation (only the first violation will be logged)
b.	Mission rule violated (TBD)
c.	Violation parameters: TBD

5-7	Response to GCS maintenance activity request
Response to GCS maintenance activity request should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	GCS maintenance activity request status: Approved / Not Approved
2.	Approved maintenance activity start time

'''
