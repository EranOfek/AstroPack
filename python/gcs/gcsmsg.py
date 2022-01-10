import xml.etree.cElementTree as ET
import xml.dom.minidom
from gcsbase import Component

'''
m_encoding = 'UTF-8'

root = ET.Element("data")
doc = ET.SubElement(root, "status", date="20210123")
ET.SubElement(doc, "name", name="john").text = "some value1"
ET.SubElement(doc, "class", name="abc").text = "some vlaue2"

dom = xml.dom.minidom.parseString(ET.tostring(root))
xml_string = dom.toprettyxml()
part1, part2 = xml_string.split('?>')

with open("FILE.xml", 'w') as xfile:
    xfile.write(part1 + 'encoding=\"{}\"?>\n'.format(m_encoding) + part2)
    xfile.close()
'''
# ============================================================================

# Common Messages Types
MSG_ACK_NACK = 'ACK'                    #
MSG_KEEP_ALIVE = 'KALIVE'               #

# SOC -> GCS Message Types
MSG_IMAGING_TASK_PARAMS = ''            #
MSG_OBRD_TASK_PARAMS = ''               #

# GCS -> SOC Message Types
MSG_IMAGING_TASK_RESPONSE = ''          #
MSG_OBRD_TASK_RESPONSE = ''             #


# ============================================================================
#
#                               Common Messages
#
# ============================================================================

# GCS Message

'''
4-1	Common Parameters

4-1.1	SOC to GCS messages
Every message created by the SOC will include the following parameters:
1.	Date and Time of message creation
2.	Source of message: should be SOC
3.	SOC task ID: a unique ID for every task created by the SOC
4.	To which GCS message ID this message is relevant (if applicable):if the message is a response to a GCS request for a maintenance window, indicate which GCS request ID was the trigger
5.	Type of task: Imaging task, OBRD task, non-imaging activity request, etc.

4-1.2	GCS to SOC messages
Every message created by the GCS will include the following parameters:
1.	Date and Time of message creation
2.	Source of message: should be GCS
3.	GCS message ID: a unique ID for every message created by the GCS
4.	To which SOC task ID this message is relevant (if applicable): if the message is a response to a SOC request or task, indicate which SOC task ID was the trigger
5.	Type of message: response to task, maintenance activity schedule, etc
'''

class MsgBase:

    # Constructor
    def __init__(self, type):
        # Data included in the message
        self.msg_id = ''        # Unique ID for every message created by the GCS/SOC
        self.msg_type = ''      # Message type
        self.source = ''        # Source of message: 'SOC' or 'GCS'
        self.msg_time = 0       # Date and Time of message creation
        self.task_id = ''       # SOC task ID: a unique ID for every task created by the SOC (if applicable)
        self.task_type = ''     # Type of task: Imaging task, OBRD task, non-imaging activity request, etc.
        self.src_msg_id = ''    # To which GCS message ID this message is relevant (if applicable)

        # Meta-data
        self.rcv_time = 0       # Date and time when message was received
        self.send_time = 0      # Date and time when message was sent
        self.process_time = 0   # Date and time when message was processed
        self.db_pk = ''         # Primary key in Message table

        # XML
        self.encoding = 'UTF-8'
        if type != '':
            self.xml_root = ET.Element(type)
        else:
            self.xml_root = None


    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Load from XML
    def read_xml(self):
        pass

    # Save to XML
    def write_xml(self):
        pass

    # Load from XML
    def load_xml(self, filename):
        pass

    # Save to XML
    def save_xml(self, filename):
        dom = xml.dom.minidom.parseString(ET.tostring(self.xml_root))
        xml_string = dom.toprettyxml()
        part1, part2 = xml_string.split('?>')

        with open(filename, 'w') as xml_file:
            xml_file.write(part1 + 'encoding=\"{}\"?>\n'.format(self.xml_encoding) + part2)
            xml_file.close()


    # Convert to string
    def to_str(self):
        pass

    # Convert from string
    def from_str(self):
        pass

    # Convert to HTML (for debugging/logging)
    def to_html(self):
        pass


# ============================================================================
# 4-8 Acknowledge response
# All messages received at both sites shall be replied with an acknowledge message. This message
# should contain the following parameters (in addition to the common parameters mentioned in 4-1):
class MsgAck(MsgBase):

    # Constructor
    def __init__(self):
        self.ack = ''       # 'ACK', 'NACK'
        self.details = ''   # NACK Details

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


# ============================================================================
# 4-9 Keep alive periodic message
# This message, originated in the SOC will have no additional information besides the common
# parameters mentioned in 4-1.
class MsgKeepAlive(MsgBase):

    # Constructor
    def __init__(self):
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


# ============================================================================
#
#                               SOC to GCS Messages
#
# ============================================================================

'''
4-2	Imaging Tasks Parameters
Imaging tasks created at the SOC should include the following parameters 
(in addition to the common parameters mentioned in 4-1):
1.	Time for beginning of imaging plan: Immediate (in case of ToO) or UTC time
2.	Total number of scientific targets in task
3.	For every scientific target:
    a.	Start time of first exposure for current target
    b.	Target coordinates (RA, Dec, Roll [optional])
    c.	Image exposure duration: default is 300 seconds
    d.	Number of exposures (from 1 to unlimited) – TBD
    e.	Start time of last exposure of current target - TBD
    f.	Camera tiles to be active: 1 and/or 2 and/or 3and/or 4 (default – all 4 tiles) (TBD)
'''

class TargetCoordinates:
    def __init__(self):
        self.RA = 0
        self.Dec = 0
        self.Roll = 0


# GCS Message
class MsgImagingTask(MsgBase):

    # Constructor
    def __init__(self):
        self.start_time = 0     # Time for beginning of imaging plan: Immediate (in case of ToO) or UTC time
        self.target_count = 0   # Total number of scientific targets in task
        self.target_list = []   # For every scientific target: MsgImagingTaskTarget


    class MsgImagingTaskTarget:

        # Constructor
        def __init__(self):
            self.start_time = 0                         # Start time of first exposure for current target
            self.target_coord = TargetCoordinates()     # Target coordinates (RA, Dec, Roll [optional])
            self.exp_duration = 0                       # Image exposure duration: default is 300 seconds
            self.num_exp = 0                            # Number of exposures (from 1 to unlimited) – TBD
            self.last_exp_time = 0                      # Start time of last exposure of current target - TBD
            self.tiles = 0                              # Camera tiles to be active: 1 and/or 2 and/or 3and/or 4 (default – all 4 tiles) (TBD)

    # Load from XML
    def load_from_xml(self, xml):
        pass

    # Save to XML
    def save_to_xml(self):
        pass

    # Convert to string
    def to_str(self):
        pass

    # Convert from string
    def from_str(self, text):
        pass

    # Convert to HTML (for debugging/logging)
    def to_html(self):
        pass



'''   
4-3	OBRD Tasks Parameters
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
'''

# OBRD Tasks Parameters
# OBRD tasks created at the SOC should include the following parameters
# (in addition to the common parameters mentioned in 4-1):
class MsgObrdTask(MsgBase):

    # Constructor
    def __init__(self):

        self.oper_start_time = 0        # 1. Start time of operation
        self.obrd_task_type = ''        # 2. Type of OBRD task: download mode selection, specific images download,
                                        #    deletion mode, images to keep in OBRD, images to delete from OBRD
        self.download_mode = ''         # 3. For download mode selection, the options are:
                                        #    a. Online (default mode)
                                        #    b. Offline with priority to images being acquired at the moment
                                        #    c. Offline with priority to images in the OBRD

        # 4. For specific images download:
        self.image_count = 0            # a. Total number of images to download
        self.image_list = []            # b. List of all images to be downloaded
        self.image_part = ''            # c. Part of image to be downloaded: full image, first half only, second half only, first and second half


        self.deletion_mode = ''         # 5. For deletion mode selection, the options are:
                                        #    a. Automatic (including inputs by SOC to delete specific images)
                                        #    b. Fully manual

        self.not_to_erase_list = []     #  6. For images to keep in OBRD:
                                        #       a. List of images to be added to the "not to be erased" list.
                                        #       This list shall contain up to 128 images
        self.images_to_delete = []      #  7. For specific images to delete from OBRD:
                                        #       a. List of images to be deleted.
                                        #       One delete command shall contain up to 124 images to be deleted


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

    # Convert to string
    def to_str(self):
        pass

    # Convert from string
    def from_str(self, text):
        pass

    # Convert to HTML (for debugging/logging)
    def to_html(self):
        pass


# ============================================================================

# ============================================================================


# OBRD Tasks Parameters
# OBRD tasks created at the SOC should include the following parameters
# (in addition to the common parameters mentioned in 4-1):
class MsgObrdTask(MsgBase):

    # Constructor
    def __init__(self):

        self.oper_start_time = 0        # 1. Start time of operation
        self.obrd_task_type = ''        # 2. Type of OBRD task: download mode selection, specific images download,
                                        #    deletion mode, images to keep in OBRD, images to delete from OBRD
        self.download_mode = ''         # 3. For download mode selection, the options are:
                                        #    a. Online (default mode)
                                        #    b. Offline with priority to images being acquired at the moment
                                        #    c. Offline with priority to images in the OBRD

        # 4. For specific images download:
        self.image_count = 0            # a. Total number of images to download
        self.image_list = []            # b. List of all images to be downloaded
        self.image_part = ''            # c. Part of image to be downloaded: full image, first half only, second half only, first and second half


        self.deletion_mode = ''         # 5. For deletion mode selection, the options are:
                                        #    a. Automatic (including inputs by SOC to delete specific images)
                                        #    b. Fully manual

        self.not_to_erase_list = []     #  6. For images to keep in OBRD:
                                        #       a. List of images to be added to the "not to be erased" list.
                                        #       This list shall contain up to 128 images
        self.images_to_delete = []      #  7. For specific images to delete from OBRD:
                                        #       a. List of images to be deleted.
                                        #       One delete command shall contain up to 124 images to be deleted


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

    # Convert to string
    def to_str(self):
        pass

    # Convert from string
    def from_str(self, text):
        pass

    # Convert to HTML (for debugging/logging)
    def to_html(self):
        pass



'''
4-7	Response to GCS maintenance activity request
Response to GCS maintenance activity request should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	GCS maintenance activity request status: Approved / Not Approved
2.	Approved maintenance activity start time
'''
class MsgMaintenanceActivityResponse(MsgBase):

    # Constructor
    def __init__(self):
        super().__init__()
        self.request_status = ''       # GCS maintenance activity request status: Approved / Not Approved
        self.approved_start_time = 0   # Approved maintenance activity start time

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


# ============================================================================
#
#                               GCS to SOC Messages
#
# ============================================================================


# ============================================================================

# ============================================================================

'''
4-5	Response to Imaging Tasks Parameters
Response to imaging tasks should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Imaging plan status: Approved / Not Approved / Approved with warnings
2.	Details (in case of status is "Not Approved"):
    a. Time of mission rule violation (only the first violation will be logged)
    b. Mission rule violated (for example, electricity, OBRD, telescope pointing, etc)
    c. Violation parameters: TBD (probably: OBRD filling status, EPS parameters violated, S/C angles with respect to celestial bodies)
3.	Details (in case of status is "Approved with warnings"):
    a. Number of warnings
    b. Time of warning #1
    c. Duration of warning #1
    d. Type of warning #1 (for example, negative power balance during imaging, no APM direct link to GCS, etc)
    e. Warning #1 detailed parameters: TBD
    f. …
    g. Time of warning #N
    h. Duration of warning #N
    i. Type of warning #N (for example, negative power balance during imaging, no APM direct link to GCS, etc)
    j. Warning #N detailed parameters: TBD
'''

class MsgImagingTaskResponse(MsgBase):

    # Constructor
    def __init__(self):
        self.plan_status = ''       # Imaging plan status: Approved / Not Approved / Approved with warnings

        # 2. Details (in case of status is "Not Approved"):
        self.na_time = 0            # a. Time of mission rule violation (only the first violation will be logged)
        self.na_rule = ''           # b. Mission rule violated (for example, electricity, OBRD, telescope pointing, etc)
        self.na_params = ''         # c. Violation parameters: TBD (probably: OBRD filling status, EPS parameters violated, S/C angles with respect to celestial bodies)

        # 3. Details (in case of status is "Approved with warnings"):
        self.warn_num = 0           # a. Number of warnings
        self.warn_list = []         # List of self.Warning

        class Warning:
            def __init__(self):
                self.warn_time = 0          # b. Time of warning #1
                self.warn_duration = 0      # c. Duration of warning #1
                self.warn_type = ''         # d. Type of warning #1 (for example, negative power balance during imaging, no APM direct link to GCS, etc)
                self.warn_details = ''      # e. Warning #1 detailed parameters: TBD
                self.warn_tbd = ''          # f. …


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

# ============================================================================

# ============================================================================
'''
4-6 Response to OBRD Tasks Parameters
Response to OBRD tasks should include the following parameters (in addition to the common
parameters mentioned in 4-1):

1. OBRD task status: Approved / Not Approved
2. Details (in case of status is "Not Approved"):
    a. Time of mission rule violation (only the first violation will be logged)
    b. Mission rule violated (TBD)
    c. Violation parameters: TBD
'''

class MsgObrdTaskResponse(MsgBase):

    # Constructor
    def __init__(self):
        self.task_status = ''       # OBRD task status: Approved / Not Approved

        # Details (in case of status is "Not Approved"):
        self.violation_time = 0     # a. Time of mission rule violation (only the first violation will be logged)
        self.violation_rule = ''    # b. Mission rule violated (TBD)
        self.violation_params = ''  # c. Violation parameters: TBD


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


# ============================================================================

# ============================================================================

# GCS Message
class MsgMaintenanceTask(MsgBase):

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


# ============================================================================

# ============================================================================

'''
4-4	GCS request for S/C, G/S or payload maintenance activity
GCS maintenance activities requests should include the following parameters (in addition to the common parameters mentioned in ‎5-1):
1.	Maintenance activity name: for example, orbit maneuver, RWA calibration, telescope focusing, ground equipment maintenance, etc.
2.	Start time of allowable time slot in which the activity can be performed
3.	End time of time allowable slot in which the activity can be performed
4.	Preferred start time of activity
5.	Duration of activity
6.	It is / is not possible to continue scientific imaging
If due to constrains, the maintenance activity has to be executed at a specific time, the start time of the window and the preferred time shall be the same, and the end time should be equal to the start time + the duration time.
'''

# GCS Message
class MsgMaintenanceActivityRequest(MsgBase):

    # Constructor
    def __init__(self):
        super().__init__()
        self.activity_name = ''     # 1. Maintenance activity name: for example, orbit maneuver, RWA calibration, telescope focusing, ground equipment maintenance, etc.
        self.start_time = 0         # 2. Start time of allowable time slot in which the activity can be performed
        self.end_time = 0           # 3. End time of time allowable slot in which the activity can be performed
        self.pref_start_time = 0    # 4. Preferred start time of activity
        self.duration = 0           # 5. Duration of activity
                                    # 6. It is / is not possible to continue scientific imaging
                                    # If due to constrains, the maintenance activity has to be executed at a specific
                                    # time, the start time of the window and the preferred time shall be the same, and the end time should be equal to the start time + the duration time.


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

# ============================================================================

# ============================================================================

