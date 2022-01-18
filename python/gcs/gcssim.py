#
# gcssim.py - Simulator Data structures and Database connectivity
#
#
# Classes in this file:
#
#   GcsSimulator            -
#   SimImageGenerator       -
#   SimTelemetryGenerator   -
#   SimValidatorResult      -
#   SimPlanValidator        -
#   SimObrdEntry            -
#   SimObrdData             -
#   SimObrdImage            -
#   SimObrdManager          -
#   SimPlanManager          -
#   SimMaintenanceManager   -
#
#

import os, time
from datetime import datetime
from gcsbase import Component
from gcsmsg import *
from gcsgui import GuiHandler, GuiMsg


# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsSimulator(Component):

    # Constructor
    def __init__(self, args):
        super().__init__()
        self.name = 'GcsSimulator'
        self.image_gen = SimImageGenerator()
        self.tele_gen = SimTelemetryGenerator()
        self.obrd = SimObrdManager()
        self.maint = SimMaintenanceManager()
        self.gui = GuiHandler()
        self.terminated = False
        self.last_image_time = 0
        self.new_image_interval = 10


    #
    def run(self):
        self.setup()
        while not self.terminated:
            self.manage()
            time.sleep(0.01)

        self.shutdown()


    #
    def setup(self):
        pass

    #
    def shutdown(self):
        pass

    #
    def manage(self):
        self.handle_incoming_msgs()
        self.generate_images()
        self.manage_cur_plan()
        self.handle_gui()

    #------------------------------------------------------------------------

    # Handle incoming message from GCS
    def handle_incoming_msgs(self):
        msg = ''
        self.handle_incoming_msg(msg)
        pass

    # Handle incoming message from GCS
    def handle_incoming_msg(self, msg):
        # Send ack
        self.send_ack(msg)
        pass

    #
    def generate_images(self):
        t = time.time()
        elapsed = self.last_image_time
        if elapsed > self.new_image_interval:
            self.last_image_time = t
            self.generate_new_image()
        pass


    def generate_new_image(self):
        filename = self.image_gen.create_image()
        if filename != '':
            pass

    def manage_cur_plan(self):
        pass

    #------------------------------------------------------------------------

    def prepare_msg(self, req_msg):
        pass


    def send_msg(self, msg):
        pass

    #
    def send_msg_ack(self, msg):
        pass

    #
    def send_msg_nack(self, msg):
        pass


    # Send message to GCS
    def send_msg_imaging_task_response(self, msg):
        pass


    # Send message to GCS
    def send_msg_obrd_task_response(self, msg):
        pass


    # Send message to GCS
    def send_msg_current_imaging_task_response(self, msg):
        pass


    # Send message to GCS
    def send_msg_current_image_retransmit_response(self, msg):
        pass


    # Send Target-Of-Opertunity
    def send_target_oo(self, task):
        pass

    # Send task for validation
    def send_task_validation(self, task):
        pass


    #------------------------------------------------------------------------

    def handle_msg(self, msg):
        pass

    def handle_telemetry(self):
        pass

    #------------------------------------------------------------------------

    #
    def handle_msg_ack(self, msg):
        pass

    #
    def handle_msg_keep_alive(self, msg):
        pass

    #
    def handle_msg_imaging_task_params(self, msg):
        pass

    #
    def handle_msg_obrd_task_params(self, msg):
        pass

    #
    def handle_msg_current_imaging_task(self, msg):
        pass


    #
    def handle_msg_image_retransmit(self, msg):
        pass

    #------------------------------------------------------------------------



    #
    def handle_gui(self):
        pass

# ===========================================================================

# ===========================================================================

# Generator of new images
class SimImageGenerator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.msg_id = ''
        self.image_width = 9500
        self.image_height = 9500
        self.image_size_pixels = self.image_width * self.image_width
        self.image_size_bytes = self.image_size_pixels * 2
        self.image_path = 'c:/gcs/sim/new_images/'

    # Create new image, return file name
    def create_image(self):
        fn = datetime.now().strftime('%y_%m_%d_%H_%M_%S_%f')[:-3] + '.img'
        filename = os.path.join(self.image_path, fn)
        f = open(filename, "wb")
        f.seek(self.image_size_bytes - 1)
        f.write(b"\0")
        f.close()
        return filename


# ===========================================================================

# ===========================================================================

# S/C and Payload telemetry will be transferred to the SOC for use as input to the mission planning
# system and for scientific use. The specific telemetry parameters which will be sent to the SOC are
# still TBD.
# Telemetry is constantly downloaded to the GCS. Each telemetry packet or parameter is collected
# by the DPSL at a specific frequency, ranging from once every second to once every several minutes
# or tens of minutes.
# Every 5 minutes (TBR), the SOCM will create a text file containing the values received during the
# last 5 minutes for every one of the selected telemetry parameters. This text file contains the
# parameter code, time of sample (in UTC format) and value.
# Parameter code format: x### or xx##, where "x" and "xx" are letters which indicate the subsystem
# from which the parameter was collected, and "###" and "##" are numbers.
# The telemetry text files will be transmitted through the data channel.
#

#
# Handling of Telemetry delivery to the SOC (TBD)
# 1. The Telemetry Collector keeps the most updated data for delivery. The list of
# parameters which will be sent to the SOC is TBD, but it will contain at least
# parameters for the electrical system, orbital data and OBRD status which are needed
# by the validation process.
# 2. The telemetry files will be transmitted to the SOC via the imagery data channel.
#

class SimTelemetryGenerator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.data_dict = {}
        self.path = ''

    # Create new telemetry data, return file name
    def create_telemetry(self):
        pass

# ===========================================================================

# ===========================================================================
# Mission Validation:
# 1. The validation process for imaging and OBRD tasks in the GCS runs simultaneously
# (TBR) with the mission planning.
# 2. During validation these are the mission rules being checked:
# a. EPS status according to power consumption in each state, available power
# from the solar panel and battery power level
# b. OBRD memory status according to the status at beginning of plan and the
# imaging and downloading operations performed during the plan
# c. Telescope pointing with respect to the Sun, Earth and Moon
# 3. Validation of a week-long imaging plan shall take under 3 minutes (TBR: if during
# software development it is found that the validation time is longer, the SOC will be
# advised to create ToO imaging plans for shorter periods).

class SimValidatorResult:

    def __init__(self):
        self.status = ''
        pass



#
class SimPlanValidator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.name = 'SimPlanValidator'
        self.msg_id = ''

    # Create new telemetry data, return file name
    def validate_plan(self):
        pass


    # Validate target
    def validate_target(self, msg, target):
        pass


# ===========================================================================
#                                       OBRD
# ===========================================================================


# Obrd entry
class SimObrdEntry:

    # Constructor
    def __init__(self):
        self.interface_name = ''

# ===========================================================================
#
# ===========================================================================

# Obrd data
class SimObrdData:

    # Constructor
    def __init__(self):
        self.jtime = 0


# Image in OBRD
class SimObrdImage:

    def __init__(self):
        self.imageid = ''       #
        self.time = 0           #
        self.filename = ''      #



#
class SimObrdManager(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.name = 'SimObrdManager'
        self.msg_id = ''
        self.image_list = []        # List of SimObrdImage
        self.download_mode = ''
        self.deletion_mode = ''

    #
    def manage(self):
        self.manage_download()
        self.manage_deletion()


    #
    def manage_download(self):
        if self.download_mode == DownloadMode.Online:
            pass
        elif DownloadMode == DownloadMode.OfflineCurrent:
            pass
        elif DownloadMode == DownloadMode.OfflineStored:
            pass
        else:
            pass

    #
    def manage_deletion(self):
        if self.deletion_mode == DeletionMode.Automatic:
            pass
        elif self.deletion_mode == DeletionMode.FullyManual:
            pass
        else:
            pass

    # Send current image
    def send_live_image(self):
        pass

    #
    def send_next_image(self):
        pass



# ===========================================================================
#
# ===========================================================================
# Simulate plan
class SimPlanManager(Component):

    def __init__(self):
        self.cur_plan = None
        pass


# ===========================================================================
#
# ===========================================================================

# Handling of S/C G/S or payload Maintenance Activities
# 1. S/C, GCS or payload maintenance requests are converted with the XML-Handler and
# sent to the SOC for approval
# 2. The SOC replies with the approved time slot for the activity.
# 3. The Queue is updated with all approved upcoming maintenance activities (Satellite
# and GCS alike).

class SimMaintenanceManager:

    def __init__(self):
        pass


    def manage(self):
        pass

