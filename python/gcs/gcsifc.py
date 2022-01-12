#
# gcsifc.py - Simulator Data structures and Database connectivity
#
#
# Classes in this file:
#
#   GcsInterface            -
#
#   ImagingTaskValidator    - Validate imaging task before sending to GCS
#
#
# Notes:
#   - Interface communicate with GCS by two shared folders: 1. Messages, 2. Images & Telemetry files
#   - Interface communicate with SOC by database tables, message format is YAML
#   - Interface save its current state to database, it can be restarted at any moment
#

import time
from gcsbase import Component
from gcsgui import GuiHandler, GuiMsg, GuiMsgType

# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsInterface(Component):

    # Constructor
    def __init__(self, args):
        super().__init__()
        self.interface_name = ''
        self.terminated = False
        self.gui = GuiHandler()

        self.keep_alive_interval = 1*60
        self.last_rcv_keep_alive_time = 0

        # Download management


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
        self.handle_gui()

        t = time.time

        # Check for received KeepAlive
        if self.keep_alive_interval > 0:
            elapsed = t - self.last_rcv_keep_alive_time
            if elapsed > 2*self.keep_alive_interval:
                self.event_keep_alive()


    # -----------------------------------------------------------------------

    # -----------------------------------------------------------------------

    def event_keep_alive(self):
        #self.make_event()
        pass


    def make_event(self):
        pass

    #
    def handle_incoming_msgs(self):
        self.handle_incoming_msg(msg)
        pass

    # @Todo: Do we need to send Ack for GCS messages?
    def handle_incoming_msg(self, msg):
        pass


    def send_keep_alive(self):
        pass

    #------------------------------------------------------------------------

    # Send message to GCS
    def handle_imaging_task_response(self, msg):
        pass

    #
    def handle_keep_alive(self):
        pass

    # Send message to GCS
    def handle_obrd_task_response(self, msg):
        pass

    # Send Target-Of-Opertunity
    def send_target_oo(self, task):
        pass

    # Send task for validatation
    def send_task_validation(self, task):
        pass

    # Send task
    def send_task(self, task):
        pass

    # Called when image has been received
    def image_received(self, image_data):
        pass

    # -----------------------------------------------------------------------
    # Save current state to database
    def save_state(self):
        pass

    # Load current state from database
    def load_state(self):
        pass

    # -----------------------------------------------------------------------




    #------------------------------------------------------------------------

    # Handle incoming message from GCS
    def handle_msg(self, msg):
        pass

    def handle_telemetry(self):
        pass


    # Handle GUI message
    def handle_gui(self):
        if self.gui:
            gui_msg = self.gui.rcv()
            if gui_msg:
                if gui_msg.type == GuiMsgType.SendKeepAlive:
                    pass



# ===========================================================================
#
# ===========================================================================

# GCS Observation Scheduler
# @Todo - Should we develop
class GcsScheduler:

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass


# ===========================================================================
#
# ===========================================================================

# Validate imaging task before sending to GCS
class ImagingTaskValidator(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.interface_name = ''

    # Validate the specified task
    def validate_task(self, task):
        pass


# ===========================================================================
#
# ===========================================================================
