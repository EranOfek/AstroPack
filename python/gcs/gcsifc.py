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
from gcsbase import Component, Config, FileProcessor
from gcsmsg import *
from gcsdb import DbQuery, Database
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

        # Load configuration
        self.conf = Config()
        self.conf.load()

        #
        self.gui = GuiHandler()

        self.keep_alive_interval = 1*60
        self.last_rcv_keep_alive_time = 0

        # Download management

        self.in_com = FileProcessor()
        self.out_com = FileProcessor()

        # Database
        self.db = Database()

        # Current state
        self.state = State()


    #
    def run(self):
        self.setup()
        self.log('run loop started')
        while not self.terminated:
            self.manage()
            time.sleep(0.01)

        self.log('run loop done')
        self.shutdown()

    #
    def setup(self):
        self.log('setup')

    #
    def shutdown(self):
        self.log('shutdown')

    # -----------------------------------------------------------------------
    #                                   Manage
    # -----------------------------------------------------------------------

    #
    def manage(self):

        # Poll incoming messages
        self.handle_incoming_msgs()
        self.handle_outgoing_msgs()
        self.manage_imaging_tasks()
        self.manage_image_downloads()
        self.manage_keep_alive()
        self.handle_gui()

        t = time.time

        # Check for received KeepAlive
        if self.keep_alive_interval > 0:
            elapsed = t - self.last_rcv_keep_alive_time
            if elapsed > 2*self.keep_alive_interval:
                self.event_keep_alive()


    # -----------------------------------------------------------------------
    #                          Send Messages to GCS
    # -----------------------------------------------------------------------

    #
    def handle_outgoing_msgs(self):
        pass

    #
    def send_msg(self, msg):

        # Convert message to XML text
        xml = msg.save_xml()

        # Insert to table
        query = self.db.new_query()
        query.exec('INSERT INTO gcs_msgs () VALUES(), ', (, xml))

        # Save file to outgoing messages folder


    #
    def send_ack(self, msg):
        pass

    # -----------------------------------------------------------------------
    #                   Handle Incoming Messages from GCS
    # -----------------------------------------------------------------------

    #
    def handle_incoming_msgs(self):
        self.handle_incoming_msg(msg)
        pass

    # @Todo: Do we need to send Ack for GCS messages?
    def handle_incoming_msg(self, msg):
        pass



    #------------------------------------------------------------------------

    # Send Target-Of-Opertunity
    def send_target_oo(self, task):
        pass

    # Send task for validatation
    def send_task_validation(self, task):
        pass

    # Send task
    def send_task(self, task):
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



    # -----------------------------------------------------------------------
    #                               Keep Alive
    # -----------------------------------------------------------------------

    #
    def send_keep_alive(self):
        pass

    #
    def handle_keep_alive(self):
        pass


    def event_keep_alive(self):
        #self.make_event()
        pass



    # -----------------------------------------------------------------------
    #                             Imaging Task
    # -----------------------------------------------------------------------
    # New imaging task:
    #   - SOC write new imaging task to table 'gcs_tasks', with new_flag=1
    #   - GIF polls gcs_tasks table for records with new_flag=1
    #   - Task YAML is converted to MsgImagingTask

    def manage_imaging_tasks(self):
        # Poll database for pending tasks for approval
        query = DbQuery()
        records = query.query('SELECT * FROM gcs_tasks WHERE new_task = 1')
        for row in records:
            taskid = row['taskid']


    def create_imaging_task_msg(self, ):
        msg = MsgImagingTask


    def send_imaging_task(self, msg):
        pass

    # Send message to GCS
    def handle_imaging_task_response(self, msg):
        pass

    # Called when imaging task is approved
    # Create images in images table
    def imaging_task_approved(self, task):
        pass




    # -----------------------------------------------------------------------
    #                       Image Download & OBRD
    # -----------------------------------------------------------------------

    def manage_image_downloads(self):
        pass


    # Called when image has been received
    def image_received(self, image_data):
        pass


    # Send message to GCS
    def handle_obrd_task_response(self, msg):
        pass


    # -----------------------------------------------------------------------
    #                               Telemetry
    # -----------------------------------------------------------------------

    def handle_telemetry(self):
        pass

    # -----------------------------------------------------------------------
    #                                 GUI
    # -----------------------------------------------------------------------
    # Handle GUI message
    def handle_gui(self):
        if self.gui:
            gui_msg = self.gui.rcv()
            if gui_msg:
                if gui_msg.type == GuiMsgType.SendKeepAlive:
                    pass


    # -----------------------------------------------------------------------
    #                         Low Level Functions
    # -----------------------------------------------------------------------


    def make_event(self):
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
