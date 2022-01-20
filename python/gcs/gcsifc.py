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

import os, sys, shutil, time, glob, uuid, yaml, io, xmlplain
import xml.etree.ElementTree as ET
from datetime import datetime
import json

import gcsbase
from gcsbase import Component, Config, FileComm
from gcsmsg import *
from gcsdb import DbQuery, Database, DbConfig

HAVE_POSTGRES = False

# ===========================================================================

# ===========================================================================

# GCS Communication Manager

class GcsInterface(Component):

    # Constructor
    def __init__(self, args):
        super().__init__()
        self.name = 'GcsInterface'
        self.terminated = False
        self.startup_time = time.time()

        # Load configuration
        self.conf = Config()
        self.conf.load()

        #
        self.gui_com = FileComm()
        self.gui_com.init(
            input_path=self.conf.obj.Gui.FromGuiPath,
            output_path=self.conf.obj.Gui.ToGuiPath,
            input_ext='yml', output_ext='yml')

        self.last_rcv_keep_alive_time = 0

        # Download management

        self.msg_com = FileComm()
        self.msg_com.init(
            input_path = self.conf.obj.Interface.MsgFromGcsPath,
            output_path = self.conf.obj.Interface.MsgToGcsPath,
            input_ext = 'xml', output_ext = 'xml')

        #self.msg_com.rcv_path = self.conf.obj.Interface.InMsgFolder
        #self.msg_com.input_file_ext = 'xml'
        #self.msg_com.out_path = ...
        # self.msg_com.out_ext = ...


        # Database
        db_conf = DbConfig()
        if HAVE_POSTGRES:
            db_conf.set_postgres()
        else:
            db_conf.set_sqlite()
        self.db = Database(conf=db_conf)

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

        #self.handle_outgoing_msgs()

        self.manage_imaging_tasks()

        #self.manage_image_downloads()

        self.manage_keep_alive()
        self.handle_gui()

        t = time.time()


    # -----------------------------------------------------------------------
    #                   Send Messages to GCS & send_msg_...()
    # -----------------------------------------------------------------------

    #
    def handle_outgoing_msgs(self):
        pass


    def prepare_msg(self, req_msg):
        pass


    # =======================================================================
    #                               send_msg_...
    # =======================================================================
    # Send ACK message
    def send_msg_ack(self, msg):
        msg = MsgAck()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send keep-alive message
    def send_msg_keep_alive(self):
        msg = MsgKeepAlive()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send imaging task
    def send_msg_imaging_task(self, msg):
        msg = MsgImagingTask()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send OBRD task
    def send_msg_obrd_task(self):
        msg = MsgObrdTask()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send response to maintenance task request
    def send_msg_maintenance_task_response(self):
        msg = MsgMaintenanceTaskResponse()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send request to get current imaging task
    def send_msg_current_imaging_task(self):
        msg = MsgAck()
        self.prepare_msg(msg)
        return self.send_msg(msg)


    # Send retransmit request of images stores in GCS
    def send_msg_retransmit_images(self):
        msg = MsgAck()
        self.prepare_msg(msg)
        return self.send_msg(msg)

    # -----------------------------------------------------------------------

    #
    def send_msg(self, msg):

        # if issubclass(MsgBase)

        # Convert message to XML text
        xml = msg.save_xml()

        # Insert to table
        query = self.db.new_query()
        #query.exec('INSERT INTO gcs_msgs () VALUES(), ', (, xml))

        # Save file to outgoing messages folder
        filename = self.msg_com.new_output_filename()
        #msg...



    # -----------------------------------------------------------------------
    #                   Handle Incoming Messages from GCS
    # -----------------------------------------------------------------------

    #
    def handle_incoming_msgs(self):
        filename = self.msg_com.poll_input()
        if filename != '':

            # XML file
            if filename.lower().endswith(MsgFileExt.XmlMsg):
                self.handle_incoming_msg(filename)


    # @Todo: Do we need to send Ack for GCS messages?
    def handle_incoming_msg(self, filename):

        # Convert XML file to YML file (save as .YML file for debugging)
        gcsbase.xml_file_to_yml_file(filename, filename + '.yml')

        # Load XML file as YAML object
        yml = gcsbase.xml_file_to_yml(filename, yml_obj=True)
        msg = gcsbase.yml_to_obj(yml)

        header = yml[MsgTag.Msg][MsgTag.Header]

        # Header.__dict__.keys()
        Header = msg.Msg.Header

        #msg = MsgBase()
        #msg.load_header(header)
        #msg_type = msg.msg_type
        msg_type = header[MsgTag.MsgType]
        self.log('MsgType: %s' % msg_type)

        #
        if msg_type == MsgType.KeepAlive:
            self.handle_msg_keep_alive(yml)

        elif msg_type == MsgType.Ack:
            self.handle_msg_ack(yml)

        elif msg_type == MsgType.ImagingTaskResponse:
            self.handle_msg_imaging_task_response(yml)

        elif msg_type == MsgType.ObrdTaskResponse:
            self.handle_msg_obrd_task_response(yml)

        elif msg_type == MsgType.MaintenanceTask:
            self.handle_msg_maintenance_task(yml)

        elif msg_type == MsgType.CurrentImagingTaskResponse:
            self.handle_msg_maintenance_task(yml)

        elif msg_type == MsgType.ImageRetransmitResponse:
            self.handle_msg_maintenance_task(yml)

        else:
            self.log('handle_incoming_msg: unknnown msg_type: %s' % msg_type)

        # Move message file to processed messages folder
        path, fn = os.path.split(filename)
        fname = os.path.join(self.conf.obj.Interface.InMsgProcessedFolder, fn)
        try:
            shutil.move(filename, fname)
        except:
            os.remove(filename, fname)

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

    def manage_keep_alive(self):
        t = time.time()

        # Check for received KeepAlive
        if self.conf.obj.KeepAlive.Interval > 0:
            if self.last_rcv_keep_alive_time == 0:
                last = self.startup_time
            else:
                last = self.last_rcv_keep_alive_time

            elapsed = t - last
            if elapsed > self.conf.obj.KeepAlive.Timeout:
                self.event_keep_alive()



    #
    def handle_msg_keep_alive(self, msg):
        t = time.time()
        self.last_rcv_keep_alive_time = t

        # Clear event

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
        query = self.db.new_query()

        # Test only
        '''
        d = {}
        d['abc'] = 'aaa'
        d['def'] = 'bbb'
        d['x'] = 1
        d['y'] = 2.0
        query.insert('my_table', d)


        records = query.query('SELECT * FROM gcs_tasks WHERE new_flag = true')
        for row in records:
            taskid = row['taskid']
        '''

    def create_imaging_task_msg(self, ):
        msg = MsgImagingTask



    # Send message to GCS
    def handle_msg_imaging_task_response(self, msg):
        pass


    # Send message to GCS
    def handle_msg_obrd_task_response(self, msg):
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





    # -----------------------------------------------------------------------
    #                       Image Download & OBRD
    # -----------------------------------------------------------------------


    # -----------------------------------------------------------------------
    #                       Image Download & OBRD
    # -----------------------------------------------------------------------




    # -----------------------------------------------------------------------
    #                               Telemetry
    # -----------------------------------------------------------------------

    def handle_telemetry(self):
        pass

    # -----------------------------------------------------------------------
    #                                 GUI
    # -----------------------------------------------------------------------

    def send_gui(self, cmd, params_dict):
        self.gui_com.send_yml_cmd(cmd, params_dict)

    # Display log
    def gui_log(self, cmd, text):
        params = { 'Text': text }
        self.send_gui('Log', params)


    # Handle GUI message
    def handle_gui(self):
        if self.gui_com:
            filename = self.gui_com.poll_input()
            if filename != '':
                try:

                    msg = gcsbase.yml_file_to_obj(filename)

                    cmd = msg.Msg.Cmd
                    if cmd == 'ConvertXmlToYml':
                        pass

                    if cmd == 'ConvertYmlToXml':
                        pass

                except:
                    pass

                os.rename(filename, filename + '._txt')
                #os.remove(filename)

    # -----------------------------------------------------------------------
    #                         Low Level Functions
    # -----------------------------------------------------------------------


    def make_event(self):
        pass



# ===========================================================================
#
# ===========================================================================

# Validate imaging task before sending to GCS
# This class run the Validator provided by MABAT as "black-box"
# @TBD - Should we run it as external process with input/output files?
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
