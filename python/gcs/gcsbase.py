#
# gcsbase.py - Base classes and definitions
#
#
# Classes in this file:
#
#   Base
#   Config
#   Component
#   Logger
#
#

import os, time, glob, uuid, yaml
from datetime import datetime
from sys import platform

# ===========================================================================
#
# ===========================================================================

# Log message to file
if platform == "win32":
    LOG_PATH = 'c:/gcs/log/'
else:
    LOG_PATH = '/tmp/gcs/log/'

if not os.path.exists(LOG_PATH):
    os.makedirs(LOG_PATH)

# ---------------------------------------------------------------------------
logfile = open(os.path.join(LOG_PATH, 'gcs.log'), 'a')
def msg_log(msg, dt = True):
    global logfile
    if msg == '': dt = False
    if dt: msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
    print(msg)
    if logfile:
        logfile.write(msg)
        logfile.write("\n")
        logfile.flush()

# ===========================================================================
#
# ===========================================================================

# Base class for all objects
class Base:

    def __init__(self):
        self.name = ''

    def log(self, msg):
        msg_log('[{}] {}'.format(self.name, msg))

# ===========================================================================
#
# ===========================================================================

# Configuration class, based on YML files
class Config(Base):

    def __init__(self):
        super().__init__()
        self.name = 'Config'
        self.filename = ''
        self.data = None

    def load(self, filename='d:/ultrasat/astropack.git/python/gcs/gcs.yml'):
        self.filename = filename
        self.log('')
        with open(filename, 'r') as stream:
            try:
                self.data = yaml.safe_load(stream)
            except yaml.YAMLError as ex:
                print(ex)

    # Singleton
    config_ = None
    @staticmethod
    def get_config():
        if not Config.config_:
            config_ = Config()
        return config_


# Parent class for all components
class Component(Base):

    # Constructor for LogFile
    def __init__(self):
        super().__init__()
        self.name   = 'Component'       # Name string
        self.owner  = None              # Indicates the component that is responsible for streaming and freeing this component
        self.uuid   = None              # Global unique ID, generated with java.util.UUID.randomUUID()
        self.tag    = None              # Optional tag (i.e. for events handling)
        self.config = None              # Configuration       # Configuration, deafult is system configuration
        self.logger = None              # MsgLogger              # Logger, default is system logger
        self.debug_mode = False         # DebugMode

        # By default use system log and configuration
        #self.logger = MsgLogger.getSingleton()
        #self.config = Configuration.getSingleton()


    # (re)Generate unique ID for each element in object
    def make_uuid(self):
        self.uuid = uuid.uuid1()
        return self.uuid


    # Generate unique ID only if not empty
    def need_uuid(self):
        if self.uuid == '':
            self.make_uuid()
        return self.uuid

    def log(self, msg):
        pass

# ===========================================================================
#
# ===========================================================================

class Logger(Base):

    # Constructor
    def __init__(self):
        super().__init__()
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

# ===========================================================================
#
# ===========================================================================

# Parent class for file based processing
#
# Polll input folder for new files:
#   Call derived processFileImpl() function
#   Delete or move the processed file to archive folder
#   Clean archive folder after specified numberof days

# All file transmission between the SOC and the GCS (and vice versa) shall be under FTP protocol
# (TBD), or file insertion into a common file folder within the "Unsecured" part of the XML filter.

class FileProcessor(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.rcv_path = ''             # Input files folder
        self.processed_path = ''       # Optional archived input files folder
        self.send_path = ''            # Optional output folder  (response/result of input files)
        self.input_file_mask = '*.*'   #
        self.KeepProcessedFiles = True   # true to keep the processed files in ProcessedPath, otherwise deleted after processing
        self.KeepOutputFiles = True      #
        self.process_files_max_age = 7      # Number of days to keep processed files in Processed Path
        self.output_files_max_age = 7       # Number of days to keep output files in Output path
        self.process_file_callback = None


    # Initialize with default settings
    def init(self, rcv_path, send_path):
        self.rcv_path = rcv_path
        self.send_path = send_path

        if self.rcv_path != '' and not os.path.isdir(self.rcv_path):
            os.mkdir(self.rcv_path)

        if self.send_path != '' and not os.path.isdir(self.send_path):
            os.mkdir(self.send_path)

    # Get file name for new outgoing message
    def get_send_filename(self, ext=''):
        fn = datetime.now().strftime('%y_%m_%d_%H_%M_%S_%f') + ext
        return os.path.join(self.send_path, fn)


    # Poll input folder with specified delay, perform single step in DelayMS == -1
    def poll_rcv(self):

        flist = glob.glob(os.path.join(self.rcv_path, self.input_file_mask), recursive=False)
        flist.sort()

        for fname in flist:
            try:
                if self.process_file_callback:
                    self.process_file_callback(fname)
            except:
                self.log('error')

            # Move file to processed folder
            path, fn = os.path.split(fname)
            processed_fname = os.path.join(self.processed_path, fn)
            shutil.move(fname, processed_fname)
            return fname


        # Delete files before specified date
        def delete_old_files(self, path, mask, delete_before):
            flist = glob.glob(os.path.join(path, mask), recursive=False)
            for fname in flist:
                t = os.path.getmtime(fname)
                ft = datetime.fromtimestamp(t)
                if ft < delete_before:
                    self.log('remove: ' + fname)
                    #os.remove(fname)


# ===========================================================================

# ===========================================================================
