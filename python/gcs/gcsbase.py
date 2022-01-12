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

import os, time, uuid, yaml
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

