# ===========================================================================
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
# ===========================================================================

import os, sys, shutil, time, glob, uuid, yaml, io, xmlplain
import xml.etree.ElementTree as ET
from datetime import datetime
import json
import configparser


# Helper function for dict2obj()
class dict2obj_hook(object):
    def __init__(self, dict_):
        self.__dict__.update(dict_)


# Convert YAML object to Python object
def dict2obj(d):
    return json.loads(json.dumps(d), object_hook=dict2obj_hook)


# @Todo - Convert back from object o YAML (and then to XML)

# ===========================================================================
#
# ===========================================================================

#
class ConfTag:
    KeepAlive = 'KeepAlive'


# ===========================================================================
#
# ===========================================================================

# Log message to file
if sys.platform == "win32":
    LOG_PATH = 'c:/gcs/log/'
else:
    LOG_PATH = '/tmp/gcs/log/'

if not os.path.exists(LOG_PATH):
    os.makedirs(LOG_PATH)

# ---------------------------------------------------------------------------
log_filename = os.path.join(LOG_PATH, 'gcs.log')
logfile = open(log_filename, 'a')
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
        self.obj = None
        self.keep_alive = {}

    def load(self, filename='d:/ultrasat/astropack.git/python/gcs/gcs.yml'):
        self.filename = filename
        self.log('')
        with open(filename, 'r') as stream:
            try:
                self.data = yaml.safe_load(stream)

                # Parse
                self.keep_alive = self.data[ConfTag.KeepAlive]

                self.obj = dict2obj(self.data)
                print(self.obj.KeepAlive.Interval)

            except yaml.YAMLError as ex:
                print(ex)

    # Singleton
    config_ = None
    @staticmethod
    def get_config():
        if not Config.config_:
            config_ = Config()
        return config_


# ===========================================================================
#
# ===========================================================================

class IniFile(Base):

    def __init__(self, filename=''):
        self.filename = ''
        self.ini = None

        if filename != '':
            self.load(filename)

    #
    def load(self, filename):
        self.filename = filename
        self.ini = configparser.ConfigParser()
        self.ini.read(self.filename)

        #print(config['DEFAULT']['path'])  # -> "/path/name/"
        #config['DEFAULT']['path'] = '/var/shared/'  # update
        #config['DEFAULT']['default_message'] = 'Hey! help me!!'  # create

    #
    def save(self):
        with open(self.filename, 'w') as f:    # save
            self.ini.write(f)


# ===========================================================================
#
# ===========================================================================

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
        self.input_file_ext = 'xml'    #
        self.send_ext = 'xml'
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

    #
    def send_yml_cmd(self, cmd, params_dict):
        msg = 'Msg:\n  Cmd:' + cmd + '\n'

        for key in params_dict:
            msg = msg + '  ' + key + ':' + params_dict[key] + '\n'

        filename = self.get_send_filename(self.send_ext)
        with open(filename, 'w') as f:
            f.write(msg)


    # Poll input folder with specified delay, perform single step in DelayMS == -1
    def poll_rcv(self):

        flist = glob.glob(os.path.join(self.rcv_path, '*.' + self.input_file_ext), recursive=False)
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
            #shutil.move(fname, processed_fname)
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

# Convert XML file to YAML file or text
# https://guillon.github.io/xmlplain/
def xml_to_yml(xml_filename, yml_filename='', yml_obj=False):

    # Read to plain object
    with open(xml_filename) as inf:
       root = xmlplain.xml_to_obj(inf, strip_space=True, fold_dict=True)

    # Output to file
    if yml_filename != '':
        with open(yml_filename, 'w') as outf:
            xmlplain.obj_to_yaml(root, outf)
            return True

    # Output to text or object
    else:
        yml_text = xmlplain.obj_to_yaml(root)
        if yml_obj:
            obj = yaml.safe_load(yml_text)
            return obj
        else:
            return yml_text


# Convert YAML text to XML file
# Args:
# Returns:
def yml_to_xml(yml, xml_filename):

    # Read the YAML file
    #with open("example-1.yml") as inf:
    #   root = xmlplain.obj_from_yaml(inf)
    if type(yml) != str:
        yml = yaml.dump(yml)

    root = xmlplain.obj_from_yaml(yml)

    # Output back XML
    with open(xml_filename, 'w') as outf:
       xmlplain.xml_from_obj(root, outf, pretty=True)


def test_yaml():
    filename = 'd:/ultrasat/astropack.git/python/gcs/gcs.yml'

    with open(filename, 'r') as stream:
        yml_obj = yaml.safe_load(stream)

    f = yml_obj['Interface']['MsgFolder']
    f = yml_obj['Interface']['List1']
    f = yml_obj['Interface']['Dict11']

    l = yml_obj['Interface']['NonUniqueKeys']
    l[3]['SameKey']


