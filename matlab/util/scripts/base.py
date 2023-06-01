#----------------------------------------------------------------------------
# Project:  ULTRASAT
# Module:   Common
# File:     base.py
# Title:    General utilities
# Author:   Chen Tishler, 01/2022
# @Dan - UT & doc
#----------------------------------------------------------------------------
#
# ===========================================================================
#
# base.py - Base classes and definitions
#
#
# Classes in this file:
#
#   Color
#   Base
#   Config
#   IniFile
#   Component
#   Logger
#
#
# ===========================================================================

import os, sys, time, configparser
from datetime import datetime
#import psutil

from colorama import Fore, Back, Style
import colorama
colorama.init()

# ===========================================================================

# Global FileComm object used from msg_log() below
# It will be set in gcsifc.py
global_gui_com = None       # Assign object of type FileComm()


def set_global_gui_com(com):
    """
    Set the global
    """
    global global_gui_com
    global_gui_com = com

# ===========================================================================
#
# ===========================================================================

class Color:
    """
    Common RGB colors
    """
    black       = 0x000000
    white       = 0xffffff
    red         = 0x0000ff
    green       = 0x008000
    blue        = 0xff0000
    gray        = 0x808080
    silver      = 0xC0C0C0
    yellow      = 0xffff00
    fuchsia     = 0xff00ff
    maroon      = 0x000080
    cyan        = 0x00FFFF
    aqua        = 0x00FFFF
    lime        = 0x00FF00
    olive       = 0x808000
    purple      = 0x800080
    teal        = 0x008080
    navy        = 0x000080


    def get_colorama_fore(color):
        fore = ''
        if color == Color.black:
            fore = Fore.BLACK
        elif color == Color.white:
            fore = Fore.WHITE
        elif color == Color.blue:
            fore = Fore.BLUE
        elif color == Color.red:
            fore = Fore.RED
        elif color == Color.green:
            fore = Fore.GREEN
        elif color == Color.yellow:
            back = Fore.YELLOW
        elif color == Color.purple:
            fore = Fore.MAGENTA
        else:
            fore = Fore.BLUE
        return fore

    def get_colorama_back(color):
        back = ''
        if color == Color.black:
            back = Back.BLACK
        elif color == Color.white:
            back = Back.WHITE
        elif color == Color.blue:
            back = Back.BLUE
        elif color == Color.red:
            back = Back.RED
        elif color == Color.green:
            back = Back.GREEN
        elif color == Color.yellow:
            back = Back.YELLOW
        elif color == Color.purple:
            back = Back.MAGENTA
        return back


    def colored_text(text, fore, back=None):
        fore = Color.get_colorama_fore(fore)
        back = Color.get_colorama_back(back)
        s = fore + back + text + Style.RESET_ALL
        return s

# ===========================================================================
#
# ===========================================================================

class LogLevel:
    """
    See AstroPack.git/matlab/base/LogLevel.m
    """
    Non = 0         # Log disabled, use it with setLogLevel()
    Fatal = 1       # Fatal error, must terminate
    Error = 2       # Error
    Assert = 3      # Assert error
    Warning = 4     # Warning
    Info = 5        # General info
    Verbose = 6     # Verbose info
    Debug = 7       # Detailed debug
    DebugEx = 8     # Very detailed debug
    Perf = 9        # Performance timing
    Test = 10       # Unit-Test
    All = 11        # All, use it with setLogLevel()


log_level_str = {
    LogLevel.Non: 'NON',
    LogLevel.Fatal: 'FTL',
    LogLevel.Error: 'ERR',
    LogLevel.Assert: 'ASR',
    LogLevel.Warning: 'WRN',
    LogLevel.Info: 'INF',
    LogLevel.Verbose: 'VRB',
    LogLevel.Debug: 'DBG',
    LogLevel.DebugEx: 'DBX',
    LogLevel.Perf: 'PRF',
    LogLevel.Test: 'TST',
    LogLevel.All: 'ALL'
}

#
log_path = ''


class Logger:
    """
    Simple logger class.
    When file size is bigger than max_size, the file is renamed to '.old' and
    a new file is started
    """

    def __init__(self, path=None, fname=None):
        super().__init__()
        self.path = path
        self.filename = None
        self.filename_ex = None
        self.base_filename = None
        self.logfile = None
        self.logfile_ex = None
        self.gui_com = None
        self.use_dt = True
        self.use_pid = True
        self.pid = os.getpid()
        #self.process_name = psutil.Process(self.pid).name()
        self.last_date = ''

        # Special options
        self.date_path = False
        self.max_size = 0
        self.rename_to_time = False

        if fname:
            self.init_log(fname=fname)


    def init_log(self, path=None, fname=None):
        """
        Initialize.
        """

        # Log message to file
        if path:
            self.path = path
        else:
            if sys.platform == "win32":
                self.path = 'c:/soc/log/'
            else:
                self.path = '/tmp/soc/log/'
                #self.path = '/var/log/soc/incoming_alerts/lvc/log/'

        if not os.path.exists(self.path):
            os.makedirs(self.path)

        if not fname:
            fname = 'soc_default.log'

        self.filename = os.path.join(self.path, fname)
        self.logfile = open(self.filename, 'a')

        if self.use_pid:
            self.filename = os.path.join(self.path, str(os.getpid()) + '.log')
            self.filename_ex = os.path.join(self.path, str(os.getpid()) + '_ex.log')
        else:
            self.filename = os.path.join(self.path, fname + '.log')
            self.filename_ex = os.path.join(self.path, fname + '_ex.log')

        self.logfile = open(self.filename, 'a')
        self.logfile_ex = open(self.filename_ex, 'a')


    def msg_log(self, msg, type=None, comp=None, use_dt=None, dt=None, gui=None, gui_com=None, color=0, bkg=0xffffff, ex=None):
        # Write message to logfile.

        if not self.filename:
            self.init_log()

        if not use_dt:
            use_dt = self.use_dt

        #
        msg = self.get_msg_log_text(msg=msg, type=type, comp=comp, use_dt=use_dt, dt=dt, ex=ex)

        # @Todo
        if self.date_path:
            fdate = datetime.now().strftime('%Y/%m/%d')
            if fdate != self.last_date:
                fpath = os.path.join(self.path, fdate)
                if not os.path.exists(fpath):
                    os.makedirs(fpath)

                self.filename = os.path.join(fpath, self.base_filename)

        if color and color != 0:
            print(Color.get_colorama_fore(color) + msg + Style.RESET_ALL)
        else:
            print(msg)

        if self.logfile:
            self.logfile.write(msg)
            self.logfile.write("\n")
            self.logfile.flush()

            # Check file size limit, then rename to '.old' and create new file
            if self.max_size > 0:
                size = self.logfile.tell()
                if size > self.max_size:
                    self.logfile.close()
                    if self.rename_to_time:
                        old_filename = self.filename[:-3] + '.' + datetime.now().strftime('%y_%m_%d__%H_%M_%S')
                    else:
                        old_filename = self.filename[:-3] + '.old'

                    if os.path.exists(old_filename):
                        os.remove(old_filename)
                    os.rename(self.filename, old_filename)
                    self.logfile = open(self.filename, 'a')

        #
        if ex and self.logfile_ex:
            self.logfile_ex.write(msg)
            self.logfile_ex.write("\n")
            self.logfile_ex.flush()

        # Send log message go GUI
        if gui:
            if not self.gui_com:
                self.gui_com = global_gui_com

            if gui_com:
                params = {'Text': msg, 'Color': color, 'Bkg': bkg}
                gui_com.send_yml_cmd('Log', params)


    def get_msg_log_text(self, msg, type='', comp='', use_dt=True, dt=None, ex=None):
        #  Prepare message log text from its fields.

        if not msg:
            use_dt = False

        if type:
            msg = '[{}] {}'.format(type, msg)

        if comp:
            msg = '[{}] {}'.format(comp, msg)

        if ex:
            try:
                msg += ' - exception: ' + str(ex)
            except:
                msg += ' - exception: (no message property)'

        if use_dt:
            if not dt:
                dt = datetime.now()

            if self.use_pid:
                msg = ('[%05d] ' % self.pid) + datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg
            else:
                msg = datetime.now().strftime('%d/%m/%y %H:%M:%S.%f')[:-3] + ' ' + msg

        return msg


    def get_log_level_str(level):
        """

        :param level:
        :return:
        """
        if not level or level == '':
            level = LogLevel.Info

        if level in log_level_str:
            text = log_level_str[level]
        else:
            text = str(level)

        return text

# ===========================================================================
#                               msg_log(...)
# ===========================================================================

default_logger = None

def init_log(path=None, fname=None):

    if not fname:
        fname = 'log'

    global default_logger
    if not default_logger:
        default_logger = Logger(path=path, fname=fname)

    return default_logger


def msg_log(msg, logger=None, type=None, comp=None, use_dt=None, dt=None, gui=None, gui_com=None, color=0, bkg=0xffffff, ex=None):
    #
    global default_logger
    if not logger:
        if not default_logger:
            init_log()

        logger = default_logger

    logger.msg_log(msg, type=type, comp=comp, use_dt=use_dt, dt=dt, gui=gui, gui_com=gui_com, color=color, bkg=bkg, ex=ex)


# ===========================================================================
#
# ===========================================================================

class Base:
    """
    Base class for all objects.
    """

    def __init__(self):
        self.name = ''
        self.base_gui_com = None
        self.log_to_gui = False
        self.root_path = os.getenv('ULTRASAT_PATH')
        self.config_path = os.path.join(self.root_path, 'config')


    def log(self, msg, type='', comp='', color=0, bkg=0, columns=None, use_dt=True, dt=None, ex=None):
        """
        Write log message with name and optional colors

        :param msg:
        :param type:
        :param comp:
        :param color:
        :param bkg:
        :param columns:
        :param use_dt:
        :param dt:
        :param ex:
        :return:
        """
        if comp == '':
            comp = self.name

        msg_log(msg, comp=comp, type=type, gui=self.log_to_gui, gui_com=self.base_gui_com, color=color, bkg=bkg, use_dt=use_dt, dt=dt, ex=ex)


# ===========================================================================
#
# ===========================================================================

class Config(Base):
    """
    Configuration class, based on YML files.
    Required packages: yaml
    """
    def __init__(self):
        super().__init__()
        self.name = 'Config'
        self.filename = ''
        self.yml = None
        self.data = None


    def load(self, filename=''):
        """
        Load configuration file.

        :param filename:
        :return:
        """
        if filename == '':
            path = os.getenv('ULTRASAT_PATH')
            if not path or path == '':
                path = 'd:/ultrasat/ultrasat.git/python/prj/src/gcs/'

            if sys.platform == "win32":
                filename = os.path.join(path, 'python/prj/src/gcs/gcs_conf_win.yml')
            else:
                filename = os.path.join(path, 'python/prj/src/gcs/gcs_conf.yml')

            print('Config.load: %s' % filename)

            self.filename = filename
            self.data = yaml_utils.yaml_file_to_obj(filename)


    config_ = None
    @staticmethod
    def get_config():
        """
        Create/return singleton configuration object.
        """
        if not Config.config_:
            config_ = Config()
            config_.load()
        return config_


# ===========================================================================
#
# ===========================================================================

class IniFile(Base):
    """
    Simple INI file read/write.
    """
    def __init__(self, filename=''):
        self.filename = ''
        self.ini = None

        if filename != '':
            self.load(filename)


    def load(self, filename: str):
        """
        Load INI file to self.ini

        :param filename:
        :return:
        """
        self.filename = filename
        self.ini = configparser.ConfigParser()
        self.ini.read(self.filename)

        #print(config['DEFAULT']['path'])  # -> "/path/name/"
        #config['DEFAULT']['path'] = '/var/shared/'  # update
        #config['DEFAULT']['default_message'] = 'Hey! help me!!'  # create


    def save(self):
        """
        Save to file.
        """
        with open(self.filename, 'w') as f:    # save
            self.ini.write(f)

# ===========================================================================
#
# ===========================================================================

class Component(Base):
    """
    Parent class for all components.
    """

    def __init__(self):
        super().__init__()
        self.name   = 'Component'       # Name string
        self.owner  = None              # Indicates the component that is responsible for streaming and freeing this component
        self.uuid   = None              # Global unique ID, generated with java.util.UUID.randomUUID()
        self.tag    = None              # Optional tag (i.e. for events handling)
        self.config = None              # Configuration       # Configuration, deafult is system configuration
        self.logger = None              # MsgLogger              # Logger, default is system logger
        self.is_utc = True              #
        self.debug_mode = False         # DebugMode

        # By default use system log and configuration
        #self.logger = MsgLogger.getSingleton()
        #self.config = Configuration.getSingleton()


    def make_uuid(self):
        """
        (re)Generate unique ID for each element in object
        """
        self.uuid = system.new_uuid()
        return self.uuid


    def need_uuid(self):
        """
        Generate unique ID only if not empty.
        """
        if self.uuid == '':
            self.make_uuid()
        return self.uuid

# ===========================================================================
#
# ===========================================================================

class Stopwatch:
    """

    """
    def __init__(self, enable=True, interval=0, delay=0, auto_restart=True, first_arrived=False):
        """

        :param enable:
        :param interval:
        :param delay:
        :param auto_restart:
        :param first_arrived:
        """
        self.start_time = 0
        self.stop_time = 0
        self.elapsed_time = 0
        self.interval = 0
        self.enabled = False
        self.first_arrived = first_arrived
        self.auto_restart = auto_restart
        if enable:
            self.start(interval=interval, delay=delay)


    def start(self, interval=0, delay=0):
        """
        Start.

        :param interval: Interval in seconds
        :param delay: Optional delay in seconds
        :return:
        """
        if interval > 0:
            self.interval = interval

        if delay > 0:
            self.start_time = self.get_time() + delay
        else:
            self.start_time = self.get_time()

        self.enabled = True


    def stop(self):
        """
        Stop, return elapsed time if was was enabled, or None

        :return:
        """
        if self.enabled:
            self.stop_time = self.get_time()
            self.elapsed_time = self.stop_time - self.start_time
            self.enabled = False
            return self.elapsed_time
        else:
            return None


    def elapsed(self):
        """
        Get elapsed time since last start.

        :return: elapsed time in seconds
        """
        if self.enabled:
            self.elapsed_time = self.get_time() - self.start_time
            return self.elapsed_time
        else:
            return 0


    def arrived(self, once=True, restart=False, stop=False):
        """
        Check if time has arrived since last start.

        :param once:
        :param restart:
        :param stop:
        :return:
        """
        result = False
        if self.first_arrived:
            self.first_arrived = False
            result = True
        elif self.enabled:
            elapsed = self.elapsed()
            if elapsed > self.interval:
                result = True
                if restart or self.auto_restart:
                    self.start()
                elif stop or once:
                    self.stop()

        return result


    def get_time(self):
        """
        Get current time in seconds, as time.time()
        @Todo - Discuss with Dan time simulator @Dan
        :return: time in seconds as time.time()
        """

        return time.time()

# ===========================================================================
#
# ===========================================================================

def debug_stopwatch():
    """
    Debug StopWatch
    :return:
    """
    sw1 = Stopwatch(interval=1, enable=True)
    sw2 = Stopwatch(interval=2, enable=True)
    sw3 = Stopwatch()

    while True:
        if sw1.arrived(restart=True):
            print('sw1 arrived: {}'.format(sw1.elapsed_time))

        elapsed = sw2.elapsed()
        if elapsed > 2:
            print('sw2 elapsed: {}'.format(elapsed))
            sw2.start()
            sw3.start(interval=0.5)

        if sw3.arrived(stop=True):
            print('sw3 arrived: {}'.format(sw2.elapsed_time))


# ===========================================================================
#
# ===========================================================================

def debug():

    # Log
    init_log()
    msg_log('log file: ' + default_logger.filename)
    #debug_stopwatch()


    # Base
    b1 = Base()
    b1.log('Log message black', color=Color.black)
    b1.log('Log message blue', color=Color.blue)
    b1.log('Log message red', color=Color.red)

    # Config
    conf = Config()
    conf.load()
    c2 = conf.get_config()
    print(c2.data.__dict__)
    assert(c2.data.Interface.MsgInPath != '')

    # IniFile
    ini = IniFile('./test.ini')
    assert(ini.ini['Test1']['Param1'] != '')

    # Component
    comp = Component()
    assert(comp.need_uuid() != '')

    # Logger
    lg = Logger('./test_logger.log')
    lg.msg_log('log msg 1')
    lg.msg_log('log msg 2')

    return True


if __name__ == '__main__':
    debug()
