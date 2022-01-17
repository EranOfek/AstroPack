import configparser
from enum import Enum
from gcsbase import Component, FileProcessor

#
# Classes in this file:
#
#   GuiMsgType
#   GuiMsg
#   GuiHandler
#


# GUI Messages
class GuiMsgType(Enum):
    SendKeepAlive = 'send_keep_alive'


# ---------------------------------------------------------------------------

# Simple message encoded as INI file section
class GuiMsg:
    
    # Constructor
    def __init__(self):
        self.section = 'msg'
        self.params = {}
        self.type = ''

    # Write params dictionary to INI file section
    def write(self, filename):
        config = configparser.ConfigParser()
        config.add_section(self.section)
        for key in self.params:
            config .set(self.section, key, self.params[key])

        # Write the new structure to the new file
        with open(filename, 'w') as configfile:
            config.write(configfile)

    # Read params dictionary from INI file section
    def read(self, filename):
        config = configparser.ConfigParser()
        config.read(filename)

        for (key, value) in config.items(self.section):
            self.params[key] = value

        # Get common parameters
        self.type = self.params['type']


# ---------------------------------------------------------------------------

class GuiHandler(Component):

    # Constructor
    def __init__(self):
        super().__init__()
        self.name = 'GuiHandler'
        self.file_procssor = FileProcessor()
        self.path = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass


    # -----------------------------------------------------------------------
    # Display log
    def send_log(self, text):
        msg = ( \
            'Msg:\n' +
            '  Cmd: Log\n' +
            '  Param: ' + text + '\n')

        self.send(msg)

    # -----------------------------------------------------------------------
    def process_msg(self, msg):
        pass


    # Send message by writing new INI file
    def send(self, msg):
        fname = self.file_procssor.get_send_filename()
        msg.write(fname)
        pass


    # Receive message by reading INI file
    def rcv(self):
        fname = self.file_procssor.poll_rcv()
        if fname != '':
            msg = GuiMsg()
            msg.read(fname)
            self.process_msg(msg)

