# General Interface Parent Class

class MsgBase(Base):

    # Constructor
    def __init__(self):
        self.interface_name = ''
        self.timestamp = None


    # Destructor
    def __del__(self):
        # Deleted
        pass

    def init(self):
        self.timestamp =

class InterfaceBase(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''


    # Destructor
    def __del__(self):
        # Deleted
        pass


    # Poll incoming message
    def receiveIncomingMsgs(self):
        pass


    # Handle single message
    def handleMsg(self, msg):
        pass


    # Send outgoing message to other party
    def sendMsg(self, msg):
        pass





