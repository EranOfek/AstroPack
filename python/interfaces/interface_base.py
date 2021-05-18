# General Interface Parent Class

class MessageBase(Base):

    # Constructor
    def __init__(self):
        self.interface_name = ''


    # Destructor
    def __del__(self):
        # Deleted
        pass



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


    def sendMsg(self, msg):
        pass





