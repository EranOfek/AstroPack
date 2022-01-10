import os, yaml


# ===========================================================================
#
# ===========================================================================
# Base class for all objects
class Base:

    def __init__(self):
        self.UserData = None


# ===========================================================================
#
# ===========================================================================
class Config:

    def __init__(self):
        self.UserData = None

    def load(self):
        with open("example.yaml", 'r') as stream:
            try:
                print(yaml.safe_load(stream))
            except yaml.YAMLError as exc:
                print(exc)



# Parent class for all components
class Component(Base):

    # Constructor for LogFile
    def __init__(self):
        self.Name   = None                # Name string
        self.Owner  = None                # Indicates the component that is responsible for streaming and freeing this component
        self.Uuid   = None                # Global unique ID, generated with java.util.UUID.randomUUID()
        self.Tag    = None                # Optional tag (i.e. for events handling)
        self.MapKey = None                # Used with ComponentMap class
        self.Config = None # Configuration       # Configuration, deafult is system configuration
        self.Log = None  # MsgLogger              # Logger, default is system logger
        self.DebugMode = False          # DebugMode

        # By default use system log and configuration
        self.Log = MsgLogger.getSingleton();
        self.Config = Configuration.getSingleton();


    # (re)Generate unique ID for each element in object
    def makeUuid(self):
        self.Uuid = uuid.uuid1()
        return self.Uuid


    # Generate unique ID only if not empty
    def needUuid(self):
        if self.Uuid == '':
            self.makeUuid()

        return self.Uuid

    def log(self, msg):
        #pass

# ===========================================================================
#
# ===========================================================================

class Logger(Component):

    # Constructor
    def __init__(self):
        self.interface_name = ''

    # Destructor
    def __del__(self):
        # Deleted
        pass

    # Validate the specified task
    def validate_task(self, task):
        pass


    #





# ===========================================================================
#
# ===========================================================================

