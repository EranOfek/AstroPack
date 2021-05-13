# Component class - this class hinerits from Base class, and most

import Base

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

        for i = 1:numel(Obj)
            Obj(i).Uuid = Component.newUuid();
        end

        if numel(Obj) == 1
            Result = Obj.Uuid;
        else
            Result = [];


    # Generate unique ID only if not empty
    def needUuid(self):

        for i = 1:numel(Obj)
            if isempty(Obj(i).Uuid)
                Obj(i).makeUuid();

        if numel(Obj) == 1:
            Result = Obj.Uuid;
        else:
            Result = [];


    # If empty, generate map key as uuid
    def needMapKey(self)

        for i = 1:numel(Obj)
            if isempty(Obj(i).MapKey)
                Obj(i).MapKey = Obj(i).needUuid();

        if numel(Obj) == 1
            Result = Obj.MapKey;
        else
            Result = [];


    # Write message to log
    def msgLog(self, Level, varargin):

        self(1).Log.msgLog(Level, varargin{:});


    # Set msg style
    def msgStyle(self, Level, Style, varargin)
        self(1).Log.msgStyle(Level, Style, varargin{:});


    '''
    function Args = selectDefaultArgsFromProp(Obj, Args)
        % Given an Args structure, go over fields - if empty, take
        % value from object property. Otherwise, use value.
        
        ArgNames = fieldnames(Args);
        for Ian=1:1:numel(ArgNames)
            if isempty(Args.(ArgNames{Ian}))
                Args.(ArgNames{Ian}) = Obj.(ArgNames{Ian});
            end
            
    '''

    # Generate Uuid
    @staticmethod
    def newUuid():

        Temp = java.util.UUID.randomUUID;
        Result = string(Temp.toString()).char;


    # Generate simple serial number, used as fast local uuid
    @staticmethod
    def newSerial():

        persistent Counter
        if isempty(Counter)
            Counter = 0;
        end
        Counter = Counter + 1;
        Result = Counter;

    @staticmethod
    def newSerialStr(varargin):
        % Generate simple serial number, used as fast local uuid
        if numel(varargin) == 1
            Result = string(varargin(1) + string(Component.newSerial())).char;
        else
            Result = string(Component.newSerial()).char;

    @staticmethod
    def unitTest():
        % unitTest for Component class
        io.msgLog(LogLevel.Test, 'Component test started');

        a = Component;
        a.msgLog(LogLevel.Test, 'a created');

        b = Component;
        b.msgLog(LogLevel.Test, 'b created');

        c = Component;
        c.msgLog(LogLevel.Test, 'c created');

        io.msgLog(LogLevel.Test, 'Testing Uuid');
        a.needUuid();
        b.needUuid();
        assert(~all(a.Uuid ~= b.Uuid));

        io.msgLog(LogLevel.Test, 'Testing MapKey');
        a.needMapKey();
        b.needMapKey();
        assert(~all(a.MapKey ~= b.MapKey));

        c(1) = Component;
        c(2) = Component;
        c.msgLog(LogLevel.Test, 'Msg');
        u = c.needUuid();
        disp(u);
        k = c.needMapKey();
        disp(k);

        io.msgStyle(LogLevel.Test, '@passed', 'Component test passed');

        Result = true;
