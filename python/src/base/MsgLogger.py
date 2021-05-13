# [StackTrace, WorkspaceIndex] = dbstack;
#
# if numel(StackTrace) < 2
#     CallerName = 'session';
#     Line      = NaN;
# else
#     CallerName = StackTrace(2).name;
#     Line      = StackTrace(2).line;
# end

# Message logger with levels
class MsgLogger:

    def __init__(self):
        self.CurLevel = LogLevel.All
        self.LogF = None
        self.UserData = None



    def msgLog(self, Level, varargin):

        # Do nothing if log is disabled
        if Level == LogLevel.None:
            return


        # Ignore levels above CurLevel
        if uint32(Level) > uint32(Obj.CurLevel)
            return


        if Level == LogLevel.Error
            Obj.msgStyle(Level, '@error', varargin{:});
            return


        if Level == LogLevel.Warning
            Obj.msgStyle(Level, '@warn', varargin{:});
            return


        LevStr = getLevelStr(Obj, Level);
        fprintf('[%s] ', LevStr);
        fprintf(varargin{:});
        fprintf('\n');

        # Log to file
        if ~isempty(Obj.LogF)
            Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});

            

             

    def msgStyle(Obj, Level, Style, varargin):

        % Do nothing if log is disabled
        if Level == LogLevel.None
            return
        end

        % Ignore levels above CurLevel
        if uint32(Level) > uint32(Obj.CurLevel)
            return
        end

        LevStr = getLevelStr(Obj, Level);
        cprintf(Style, '[%s] ', LevStr);
        cprintf(Style, varargin{:});
        fprintf('\n');

        % Log to file
        if ~isempty(Obj.LogF)
            Obj.LogF.write2(sprintf('[%s]', LevStr), varargin{:});


    def getLevelStr(self, Level):

        % Convert enum to string
        s = '';
        switch Level
            case LogLevel.None
                s = 'NON';
            case LogLevel.Error
                s = 'ERR';
            case LogLevel.Warning
                s = 'WRN';
            case LogLevel.Info
                s = 'INF';
            case LogLevel.Verbose
                s = 'VRB';
            case LogLevel.Debug
                s = 'DBG';
            case LogLevel.Test
                s = 'TST';
            otherwise
                error('Unknown LogLevel');

        Result = s;

        

    def msgStack(Obj, Level, varargin):
        [StackTrace, WorkspaceIndex] = dbstack;

        Obj.msgLog(Level, varargin{:});
        Obj.msgLog(Level, 'StackTrace:');

        if numel(StackTrace) < 2
            CallerName = 'session';
            Line      = NaN;
        else
            va = sprintf(varargin{:});
            for i = 2:numel(StackTrace)
                Msg = sprintf('File: %s, Line: #%d, Caller: %s, varargin: %s', StackTrace(i).file, StackTrace(i).line, StackTrace(i).name, va);
                Obj.msgLog(Level, Msg);

        #Msg = sprintf('File: %s, Line: #%d, Caller: %s - %s', File, Line, CallerName, sprintf(varargin{:}));
        #Obj.msgLog(Level, Msg);
        Obj.msgLog(Level, '');


    # Return singleton object
    def getSingleton():

        persistent PersObj
        if isempty(PersObj)
            PersObj = MsgLogger;
        end
        Result = PersObj;
    end
        

    # Set global LogLevel
    def setLogLevel(Level):

        m = MsgLogger.getSingleton();
        m.CurLevel = Level;
    end


    # Get global LogLevel
    def getLogLevel():

        Value = MsgLogger.getSingleton().CurLevel;




    def unitTestStackTrace(Count):
        if Count > 0
            M = MsgLogger.getSingleton();
            M.msgStack(LogLevel.Test, 'Recursion(%d)', Count);
            MsgLogger.unitTestStackTrace(Count-1);
        end
    end


    def unitTest():
        fprintf('MsgLogger test started\n');

        % Test cprintf (in external/)
        fprintf('cprintf test started\n');
        cprintf('text',    'regular black text\n');
        cprintf('hyper',   'followed %s', 'by');
        cprintf('-comment','& underlined\n');
        cprintf('err',     'elements: ');
        cprintf('cyan',    'cyan ');
        cprintf('_green',  'underlined green\n');
        fprintf('cprintf test done\n');

        % Test MsgLogger
        M = MsgLogger.getSingleton();
        M.msgLog(LogLevel.Test, 'Test: %d', uint32(LogLevel.Test));
        M.msgLog(LogLevel.Debug, 'Test: %d', uint32(LogLevel.Debug));
        M.msgLog(LogLevel.Info, 'Test: %d', uint32(LogLevel.Info));
        M.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));
        M.msgLog(LogLevel.Error, 'Test: %d', uint32(LogLevel.Error));
        M.msgLog(LogLevel.None, 'Test: %d', uint32(LogLevel.None));

        M.msgStack(LogLevel.Test, 'MyStackTrace: %d', 123);
        MsgLogger.unitTestStackTrace(5);

        % Test msgStyle
        M.msgStyle(LogLevel.Test, 'blue', 'Message in blue');
        M.msgStyle(LogLevel.Test, 'red', 'Message in red');

        fprintf('MsgLogger test passed\n');
        Result = true;
