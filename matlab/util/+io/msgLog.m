% Log message to global MsgLogger, see LogLevel.m for list of levels

function msgLog(Level, varargin)
    m = MsgLogger.getSingleton();
    m.msgLog(Level, varargin{:});
end
