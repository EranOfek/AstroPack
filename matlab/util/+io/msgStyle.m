% Log message to global MsgLogger, see LogLevel.m for list of levels
% See external/csprintf.m for list of styles.

function msgLog(Level, Style, varargin)
    m = MsgLogger.getSingleton();
    m.msgStyle(Level, Style, varargin{:});
end
