
function msgLog(Level, Style, varargin)
    m = MsgLogger.getSingleton();
    m.msgStyle(Level, Style, varargin{:});
end
