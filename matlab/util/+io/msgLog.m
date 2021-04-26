
function msgLog(Level, varargin)
    m = MsgLogger.getSingleton();
    m.msgLog(Level, varargin{:});
end
