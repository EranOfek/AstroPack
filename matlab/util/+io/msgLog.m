
function msgLog(Level, varargin)
    m = MsgLogger.getSingle();
    m.msgLog(Level, varargin{:});
end
