% Log message to global MsgLogger, see LogLevel.m for list of levels
% See external/csprintf.m for list of styles.

function msgLogStruct(Level, Prompt, Struct)
    m = MsgLogger.getSingleton();
    s = tools.struct.struct2text(Struct);
    m.msgLog(Level, '%s: %s', Prompt, s);
end
