function msgLogStruct(Level, Prompt, Struct)
    % Log struct fields to singeton MsgLogger
    % Input:    Level       - LogLevel enumeration (see LogLevel.m)
    %           Prompt      - char with optional text, displaed before the struct fields
    %           Struct      - struct to be logged
    % Example:  io.msgLogStruct(LogLevel.Info, 'MyStruct', MyStruct)
    
    m = MsgLogger.getSingleton();
    s = tools.struct.struct2text(Struct);
    m.msgLog(Level, '%s: %s', Prompt, s);
end
