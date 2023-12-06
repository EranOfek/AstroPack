function msgLogThrow(Level, Ex, varargin)
    % Log message to singeton MsgLogger and throw exception
    % Input:    Level       - LogLevel enumeration (see LogLevel.m)
    %           Ex          - MException object
    %           varargin    - Any fprintf inputs
    % Example:  io.msgLog(LogLevel.Info, 'Elapsed time: %0.4f', toc)
    
    m = MsgLogger.getSingleton();
    m.msgLogEx(Level, Ex, varargin{:});
    throw(Ex);
end
