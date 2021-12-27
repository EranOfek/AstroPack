function msgLogEx(Level, Ex, varargin)
    % Log MException message to singeton MsgLogger, call from within a 'catch' block
    % Input:    Level       - LogLevel enumeration (see LogLevel.m)
    %           Ex          - MException object
    %           varargin    - Any fprintf inputs
    % Example:  io.msgLogEx(LogLevel.Info, Ex, 'Elapsed time: %0.4f', toc)
    
    m = MsgLogger.getSingleton();
    m.msgLogEx(Level, Ex, varargin{:});
end
