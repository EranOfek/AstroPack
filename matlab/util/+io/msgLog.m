function msgLog(Level, varargin)
    % Log message to singeton MsgLogger
    % Input:    Level       - LogLevel enumeration (see LogLevel.m)
    %           varargin    - Any fprintf inputs
    % Example:  io.msgLog(LogLevel.Info, 'Elapsed time: %0.4f', toc)
    
    m = MsgLogger.getSingleton();
    m.msgLog(Level, varargin{:});
end
