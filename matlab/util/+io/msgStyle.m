function msgStyle(Level, Style, varargin)
    % Log message to singleton MsgLogger, with style/color supported by cprintf.m 
    % See external/cprintf.m for list of styles.
    % Input:    Level       - LogLevel enumeration (see LogLevel.m)
    %           Style       - Color/style supported by cprintf.m, for example 'red'
    %           varargin    - Any fprintf inputs
    % Example:  io.msgStyle(LogLevel.Info, 'blue', 'Elapsed time: %0.4f', toc)

    m = MsgLogger.getSingleton();
    m.msgStyle(Level, Style, varargin{:});
end
