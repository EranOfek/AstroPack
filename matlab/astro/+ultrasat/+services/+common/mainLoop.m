function Result = mainLoop(Args)
    % Thin wrapper: create JsonFileIpc from Args and call run() (blocking).
    % Optional: set log level before run.
    arguments
        Args.InputPath
        Args.ProcessedPath
        Args.Handler
        Args.UserParam = []
        Args.DelaySec = 0.1
        Args.WatchdogFileName = []
        Args.WatchdogInterval = 10
        Args.MaxRunTime = []
    end

    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'disp');
    io.msgLog(LogLevel.Info, '=========== Service mainLoop started - Input folder: %s', strrep(Args.InputPath, '\', '\\'));

    ipc = ultrasat.services.common.JsonFileIpc( ...
        'InputPath', Args.InputPath, ...
        'ProcessedPath', Args.ProcessedPath, ...
        'Handler', Args.Handler, ...
        'UserParam', Args.UserParam, ...
        'DelaySec', Args.DelaySec, ...
        'WatchdogFileName', Args.WatchdogFileName, ...
        'WatchdogInterval', Args.WatchdogInterval, ...
        'MaxRunTime', Args.MaxRunTime);
    Result = ipc.run();
    io.msgStyle(LogLevel.Test, '@passed', 'Service mainLoop passed');
end
