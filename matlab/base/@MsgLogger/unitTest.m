function Result = unitTest()
    % MsgLogger.unitTset
    
    fprintf('MsgLogger test started\n');

    % Test cprintf (in external/)
    fprintf('cprintf test started\n');
    cprintf('text',    'regular black text\n');
    cprintf('hyper',   'followed %s', 'by');
    cprintf('-comment','& underlined\n');
    cprintf('err',     'elements: ');
    cprintf('cyan',    'cyan ');
    cprintf('_green',  'underlined green\n');
    fprintf('cprintf test done\n');

    % Test MsgLogger
    M = MsgLogger.getSingleton();            

    % Set specific log level
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');            
    MsgLogger.setLogLevel(LogLevel.Warning, 'type', 'disp');            
    io.msgLog(LogLevel.Info, 'This should go to file only');            
    io.msgLog(LogLevel.Warning, 'This should go to file and display');
    MsgLogger.setLogLevel(LogLevel.Test, 'type', 'all');
    io.msgLog(LogLevel.Test, 'Back to all');

    % Test MsgLogger            
    M.msgLog(LogLevel.Test,    'Test: %d', uint32(LogLevel.Test));
    M.msgLog(LogLevel.Debug,   'Test: %d', uint32(LogLevel.Debug));
    M.msgLog(LogLevel.Info,    'Test: %d', uint32(LogLevel.Info));
    M.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));
    M.msgLog(LogLevel.Error,   'Test: %d', uint32(LogLevel.Error));
    M.msgLog(LogLevel.Fatal,   'Test: %d', uint32(LogLevel.Fatal));            
    M.msgLog(LogLevel.None,    'Test: %d', uint32(LogLevel.None));

    % Stack trace
    M.msgStack(LogLevel.Test, 'MyStackTrace: %d', 123);            
    unitTestStackTrace(5);

    % Test msgStyle
    M.msgStyle(LogLevel.Test, 'blue', 'Message in blue');
    M.msgStyle(LogLevel.Test, 'red', 'Message in red');

    fprintf('MsgLogger test passed\n');
    Result = true;
end


function unitTestStackTrace(Count)
    % Recursive test for stack trace log
    if Count > 0
        M = MsgLogger.getSingleton();            
        M.msgStack(LogLevel.Test, 'Recursion(%d)', Count);            
        MsgLogger.unitTestStackTrace(Count-1);
    end
end
