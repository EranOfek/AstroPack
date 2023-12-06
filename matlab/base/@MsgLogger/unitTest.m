function Result = unitTest()
    % MsgLogger.unitTset
    
    fprintf('MsgLogger test started\n');

    % Uncomment to call this function
    % exampleFromConstructor()
    
    % Get singleton object
    if ~isunix
        Path = 'C:\Temp';
    else
        Path = '/tmp';
    end
    
    % Test cprintf (in external/)
    fprintf('cprintf test started\n');
    cprintf('text',    'regular black text\n');
    cprintf('hyper',   'followed %s', 'by');
    cprintf('-comment','& underlined\n');
    cprintf('err',     'elements: ');
    cprintf('cyan',    'cyan ');
    cprintf('_green',  'underlined green\n');
    fprintf('cprintf test done\n');

    % Test MsgLogger, if the singleton object has not been created yet,
    % it will initialize it with the specified file name
    M = MsgLogger.getSingleton('FileName', fullfile(Path, 'UnitTestLogger'), 'UseTimestamp', true);
      
    % Set specific log level
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');            
    MsgLogger.setLogLevel(LogLevel.Warning, 'type', 'disp');            
    
    % Set log level for file and console
    io.msgLog(LogLevel.Info, 'This should go to file only');                
    io.msgLog(LogLevel.Warning, 'This should go to file and display');
    
    % Set the same log level for both file and console
    MsgLogger.setLogLevel(LogLevel.Test, 'type', 'all');
    io.msgLog(LogLevel.Test, 'Back to all');

    % Test MsgLogger            
    M.msgLog(LogLevel.Test,    'Test: %d', uint32(LogLevel.Test));
    M.msgLog(LogLevel.Debug,   'Test: %d', uint32(LogLevel.Debug));
    M.msgLog(LogLevel.Info,    'Test: %d', uint32(LogLevel.Info));
    M.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));
    M.msgLog(LogLevel.Assert,  'Test: %d', uint32(LogLevel.Assert));
    M.msgLog(LogLevel.Error,   'Test: %d', uint32(LogLevel.Error));
    M.msgLog(LogLevel.Fatal,   'Test: %d', uint32(LogLevel.Fatal));            
    M.msgLog(LogLevel.None,    'Test: %d', uint32(LogLevel.None));

    % Stack trace
    M.msgStack(LogLevel.Test, 'MyStackTrace: %d', 123);            
    unitTestStackTrace(5);

    % Test msgStyle
    M.msgStyle(LogLevel.Test, 'blue', 'Message in blue');
    M.msgStyle(LogLevel.Test, 'red', 'Message in red');

    % Array
    M.msgLog(LogLevel.Error, {'A', 'B', 'C'});
    
    % Test exception
    try
        throw(MException('MyFunc:MyExType', 'This is my message'));
    catch Ex
        M.msgLogEx(LogLevel.Error, Ex, 'Catched exception');
    end
    
    % Create another logger, besides the singleton one.
    MyLog = MsgLogger('FileName', fullfile(Path, 'OtherLogFile'), 'UseTimestamp', true);
    MyLog.setLogLevel(LogLevel.Info, 'type', 'file');            
    MyLog.setLogLevel(LogLevel.Warning, 'type', 'disp');                
    MyLog.msgLog(LogLevel.Test,    'Test: %d', uint32(LogLevel.Test));
    MyLog.msgLog(LogLevel.Debug,   'Test: %d', uint32(LogLevel.Debug));
    MyLog.msgLog(LogLevel.Info,    'Test: %d', uint32(LogLevel.Info));
    MyLog.msgLog(LogLevel.Warning, 'Test: %d', uint32(LogLevel.Warning));    
               
    fprintf('MsgLogger test passed\n');
    Result = true;
end


function unitTestStackTrace(Count)
    % Recursive test for stack trace log
    if Count > 0
        M = MsgLogger.getSingleton();            
        M.msgStack(LogLevel.Test, 'Recursion(%d)', Count);            
        unitTestStackTrace(Count-1);
    end
end


function exampleFromConstructor()
    % Usage with a singleton logger:
    
    % Call first to create the singleton with the file name.
    % Must be called before creating Configuration object which
    % uses io.msgLog().

    % Settings UseTimestamp=true add the current system time to
    % the file name, for example:
    % '/tmp/2021-10-27__14-44-10-OtherLogFile'

    LogFileName = 'C:/temp/SystemLogFileName.log';
    MsgLogger.getSingleton('FileName', LogFileName, 'UseTimestamp', true);

    % See base/LogLevel.m for list of log levels.
    % Set log level for log file, only messages with log level 
    % with this and higher priority  will be logged to the file.
    MsgLogger.setLogLevel(LogLevel.Info, 'type', 'file');            

    % Set log level for log file, only messages with log level 
    % with this and higher priority  will be logged to console.
    MsgLogger.setLogLevel(LogLevel.Warning, 'type', 'disp');            

    io.msgLog(LogLevel.Info, 'This should go to file only');              
    io.msgLog(LogLevel.Warning, 'This should go to file and display');

end
