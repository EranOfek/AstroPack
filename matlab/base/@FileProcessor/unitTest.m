function Result = unitTest()
    % FileProcessor.unitTest
    
    io.msgLog(LogLevel.Test, 'FileProcessor test started');

    % Create instances
    fp = FileProcessor;
    
    io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed');                          
    Result = true;
end
