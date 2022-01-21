
function Result = unitTest()
    % Unit-Test
        
    io.msgStyle(LogLevel.Test, '@start', '... test started')

    
    io.msgStyle(LogLevel.Test, '@passed', '... test passed');
    Result = true;
end
