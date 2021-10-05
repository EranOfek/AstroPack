function Result = unitTest()
    % Db.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'Db test started\n');

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'Db test passed')
    Result = true;
end

