function Result = unitTest()
    % DbDriver.unitTest    
    io.msgStyle(LogLevel.Test, '@start', 'PlannerDb test started\n');


    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'PlannerDb test passed')
    Result = true;
end
