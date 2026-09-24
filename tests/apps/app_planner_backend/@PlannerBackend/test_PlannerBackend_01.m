
function Result = unitTest()
    % Unit-Test

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'PlannerBackend test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    Backend = PlannerBackend();
       
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'PlannerBackend test passed')
    Result = true;
end
