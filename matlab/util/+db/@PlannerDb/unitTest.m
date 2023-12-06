
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'PlannerDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    Planner = db.PlannerDb();
    
    % Prepare Plan data
    Plan = struct;
    Plan.plan_title = 'My test';
    
    % Prepare Targets data
    Targets(1) = struct;
    Targets(1).trg_ra = 1;
    Targets(1).trg_dec = 2;
    Targets(2).trg_ra = 11;
    Targets(2).trg_dec = 12;    
    
    % Add Plan with Targets
    Planner.addPlan(Plan, Targets);
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'PlannerDb test passed')
    Result = true;
end
