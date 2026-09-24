
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'IncomingAlertsDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "socdb" should exist');
  
    ADB = db.IncomingAlertsDb();
           
     
%   ADB.selectAlerts()
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'IncomingAlertsDb test passed')
    Result = true;
end
