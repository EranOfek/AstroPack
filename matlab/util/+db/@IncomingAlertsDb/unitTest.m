
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'IncomingAlertsDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "socdb" should exist');

    % Required on Windows, need to compile it with mex in this folder
    addpath('external/str2doubles/str2doubles');

    % Create AstroDb object with default connection parameters
    TestSSH = true;
    if TestSSH
        db.AstroDb.setupSSH();
    end
    
    ADB = db.IncomingAlertsDb();
    
    Tables = ADB.Query.select('*','TableName','pg_tables','Where','schemaname = ''public''');
    Tables.Data.tablename % show public tables in the DB
    
     
%   LDB.Query.select('pk', 'TableName', lower(Args.DBtable), 'Where', 'filename like ''%LAST%''', 'OutType', 'Table');
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'IncomingAlertsDb test passed')
    Result = true;
end
