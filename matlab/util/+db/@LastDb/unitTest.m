
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'LastDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Create object with default connection parameters
    LDB = db.LastDb();
    LDB.createTables();

    FileName = 'abc.fits';
    AH = AstroHeader(FileName);
    LDB.addRawImage(FileName, AH);

    io.msgStyle(LogLevel.Test, '@passed', 'LastDb test passed')
    Result = true;
end
