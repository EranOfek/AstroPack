
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'LastDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Load AstroHeader object from image FITS file
    AH = AstroHeader();
    FileName = 'c:/ultrasat/last/LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.txt';        
    AH.readFromTextFile(FileName);   
    
    % Create LastDb object with default connection parameters
    LDB = db.LastDb();
    
    % Create tables (optional)
    CreateTables = false;
    if CreateTables
        LDB.createTables();
    end

    % Insert new row to table
    LDB.addRawImage(FileName, AH);

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'LastDb test passed')
    Result = true;
end
