
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'LastDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Load AstroHeader object from image FITS file
    AH = AstroHeader();
    TxtFileName = 'c:/ultrasat/last/LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.txt';           
    %TxtFileName = 'c:/ultrasat/last/LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.txt';        
    AH.readFromTextFile(TxtFileName);   
    
    FitsFileName = 'c:/ultrasat/last/LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.fits';
    %AH = AstroHeader(FitsFileName);

    % Create LastDb object with default connection parameters
    TestSSH = true;
    if TestSSH
        db.LastDb.setupSSH();
    end
    
    LDB = db.LastDb();
    
    % Create tables (optional)
    CreateTables = false;
    if CreateTables
        LDB.createTables();
    end

    % Insert new row to table
    LDB.addRawImage(FileName, AH);
    
    % Insert with additional fields, field are converted to LOWERCASE
    % Overwrite existing fields of AstroHeader, ignore columns that does 
    % not exist in the database table. 
    AddCols = struct;
    AddCols.ProcStat = sprintf('My notes at %s', datestr(now, 'yyyy/mm/dd HH:MM:SS'));
    AddCols.focus = 77777;
    AddCols.DoesNotExist = 'blabla';
    LDB.addRawImage(FileName, AH);
    LDB.addRawImage(FileName, AH, AddCols);
    
    % Select numer of columns
    Count = LDB.Query.selectTableRowCount('TableName', 'raw_images');
    disp(Count);
    
    % Select
    Data = LDB.Query.select('*', 'TableName', 'raw_images', 'Limit', 1000);
    if numel(Data.Data) > 0
        disp(Data.Data(end))
    end
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'LastDb test passed')
    Result = true;
end
