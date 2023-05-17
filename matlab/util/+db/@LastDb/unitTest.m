
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'LastDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Required on Windows, need to compile it with mex in this folder
    addpath('external/str2doubles/str2doubles');

    % Create LastDb object with default connection parameters
    TestSSH = false;
    if TestSSH
        db.LastDb.setupSSH();
    end
    
    LDB = db.LastDb();
   
    
    % Insert new row to table
    FitsFileName = 'c:/ultrasat/last/a1.fits';
    AH = AstroHeader(FitsFileName);    
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(~isempty(xx));
    %LDB.addImage('raw_images', FitsFileName, AH, 'xxhash', xx);
    LDB.addRawImage(FitsFileName, AH, 'xxhash', xx);    
    
    % Create tables (optional)
    CreateTables = false;
    if CreateTables
        LDB.createTables();
    end

    % Load AstroHeader object from image FITS file, convert to DbRecord
    FitsFileName = 'c:\ultrasat\last\a1.fits';
    AH = AstroHeader(FitsFileName);    
    R = db.DbRecord(AH);    
   
    FitsFileName = 'c:\ultrasat\last\sample.image.fits';
    AH = AstroHeader(FitsFileName);    
        
    AH = AstroHeader();    
    TxtFileName = 'c:/ultrasat/last/LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.txt';           
    AH.readFromTextFile(TxtFileName);   
    
    AH = AstroHeader();    
    TxtFileName = 'c:/ultrasat/last/LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.txt';        
    AH.readFromTextFile(TxtFileName);   
         
    TxtFileName = 'c:/ultrasat/last/h1.txt';
    AH = AstroHeader();
    AH.readFromTextFile(TxtFileName);   
    
    
    % 
    FitsFileName = 'c:/ultrasat/last/a1.fits';
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(xx ~= 0);
    AH = AstroHeader(FitsFileName);    
       
    % Insert new row to table
    FitsFileName = 'c:/ultrasat/last/a1.fits';
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(xx ~= 0);    
    LDB.addImage('raw_images', FitsFileName, AH, 'xxhash', xx);
    LDB.addRawImage(FitsFileName, AH, 'xxhash', xx);
    
    %
    LDB.addRawImage(FitsFileName, AH, 'xxhash', xx, 'Select', true);    
    
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
