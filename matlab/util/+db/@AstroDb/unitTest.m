
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains
    
    cprintf('hyper','Please, set the appropriate DataDir before running the test (see the next lines).\n');
%     DataDir = 'c:/ultrasat/last/';        % Windows
    DataDir = '/home/sasha/LAST/SampleData/'; % Linux
    %%%%%%%%%%%%%%%%%%%%

    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'AstroDb test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Required on Windows, need to compile it with mex in this folder
    addpath('external/str2doubles/str2doubles');

    % Create AstroDb object with default connection parameters
    TestSSH = true;
    if TestSSH
        db.AstroDb.setupSSH();
    end
    
    LDB = db.AstroDb();
    
    Tables = LDB.Query.select('*','TableName','pg_tables','Where','schemaname = ''public''');
    Tables.Data.tablename % show public tables in the DB
    
    % Create tables (optional)
    CreateTables = false;
    if CreateTables
        LDB.createTables();
    end
      
    % Insert a new row to tables
    FitsFileName = strcat(DataDir,'LAST.01.01.04_20230308.112234.596_clear_191+31_001_001_001_sci_raw_Image_1.fits');
    AH = AstroHeader(FitsFileName);    
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(~isempty(xx));
    %LDB.addImage('raw_images', FitsFileName, AH, 'xxhash', xx);
    pk = LDB.addImage('raw_images',FitsFileName, AH, 'xxhash', xx);    
    if ~isempty(pk)
        disp(pk);
        LDB.Query.deleteRecord('TableName', 'raw_images', 'Where', sprintf('pk = %d', pk));
    end
    
    pk = LDB.addImage('raw_images',FitsFileName, AH, 'xxhash', xx);        
    disp(pk);
    
    % test PROC and COADD:
    FitsFileName = strcat(DataDir,'LAST.01.03.01_20230427.213718.470_clear_219+50_001_001_024_sci_proc_Image_1.fits');
    AH = AstroHeader(FitsFileName);  
    pk = LDB.addImage('proc_images',FitsFileName, AH, 'xxhash', xx);        
    disp(pk);
    
    FitsFileName = strcat(DataDir,'LAST.01.03.01_20230427.213408.398_clear_219+50_001_001_021_sci_coadd_Image_1.fits');
    AH = AstroHeader(FitsFileName);  
    pk = LDB.addImage('coadd_images',FitsFileName, AH, 'xxhash', xx);        
    disp(pk);
    
    % test CAT 
    FitsFileName = strcat(DataDir,'LAST.01.03.01_20230427.213408.398_clear_219+50_001_001_024_sci_coadd_Cat_1.fits');
    AC = AstroCatalog(FitsFileName);  
    pk = LDB.addCatalog('src_catalog', AC); 
    disp(pk);
    
    % test populateImageDB
    ImageFiles  = dir ( fullfile(DataDir, '**', 'LAST*sci*raw_Image*.fits') );
    ImNum = numel(ImageFiles);
    Imfiles = repmat({''}, ImNum, 1);
    Images  = repmat(AstroImage(), ImNum, 1);
    Headers = repmat(AstroHeader(), ImNum, 1);
    for Img = 1:1:ImNum
        Imfiles{Img} = fullfile(ImageFiles(Img).folder, ImageFiles(Img).name);
        Images(Img) = AstroImage(Imfiles(Img));
        Headers(Img).Data = Images(Img).Header;
    end
    pk = LDB.insert ( Imfiles, 'Table', 'raw_images' );
    disp(pk);
    pk = LDB.insert ( Images, 'Table', 'raw_images' );
    disp(pk);
    pk = LDB.insert ( Headers, 'Table', 'raw_images');
    disp(pk);
    
    % test updateByTupleID
    LDB.updateByTupleID(pk,'ra',218, 'Table', 'proc_images')
    
    % Load AstroHeader object from image FITS file, convert to DbRecord
    FitsFileName = strcat(DataDir,'LAST.01.03.01_20230427.213408.398_clear_219+50_001_001_021_sci_coadd_Image_1.fits');
    AH = AstroHeader(FitsFileName);    
    R = db.DbRecord(AH);    
   
    AH = AstroHeader();    
    TxtFileName = strcat(DataDir,'LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.txt');           
    AH.readFromTextFile(TxtFileName);   
    
    AH = AstroHeader();    
    TxtFileName = strcat(DataDir,'LAST.01.08.04_20230125.192423.674_clear_143+41_010_001_001_sci_raw_Image_1.txt');        
    AH.readFromTextFile(TxtFileName);   
         
    % 
    FitsFileName = strcat(DataDir,'LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.fits');
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(~strcmp(xx,''));
    AH = AstroHeader(FitsFileName);    
       
    % Insert new row to table
    FitsFileName = strcat(DataDir,'LAST.01.02.01_20230401.000728.762_clear_180+53_002_001_001_sci_raw_Image_1.fits');
    xx = tools.checksum.xxhash('FileName', FitsFileName);
    assert(~strcmp(xx,''));
    LDB.addImage('raw_images', FitsFileName, AH, 'xxhash', xx);
    
    %
    pk = LDB.addImage('raw_images', FitsFileName, AH, 'xxhash', xx, 'Select', true);    
    
    % Insert with additional fields, field are converted to LOWERCASE
    % Overwrite existing fields of AstroHeader, ignore columns that does 
    % not exist in the database table. 
    AddCols = struct;
    AddCols.ProcStat = sprintf('My notes at %s', datestr(now, 'yyyy/mm/dd HH:MM:SS'));
    AddCols.focus = 77777;
    AddCols.DoesNotExist = 'blabla';
    LDB.addImage('raw_images',FitsFileName, AH);
    LDB.addImage('raw_images',FitsFileName, AH, 'AddCols',AddCols);
    
    % Select numer of columns
    Count = LDB.Query.selectTableRowCount('TableName', 'raw_images');
    disp(Count);
    
    % Select
    Data = LDB.Query.select('*', 'TableName', 'raw_images', 'Limit', 1000);
    if numel(Data.Data) > 0
        disp(Data.Data(end))
    end
    
%   LDB.Query.select('*', 'TableName', 'test_coadd_images', 'Where', 'filename like ''%LAST%''', 'OutType', 'Table')
%   LDB.Query.select('*', 'TableName', 'test_proc_images', 'Where', 'filename like ''%LAST%''', 'OutType', 'Table')
%   LDB.Query.deleteRecord('TableName', 'raw_images', 'Where', 'filename like ''%143%''')          
%   LDB.Query.select('*', 'TableName',lower(Args.DBtable),'Where', 'ra > 179','OutType','Table');
%   LDB.Query.select('pk', 'TableName', lower(Args.DBtable), 'Where', 'filename like ''%LAST%''', 'OutType', 'Table');
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'AstroDb test passed')
    Result = true;
end
