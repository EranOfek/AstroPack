
function Result = unitTest()
    % AstroDb.unitTest
    Result = true;
    return;
    
    io.msgStyle(LogLevel.Test, '@start', 'AstroDb test started')

    % Get db connection
    Conn = db.Db.getLast();
    Q = db.DbQuery(Conn);
    Q.query('SELECT version()');
    assert(Q.ColCount == 1);
    pgver = Q.getField('version');
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    HeaderTableName = 'raw_images';
    CatalogTableName = 'sources_proc_cropped';

    % Create catalog
    SqlText = ['SELECT * from ', CatalogTableName, ' LIMIT 1'];
    Q.query(SqlText);
    ColNames = Q.getFieldNamesOfType('Double');            
    Cols = numel(ColNames);
    Rows = 10;

    % https://undocumentedmatlab.com/articles/faster-csvwrite-dlmwrite
    % https://github.com/nazarovsky/mex-writematrix
    % 
    for Iter=1:5
        io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d', Rows, Cols);
        T1 = tic();
        AC = AstroTable({rand(Rows, Cols)}, 'ColNames', ColNames);
        T = toc();
        io.msgLog(LogLevel.Test, 'Preparing rand Catalog: Rows: %d, Cols: %d: = %.4f', Rows, Cols, T);

        T1 = tic();
        FileName = sprintf('c:\\temp\\Cat-%d.csv', Rows);
        AC.csvWrite(FileName);
        T = toc();
        io.msgLog(LogLevel.Test, 'csvWrite: Rows: %d, Cols: %d = %.4f', Rows, Cols, T);

        Rows = Rows * 10;
    end

    Rows = 10;
    io.msgLog(LogLevel.Test, 'Preparing test Catalog: Rows: %d, Cols: %d', Rows, Cols);
    AC = AstroTable({rand(Rows, Cols)}, 'ColNames', ColNames);

    % Insert catalog to table
    res = db.AstroDb.insertCatalog(AC, CatalogTableName);

    %------------------------------------------------- Prepare test data          
    % Create fits file with header
    Folder = tempdir; 
    %Folder = 'c:\temp';
    Header = { 'RA_NE', [1], 'Comment 1';  'RA_SE', [2], 'Comment 2'; 'RA_SW', [3], 'Comment 3';  'RA_NW', [4], 'Comment 4' };
    ImageData = zeros(10, 10);
    ImageName = fullfile(Folder, 'AstroImageDbTest.fits');
    if isfile(ImageName)
        delete(ImageName);
    end

    fitswrite(ImageData, ImageName)            
    FITS.write_keys(ImageName, Header);
    AH = AstroHeader(ImageName);
    assert(all(size(AH.Data)));

    % Create catalog            
    AC = AstroTable({rand(10, 4), rand(10, 4)}, 'ColNames', {'ra', 'dec', 'sn_best', 'sn_delta'});


    % Insert catalog to table
    Count = 10;
    for i=1:Count
        res = db.AstroDb.insertCatalog(AC, CatalogTableName);
    end


    %------------------------------------------------- Insert Header
    % Insert header to table
    tic;
    Count = 10;
    for i=1:Count
        res = db.AstroDb.insertHeader(AH, HeaderTableName);
    end
    toc

    %------------------------------------------------- Insert Catalog
    % Insert catalog to table
    Count = 10;
    for i=1:Count
        res = db.AstroDb.insertCatalog(AC, CatalogTableName);
    end

    %----------------------------------------------------- Batch

    % Insert many with batch = 1
    Count = 0;  %1000;
    tic();
    for i=1:Count
        res = db.AstroDb.insertHeader(AH, HeaderTableName);
    end            
    Toc = toc();
    io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d): %.6f', Count, Toc);  

    % Insert many with batch = 1000
    Count = 0; %100;
    BatchSize = 1000;
    tic();
    for i=1:Count
        res = db.AstroDb.insertHeader(AH, HeaderTableName, 'BatchSize', BatchSize);
    end

    Toc = toc();
    io.msgLog(LogLevel.Info, 'insertHeader time (Count=%d, Batch=%d): %.6f', Count, BatchSize, Toc);  


    io.msgStyle(LogLevel.Test, '@passed', 'AstroDb test passed')
    Result = true;
end
