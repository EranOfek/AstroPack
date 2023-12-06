
function Result = stressTest()
    io.msgStyle(LogLevel.Test, '@start', 'AstroDb stressTest started')              

    % Get db connection
    Conn = db.Db.getLast();
    Q = db.DbQuery(Conn);
    Q.query('SELECT version()');
    assert(Q.ColCount == 1);
    pgver = Q.getField('version');
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    %HeaderTableName = 'raw_images';
    CatalogTableName = 'sources_proc_cropped';

    % Create catalog with column names matching all fields with data type Double
    SqlText = ['SELECT * from ', CatalogTableName, ' LIMIT 1'];
    Q.query(SqlText);
    ColNames = Q.getFieldNamesOfType('Double');                       
    Cols = numel(ColNames);
    Rows = 100*1000;            
    io.msgLog(LogLevel.Test, 'Preparing test Catalog: Rows: %d, Cols: %d', Rows, Cols);
    AC = AstroTable({rand(Rows, Cols)}, 'ColNames', ColNames);

    % Insert catalog to table (with default BatchSize)
    Count = 1000;
    for i=1:Count
        db.AstroDb.insertCatalog(AC, CatalogTableName);
    end

    io.msgStyle(LogLevel.Test, '@passed', 'AstroDb stressTest done')
    Result = true;            
end

