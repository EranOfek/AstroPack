
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
        
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
    
    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'    
    Q = db.DbQuery('unittest');    
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    % CsvFileName
    Q.select('*', 'TableName', 'master_table', 'CsvFileName', 'C:/temp/test_select.csv');
    
    
    % Get tables list
    TablesList = Q.getTablesList();
    ColumnList = Q.getTableColumnList('master_table');
    PkList = Q.getTablePrimaryKey('master_table');
    IndexList = Q.getTableIndexList('master_table');
    
    % Convert AstroHeader to DbRecord
    H = AstroHeader();
    H.insertKey('Key1');
    H.insertKey({'KeyA','ValueA','CommentA';'KeyB','ValueB','CommentB'},'end-1');
    R = db.DbRecord(H);
    
    
    H = AstroHeader();
    H.insertKey({'FInt1',1,'CommentA';'FInt2',2,'CommentB';'FIntXX','XX','CommentXX'},'end-1');
    Q.insert(H, 'TableName', 'master_table', 'InsertRecFunc', @make_recid, 'ColumnsOnly', true);
    
    %
    Q = db.DbQuery('unittest:master_table');
    Q = db.DbQuery('unittest', 'TableName', 'master_table');
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Q.selectCount());
    
    testUpdate(Q);
    %testDelete(Q);
    
    %testInsert(Q);
    %testSelect(Q);        
        
    ColNames = 'fdouble1,fdouble2,fdouble3';
    if isfield(Q.Config.Data.Database.DbConnections.UnitTest, 'DoubleFields')
        ColNames = Q.Config.Data.Database.DbConnections.UnitTest.DoubleFields;
    end
    
    Cols = numel(strsplit(ColNames, ','));
    Mat = rand(1000, Cols);
    R = db.DbRecord(Mat, 'ColNames', ColNames);
    %Q.insert(R, 'InsertRecFunc', @make_recid);
    Q.insert(Mat, 'ColNames', ColNames, 'InsertRecFunc', @make_recid, 'InsertRecArgs', {'PK', 'recid'});
    
    % Select fields
    Q = db.DbQuery('unittest:master_table');    
    Limit = 10000;    
    Columns = 'recid,fdouble1,fdouble2,fdouble3';
    
    % Still need to find a solution for this:
    % https://gpdb.docs.pivotal.io/6-9/admin_guide/load/topics/g-loading-data-with-copy.html
    % The COPY source file must be accessible to the postgres process on the master host. 
    % Specify the COPY source file name relative to the data directory on the master host, or specify an absolute path.
    UseCopy = false;  %%% !!!!!!!!    
    
    % CsvFileName
    Q.select(Columns, 'CsvFileName', 'C:/temp/test_select.csv');

    % Compare performance of SELECT vs COPY TO
    for Iter=1:5
        TempFile = 'C:/temp/__tmp1.csv';
        delete(TempFile);

        t = tic();
        RecSelect = Q.select(Columns,  'Limit', Limit);
        io.msgStyle(LogLevel.Test, 'magenta', 'SELECT: %0.5f', double(tic()-t)/1e7);

        t = tic();
        
        % @Todo
        RecCopy   = Q.select(Columns,  'Limit', Limit, 'UseCopy', UseCopy);   %, 'TempName', TempFile);
        io.msgStyle(LogLevel.Test, 'magenta', 'SELECT using COPY: %0.5f', double(tic()-t)/1e7);

        assert(numel(RecSelect.Data) == numel(RecCopy.Data));
    end
    
    %Mat1 = Q.select('fdouble1,fdouble2,fdouble3',       'Where', 'fdouble1 > fdouble2', 'OutType', 'mat', 'Limit', Limit);
    Rec1 = Q.select('recid,fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'Limit', Limit, 'UseCopy', UseCopy, 'TempName', 'C:/temp/__tmp1.csv');
    Rec2 = Q.select('recid,fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'Limit', Limit, 'UseCopy', UseCopy, 'TempName', 'C:/temp/__tmp2.csv');
        
    
    % Insert Mat
    Q = db.DbQuery('unittest:master_table', 'InsertRecFunc', @make_recid);    
    Mat = rand(10, 3);
    Q.insert(Mat, 'ColNames', 'fdouble1,fdouble2,fdouble3');
    
    DoubleColumns = 'fdouble1,fdouble2,fdouble3';
    Cols = numel(strsplit(DoubleColumns, ','));
    Mat = rand(1000, Cols);
    Q.insert(Mat, 'ColNames', DoubleColumns);
    
    % Call tests using 'UnitTest' database    
    TestAll = true;
    if TestAll
        testSelect(Q);        
        testInsertRaw(Q);
        testInsert(Q);
        
        % @Todo - Not fully implemented yet
        %testUpdate(Q);
        %testDelete(Q);
        %testCopy(Q);
        
    end
        
    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
    Result = true;
end


% @Perf
% 
function Result = make_recid(Query, Rec, First, Last, Args)
    arguments
        Query   db.DbQuery
        Rec     db.DbRecord
        First
        Last
        Args.PK = 'recid'
    end
    UU = Rec.newKey();
    for i=First:Last
        Rec.Data(i).(Args.PK) = sprintf('PK_%s_%08d', UU, i);
    end
    Result = true;
end


%==========================================================================
%                              Test Select
%==========================================================================

function Result = testSelect(Q)
    % Test SELECT functionality and DbQuery.select()    
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery.select test started')
    
    Count = Q.selectCount('TableName', 'master_table');
    if Count == 0
        Result = false;
        io.msgLog(LogLevel.Warning, 'testSelect: Table master_table is empty, cannot test select. Try again after calling testInsert');
        return;
    end

    %----------------------------------------------------- Simple select & convert
    Limit = 100;
    R = Q.select('*', 'TableName', 'master_table', 'Limit', Limit);
    assert(isa(R, 'db.DbRecord'));       
    
    % Table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'recid').recid{1}, R.Data(i).recid));
    end

    % Cell
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{i, 1}, R.Data(i).recid));
    end    

    % Select and load to matrix
    R = Q.select('fdouble1,fdouble2', 'Where', 'fdouble > 0');    
    Mat = R.convert2mat();
    for i=1:numel(R.Data)
        assert(Mat(i, 1) == R.Data(i).fdouble1);
        assert(Mat(i, 2) == R.Data(i).fdouble2);
    end    

    % @Todo @QA - write test
    AstTab = R.convert2AstroTable();     
    AstCat = R.convert2AstroCatalog();

    
    % Select and load records, conevrt to table
    Limit = 100;
    Record = Q.select('*', 'TableName', 'master_table', 'Limit', Limit);
    assert(isa(Record, 'db.DbRecord'));       
    Table = Record.convert2table();
    
    % Select and load records, automatically convert to output type
    Columns = '*';
    Table = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'table', 'Limit', Limit);
    assert(isa(Table, 'table'));       
    Cell = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'cell', 'Limit', Limit);
    assert(isa(Cell, 'cell'));  
    
    %----------------------------------------------------- Select double fields into Mat/AstroTable
    Columns = 'fdouble1,fdouble2';
    Mat = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'mat', 'Limit', Limit);    
    assert(isa(Mat, 'double'));       
    AstTable = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'AstroTable', 'Limit', Limit);
    assert(isa(AstTable, 'AstroTable'));       
    AstCatalog = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'AstroCatalog', 'Limit', Limit);    
    assert(isa(AstCatalog, 'AstroCatalog'));       
    
    % Switch to another table
    Q.TableName = 'master_table';

       
    % Test copy to/from table - high performance insert/export operations
    % DbQuery.copyTest();

    R = Q.select('*', 'Limit', 1);
    assert(numel(R.Data) == 1);

    % getTableColumnList     
    Columns = Q.getTableColumnList('master_table');
    assert(numel(Columns) > 0);
    disp(Columns);

    % Select and load records, automatically convert to output type
    Columns = 'fdouble1,fdouble2,fdouble3,fdouble4,fdouble5';
    Where = 'fdouble1 > fdouble2';
    Limit = 100000;
    Output = 'mat';
    Mat = Q.select(Columns, 'TableName', 'master_table', 'where', Where, 'OutType', Output, 'Limit', Limit);
    Size = size(Mat);
    disp(Size(1));
    
    % ---------------------------------------------- Select - all field types
    % NOTE: At this point, we assume that tables master_table and
    % details_table exist and are not empty

    % Select two fields from table, using LIMIT
    Q.query('SELECT count(*)');
    count = Q.getColumn('count');
    if count > 0

        R = Q.select('RecId, FInt', 'Limit', 5);
        assert(Q.ColCount == 2);

        % Get fields list as celarray
        columns = Q.getColumnList();
        assert(all(size(columns)) > 0);

        % @Todo @Chen
        % Get fields list as empty table
        %tab = Q.getFieldTable();
        %assert(all(size(tab)) > 0);

        % Get all fields (Note: Field names are lower-case only)
        % All these fields should exist in table 'master_table'
        R = Q.select('*', 'Limit', 5);
        Rec = R.Data(1);
        RecID = Rec.recid;
        assert(~isempty(Rec.recid));            
        InsertTime = Rec.inserttime;
        UpdateTime = Rec.updatetime;
        FInt = Rec.fint;
        FBigInt = Rec.fbigint;
        FBool = Rec.fbool;
        FDouble = Rec.fdouble;
        FTimestamp = Rec.ftimestamp;
        FString = Rec.fstring;

        % Try to access undefined field
        catched = false;
        try
            temp = Rec.NameOfUnknownField;
        catch
            catched = true;
        end
        assert(catched);

        % Test select() function
        % select RecId, FInt, FBigInt from master_table where recid != ''
        Rec2 = Q.select('RecID, Fint', 'TableName', 'master_table', 'Where', 'Fint > 0');
        assert(numel(Rec2.Data) >= 0);
    else
        io.msgStyle(LogLevel.Test, '@warn', 'Table master_table is empty, select tests are skipped');
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery.select test passed')    
    Result = true;
end

%==========================================================================
%                              Test Insert
%==========================================================================
function Result = testInsertRaw(Q)
    % Assume that we are the single process writing to this table at the moment
    
    %----------------------------------------------------- INSERT with exec()
    % Insert using raw SQL by calling Q.exec() directly
    CountBeforeInsert = Q.selectCount();
    InsertCount = 10;
    io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectCount());
    for i = 1:InsertCount
        Q.SqlText = sprintf("INSERT INTO master_table (recid, fint, fdouble) VALUES('%s', %d, %f)",...
            Component.newUuid(), randi(100), randi(100000));
        Q.exec();
    end
    io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
    
    % Assume that we are the single process writing to this table at the moment
    CountAfterInsert = Q.selectCount();
    assert(CountBeforeInsert + InsertCount == CountAfterInsert); 
    
    % Insert batch using raw SQL: Prepare multiple INSERT lines   
    % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
    TestBatch = true;
    if (TestBatch)
        CountBeforeInsert = Q.selectCount();
        InsertCount = 10;
        
        % Prepare multi-line INSERT 
        Sql = '';
        for i = 1:InsertCount
            % Prepare statement
            uuid = Component.newUuid();
            SqlLine = sprintf("INSERT INTO master_table(RecID,FInt,FString) VALUES ('%s',%d,'Batch_%03d');", uuid, i, i).char;
            Sql = [Sql, SqlLine];
        end           
        
        % Execute as one statement
        io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectCount());        
        Q.exec(Sql);
        assert(Q.ExecOk);
        io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
        CountAfterInsert = Q.selectCount();
        assert(CountBeforeInsert + InsertCount == CountAfterInsert); 
    end
end

%==========================================================================

function Result = makePrimaryKeyForMat(Q, Rec, First, Last)
    % Make priamry key, called by DbQuery.insert()

    %
    for i=First:Last
        Rec.Data(i).recid = sprintf('Callback_%05d_%s', i, Rec.newKey());
    end
    
    Result = true;
end


function Result = testInsert(Q)
    % Test DbQuery.insert()
    
    % Assume that we are the single process writing to this table at the moment
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery.insert test started')
    
    %----------------------------------------------------- insert()
    Test1 = true;
    if Test1
        % Prepare DbRecord with 3 records, generate primary key
        Iters = 4;
        Count = 1;
        for Iter=1:Iters
            R = db.DbRecord;    
            for i = 1:Count
                R.Data(i).recid = R.newKey();
                R.Data(i).fint = 1000 + i;
                R.Data(i).fbool = true;
                R.Data(i).fstring = sprintf('MyStr_%03d', i);        
            end    
            io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectCount());
            Q.insert(R, 'BatchSize', 1000);
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
            Count = Count*10;
        end

        % Insert Mat, generate primary key in struct and use DbRecord.merge()
        Rows = 10;
        Mat = rand(Rows, 2);
        R = db.DbRecord(Mat, 'ColNames', {'fdouble1', 'fdouble2'});
        S = struct;
        for i=1:Rows
            S(i).recid = R.newKey();
        end

        % Merge DbRecord with struct-array, for example to add primary-key or
        % other data to Matrix
        R.merge(S);
        
        Q.insert(R, 'BatchSize', 100);
        io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());    
    end
    
    
    % Insert with user-function to generate primary key
    Test2 = true;
    if Test2
        Iters = 5;
        Count = 1;
        Cols = 20;
        ColNames = {};
        for i=1:Cols
            ColNames{i} = sprintf('fdouble%d', i);
        end        
        
        for Iter=1:Iters
            Mat = rand(Count, Cols);
            Q.insert(Mat, 'ColNames', ColNames, 'InsertRecFunc', @makePrimaryKeyForMat, 'BatchSize', 10000);
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());    
            Count = Count * 10;
        end
    end

    % @Todo - test insert() using copy, for large number of records
    TestCopy = false;
    if TestCopy
    end
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery.insert test passed')
    Result = true;
end

%==========================================================================

function Result = testUpdate(Q)
    % Test DbQuery.update()
    
    Q.TableName = 'master_table';
    
    % Insert some records
    Uuid = Component.newUuid();
    RecCount = 5;
    R = db.DbRecord;    
    for i = 1:RecCount
        R.Data(i).recid = sprintf('%s_%02d', Uuid, i);
        R.Data(i).fint = 0;
        R.Data(i).fdouble = 0;            
        R.Data(i).fstring = sprintf('MyStr_%03d', i);        
    end    
    Q.insert(R);
    
    % Update all records with recid that starts with Uuid_
    for i=1:10
        Where = sprintf('recid like ''%s_%%''', Uuid);
        MyStr = sprintf('NewValue_%04d', i);
        Q.update(sprintf('fint=%d,fdouble=%f,fstring=''%s''', i, 0.1*i, MyStr), 'Where', Where);

        R = Q.select('fint,fdouble,fstring', 'Where', Where);        
        for j=1:RecCount
            assert(R.Data(j).fint == i);
            assert(strcmp(R.Data(j).fstring, MyStr));
        end
    end
    
    Result = true;
end

%==========================================================================

function Result = testDelete(Q)
    % Test DbQuery.deleteRecord()
    
    Result = false;
    Q.TableName = 'master_table';
    for Iter=1:5
        Count1 = Q.selectCount();    
        if Count1 > 0
            R = Q.select('recid', 'Limit', 1);
            assert(isa(R, 'db.DbRecord'));       
            recid = R.Data(1).recid;
            Where = sprintf("recid = '%s'", recid);
            Q.deleteRecord('Where', Where);

            Count2 = Q.selectCount();
            assert(Count2 == (Count1-1));
        end
    end
    
    Result = true;
end

%==========================================================================

function Result = testCopy(Q)
    
    Result = true;
    return;
    
    % ---------------------------------------------- copy
    % https://www.postgresqltutorial.com/psql-commands/
    % https://kb.objectrocket.com/postgresql/postgresql-psql-examples-part-2-1043
    % From psql:
    % psql -U postgres
    % dbname unittest
    % \dt
    % \d master_table
    % \COPY master_table TO 'c:\temp\aa1.csv' DELIMITER ',' CSV HEADER

    Q.copyFrom('master_table', 'c:\temp\aa1c.csv');
    Q.copyFrom('master_table', 'c:\\temp\\aa1c.csv', 'Columns', 'recid,fint');
    Q.copyTo('master_table', 'c:\\temp\\a1.csv');           
    Q.copyTo('master_table', 'c:\\temp\\a2.csv', 'Columns', 'recid,fint');

    % Q.exec("copy master_table to 'c:\\temp\\a2.csv' delimiter ',' csv header");

    MyFileName = mfilename('fullpath');       
    [MyPath, ~, ~] = fileparts(MyFileName);            
    CsvFileName = 'unittest_csv1_to.csv';  %fullfile(MyPath, 'unittest_csv1.csv');

    %Q.copyFrom('master_table', CsvFileName);

    Result = true;
end

