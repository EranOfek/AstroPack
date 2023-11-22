
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
    try
        pgver = Q.getDbVersion();
    catch
        warning('Connection to the DB cannot be establsihed');
        Result = 2;
        return        
    end
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    % Test database/table/user creation
    testAdmin();
    
    % Select into CSV file (CsvFileName)
    % Shared folder must be accessible
    % When working on Windows, make sure that the shared folder in visible
    % in the File Explorer
    % NOTE: When using 'UseCopy'=true, there must be a shared folder on the
    % server which is mountable from the client. See details in +db/doc/ folder.
    CsvFileName = Q.select('*', 'TableName', 'master_table', 'Limit', 10000, 'UseCopy', true, 'Load', false);
    %assert(~isempty(CsvFileName));
    %assert(isfile(CsvFileName));    
    
    % Select into specified CSV file
    % NOTE: DbQuery.writeResultSetToCsvFile() fails sometimes from unknown
    % reasons. Seems that the Java object CSVWriter fails. This behaviour
    % is not clear yet.
    CsvFileName = fullfile(tools.os.getTestWorkDir(), 'unittest_dbquery_select1.csv');
    if isfile(CsvFileName)
        delete(CsvFileName);
    end
    assert(~isfile(CsvFileName));    
    Q.select('*', 'TableName', 'master_table', 'Limit', 10000, 'CsvFileName', CsvFileName);
    %assert(isfile(CsvFileName));    
    
    % Get Metadata    
    % Get tables list
    TablesList = Q.getTablesList();
    assert(~isempty(TablesList));
    ColumnList = Q.getTableColumnNames('master_table');
    assert(~isempty(ColumnList));
    
    Q = db.DbQuery('unittest:master_table');
    Rec = Q.select('*', 'TableName', 'master_table', 'Limit', 1000);
    assert(Rec.getRowCount() > 0);
    assert(Rec.getRowCount() <= 1000);
    
    ColNames = Q.getColumnNamesOfType('Double');
    assert(~isempty(ColNames));
    assert(strcmpi(Q.getColumnType(ColNames(1)), 'Double'));
    first_col = ColNames(1);
    assert(strcmpi(Q.columnName2matlab(first_col{1}),'fdouble'));
    
    % Get list of fields composing the primary key
    PkList = Q.getTablePrimaryKey('master_table');
    assert(~isempty(PkList));
    
    % Get list of index NAMES (not the fields)
    IndexList = Q.getTableIndexList('master_table');
    assert(~isempty(IndexList));
    
    % Convert AstroHeader to DbRecord
    H = AstroHeader();
    H.insertKey('Key1');
    H.insertKey({'KeyA','ValueA','CommentA';'KeyB','ValueB','CommentB'},'end-1');
    R = db.DbRecord(H);
    Q.insert(H, 'TableName', 'master_table', 'InsertRecFunc', @make_recid, 'ColumnsOnly', true);
    
    H = AstroHeader();
    H.insertKey({'FInt1',1,'CommentA';'FInt2',2,'CommentB';'FIntXX','XX','CommentXX'},'end-1');
    Q.insert(H, 'TableName', 'master_table', 'InsertRecFunc', @make_recid, 'ColumnsOnly', true);
    
    %
    Q = db.DbQuery('unittest:master_table');
    Q = db.DbQuery('unittest', 'TableName', 'master_table');
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Q.selectTableRowCount());
            
    ColNames = 'fdouble1,fdouble2,fdouble3';
    if isfield(Q.Config.Data.Database.DbConnections.UnitTest, 'DoubleFields')
        ColNames = Q.Config.Data.Database.DbConnections.UnitTest.DoubleFields;
    end
    
    Cols = numel(strsplit(ColNames, ','));
    Mat = rand(1000, Cols);
    Q.insert(Mat, 'ColNames', ColNames, 'InsertRecFunc', @make_recid, 'InsertRecArgs', {'PK', 'recid'});
    
    % Select fields
    Q = db.DbQuery('unittest:master_table');    
    Limit = 10000;    
    Columns = 'recid,fdouble1,fdouble2,fdouble3';    

    % Compare performance of SELECT vs COPY TO
    TestPerf = false;
    if TestPerf
        CsvFileName = fullfile(tools.os.getTestWorkDir(), 'unittest_dbquery_select.csv');    
        for Iter=1:3
            delete(CsvFileName);

            t = tic();
            RecSelect = Q.select(Columns,  'Limit', Limit, 'Load', true);
            io.msgStyle(LogLevel.Test, 'magenta', 'SELECT: %0.5f', double(tic()-t)/1e7);

            t = tic();
            RecCopy = Q.select(Columns,  'Limit', Limit, 'UseCopy', true, 'Load', true);
            io.msgStyle(LogLevel.Test, 'magenta', 'SELECT using COPY: %0.5f', double(tic()-t)/1e7);

            assert(numel(RecSelect.Data) == numel(RecCopy.Data));
        end   
    end
    
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
        
        % Deleting all records
        Q.deleteRecord();
        
        testInsertRaw(Q);
        testInsert(Q);
        testSelect(Q);
        
        % @Todo - Not fully implemented yet
        testUpdate(Q);
        testDelete(Q);
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
    
    Count = Q.selectTableRowCount('TableName', 'master_table');
    if Count == 0
        Result = false;
        io.msgLog(LogLevel.Warning, 'testSelect: Table master_table is empty, cannot test select. Try again after calling testInsert');
        return;
    end

    %----------------------------------------------------- Simple select & convert
    Limit = 100;
    R = Q.select('*', 'TableName', 'master_table', 'Limit', Limit);
    assert(isa(R, 'db.DbRecord'));
    
    %CsvFileName = Q.select('*', 'TableName', 'master_table', 'Limit', Limit, 'UseCsv', true, 'Load', false');
    %csvtemp = io.files.readtable1(CsvFileName, 'ReadVariableNames', false);
    %assert(~isempty(csvtemp));
    %assert(height(csvtemp) == length(R.Data));
    
    % Table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'recid').recid{1}, R.Data(i).recid));
    end

    % Cell
    % NOTE: Now table's primary key is IDENTITY COLUMN, 'recid' is the
    % second column, therefore we use 'Cel{u, 2}'
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{i, 2}, R.Data(i).recid));
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

    % getTableColumnNames     
    Columns = Q.getTableColumnNames('master_table');
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
    CountBeforeInsert = Q.selectTableRowCount();
    InsertCount = 10;
    io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectTableRowCount());
    for i = 1:InsertCount
        Q.SqlText = sprintf("INSERT INTO master_table (recid, fint, fdouble) VALUES('%s', %d, %f)",...
            Component.newUuid(), randi(100), randi(100000));
        Q.exec();
    end
    io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectTableRowCount());
    
    % Assume that we are the single process writing to this table at the moment
    CountAfterInsert = Q.selectTableRowCount();
    assert(CountBeforeInsert + InsertCount == CountAfterInsert); 
    
    % Insert batch using raw SQL: Prepare multiple INSERT lines   
    % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
    TestBatch = true;
    if (TestBatch)
        CountBeforeInsert = Q.selectTableRowCount();
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
        io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectTableRowCount());        
        Q.exec(Sql);
        assert(Q.ExecOk);
        io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectTableRowCount());
        CountAfterInsert = Q.selectTableRowCount();
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
            io.msgLog(LogLevel.Debug, 'Count before insert: %d', Q.selectTableRowCount());
            Q.insert(R, 'BatchSize', 1000);
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectTableRowCount());
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
        io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectTableRowCount());    
    end
    
    
    % Insert with user-function to generate primary key
    % NOTE: Requries access to server shared folder
    TestInsert2 = false;
    if TestInsert2
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
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectTableRowCount());    
            Count = Count * 10;
        end
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
    Q.InsertRecFunc = '';
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
        Count1 = Q.selectTableRowCount();    
        if Count1 > 0
            R = Q.select('recid', 'Limit', 1);
            assert(isa(R, 'db.DbRecord'));       
            recid = R.Data(1).recid;
            Where = sprintf("recid = '%s'", recid);
            Q.deleteRecord('Where', Where);

            Count2 = Q.selectTableRowCount();
            assert(Count2 == (Count1-1));
        end
    end
    
    Result = true;
end



function Result = testAdmin()
    % Admin Unit-Test (see also examples.m)
    %
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use pgAdmin and DataGrip by JetBrains
    %
    % Requirements:
    %
    % 1. Install Postgres v14 - See detailed instructions in files:
    %
    %       postgres_installation_ubuntu18.md
    %       postgres_shared_folder.md
    %
    % 2. Install pgAdmin4 (version 6.3) as administration tool    
    %    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    %    On Linux, use DataGrip by JetBrains (License is required)
    %
    % 3. You need to have configuration file with database user and password, as:
    %
    %       config/local/Database.DbConnections.UnitTest.yml
    %
    % 4. Database 'unittest' should already exist with table 'master_table'
    %
    
    % Modify Host= below if you do not have access to server 'gauss'
    Host = 'gauss';
    
    % Uncomment this line if you do not have access to server 'gauss'
    % and your database server is local on your computer.
    %Host = 'localhost'
        
    % Set log levels for maximum details
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'Admin test started')
    io.msgLog(LogLevel.Test, 'DATABASE SERVER HOST: %s', Host);
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
    
    % Connect to server, this is the command line syntax of 'psql'
    % psql -h gauss -U admin -d unittest -W
    
    % Get path to folder of sources, to run '.sql' files
    [MyPath,~,~] = fileparts(mfilename('fullpath'));
    
    % Create DbQuery instance with connetion details from configuration
    % file 'Database.DbConnections.UnitTest.yml', so Query created below
    % is linked to this connection.
    Query = db.DbQuery('unittest');
    DbList1 = Query.getDbList();
    assert(numel(DbList1) > 0);
    
    % Create Query without connection, use it to create Config file
    Query = db.DbQuery();    
    ConfigFileName = Query.createConnectionConfig('DatabaseName', 'admin_db', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    assert(~strcmp(ConfigFileName, ''));
  
    % Create DbQuery from arguments
    Query = db.DbQuery('Host', Host, 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd', 'DatabaseName', 'unittest');    
    assert(Query.isTableExist('master_table'));      
    
    % getSchemaTable
    [Schema, TN] = db.DbQuery.getSchemaTable('table_name');
    assert(strcmp(Schema, 'public') && strcmp(TN, 'table_name'));
    [Schema, TN] = db.DbQuery.getSchemaTable('schema.table_name');
    assert(strcmp(Schema, 'schema') && strcmp(TN, 'table_name'));
    
    % getSchema
    Schema = db.DbQuery.getSchema('table_name');
    assert(strcmp(Schema, 'public'));
    Schema = db.DbQuery.getSchema('schema.table_name');
    assert(strcmp(Schema, 'schema'));
    
    % getTable
    TN = db.DbQuery.getTable('table_name');
    assert(strcmp(TN, 'table_name'));
    TN = db.DbQuery.getTable('schema.table_name');
    assert(strcmp(TN, 'table_name'));    
     
    % CamelToSnake, SnakeToCamel
    s = db.DbQuery.camelToSnake('myCamelCase');
    assert(strcmp(s, 'my_camel_case'));
    s = db.DbQuery.snakeToCamel('my_camel_case');    
    assert(strcmp(s, 'myCamelCase'));    
    
    % NameToColumnName
    s = db.DbQuery.NameToColumnName('abc');
    assert(strcmp(s, 'abc'));
    s = db.DbQuery.NameToColumnName('group');
    assert(strcmp(s, 'f_group'));    
    
    % ColumnNameToName
    s = db.DbQuery.ColumnNameToName('mycol');
    assert(strcmp(s, 'mycol'));
    s = db.DbQuery.ColumnNameToName('f_group');
    assert(strcmp(s, 'group'));
    s = db.DbQuery.ColumnNameToName('f_abc');
    assert(strcmp(s, 'f_abc'));    
    
    
    % Get list of databases
    DbList1 = Query.getDbList();
    assert(numel(DbList1) > 0);
            
    % Create empty database
    Query.createDb('DatabaseName', 'mydb1');
    assert(Query.isDbExist('mydb1'));

    %--------------------------------------- Q2: Test with new database'mydb1'
    % Connect Q2 to the new db (MUST DO)
    Q2 = db.DbQuery(Query, 'Database', 'mydb1');
    
    % Create schema and table in it
    Q2.createSchema('test_schema');
    Q2.createTable('TableName', 'test_schema.test_table', 'AutoPk', 'pk', 'Drop', true);
    assert(Q2.isTableExist('test_schema.test_table'));
    Q2.dropTable('test_schema.test_table');
    assert(~Q2.isTableExist('test_schema.test_table'));    
    
    % Create table
    Q2.createTable('TableName', 'mytable1', 'AutoPk', 'pk', 'Drop', true);
    assert(Q2.isTableExist('mytable1'));
    Q2.addColumn('mytable1', 'f1', 'double', 'default 0', 'Comment', 'comment for f1');
    Q2.addColumn('mytable1', 'f2', 'double', 'default 0', 'Comment', 'my comment for f2');
    Q2.addColumn('mytable1', 'f3', 'double', 'default 0', 'index', true, 'Comment', 'comment for f3');
    Q2.addIndexOnColumn('mytable1', 'f1');
    
    assert(Q2.isColumnExist('mytable1', 'f1'));
    assert(Q2.isColumnExist('mytable1', 'f2'));
    
    Comment = Q2.getColumnComment('mydb1', 'mytable1', 'f2');
    assert(strcmp(Comment, 'my comment for f2'));
    
    Comments = Q2.getTableColumnsComments('mydb1', 'mytable1');
    assert(numel(Comments) == 4);
    assert(strcmp(Comments(3).column_name, 'f2'));
    assert(strcmp(Comments(3).column_comment, 'my comment for f2'));
       
    % Remove existing column comment
    Q2.addColumn('mytable1', 'f2', 'double', 'default 0', 'Comment', 'NULL');    
    Comment = Q2.getColumnComment('mydb1', 'mytable1', 'f2');
    assert(strcmp(Comment, ''));
    
    Q2.dropTable('mytable1');
    assert(~Q2.isTableExist('mytable1'));    
    
    % Create table with UUID primary key
    Q2.createTable('TableName', 'mytable2', 'AutoPk', 'pk', 'UuidPk', true, 'Drop', true);
    assert(Q2.isTableExist('mytable2'));
    
    %--------------------------------------- Q2 done
    
    % Create database 'dbadmin_unittest' from sqlfile
    Query.createDb('SqlFileName', fullfile(MyPath, 'unitTest_createDatabase.sql'));
    assert(Query.isDbExist('unittest'));
    
    % Create DB from xlsx file (Google Sheets)
    exist = Query.isDbExist('unittest3');
    Query.createDb('XlsxFileName', fullfile(MyPath, 'unittest3.xlsx'));
    assert(Query.isDbExist('unittest3'));
    
    % Create DB with args - no support yet    
    % @Todo    
    
    % Create table with specified SQL text
    SqlText = [...
        'DROP TABLE IF EXISTS newtable_1; '...
        'CREATE TABLE newtable_1 ('...
        'RecID VARCHAR NOT NULL,'...
        'FDouble1 DOUBLE PRECISION DEFAULT 0,'...
        'FInt1 INTEGER DEFAULT 0,'...
        'FString1 VARCHAR,'...
        'CONSTRAINT table1_pkey PRIMARY KEY(RecID)'...
        ');' ];

    % Execute the SQL text and check that table was created
    Query.createTable('SqlText', SqlText);
    assert(Query.isTableExist('newtable_1'));
    
    % Create table with args
    Query.createTable('TableName', 'newtable_2', 'PrimaryKeyDef', 'new_Pkey int');
    assert(Query.isTableExist('newtable_2'));
    
    % Create table with SQL file
    Query.createTable('SqlFileName', fullfile(MyPath, 'unitTest_createTable.sql'));
    assert(Query.isTableExist('newtable_3'));
    
    % Add new column to existing table
    Query.addColumn('newtable_1', 'MyColA', 'INTEGER', 'DEFAULT 0');
    assert(Query.isColumnExist('newtable_1', 'MyColA'));   

    % Add index to existing table
    IndexList1 = Query.getTableIndexList('newtable_1');
    index_added = Query.addIndex('newtable_1', 'newtable_1_idx_FDouble5', 'FDouble1');
    IndexList2 = Query.getTableIndexList('newtable_1');
    assert(index_added && any(strcmpi(IndexList2, 'newtable_1_idx_FDouble5')));
    
    % Users:
    % Get list of users (roles)
    UserList1 = Query.getUserList();    
    assert(numel(UserList1) > 0);
    
    % Add user
    Query.addUser('Test1', 'Password1');
    UserList2 = Query.getUserList();
    assert(any(strcmpi(UserList2, 'test1')));
    %assert(length(UserList2) == length(UserList1)+1);  @@@@Dan 

    % Remove user
    Query.removeUser('Test1');
    UserList3 = Query.getUserList();
    assert(~any(strcmpi(UserList3, 'test1')));
    %assert(length(UserList3) == length(UserList1));
    
    io.msgStyle(LogLevel.Test, '@passed', 'Admin test passed');
    Result = true;
end

