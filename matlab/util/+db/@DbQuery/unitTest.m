
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
    
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Create query with database:table, get number of records
    Q = db.DbQuery('unittest:master_table');
    Count = Q.selectCount();
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Count);
        
    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'    
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));
   
    % Call tests using 'UnitTest' database

    TestAll = true;
    if TestAll
        testSelect(Q);        
        testInsertRaw(Q);
        testInsert(Q);
        %testUpdate(Q);
        %testDelete(Q);
        %testCopy(Q);
        %testMisc(Q);

        % Tests using 'Pipeline' database
        %testPipeline();
    end
    
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
    Result = true;
end


%==========================================================================

function Result = testSelect(Q)
    % Test SELECT functionality and DbQuery.select()
    
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
    R = Q.select('fdouble,ftimestamp', 'Where', 'fdouble > 0');    
    Mat = R.convert2mat();
    for i=1:numel(R.Data)
        assert(Mat(i, 1) == R.Data(i).fdouble);
        assert(Mat(i, 2) == R.Data(i).ftimestamp);
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
    Fields = '*';
    Table = Q.select(Fields, 'TableName', 'master_table', 'Convert', 'table', 'Limit', Limit);
    assert(isa(Table, 'table'));       
    Cell = Q.select(Fields, 'TableName', 'master_table', 'Convert', 'cell', 'Limit', Limit);
    assert(isa(Cell, 'cell'));  
    
    %----------------------------------------------------- Select double fields into Mat/AstroTable
    Fields = 'fdouble,ftimestamp';
    Mat = Q.select(Fields, 'TableName', 'master_table', 'Convert', 'mat', 'Limit', Limit);    
    assert(isa(Mat, 'double'));       
    AstTable = Q.select(Fields, 'TableName', 'master_table', 'Convert', 'AstroTable', 'Limit', Limit);
    assert(isa(AstTable, 'AstroTable'));       
    AstCatalog = Q.select(Fields, 'TableName', 'master_table', 'Convert', 'AstroCatalog', 'Limit', Limit);    
    assert(isa(AstCatalog, 'AstroCatalog'));       
    
    % Switch to another table
    Q.TableName = 'master_table';

       
    % Test copy to/from table - high performance insert/export operations
    % DbQuery.copyTest();

    R = Q.select('*', 'Limit', 1);
    assert(numel(R.Data) == 1);

    % getTableFieldList     
    Fields = Q.getTableFieldList('master_table');
    assert(numel(Fields) > 0);
    disp(Fields);

    % ---------------------------------------------- Select - all field types
    % NOTE: At this point, we assume that tables master_table and
    % details_table exist and are not empty

    % Select two fields from table, using LIMIT
    Q.query('SELECT count(*)');
    count = Q.getField('count');
    if count > 0

        R = Q.select('RecId, FInt', 'Limit', 5);
        assert(Q.ColCount == 2);

        % Get fields list as celarray
        fields = Q.getFieldList();
        assert(all(size(fields)) > 0);

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
    
    Result = true;

end

%==========================================================================

function Result = makePrimaryKeyForMat(Q, Rec, Index)
    % Make priamry key, called for every row of Rec.Data(Index)

    if Index == 0
        First = 1;
        Last = numel(Rec.Data);
    else
        First = Index;
        Last = Index;
    end
    
    %
    for i=First:Last
        Rec.Data(i).recid = sprintf('Callback_%05d_%s', i, Rec.newKey());
    end
    
    Result = true;
end


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


function Result = testInsert(Q)
    % Assume that we are the single process writing to this table at the moment
    
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
            Result = Q.insert(R, 'BatchSize', 100);
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());
            Count = Count*10;
        end

        % Insert Mat, generate primary key in struct and use DbRecord.merge()
        Rows = 10;
        Mat = rand(Rows, 2);
        R = db.DbRecord(Mat, 'ColNames', {'fdouble', 'ftimestamp'});
        S = struct;
        for i=1:Rows
            S(i).recid = R.newKey();
        end

        % Merge DbRecord with struct-array, for example to add primary-key or
        % other data to Matrix
        R.merge(S);
        Result = Q.insert(R, 'BatchSize', 100);
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
            R = db.DbRecord(Mat, 'ColNames', ColNames);
            Result = Q.insert(R, 'PrimaryKeyFunc', @makePrimaryKeyForMat, 'BatchSize', 10000);
            io.msgLog(LogLevel.Debug, 'Count after insert: %d', Q.selectCount());    
            Count = Count * 10;
        end
    end
    
%       
%     % ---------------------------------------------- insertRecord: struct
% 
%     % Create struct with different types of fields
%     s = struct;            
%     s.recid = Component.newUuid();
%     Q.insert('master_table', s);            
% 
%     % int32
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fint = int32(1);
%     Q.insert('master_table', s);            
% 
%     % bool
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fbool = true;
%     Q.insert('master_table', s);             
% 
%     % bigint            
%     s = struct;            
%     s.recid = Component.newUuid();            
%     s.fbigint = int64(3);
%     Q.insert('master_table', s);             
% 
%     % double
%     s = struct;            
%     s.recid = Component.newUuid();            
%     s.fdouble = double(5);
%     Q.insert('master_table', s);                         
% 
%     % string
%     s = struct;            
%     s.recid = Component.newUuid();                        
%     s.fstring = 'abcd';
%     Q.insert('master_table', s);             
% 
%     % Insert struct with field mapping
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fintTest = int32(1);
%     map = struct;
%     map.fintTest = 'fint';            
%     Q.insert('master_table', s, 'FieldMap', map);
% 
%     % ---------------------------------------------- insertRecord: DbRecord
%     r = db.DbRecord;
%     r.addProp('recid', Component.newUuid());
%     r.addProp('fint', 3);
%     Q.insert('master_table', r);

end


%==========================================================================

function Result = testUpdate(Q)

    Result = true;
    return;
    
    % ---------------------------------------------- Update

    % string
    s = struct;            
    s.recid = Component.newUuid();                        
    s.fstring = 'Original Text';
    Q.insertRecord('master_table', s);             

    u = struct;
    u.fstring = 'My new TEXT';
    where = struct;
    where.recid = s.recid;
    Q.updateRecord('master_table', u, where);                         
    %function Result = updateRecord(Obj, TableName, Rec, WhereRec, Args)

    Q.deleteRecord('master_table', where);

    io.msgLog(LogLevel.Test, 'testing UPDATE...');
    UpdateCount = 100;
    uuid = Component.newUuid();
    sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
    Q.exec(sql);
    for i = 1:UpdateCount
        sql = sprintf("UPDATE master_table SET FInt=%d WHERE RecID='%s'", i, uuid);
        Q.exec(sql);

        sql = sprintf("SELECT RecID,FInt from master_table where RecID='%s'", uuid).char;
        Q.query(sql);
        val = Q.getField('FInt');
        assert(val == i);
    end            

    count2 = Q.selectCount('master_table');
    assert(count2 == count+1);

    
    Result = true;
end

%==========================================================================

function Result = testDelete(Q)

    Result = true;
    return;
    
    % ---------------------------------------------- Delete
    sql = sprintf("DELETE FROM master_table WHERE RecID='%s'", uuid);
    Q.exec(sql);           
    count2 = Q.selectCount('master_table');
    assert(count2 == count);

    % ---------------------------------------------- Create database
    %

    
    Result = true;
end

%==========================================================================

function Result = testMisc(Q)

    Result = true;
    return;
    
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
    Q.copyFrom('master_table', 'c:\\temp\\aa1c.csv', 'Fields', 'recid,fint');
    Q.copyTo('master_table', 'c:\\temp\\a1.csv');           
    Q.copyTo('master_table', 'c:\\temp\\a2.csv', 'Fields', 'recid,fint');

    % Q.exec("copy master_table to 'c:\\temp\\a2.csv' delimiter ',' csv header");

    MyFileName = mfilename('fullpath');       
    [MyPath, ~, ~] = fileparts(MyFileName);            
    CsvFileName = 'unittest_csv1_to.csv';  %fullfile(MyPath, 'unittest_csv1.csv');

    %Q.copyFrom('master_table', CsvFileName);

    Result = true;
end

%==========================================================================
%
%==========================================================================

function Result = testPipeline()

    io.msgStyle(LogLevel.Test, '@start', 'DbQuery-Pipeline test started')
    io.msgLog(LogLevel.Test, 'Postgres database "pipeline" should exist');
    
    %Q = DbQuery('pipeline', 'table', 'rawimages');
    
    %Q = DbQuery('pipeline:rawimages')
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery-Pipeline test passed')
    
    Result = true;
end

