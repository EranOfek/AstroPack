

function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
    
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % Connect
    % NOTE: Database 'unittest' should exist
    Conn = db.Db.getUnitTest();
    Q = db.DbQuery('Connection', Conn);
    
    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'    
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));
    R = Q.loadResultSet();
    assert(strcmp(R.Data(1).version, pgver));
    
    %
    Q.TableName = 'master_table';
    
    % Select    
    %testSelect(Q);

    % Copy (import from file)
    %testCopy();
    
    % Insert
    testInsert(Q);
    
    % Update
    %testUpdate(Q);

    % Delete
    %testDelete(Q);

    % Other operations
    %testMisc();

    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
    Result = true;
end


%==========================================================================

function Result = testSelect(Q)

    % Select all fields, load to table and cell
    Q.TableName = 'master_table';
    R = Q.select('*');

    % Table
    Tab = R.convert2table();
    for i=1:numel(R.Data)
        assert(strcmp(Tab(i, 'recid').recid{1}, R.Data(i).recid));
    end

    % Cell
    Cel = R.convert2cell();
    for i=1:numel(R.Data)
        assert(strcmp(Cel{1, i}, R.Data(i).recid));
    end    

    % Select and load to matrix
    R = Q.select('fdouble,ftimestamp', 'Where', 'fdouble > 0');    
    Mat = R.convert2mat();
    for i=1:numel(R.Data)
        assert(Mat(1, i) == R.Data(i).fdouble);
        assert(Mat(2, i) == R.Data(i).ftimestamp);
    end    

    % 
    AstTab = R.convert2AstroTable();     
    AstCat = R.convert2AstroCatalog();

    % Test copy to/from table - high performance insert/export operations
    % DbQuery.copyTest();

    R = Q.select('*', 'Limit', 1);
    assert(numel(R.Data) == 1);

    % getTableFieldList     
    Fields = Q.getTableFieldList('master_table');
    assert(numel(Fields) > 0);
    disp(Fields);

    % ---------------------------------------------- Select (more)
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

        % Get fields list as empty table
        tab = Q.getFieldTable();
        assert(all(size(tab)) > 0);

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
            temp = Rec.abcabcabcabc;
        catch
            catched = true;
        end
        assert(catched);

        % Test select() function
        % select RecId, FInt, FBigInt from master_table where recid != ''
        Rec2 = Q.select('RecID, Fint', 'TableName', 'master_table', 'Where', 'Fint > 0');
        assert(numel(Rec2.Data) > 0);
    else
        io.msgStyle(LogLevel.Test, '@warn', 'Table master_table is empty, select tests are skipped');
    end
    
    Result = true;

end

%==========================================================================

function Result = testInsert(Q)
    
    % Insert using raw sql
    CountBeforeInsert = Q.selectTableCount();
    InsertCount = 10;
    for i = 1:InsertCount
        Q.SqlText = sprintf("INSERT INTO master_table (recid, fint, fdouble) VALUES('%s', %d, %f)",...
            Component.newUuid(), randi(100), randi(100000));
        Q.exec();
    end
    CountAfterInsert = Q.selectTableCount();
    assert(CountBeforeInsert + InsertCount == CountAfterInsert); 
    
    % Insert batch using raw sql: Prepare multiple INSERT lines   
    % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
    TestBatch = true;
    if (TestBatch)
        CountBeforeInsert = Q.selectTableCount();
        InsertCount = 10;
        Sql = '';
        for i = 1:InsertCount
            % Prepare statement
            uuid = Component.newUuid();
            SqlLine = sprintf("INSERT INTO master_table(RecID,FInt,FString) VALUES ('%s',%d,'Batch_%03d');", uuid, i, i).char;
            Sql = [Sql, SqlLine];
        end           
        Q.exec(Sql);
        assert(Q.ExecOk);            
        CountAfterInsert = Q.selectTableCount();
        assert(CountBeforeInsert + InsertCount == CountAfterInsert); 
    end

    % Test 
    % Prepare DbRecord with 3 records
    R = db.DbRecord;    
    for i = 1:300
        R.Data(i).recid = R.newKey();
        R.Data(i).fint = 1000 + i;
        R.Data(i).fstring = sprintf('MyStr_%03d', i);        
    end
    
    Result = Q.insertDbRecord(R, 'BatchSize', 100);
    
    % Todo: @Eran - What is the correct order of row,col ???
    Rows = 10;
    Mat = rand(2, Rows);
    R = db.DbRecord(Mat, 'ColNames', {'fdouble', 'ftimestamp'});
    S = struct;
    for i=1:Rows
        S(i).recid = db.DbRecord.newKey();
    end
    R.merge(S);
    Result = Q.insertDbRecord(R, 'BatchSize', 100);
    
%       
%     % ---------------------------------------------- insertRecord: struct
% 
%     % Create struct with different types of fields
%     s = struct;            
%     s.recid = Component.newUuid();
%     Q.insertRecord('master_table', s);            
% 
%     % int32
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fint = int32(1);
%     Q.insertRecord('master_table', s);            
% 
%     % bool
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fbool = true;
%     Q.insertRecord('master_table', s);             
% 
%     % bigint            
%     s = struct;            
%     s.recid = Component.newUuid();            
%     s.fbigint = int64(3);
%     Q.insertRecord('master_table', s);             
% 
%     % double
%     s = struct;            
%     s.recid = Component.newUuid();            
%     s.fdouble = double(5);
%     Q.insertRecord('master_table', s);                         
% 
%     % string
%     s = struct;            
%     s.recid = Component.newUuid();                        
%     s.fstring = 'abcd';
%     Q.insertRecord('master_table', s);             
% 
%     % Insert struct with field mapping
%     s = struct;            
%     s.recid = Component.newUuid();
%     s.fintTest = int32(1);
%     map = struct;
%     map.fintTest = 'fint';            
%     Q.insertRecord('master_table', s, 'FieldMap', map);
% 
%     % ---------------------------------------------- insertRecord: DbRecord
%     r = db.DbRecord;
%     r.addProp('recid', Component.newUuid());
%     r.addProp('fint', 3);
%     Q.insertRecord('master_table', r);

end


%==========================================================================

function Result = testUpdate(Q)

    
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
end


%==========================================================================

function Result = testCopy(Q)
    
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
