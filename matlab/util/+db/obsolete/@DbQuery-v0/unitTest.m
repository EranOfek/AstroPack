

function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % ---------------------------------------------- Connect
    % NOTE: Database 'unittest' should exist

    % Create database connection
    %Conn = db.DbConnection;
    %Conn.DatabaseName = 'unittest';
    %Conn.open();

    Conn = db.Db.getUnitTest();

    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
    Q = db.DbQuery(Conn);
    Q.query('SELECT version()');
    assert(Q.ColCount == 1);
    pgver = Q.getField('version');
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

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

    % ---------------------------------------------- getTableFieldList 
    Q = db.DbQuery(Conn);
    Fields = Q.getTableFieldList('master_table');
    disp(Fields);

    % ---------------------------------------------- Select
    % NOTE: At this point, we assume that tables master_table and
    % details_table exist and are not empty

    % Select two fields from table, using LIMIT
    Q = db.DbQuery(Conn);
    Q.query('SELECT count(*) FROM master_table');
    count = Q.getField('count');
    if count > 0

        Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
        assert(Q.ColCount == 2);

        % Get fields list as celarray
        fields = Q.getFieldList();
        assert(all(size(fields)) > 0);

        % Get fields list as empty table
        tab = Q.getFieldTable();
        assert(all(size(tab)) > 0);

        % Get entire record (Note: Field names are lower-case only)
        Rec = Q.getRecord();
        assert(~isempty(Rec.recid));
        assert(~isempty(Rec.fint));

        % Load entire result set
        Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
        B = Q.loadAll();
        assert(~isempty(B));

        % Load entire result set to memory
        Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
        Data = Q.loadAll();
        assert(size(Data, 2) == 2);

%                 % Load as table
%                 Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
%                 Tab = Q.loadTable();
%                 sz = size(Tab)
%                 assert(sz(1) > 1);
%                 assert(sz(2) > 1);

        % Select all fields from table, using LIMIT
        Q.query(['SELECT * FROM master_table LIMIT 10']);

        % Load current record to memory
        delete(Rec);
        Rec = [];
        assert(isempty(Rec));
        Rec = Q.getRecord();
        assert(~isempty(Rec));

        % Get all fields (Note: Field names are lower-case only)
        % All these fields should exist in table 'master_table'
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
        Q.select('RecID, Fint', 'master_table', 'where', 'Fint > 0');
        Rec2 = Q.getRecord();
        assert(~isempty(Rec2));  
    else
        io.msgStyle(LogLevel.Test, '@warn', 'Table master_table is empty, select tests are skipped');
    end

    % @Todo - fails on changes of 22/06/2021 !!!
    Result = true;

    % ---------------------------------------------- insertRecord: struct

    % Create struct with different types of fields
    s = struct;            
    s.recid = Component.newUuid();
    Q.insertRecord('master_table', s);            

    % int32
    s = struct;            
    s.recid = Component.newUuid();
    s.fint = int32(1);
    Q.insertRecord('master_table', s);            

    % bool
    s = struct;            
    s.recid = Component.newUuid();
    s.fbool = true;
    Q.insertRecord('master_table', s);             

    % bigint            
    s = struct;            
    s.recid = Component.newUuid();            
    s.fbigint = int64(3);
    Q.insertRecord('master_table', s);             

    % double
    s = struct;            
    s.recid = Component.newUuid();            
    s.fdouble = double(5);
    Q.insertRecord('master_table', s);                         

    % string
    s = struct;            
    s.recid = Component.newUuid();                        
    s.fstring = 'abcd';
    Q.insertRecord('master_table', s);             

    % Insert struct with field mapping
    s = struct;            
    s.recid = Component.newUuid();
    s.fintTest = int32(1);
    map = struct;
    map.fintTest = 'fint';            
    Q.insertRecord('master_table', s, 'FieldMap', map);

    % ---------------------------------------------- insertRecord: DbRecord
    r = db.DbRecord;
    r.addProp('recid', Component.newUuid());
    r.addProp('fint', 3);
    Q.insertRecord('master_table', r);

    % ---------------------------------------------- Insert with plain SQL text

    % Insert records            
    io.msgLog(LogLevel.Test, 'testing INSERT...');
    InsertCount = 1;
    for i = 1:InsertCount
        % Prepare statement
        uuid = Component.newUuid();
        sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, i).char;
        Q.exec(sql);
        assert(Q.ExecOk);
    end           

    % Make sure all records were inserted
    Q.query('SELECT count(*) FROM master_table');
    count = Q.getField('count');
    assert(count > InsertCount);

    % Insert batch
    % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
    TestBatch = false;
    if (TestBatch)
        InsertCount = 1;
        sql = '';
        for i = 1:InsertCount
            % Prepare statement
            uuid = Component.newUuid();
            sql2 = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d);", uuid, i).char;
            sql = [sql sql2];
        end           
        Q.exec(sql);
        assert(Q.ExecOk);            
    end

    % Test insert() function

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

    % ---------------------------------------------- Delete
    sql = sprintf("DELETE FROM master_table WHERE RecID='%s'", uuid);
    Q.exec(sql);           
    count2 = Q.selectCount('master_table');
    assert(count2 == count);

    % ---------------------------------------------- Create database
    %

    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
    Result = true;
end

