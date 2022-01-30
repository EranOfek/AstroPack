
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
        
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'DbAdmin test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
    
    % Connect to server
    % psql -h gauss -U admin -d unittest -W
    
    % Create DbAdmin from DbQuery Connection
    Q = db.DbQuery('unittest');
    Admin = db.DbAdmin('DbQuery', Q);
    
    % Create DbAdmin without connection, use it to create Config file
    Admin = db.DbAdmin();    
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'admin_db', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    assert(~strcmp(ConfigFileName, ''));

    
    % Create DbAdmin from arguments
    Admin = db.DbAdmin('Host', 'localhost', 'Port', 5432, 'UserName', 'Admin', 'Password', 'Passw0rd', 'DatabaseName', 'unittest');    
    
    
    % Get list of databases
    DbList1 = Admin.getDbList();
    
    % Create DB from sqlfile
    Admin.createDatabase('SqlFileName', 'unitTest_createDatabase.sql');
    
    % Create DB from xls
    %Admin.createDatabase('XlsFileName', 'unitTest_createDatabase.xlsx');
    
    % Create DB with args - no support yet
    
    
    
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

    Admin.createTable('SqlText', SqlText);
    
    % Create table with args
    Admin.createTable('TableName', 'newtable_2', 'PrimaryKeyDef', 'new_Pkey int');
    
    % Create table with sql file
    Admin.createTable('SqlFileName', 'unitTest_createTable.sql');
    
    
    % Add column to table
    Admin.addColumn('newtable_1', {'MyColA'}, {'INTEGER'}, {'DEFAULT 0'});
    

    % Add index to table
    index_added = 0;
    IndexList1 = Q.getTableIndexList('newtable_1');
    index_added = Admin.addIndex('newtable_1', 'newtable_1_idx_FDouble5', 'USING btree (FDouble1)');
    IndexList2 = Q.getTableIndexList('newtable_1');
    assert((index_added == 1) && (length(IndexList2) == length(IndexList1)+1));


    % Users:
    % Get list of users (roles)
    UserList1 = Admin.getUserList();    
    
    % Add user
    Admin.addUser('Test1', 'Password1');
    UserList2 = Admin.getUserList();
    
    assert(length(UserList2) == length(UserList1)+1);

    % Remove user
    Admin.removeUser('Test1');
    UserList3 = Admin.getUserList();
    
    assert(length(UserList3) == length(UserList1));
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbAdmin test passed');
    Result = true;
end

