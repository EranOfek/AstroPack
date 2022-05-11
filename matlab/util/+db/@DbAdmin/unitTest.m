
function Result = unitTest()
    % DbAdmin Unit-Test (see also DbAdmin.examples)
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
    io.msgStyle(LogLevel.Test, '@start', 'DbAdmin test started')
    io.msgLog(LogLevel.Test, 'DATABASE SERVER HOST: %s', Host);
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
    
    % Connect to server, this is the command line syntax of 'psql'
    % psql -h gauss -U admin -d unittest -W
    
    % Get path to folder of sources, to run '.sql' files
    [MyPath,~,~] = fileparts(mfilename('fullpath'));
    
    % Create DbQuery instance with connetion details from configuration
    % file 'Database.DbConnections.UnitTest.yml', so DbAdmin created below
    % is linked to this connection.
    Q = db.DbQuery('unittest');
    
    % Create DbAdmin from DbQuery Connection
    Admin = db.DbAdmin('DbQuery', Q);
    DbList1 = Admin.getDbList();
    assert(numel(DbList1) > 0);
    
    % Create DbAdmin without connection, use it to create Config file
    Admin = db.DbAdmin();    
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'admin_db', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    assert(~strcmp(ConfigFileName, ''));
  
    % Create DbAdmin from arguments
    Admin = db.DbAdmin('Host', Host, 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd', 'DatabaseName', 'unittest');    
    assert(Admin.Query.isTableExist('master_table'));
    
    % Get list of databases
    DbList1 = Admin.getDbList();
    assert(numel(DbList1) > 0);
    
    % Create database 'dbadmin_unittest' from sqlfile
    Admin.createDatabase('SqlFileName', fullfile(MyPath, 'unitTest_createDatabase.sql'));
    assert(Admin.isDatabaseExist('unittest'));
    
    % Create DB from xlsx file (Google Sheets)
    Admin.createDatabase('XlsxFileName', fullfile(MyPath, 'unittest2.xlsx'));
    assert(Admin.isDatabaseExist('unittest2'));
    
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
    Admin.createTable('SqlText', SqlText);
    assert(Admin.Query.isTableExist('newtable_1'));
    
    % Create table with args
    Admin.createTable('TableName', 'newtable_2', 'PrimaryKeyDef', 'new_Pkey int');
    assert(Admin.Query.isTableExist('newtable_2'));
    
    % Create table with SQL file
    Admin.createTable('SqlFileName', fullfile(MyPath, 'unitTest_createTable.sql'));
    assert(Admin.Query.isTableExist('newtable_3'));
    
    % Add new column to existing table
    Admin.addColumn('newtable_1', {'MyColA'}, {'INTEGER'}, {'DEFAULT 0'});
    assert(Admin.Query.isColumnExist('newtable_1', 'MyColA'));   

    % Add index to existing table
    IndexList1 = Q.getTableIndexList('newtable_1');
    index_added = Admin.addIndex('newtable_1', 'newtable_1_idx_FDouble5', 'USING btree (FDouble1)');
    IndexList2 = Q.getTableIndexList('newtable_1');
    assert(index_added && any(strcmpi(IndexList2, 'newtable_1_idx_FDouble5')));
    
    % Users:
    % Get list of users (roles)
    UserList1 = Admin.getUserList();    
    assert(numel(UserList1) > 0);
    
    % Add user
    Admin.addUser('Test1', 'Password1');
    UserList2 = Admin.getUserList();
    assert(any(strcmpi(UserList2, 'test1')));
    %assert(length(UserList2) == length(UserList1)+1);  @@@@Dan 

    % Remove user
    Admin.removeUser('Test1');
    UserList3 = Admin.getUserList();
    assert(~any(strcmpi(UserList3, 'test1')));
    %assert(length(UserList3) == length(UserList1));
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbAdmin test passed');
    Result = true;
end

