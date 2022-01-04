
function Result = unitTest()
    % Unit-Test
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
        
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug, 'type', 'all');
    io.msgStyle(LogLevel.Test, '@start', 'DbAdmin test started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
    
    Q = db.DbQuery('unittest');
    Admin = db.DbAdmin('DbQuery', Q);
    %Admin = db.DbAdmin('Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    
    Admin.xls2sql('D:\Ultrasat\AstroPack.git\database\xlsx\unittest4.xlsx');
    
    Admin.createDatabase('SqlFileName', 'D:\Ultrasat\AstroPack.git\database\xlsx\unittest\unittest2.sql');
    
    
    %Admin.createTable('SqlFileName', 'D:\Ultrasat\AstroPack.git\database\xlsx\unittest\unittest_table3.sql');
    
    SqlText = [...
        'CREATE TABLE public.table4 '...
        'RecID VARCHAR NOT NULL,'...
        'FDouble1 DOUBLE PRECISION DEFAULT 0,'...
        'FInt1 INTEGER DEFAULT 0,'...
        'FString1 VARCHAR,'...
        'CONSTRAINT table3_pkey PRIMARY KEY(RecID)'...
        ');' ];

    Admin.createTable('SqlText', SqlText);

    %IndexList1 = Q.getTableIndexList('master_table');
    %Admin.addIndex('master_table', 'master_table_idx_FDouble2', 'USING btree (FDouble2)');
    %IndexList2 = Q.getTableIndexList('master_table');
        
    Admin.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0');

    %Admin.addUser('Test1', 'Password1');

    %Admin.removeUser('Test1');
    
    io.msgStyle(LogLevel.Test, '@passed', 'DbAdmin test passed');
    Result = true;
end

