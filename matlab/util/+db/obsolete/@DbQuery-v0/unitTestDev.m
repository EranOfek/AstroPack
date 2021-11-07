

function Result = unitTestDev()
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

    s = [];
    ItersCount = 3;
    for i = 1:ItersCount                      
        s(i).recid = Component.newUuid();
    end
    Q.insertRecord('master_table', s, 'BatchSize', ItersCount);

    Result = true;
end


