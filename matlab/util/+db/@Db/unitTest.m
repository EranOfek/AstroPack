function Result = unitTest()
% db.Db.unitTest - Integration tests for ClickHouse JDBC and db.Db
%
% Runs four inner tests against Euclid (10.150.28.18:8123):
%   1. testJdbcVersion          - raw JDBC SELECT version()
%   2. testJdbcCountVisitImages - raw JDBC SELECT count(*) FROM last.visit_images
%   3. testDbClassRead          - db.Db read queries as last_user
%   4. testDbClassInsert        - db.Db create/insert/select as default
%
% Returns true on success, errors on failure.

    testJdbcVersion();
    testJdbcCountVisitImages();
    testDbClassRead();
    testDbClassInsert();

    Result = true;
end


function testJdbcVersion()
% Raw JDBC: SELECT version()

    Version = runJdbcQuery("SELECT version()", @(Rs) string(Rs.getString(1)));
    fprintf('ClickHouse server version: %s\n', Version);
end


function testJdbcCountVisitImages()
% Raw JDBC: SELECT count(*) FROM last.visit_images

    Count = runJdbcQuery("SELECT count(*) FROM last.visit_images", @(Rs) Rs.getLong(1));
    fprintf('last.visit_images row count: %d\n', Count);
end


function testDbClassRead()
% db.Db read queries as last_user / physics

    DB = connectTestDb('last_user', 'physics');
    Cleanup = onCleanup(@() safeDisconnect(DB));

    DB.useDB('last');

    T = DB.query("SELECT version()");
    disp(T);

    T = DB.query("SELECT count(*) FROM last.visit_images");
    disp(T);
end


function testDbClassInsert()
% db.Db write test as default / PassRoot: create test.matlab_insert, insert, select

    DB = connectTestDb('default', 'PassRoot');
    Cleanup = onCleanup(@() safeDisconnect(DB));

    DB.query('CREATE DATABASE IF NOT EXISTS test', 'IsExec', true);
    DB.query('DROP TABLE IF EXISTS test.matlab_insert', 'IsExec', true);

    Error = DB.createTable('test.matlab_insert', ...
        ["id"; "name"; "val"], ...
        ["UInt32"; "String"; "Float64"]);
    if ~isempty(Error)
        error('ClickHouse:CreateTableFailed', '%s', Error);
    end

    T = table(uint32(1), "matlab_test", 3.14, ...
        'VariableNames', {'id', 'name', 'val'});
    Error = DB.insertCharDump('test.matlab_insert', T);
    if ~isempty(Error)
        error('ClickHouse:InsertFailed', '%s', Error);
    end

    T = DB.query("SELECT * FROM test.matlab_insert");
    disp(T);

    if height(T) ~= 1
        error('ClickHouse:InsertVerifyFailed', 'Expected 1 row, got %d.', height(T));
    end
end


function Value = runJdbcQuery(Sql, ParseFn)
% Execute a JDBC query while conn/statement stay in this function scope

    JarFile  = getClickHouseJarFile();
    Host     = "10.150.28.18";
    Port     = 8123;
    Database = "last";
    Username = "last_user";
    Password = "physics";

    javaaddpath(JarFile);

    Driver = com.clickhouse.jdbc.ClickHouseDriver();
    Props  = java.util.Properties();
    Props.setProperty("user", Username);
    Props.setProperty("password", Password);

    Url = sprintf("jdbc:clickhouse:http://%s:%d/%s", Host, Port, Database);
    fprintf("Connecting to:\n%s\n\n", Url);

    Conn = Driver.connect(Url, Props);
    if isempty(Conn)
        error('ClickHouse:JDBCConnectionFailed', ...
            'The JDBC driver did not accept the connection URL.');
    end
    ConnCleanup = onCleanup(@() Conn.close());

    Statement = Conn.createStatement();
    StatementCleanup = onCleanup(@() Statement.close());

    ResultSet = Statement.executeQuery(Sql);
    ResultSetCleanup = onCleanup(@() ResultSet.close());

    if ~ResultSet.next()
        error('ClickHouse:EmptyResult', '%s returned no rows.', Sql);
    end

    Value = ParseFn(ResultSet);
end


function JarFile = getClickHouseJarFile()
% Locate clickhouse-jdbc-0.9.3-all.jar next to this file, with fallback path

    LocalJar = fullfile(fileparts(mfilename('fullpath')), 'clickhouse-jdbc-0.9.3-all.jar');
    FallbackJar = "C:\AstroPack\Data\clickhouse-jdbc-0.9.3-all.jar";

    if isfile(LocalJar)
        JarFile = LocalJar;
    elseif isfile(FallbackJar)
        JarFile = FallbackJar;
    else
        error('ClickHouse:JarNotFound', ...
            'clickhouse-jdbc-0.9.3-all.jar not found at %s or %s', LocalJar, FallbackJar);
    end
end


function DB = connectTestDb(User, Password)
% Create db.Db and connect using the local JDBC jar (bypasses Installer)

    DB = db.Db;
    DB.Host     = '10.150.28.18';
    DB.DbName   = 'last';
    DB.User     = User;
    DB.Password = Password;
    DB.Conn = db.Db.connectCH_Java( ...
        'DbName',   DB.DbName, ...
        'Host',     DB.Host, ...
        'Port',     DB.Port, ...
        'User',     DB.User, ...
        'Password', DB.Password, ...
        'JarFile',  getClickHouseJarFile());
end


function safeDisconnect(DB)
% Disconnect DB object; ignore errors on cleanup

    try
        DB.disconnect;
    catch
    end
end
