% File:    DbAdmin.m
% Class:   DbAdmin
% Title:   Database administrator utils for PostgreSQL, based on DbQuery
% Author:  Chen Tishler
% Created: December 2021
% Note:    For more documentation see .md files in +db/doc/
%
%--------------------------------------------------------------------------
%
%#docgen
%
% Methods:
%    DbAdmin - Create new DbAdmin obeject Input: 'DbCon' - 'Host' - 'Database' -
%    addColumn - Add single or multiple columns to table Input: Output: Example: Obj.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0') Refs: https://www.postgresqltutorial.com/postgresql-add-column/
%    addIndex - Add single index to table, may include one or multiple fields Input: TableName - Table name to be altered IndexName - Unique index name, usually composed as
%    addUser - Add database user Input: UserName - User name Password - Pasword string 'DatabaseName' - If specified user will be granted only for this database 'Permission' - 'read', 'write', 'full'
%    createConnectionConfig - Create database connection in config/local/Database.DbConnections.UnitTest.yml Input: 'FileName' = '' % 'DatabaseName' = '' % 'Host' = 'localhost' % Host name or IP address
%    createDatabase - Create database Input: 'XlsFileName' - When specified, 'DatabaseName' -
%    createTable - Create database table, @Todo Input: Output: Example: db.DbAdmin.createTable('unittest', ) Refs: https://www.postgresql.org/docs/8.0/sql-createuser.html
%    delete -
%    getDbList - Get list of databases Input: - Output: Cell array with list of databases Example: List = Obj.getTablesList() Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    getUserList - Get columns list of specified table as cell array Input: - Output: Cell array Example: List = Obj.getTablesList() Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    isDatabaseExist - Check if database exists Input: DbName Output: true if exists Example: List = Obj.isDatabaseExist('my_database') Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    removeUser - Remove user Input: UserName to remove Output: true on success Example: db.DbAdmin.removeUser('robert') Refs: https://www.postgresql.org/docs/9.4/sql-dropuser.html
%    setConn - Input: Conn - Output: true on success Example: Obj.setConn(DbCon);
%
% Methods: Static
%    startGui - Run gui utility - @TODO - Currently DO NOT USE Input: - Output: - Example: db.DbAdmin.startGui
%
% Methods: Hidden
%    exec - Execute SQL statement, by calling Obj.Query.exec() Input: SqlText - Statement text Output: true on success Example: Obj.exec('DROP USER IF EXISTS user1')
%    runPsql - Run 'psql' external utility with command line parameters. Input: XlsFileName Output: Example: db.DbAdmin.runPsql( psql -h gauss -p 5432 -U admin -W -d postgres -f unittest_postgres.sql
%    xlsx2sql - Convert XLSX file downloaded from Google Drive to SQL file Note: Requires ULTRASAT repository and ULTRASAT_PATH environment var to be set correctly. Note: python3 (python3.exe on Windows) should be on system PATH Input: XlsFileName
%
%#/docgen

classdef DbAdmin < Component

    % Properties
    properties (SetAccess = public)

        % Connection details
        Conn            = []        % DbConnection
        Query           = []        % DbQuery, required to execute statements
        Host            = ''        % Host name or IP address
        Port            = 5432      % Port number, default if 5432
        DatabaseName    = ''        % Use 'postgres' when creating databases or for general functionality
        UserName        = ''        % User name for connection
        Password        = ''        % Password for connection
        Shell           = ''        % Linux shell, 'tcsh' or 'bash', empty for auto detect by $SHELL env
    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = DbAdmin(Args)
            % Create new DbAdmin object
            % Input   : -
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            		            
            %             'DbQuery'   - DbQuery object used for connection to database
            %             'DbCon'     - DbConnection  object used for connection to database 
            %
            %             These arguments are used when DbQuery and DbCon are not set:
            %             'Host'      - Host name
            %             'Database'  - Database name
            %             'UserName'  - User name
            %             'Password'  - Password
            %             'Port'      - Port number
            %
            % Output  : - New DbAdmin object
            % Author  : Chen Tishler (2021)
            % Example :
            %   % Create query object for 'UnitTest' database alias 'UnitTest'
            %   % Configuration is loaded from config/Database.DbConnections.UnitTest.yml
            %   Q = DbAdmin('UnitTest')
            %
            arguments
                Args.DbQuery       = []        % DbQuery object
                Args.DbCon         = []        % DbConnection object
                
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = ''        % Host name or IP address
                Args.Port          = 5432      % Port number
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.UserName      = ''        % User name
                Args.Password      = ''        % Password
            end

            % Setup component
            Obj.setName('DbAdmin');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

            % Set connection from DbQuery or DbCon
            if ~isempty(Args.DbQuery)
                Obj.setConn(Args.DbQuery.Conn);
            elseif ~isempty(Args.DbCon)
                Obj.setConn(Args.DbCon);
                
            % Create new connection from other arguments
            else                
                NewCon = db.DbConnection('Host', Args.Host, 'Port', Args.Port, ...
                    'DatabaseName', Args.DatabaseName, 'UserName', Args.UserName, 'Password', Args.Password);
                
                Obj.setConn(NewCon);
            end
                       
            % Create DbQuery instance connected to the database, as set above
            Obj.Query = db.DbQuery(Obj.Conn);
        end


        function delete(Obj)
            % Destructor            
            % Internally called by Matlab when the object is destroyed.
            Obj.clear();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end

    end

    %----------------------------------------------------------------------
    methods % Database creating & modification
        
        function Result = createDatabase(Obj, Args)
            % Create database, from .XLSX or .SQL file
            %
            % Input : - DbAdmin object
            %         * Pairs of ...,key,val,...
            %           The following keys are available:            			            
            %           'XlsxFileName' - .xlsx file from Google Sheets - Specify XLSX file
            %           'SqlFileName'  - .sql file name - Specify SQL script to run
            %
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : -
            % Instructions:
            %    1. Download database definition from Google Sheet by selecting
            %       File -> Download -> (XLS)
            %
            
            arguments
                Obj
                Args.XlsxFileName   = ''        %
                Args.SqlFileName    = ''        %                
            end
            
            Result = false;
            
            % Execute SQL file using psql
            if ~isempty(Args.SqlFileName)
                if isfile(Args.SqlFileName)
                    Obj.runPsql('SqlFileName', Args.SqlFileName);
                    Result = true;
                else
                    Obj.msgLog(LogLevel.Error, 'createDatabase: Input SQL file not found: %s', Args.SqlFileName);
                end
                
            % Extract database definition from XLS file and execute the
            % resulting SQL file using psql
            elseif ~isempty(Args.XlsxFileName)
                if isfile(Args.XlsxFileName)
                    SqlFileName = Obj.xlsx2sql(Args.XlsxFileName);
                    if ~isempty(SqlFileName) && isfile(SqlFileName)
                        Obj.runPsql('SqlFileName', SqlFileName);
                        Result = true;
                    end
                else
                    Obj.msgLog(LogLevel.Error, 'createDatabase: Input XLSX file not found: %s', Args.XlsxFileName);
                end               
            end

        end

        
        function Result = createConnectionConfig(Obj, Args)
            % Create database connection in config/local/Database.DbConnections.UnitTest.yml
            % Input : - DbAdmin object
            %         * Pairs of ...,key,val,...
            %           The following keys are available:            			            
            %           'FileName'        = ''                %
            %           'DatabaseName'    = ''                %
            %           'Host'            = 'localhost'       % Host name or IP address
            %           'Port'            = 5432              %
            %           'DriverName'      = 'postgres'        % Driver name
            %           'UserName'        = ''                % Login user
            %           'Password'        = ''                % Login password
            %           'ServerSharePath' = '' %              % Path to shared folder on the server, for COPY statements
            %           'MountSharePath'  = ''
            %           'WinMountSharePath'  = ''
            % Output : - Configuration file name on success
            % Author : Chen Tishler (2021)
            % Example: db.DbAdmin.createConnectionConfig('DatabaseName', 'unittest5', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
            % Database.DbConnections.UnitTest.yml
            
            arguments
                Obj
                Args.FileName        = ''                %
                Args.DatabaseName    = ''                %
                Args.Host            = 'localhost'       % Host name or IP address
                Args.Port            = 5432              %
                Args.DriverName      = 'postgres'        % Driver name
                Args.UserName        = ''                % Login user
                Args.Password        = ''                % Login password
                Args.ServerSharePath = '/var/samba/pgshare'   % Path to shared folder on the server, for COPY statements
                Args.MountSharePath  = '/media/gauss_pgshare' %
                Args.WinMountSharePath  = 'S:\'               %
            end
            
            % Prepare file name if not specified
            Result = [];
            if isempty(Args.FileName)
                ConfigPath = fullfile(tools.os.getAstroPackConfigPath(), 'local');
                Args.FileName = fullfile(ConfigPath, strcat('Database.DbConnections.', Args.DatabaseName, '.yml'));
            end
            
            % Create file
            Fid = fopen(Args.FileName, 'wt');
            if Fid > -1
                fprintf(Fid, '# %s\n\n',                        Args.FileName);
                fprintf(Fid, 'DatabaseName    : ''%s''\n',      Args.DatabaseName);
                fprintf(Fid, 'Host            : ''%s''\n',      Args.Host);
                fprintf(Fid, 'Port            : %d\n',          Args.Port);
                fprintf(Fid, 'DriverName      : ''postgres''\n');
                fprintf(Fid, 'UserName        : ''%s''\n',      Args.UserName);
                fprintf(Fid, 'Password        : ''%s''\n',      Args.Password);
                fprintf(Fid, 'ServerSharePath : ''%s''\n',      Args.ServerSharePath);
                fprintf(Fid, 'MountSharePath  : ''%s''\n',      Args.MountSharePath);
                fprintf(Fid, 'WinMountSharePath : ''%s''\n',    Args.WinMountSharePath);
                fclose(Fid);
                
                if isfile(Args.FileName)
                    Result = Args.FileName;
                end
            end            
        end
              
        
        function Result = createTable(Obj, Args)
            % Create database table, @Todo
            % Input   : - DbAdmin object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			                        
            %             'SqlText'       - SQL text to execute
            %             'SqlFileName'   - File name of SQL script to execute
            %             'TableName'     - Table name, if not specified, Obj.TableName is used
            %             'PrimaryKeyDef' - Definition of primary key (SQL)
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbAdmin.createTable('unittest', )
            % Refs    : https://www.postgresql.org/docs/8.0/sql-createuser.html
            % SQL     : [DROP TABLE IF EXISTS customers CASCADE;]
            %           CREATE TABLE customers (
            %             id SERIAL PRIMARY KEY,
            %             customer_name VARCHAR NOT NULL
            %           );
            %
            arguments
                Obj
                Args.SqlText        = ''
                Args.SqlFileName    = ''
                Args.TableName      = ''
                Args.PrimaryKeyDef  = ''
            end


            Result = false;
            SqlText = '';
            
            % SQL text
            if ~isempty(Args.SqlText)
                SqlText = Args.SqlText;
                
            % SQL file name
            elseif ~isempty(Args.SqlFileName)
                SqlText = fileread(Args.SqlFileName);
                
            % Table name with Primary key
            elseif ~isempty(Args.TableName) && ~isempty(Args.PrimaryKeyDef)
                SqlText = sprintf('CREATE TABLE %s (%s PRIMARY KEY)', Args.TableName, Args.PrimaryKeyDef);
            end
            
            if ~isempty(SqlText)
                Result = Obj.exec(SqlText);
            end
        end
        
        
        function Result = addColumn(Obj, TableName, ColumnName, DataType, ColumnDef)
            % Add single or multiple columns to table
            % Input   : - DbAdmin object
            %           - Table name
            %           - Column name
            %           - DataType   - Data field type, INTEGER, etc. see
            %             https://www.postgresql.org/docs/current/datatype.html
            %           - Additional text for column definition, i.e. DEFAULT ...
            %
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0')
            % Refs    : https://www.postgresqltutorial.com/postgresql-add-column/
            % SQL     : ALTER TABLE table_name
            %           ADD COLUMN column_name1 data_type constraint,
            %           ADD COLUMN column_name2 data_type constraint,
            %           ...
            %           ADD COLUMN column_namen data_type constraint;
            arguments
                Obj
                TableName               %
                ColumnName              %
                DataType                %
                ColumnDef               %
            end

            % Validate input, size of arrays must match
            assert(numel(ColumnName) == numel(DataType));
            assert(numel(ColumnName) == numel(ColumnDef));
            
            % Prepare statement text
            SqlText = sprintf('ALTER TABLE %s ', TableName);

            % Add definitions of all specified columns
            for i=1:numel(ColumnName)
                SqlText = sprintf('%s ADD COLUMN %s %s %s', SqlText, ColumnName{i}, DataType{i}, ColumnDef{i});
                if i == numel(ColumnName)
                    SqlText = strcat(SqlText, ';');
                else
                    SqlText = strcat(SqlText, ',');
                end
            end
            
            Result = Obj.exec(SqlText);
        end

        
        function Result = addIndex(Obj, TableName, IndexName, IndexDef)
            % Add single index to table, may include one or multiple fields
            % Input   : - DbAdmin object
            %           - Table name to be altered
            %           - Unique index name, usually composed as TableName_idx_FieldNames, 
            %             for example 'master_table_idx_FDouble2'
            %           - Index definition text with list of fields, for example: 'USING btree (FDouble2)'
            %
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : addIndex('master_table', 'master_table_idx_FDouble2', 'USING btree (FDouble2)');
            % Refs    : https://www.postgresql.org/docs/9.1/sql-createindex.html
            %           https://www.postgresqltutorial.com/postgresql-indexes/postgresql-create-index/
            % SQL     : CREATE INDEX index_name ON table_name [USING method]
            %            (
            %               column_name [ASC | DESC] [NULLS {FIRST | LAST }],
            %               ...
            %            );
            arguments
                Obj                 %
                TableName           %
                IndexName           %
                IndexDef            %
            end
                        
            SqlText = sprintf('CREATE INDEX %s ON %s %s', IndexName, TableName, IndexDef);
            Result = Obj.exec(SqlText);
        end
        

        function Result = getDbList(Obj)
            % Get list of databases
            % Input   : - DbAdmin object
            % Output  : - Cell array with list of databases
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getTablesList()
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = 'SELECT datname FROM pg_database WHERE datistemplate = false';
            Result = Obj.Query.selectColumn(Text, 'datname');
        end
        
        
        function Result = isDatabaseExist(Obj, DbName)
            % Check if the specified database exists
            % Input   : - DbAdmin object
            %           - DbName - database name to check for
            % Output  : - true if exists
            % Author  : Chen Tishler (2021)
            % Example : Obj.isDatabaseExist('my_database_name')
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = sprintf('SELECT datname FROM pg_database WHERE datistemplate = false AND datname = ''%s''', lower(DbName));
            List = Obj.Query.selectColumn(Text, 'datname');
            Result = any(strcmpi(List, DbName));
        end
        
    end
    
    %----------------------------------------------------------------------
    methods % User Management
        
        % https://www.postgresql.org/docs/14/user-manag.html
        %
        % PostgreSQL manages database access permissions using the concept of roles.
        % A role can be thought of as either a database user, or a group of database
        % users, depending on how the role is set up. Roles can own database objects
        % (for example, tables and functions) and can assign privileges on those
        % objects to other roles to control who has access to which objects.
        % Furthermore, it is possible to grant membership in a role to another role,
        % thus allowing the member role to use privileges assigned to another role.
        %
        % The concept of roles subsumes the concepts of "users" and "groups".
        % In PostgreSQL versions before 8.1, users and groups were distinct kinds
        % of entities, but now there are only roles. Any role can act as a user,
        % a group, or both.
        %
        % Database roles are conceptually completely separate from operating
        % system users. In practice it might be convenient to maintain a
        % correspondence, but this is not required. Database roles are global
        % across a database cluster installation (and not per individual database).
        %
        % Every connection to the database server is made using the name of
        % some particular role, and this role determines the initial access
        % privileges for commands issued in that connection.
        % The role name to use for a particular database connection is indicated
        % by the client that is initiating the connection request in an
        % application-specific fashion. For example, the psql program uses
        % the -U command line option to indicate the role to connect as.
        % Many applications assume the name of the current operating system
        % user by default (including createuser and psql).
        % Therefore it is often convenient to maintain a naming correspondence
        % between roles and operating system users.
        %
        % CREATE ROLE admin WITH LOGIN SUPERUSER CREATEDB CREATEROLE PASSWORD 'Passw0rd';
        %
                
        function Result = addUser(Obj, UserName, Password, Args)
            % Add database user
            % Input   : - DbAdmin object
            %           - User name
            %           - Password string
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'DatabaseName' - If specified user will be granted only for this database
            %             'Permission'   - 'read', 'write', 'full'
            % Output  : true on sucess
            % Author  : Chen Tishler (2021)
            % Example : db.DbAdmin.addUser('robert', 'pass123')
            % Refs    : https://www.postgresql.org/docs/8.0/sql-createuser.html
            %           https://stackoverflow.com/questions/760210/how-do-you-create-a-read-only-user-in-postgresql
            % SQL     : CREATE USER user user_name WITH ENCRYPED PASSWORD 'mypassword';
            %           GRANT ALL PRIVILEGES ON DATABASE sample_db TO user_name;
            %
            % @Todo   : Need to research and learn more about creating read-only users
            %
            arguments
                Obj                     %
                UserName                %
                Password                %
                Args.DatabaseName = ''  %
                Args.Permission = ''    %
            end
            
            % 1. Create new user
            SqlText = sprintf('CREATE USER %s WITH PASSWORD ''%s''', UserName, Password);
            Result = Obj.exec(SqlText);
            
            % 2. Grant the CONNECT access
            SqlText = sprintf('GRANT CONNECT ON DATABASE %s TO %s', Args.DatabaseName, UserName);
            Result = Obj.exec(SqlText);
            
            % 3. Grant full access
            if ~isempty(Args.DatabaseName) && (strcmp(Args.Permission, 'write') || strcmp(Args.Permission, 'full'))
                SqlText = sprintf('GRANT ALL PRIVILEGES ON DATABASE %s TO %s', Args.DatabaseName, UserName);
                Result = Obj.exec(SqlText);
            end
            
            % Create read-only user on specified database
            % https://ubiq.co/database-blog/how-to-create-read-only-user-in-postgresql/
            if ~isempty(Args.DatabaseName) && strcmp(Args.Permission, 'read')
                
                % Assign permission to this read only user
                SqlText = sprintf('GRANT USAGE ON SCHEMA public TO %s', UserName);
                Result = Obj.exec(SqlText);
                
                % Assign permission to this read only user
                SqlText = sprintf('GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s', UserName);
                Result = Obj.exec(SqlText);
                
                % Assign permissions to read all newly tables created in the future
                SqlText = sprintf('ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO %s', UserName);
                Result = Obj.exec(SqlText);
            end
        end
        
        
        function Result = removeUser(Obj, UserName)
            % Remove specified user from database users list
            %
            % NOTE: This function may fail if there are dependecies between
            % the user and other objects such as tables. Use pgAdmin to remove users
            % and dependecies.
            %
            % Input   : - DbAdmin object
            %           - UserName to remove
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbAdmin.removeUser('robert')
            % Refs    : https://www.postgresql.org/docs/9.4/sql-dropuser.html
            % SQL     : DROP USER [ IF EXISTS ] name [, ...]
            arguments
                Obj                 %
                UserName            %
            end

            SqlText = sprintf('DROP USER IF EXISTS %s', UserName);
            Result = Obj.exec(SqlText);
        end
        
        
        function Result = getUserList(Obj)
            % Get database users list. 
            % Note that users are per server, and may be used with multiple databases.
            % Input   : - DbAdmin object
            % Output  : - Cell array with names of database users
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getTablesList()
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = [...
                'SELECT usename AS role_name, '...
                '  CASE '...
                '     WHEN usesuper AND usecreatedb THEN '...
                '       CAST(''superuser, create database'' AS pg_catalog.text) '...
                '     WHEN usesuper THEN '...
                '        CAST(''superuser'' AS pg_catalog.text) '...
                '     WHEN usecreatedb THEN '...
                '        CAST(''create database'' AS pg_catalog.text) '...
                '     ELSE '...
                '        CAST('''' AS pg_catalog.text) '...
                '  END role_attributes '...
                'FROM pg_catalog.pg_user '...
                'ORDER BY role_name desc; '...
                ];
            
            Result = Obj.Query.selectColumn(Text, 'role_name');
        end

    end
    %----------------------------------------------------------------------
    
    methods(Hidden)
        
        function Result = setConn(Obj, Conn)
            % Set connection data from the specified DbConnection object
            % Input   : - DbAdmin object
            %           - Conn - DbConnection object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.setConn(DbCon);
            
            Obj.Conn = Conn;
            Obj.Host = Conn.Host;
            Obj.Port = Conn.Port;
            Obj.DatabaseName = Conn.DatabaseName;
            Obj.UserName = Conn.UserName;
            Obj.Password = Conn.Password;
            Result = true;
        end
        
        
        function Result = exec(Obj, SqlText)
            % Execute SQL statement, by calling Obj.Query.exec()
            % Input   : - DbAdmin object
            %           - SqlText - Statement text, char
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.exec('DROP USER IF EXISTS user1')
            Result = Obj.Query.exec(SqlText);
        end
        
        
        function Result = xlsx2sql(Obj, XlsxFileName)
            % Convert XLSX file downloaded from Google Drive to SQL file, using xlsx2sql.py
            % Note: Requires ULTRASAT repository and ULTRASAT_PATH environment
            %       var to be set correctly.
            % Note: python3 (python3.exe on Windows) should be on PATH ('System' PATH on Windows)
            % Input   : - DbAdmin object
            %           - XlsFileName
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.xlsx2sql('c:\temp\_xls\unittest.xlsx')
            arguments
                Obj             % 
                XlsxFileName    %
            end
            
            Result = '';
            if ~isfile(XlsxFileName)
                return;
            end
                
            PWD = pwd;
            try
               
                % Search xlsx2sql.py in Ultrasat folder
                Path = tools.os.getUltrasatPath();
                flist = [];
                if ~isempty(Path) && isfolder(Path)
                    flist = dir(fullfile(Path, '**\xlsx2sql.py'));
                    if numel(flist) == 0
                        Path = tools.os.getAstroPackPath();
                        if ~isempty(Path) && isfolder(Path)
                            flist = dir(fullfile(Path, '**\xlsx2sql.py'));                        
                        end
                    end
                end
                if numel(flist) > 0
                    PyScript = fullfile(flist(1).folder, flist(1).name);
                else
                    io.msgLog(LogLevel.Error, 'xlsx2sql.py: xlsx2sql.py not found in source code folders');
                    return;
                end
                    
                
                % Prepare path to Python script
                [Path, FName] = fileparts(XlsxFileName);
                cd(Path);
                if ~isfile(PyScript)
                    io.msgLog(LogLevel.Info, 'xlsx2sql.py: File not found: %s', 'xlsx2sql.py');
                    return;
                end
                
                % Prepare command line, assume we have 'python3' installed
                % Note: python3 (python3.exe on Windows) should be on
                % SYSTEM PATH (not USER PATH).
                % See: https://stackoverflow.com/questions/47539201/python-is-not-recognized-windows-10
                % For example: Add both C:\Python38 and C:\Python38\Scripts
                Args = sprintf('-x %s', XlsxFileName);
                Obj.msgLog(LogLevel.Info, 'xlsx2sql.py: %s', Args);
                
                % Execute python3 xlsx2sql.py, this may take a while...
                Obj.msgLog(LogLevel.Info, 'xlsx2sql.py: Executing %s', PyScript);
                
                [Status, Output] = tools.os.runPython(PyScript, 'ArgsStr', Args);
                if Status ~= 0
                    Obj.msgLog(LogLevel.Error, 'xlsx2sql.py: FAILED to execute, make sure that python3 is found on your PATH: %s', PyScript);
                end
                
                % Prepare file name of generated SQL
                SqlFileName = sprintf('%s%s%s_postgres.sql', FName, filesep, FName);
                
                % Check that SQL file was created in current folder
                if isfile(SqlFileName)
                    SqlFileName = fullfile(pwd, SqlFileName);
                    if isfile(SqlFileName)
                        Result = SqlFileName;
                    else
                        io.msgLog(LogLevel.Error, 'xlsx2sql.py: SQL file was not generated in current folder: %s', SqlFileName);
                        SqlFileName = '';
                    end
                else
                    Obj.msgLog(LogLevel.Error, 'xlsx2sql.py: SQL file was not generated: %s', SqlFileName);
                    SqlFileName = '';
                end
                
            catch Ex
            end
            cd(PWD);
        end
        
        
        function Result = runPsql(Obj, Args)
            % Run the 'psql' external utility with command line parameters.
            % Input   : - DbAdmin object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'Host'          - Host name or IP address to connect
            %             'Port'          - Port number, 0 to use default
            %             'DatabaseName'  - Database name, use 'postgres' to when creating databases or for general
            %             'UserName'      - User name
            %             'Password'      - Password
            %             'SqlFileName'   - SQL file name to be executed
            %             'Params'        - Additional parameters for command line            
            %
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbAdmin.runPsql(
            %               'psql -h gauss -p 5432 -U admin -W -d postgres -f unittest_postgres.sql')
            % Note    : psql (psql.exe on Windows) must be on the system PATH
            %           i.e. Add C:\Program Files\PostgreSQL\14\bin to SYSTEM PATH
            %
            arguments
                Obj
                Args.Host          = ''        % Host name or IP address to connect
                Args.Port          = 0         % Port number, 0 to use default
                Args.DatabaseName  = ''        % Database name, use 'postgres' to when creating databases or for general
                Args.UserName      = ''        % User name
                Args.Password      = ''        % Password
                Args.SqlFileName   = ''        % SQL file name to be executed
                Args.Params        = ''        % Additional parameters for command line
            end

            Result = false;
            
            if isempty(Args.Host)
                Args.Host = Obj.Host;
            end
            
            if Args.Port == 0
                Args.Port = Obj.Port;
            end

            if isempty(Args.DatabaseName)
                Args.DatabaseName = Obj.DatabaseName;
            end

            if isempty(Args.UserName)
                Args.UserName = Obj.UserName;
            end
        
            if isempty(Args.Password)
                Args.Password = Obj.Password;
            end

            try
                                
                % Prepare command line
                Cmd = sprintf('psql -h %s -p %d -U %s -w', Args.Host, Args.Port, Args.UserName);
                
                % -d
                if ~isempty(Args.DatabaseName)
                    Cmd = sprintf('%s -d %s', Cmd, Args.DatabaseName);
                end
                
                % -f
                if ~isempty(Args.SqlFileName)
                    Cmd = sprintf('%s -f %s', Cmd, Args.SqlFileName);
                end
                
                % Additional params
                if ~isempty(Args.Params)
                    Cmd = sprintf('%s %s', Cmd, Args.Params);
                end
                
                % Password
                if ~isempty(Args.Password)
                    
                    % Windows - note that we MUST NOT have a spaces next to '&&'
                    if tools.os.iswindows()
                        Cmd = sprintf('set PGPASSWORD=%s&&%s', Args.Password, Cmd);
                        
                    % Linux - use 'export'
                    else
                        
                        % Check which shell we use
                        if isempty(Obj.Shell)
                            Obj.Shell = getenv('SHELL');
                        end
                        
                        % bash / tcsh
                        if contains(Obj.Shell, 'tcsh')
                            Cmd = sprintf('setenv PGPASSWORD ''%s'' ; %s', Args.Password, Cmd);
                        else
                            Cmd = sprintf('export PGPASSWORD=''%s'' ; %s', Args.Password, Cmd);
                        end
                    end
                end
                
                io.msgLog(LogLevel.Info, 'psql: system( %s )', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, 'psql: %d', Status);
                io.msgLog(LogLevel.Info, 'psql: %s', Output);
                if Status ~= 0
                    Obj.msgLog(LogLevel.Error, 'runPsql: FAILED to execute, make sure that psql is found on your PATH: %s', Cmd);
                end
                Result = true;
            catch Ex
                io.msgLogEx(LogLevel.Info, Ex, 'psql');
            end
        end
                
    end


    methods(Static)
        
        function Result = startGui()
            % Run gui utility process - @TODO - Not completed - DO NOT USE!
            % Input   : -
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbAdmin.startGui

            Result = false;
            try
                                
                % Prepare command line
                Path = fullfile(tools.os.getUltrasatPath(), 'python', 'utils', 'utils_gui');
                if ~isfolder(Path)
                    io.msgLog(LogLevel.Error, 'DbAdmin.startGui: Path not found: %s', Path);
                    return;
                end
                
                % Set utility name
                if tools.os.islinux
                    Cmd = sprintf('%s%s%s', Path, filesep, 'utils_gui');
                else
                    Cmd = sprintf('%s%s%s', Path, filesep, 'utils_gui.exe');
                end
 
                % Found - run it
                if isfile(Cmd)
                    io.msgLog(LogLevel.Info, 'DbAdmin.startGui: system( %s )', Cmd);
                    [Status, Output] = system(Cmd);
                    %io.msgLog(LogLevel.Info, 'startGui: %d', Status);
                    %io.msgLog(LogLevel.Info, 'startGui: %s', Output);
                    Result = true;
                    
                % File not found
                else
                    io.msgLog(LogLevel.Error, 'DbAdmin.startGui: File not found, use Lazarus to compile the project: %s', Cmd);
                end
                
            catch Ex
                io.msgLogEx(LogLevel.Info, Ex, 'DbAdmin.startGui');
            end
        end
        
    end
    
    
    methods(Static) % Static

        function help
            % Show MLX manual
            tools.os.class_mlx(mfilename('fullpath'))
        end
        
        Result = unitTest()
            % Unit-Test

        Result = examples()
            % Examples
    end
end
