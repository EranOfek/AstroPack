% DbAdmin - PostgresSQL Database Administrator Class
%
% Basic usage:
%
%
%
% PostgreSQL V14 - Installation instructions for Linux:
%
%     https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/
%
% Create database on remote server (password: 'Passw0rd')
%
%     psql -h gauss -p 5432 -U admin -W -d postgres -f unittest.sql
%
%--------------------------------------------------------------------------

classdef DbAdmin < Component

    % Properties
    properties (SetAccess = public)

        % Connection details
        Conn            = []        % DbConnection
        Query           = []        % DbQuery        
        Host            = ''        %
        Port            = 5432      %                               
        DatabaseName    = ''        % Use 'postgres' to when creating databases or for general
        User            = ''        %
        Password        = ''        %
        TableName       = ''        %        
    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = DbAdmin(Args)
            % Create new DbAdmin obeject
            % Input:
            %    'DbCon'     -
            %    'Host'      -
            %    'Database'  -
            %    'User'      -
            %    'Password'  - 
            %    'Port'      - 
            %    'TableName' - 
            %
            % Examples:
            %   % Create query object for 'UnitTest' database alias 'UnitTest'
            %   Q = DbAdmin('UnitTest')
            %
            arguments
                Args.DbQuery       = []        %
                Args.DbCon         = []        % DbConnection object
                Args.Host          = ''        %
                Args.Port          = 5432      %                               
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.User          = ''        %
                Args.Password      = ''        %
                Args.TableName     = ''        %
            end

            % Setup component
            Obj.setName('DbAdmin');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

            % Set connection
            if ~isempty(Args.DbQuery)
                Obj.Conn = Args.DbQuery.Conn;
            elseif ~isempty(Args.DbCon)
                Obj.Conn = Args.DbCon;
            else
                Obj.Conn = db.DbConnection('Host', Args.Host, 'Port', Args.Port, ...
                    'DatabaseName', Args.DatabaseName, 'User', Args.User, 'Password', Args.Password);
            end
                       
            % Override TableName and set other properties
            Obj.setProps(Args);
            
            Obj.Query = db.DbQuery(Obj.Conn);

        end


        % Destructor
        function delete(Obj)
            Obj.clear();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end

    %----------------------------------------------------------------------
    methods
        
        function Result = createDatabase(Obj, Args)
            % Create empty database
            % Input:   'DatabaseName'  - 
            %          'SqlFileName'   -
            %          'XlsFileName'   -
            % Output:  true on success
            % Example: -            
            % Instructions
            
            arguments
                Obj
                Args.DatabaseName   = ''        %
                Args.Script         = ''        % Script text
                Args.SqlFileName    = ''        %
                Args.XlsFileName    = ''        %
            end
            
            if ~isempty(Args.SqlFileName)
                if isfile(Args.SqlFileName)
                    Obj.runPsql('SqlFileName', Args.SqlFileName);
                end
                
            elseif ~isempty(Args.XlsFileName)
                SqlFileName = Obj.xls2sql(Args.XlsFileName);
                if ~isempty(SqlFileName) && isfile(SqlFileName)
                    Obj.runPsql(SqlFileName);
                end
            else
            end

        end

        
        function Result = createConnection(Obj, DatabaseName, Args)
            % Create database connection in config/local/Database.DbConnections.UnitTest.yml
            % Input:   DbName
            % Output:  true on success
            % Example: db.DbAdmin.createConnection()
            % Database.DbConnections.UnitTest.yml
            
            arguments
                Obj
                DatabaseName                            %
                Args.Host           = 'localhost'       % Host name or IP address
                Args.Port           = 5432
                Args.DriverName     = 'postgres'        % Driver name
                Args.UserName       = ''                % Login user
                Args.Password       = ''                % Login password
            end
            
        end
              
        
        function Result = createTable(Obj, Args)
            % Create database table, @Todo
            % Input:   
            % Output:  
            % Example: db.DbAdmin.createTable('unittest', )
            % https://www.postgresql.org/docs/8.0/sql-createuser.html
            arguments
                Obj
                Args.SqlText        = ''
                Args.SqlFileName    = ''
                Args.TableName      = ''
                Args.PrimaryKeyDef  = ''
            end

%         DROP TABLE IF EXISTS customers CASCADE;
% 
%         CREATE TABLE customers (
%             id SERIAL PRIMARY KEY,
%             customer_name VARCHAR NOT NULL
%         );

            % Read input file
            SqlText = '';
            if ~isempty(Args.SqlText)
                SqlText = Args.SqlText;
            elseif ~isempty(Args.SqlFileName)
                SqlText = fileread(Args.SqlFileName);
            else                
                SqlText = sprintf('CREATE TABLE %s (%s PRIMARY KEY)', TableName, PrimaryKeyDef);
            end
            
            if ~isempty(SqlText)
                Obj.exec(SqlText);            
            end
        end
        
        
        function Result = addColumn(Obj, TableName, ColumnName, DataType, ColumnDef)
            % Add column to table
            % Input:   
            % Output:  
            % Example: Obj.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0')
            arguments
                Obj
                TableName               %
                ColumnName              %
                DataType                %
                ColumnDef               %                            
            end

            
            % https://www.postgresqltutorial.com/postgresql-add-column/
            
            SqlText = sprintf('ALTER TABLE %s ADD COLUMN %s %s %s', TableName, ColumnName, DataType, ColumnDef);            
            Result = Obj.exec(SqlText);

%             ALTER TABLE table_name
%             ADD COLUMN new_column_name data_type constraint;

%             ALTER TABLE table_name
%             ADD COLUMN column_name1 data_type constraint,
%             ADD COLUMN column_name2 data_type constraint,
%             ...
%             ADD COLUMN column_namen data_type constraint;
        end

        
        function Result = addIndex(Obj, TableName, IndexName, IndexDef)
            %
            % Input:   
            % Output:  
            % Example: db.DbAdmin.addIndex()
            %
            
            arguments
                Obj                 %
                TableName           %
                IndexName           %                
                IndexDef            %
            end
            
            % https://www.postgresql.org/docs/9.1/sql-createindex.html
            % https://www.postgresqltutorial.com/postgresql-indexes/postgresql-create-index/
            %for field in self.field_list:
            %if field.index:
            %    index_name = self.table_name + '_idx_' + field.field_name
            %    self.write('CREATE INDEX {} ON public.{}\n  USING {} ({});\n\n'.format(index_name, self.table_name, field.index_method, field.field_name))
            % CREATE INDEX index_name ON table_name [USING method]
            %(
            %    column_name [ASC | DESC] [NULLS {FIRST | LAST }],
            %    ...
            %);

            %if isempty(Args.TableName)
            %    Args.TableName = Obj.TableName;
            %end
            
            SqlText = sprintf('CREATE INDEX %s ON %s %s', IndexName, TableName, IndexDef);            
            Result = Obj.exec(SqlText);            
        end        
       
        
        function Result = addUser(Obj, UserName, Password, Args)
            % Add database user
            % Input:   
            % Output:  
            % Example: db.DbAdmin.addUser('robert', 'pass123')
            arguments
                Obj                     %
                UserName                %
                Password                %
                Args.DatabaseName = ''  %
            end

            
            % https://www.postgresql.org/docs/8.0/sql-createuser.html
            % create user user_name with encrypted password 'mypassword';
            % grant all privileges on database sample_db to user_name;
            
            SqlText = sprintf('CREATE USER %s WITH ENCRYPED PASSWORD ''%s''', UserName, Password);
            Obj.exec(SqlText);
            
            if ~isempty(Args.DatabaseName)
                SqlText = sprintf('GRANT ALL PRIVILEGES ON %s TO %s', Args.DatabaseName, UserName);
                Obj.exec(SqlText);
            end
        end
        
        
        function Result = removeUser(Obj, UserName)
            %
            % Input:   
            % Output:  
            % Example: db.DbAdmin.removeUser('robert')
            
            arguments
                Obj                 %
                UserName            %

            end
            
            % https://www.postgresql.org/docs/9.4/sql-dropuser.html
            % DROP USER [ IF EXISTS ] name [, ...]
            
            SqlText = sprintf('DROP USER IF EXISTS %s', UserName);
            Obj.exec(SqlText);
        end        
        
        
    end
    
    
    methods(Hidden)    
        
        function Result = exec(Obj, SqlText)
            % Execute SQL statement
            % Input:   SqlText - Statement text
            % Output:  true on sucess
            % Example: Obj.exec('DROP USER IF EXISTS user1')
            Result = Obj.Query.exec(SqlText);
        end
        
        
        function Result = writeLocalConfig(Obj, DatabaseName, Args)
            % config/local/Database.DbConnections.UnitTest.yml
            % Input: 
            %    DatabaseName 
            %    'UserName'        = ''       %
            %    'Password'        = ''       %
            %    'Host'            = ''       %
            %    'Port'            = 5432     %
            %    'ServerSharePath' = ''       % Path to shared folder on the server, for COPY statements

            % Output: true on success
            % Example: Obj.writeLocalConfig()
            
            arguments
                Obj
                DatabaseName 
                Args.UserName        = ''       %
                Args.Password        = ''       %
                Args.Host            = ''       %
                Args.Port            = 5432     %
                Args.ServerSharePath = ''       % Path to shared folder on the server, for COPY statements

            end
            
            Result = false;
            ConfigPath = '';
            FileName = fullfile(ConfigPath, 'local', strcat('Database.DbConnections.', DatabaseName, '.yml'));
            
            Fid = fopen(Obj.FileName, 'wt');
            
            fprintf(Fid, '# %s\n', FileName);

            fprintf(Fid, 'DatabaseName    : ''%s''        # Database name\n', DatabaseName);
            fprintf(Fid, 'Host            : ''%s''        # Host name or IP address\n', Args.Host);
            fprintf(Fid, 'Port            : %d            # Port number\n', Args.Port);            
            fprintf(Fid, 'DriverName      : ''postgres''  # Driver name\n');
            fprintf(Fid, 'UserName        : ''%s''        # Login user\n', Args.UserName);
            fprintf(Fid, 'Password        : ''%s''        # Login password\n', Args.Password);

            fclose(Fid);
            if isfile(FileName)
                Result = true;
            end
            
        end        

        
        
        function SqlFileName = xls2sql(Obj, XlsFileName)
            % Convert XLSX file downloaded from Google Drive to SQL file
            % Input:   XlsFileName
            % Output:  
            % Example: db.DbQuery.xls2sql('c:\temp\_xls\unittest.xlsx')
            arguments
                Obj
                XlsFileName
            end
            
            SqlFileName = '';
            if ~isfile(XlsFileName)
                return;
            end
                
            PWD = pwd;
            try
                
                [Path, FName] = fileparts(XlsFileName);
                cd(Path);
                Py = fullfile(tools.os.getAstroPackPath, 'python', 'utils', 'database_utils', 'xlsx2sql.py');
                Cmd = sprintf('python3 %s -f %s', Py, XlsFileName);
                io.msgLog(LogLevel.Info, 'xlsx2sql.py: %s', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, '%d', Status);
                io.msgLog(LogLevel.Info, '%s', Output);
                
                SqlFileName = sprintf('%s%s%s.sql', FName, filesep, FName);
                if ~isfile(SqlFileName)
                    SqlFileName = '';
                end
                
            catch Ex
            end
            cd(PWD);
        end
        
        
        function Result = runPsql(Obj, Args)
            % 
            % Input:   XlsFileName
            % Output:  
            % Example: db.DbAdmin.runPsql(
            % psql -h gauss -p 5432 -U admin -W -d postgres -f unittest.sql
            arguments
                Obj
                Args.Host          = ''        %
                Args.Port          = 5432      %                               
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.UserName      = ''        %
                Args.Password      = ''        %
                Args.SqlFileName   = ''        %
                Args.Params        = ''        %                
                Args.TableName     = ''        %
            end

            Result = false;
            
            if ~isempty(Args.Host)
                Args.Host = Obj.Host;
            end
            Args.Host = Obj.Conn.Host;
            
            if Args.Port == 0
                Args.Port = Obj.Port;
            end            
            Args.Port = Obj.Conn.Port;

            if isempty(Args.DatabaseName)
                Args.DatabaseName = Obj.DatabaseName;
            end

            if isempty(Args.UserName)
                Args.UserName = Obj.UserName;
            end
            Args.UserName = Obj.Conn.UserName;
        
            if isempty(Args.Password)
                Args.Password = Obj.Password;
            end
            Args.Password = Obj.Conn.Password;

            try
                                
                % Prepare command line
                Cmd = sprintf('psql -h %s -port %d -U %s -w', Args.Host, Args.Port, Args.UserName);
                
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
                    if tools.os.iswindows()
                        Cmd = sprintf('set PGPASSWORD=%s&&%s', Args.Password, Cmd);
                    else
                        Cmd = sprintf('export PGPASSWORD=''%s'' ; %s', Args.Password, Cmd);
                    end
                end
                
                io.msgLog(LogLevel.Info, 'psql: %s', Cmd);
                [Status, Output] = system(Cmd);
                io.msgLog(LogLevel.Info, 'psql: %d', Status);
                io.msgLog(LogLevel.Info, 'psql: %s', Output);
                Result = true;
            catch Ex
                io.msgLogEx(LogLevel.Info, Ex, 'psql');
            end
        end
                
    end


    methods(Static) % Unit-Tests

        Result = unitTest()
            % Unit-Test

    end
end
