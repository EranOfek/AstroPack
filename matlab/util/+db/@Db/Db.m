% db.Db - A class for accessing databases
%

classdef Db < Component
    %
    
    properties       
        DbType   = "Clickhouse"; % 'Postgress'|'Clickhouse'
        DbName   = []; %"test_db";
        User     = "default"; %"";
        Password = "spotpot"; %[];  % if empty, then use the PasswordsManager class to find and populate the password
        Host     = "localhost"; %[];
        Port     = "8123"; %[];
        
        ConnType = 'java';  % 'java'|'http'
        
    end
    
    properties (Hidden)
        Conn     = []; % connectivity information (for ConnType=java)
    end
    
    properties (Hidden, Constant)
       
    end
    
    
    methods % Constructor
       
        %function Obj = Db(Args)
        %    % Constructor for db.Db
        %end
             
    end
    
    methods % setter/getters
        function Val=get.DbName(Obj)
            % Getter for DbName
            
            Val = Obj.DbName;
            if isempty(Val)
                Struct     = Obj.getDbConfig;
                Val        = Struct.DbName{1};
                Obj.DbName = Val; 
            end            
            
        end
        
        function Val=get.Host(Obj)
            % Getter for Host
            
            Val = Obj.Host;
            if isempty(Val)
                [~,Val]    = Obj.getDbConfig(Obj.DbName);
                Obj.Host   = Val; 
            end    
        end
        
        function Val=get.Port(Obj)
            % Getter for Port
            
            Val = Obj.Port;
            if isempty(Val)
                [~,~,Val]  = Obj.getDbConfig(Obj.DbName);
                Obj.Port  = Val; 
            end    
        end
       
        function Val=get.Conn(Obj)
            % Getter for Conn (Java connection)
           
            Val = Obj.Conn;
            if isempty(Val)
                Val = db.Db.connectCH_Java('DbName',Obj.DbName, 'Host',Obj.Host, 'Port',Obj.Port, 'User',Obj.User, 'Password',Obj.Password);
                Obj.Conn = Val;
            end
        end

        function set.DbName(Obj, Val)
            % Setter for DbName


        end
    end
      
    methods (Static) % construction
        
        function [Conn, JarFile]=connectCH_Java(Args)
            % Connect to ClickHouse DB via Java
            %   To close connection use: close(Conn)
            %
            
            arguments
                Args.DbName  = 'test_db';
                Args.Host    = 'localhost';
                Args.Port    = '8123';
                Args.User    = 'default';
                Args.Password = 'spotpot';
                Args.JarFile = []; %'/home/eran/jdbc/clickhouse-jdbc-0.7.0-all.jar';
                Args.Driver  = 'com.clickhouse.jdbc.ClickHouseDriver';  % 'ru.yandex.clickhouse.ClickHouseDriver'
                Args.BaseURL = "jdbc:clickhouse";
            end
            
            if isempty(Args.JarFile)
                I = Installer;
                JarDir = I.getDataDir(I.Items.ClickHouseJar);
                PWD = pwd;
                cd(JarDir);
                Fd = dir('*.jar');
                JarFile = sprintf('%s%s%s',JarDir,filesep,Fd.name);
                cd(PWD);
            else
                JarFile = Args.JarFile;
            end
            if contains(JarFile,'~')
                JarFile = tools.os.relPath2absPath(JarFile);
            end

            % Check if Jar file exist
            if ~isfile(JarFile)
                fprintf('Jar file: %s not found\n',JarFile);
                fprintf('Use I=Installer; I.install(''ClickHouseJar'')\n');
                error('Jar file not found');
            end

            JdbcURL = sprintf("%s://%s:%s/%s",Args.BaseURL, Args.Host, Args.Port, Args.DbName);
                
         
            % Add JDBC driver to the MATLAB Java path if it's not already added
            javaaddpath(JarFile);
            if ~ismember(JarFile, javaclasspath('-dynamic'))
                javaaddpath(Args.JarFile);
            end

            % Set up the JDBC connection
            Conn = database('', Args.User, Args.Password, Args.Driver, JdbcURL);
        end


    end
    
    
    methods % utilities
        function Obj=disconnectCH_Java(Obj)
            % disconnect Clickhouse Java connection
            % Input  : - self.
            % Output : - self.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.dissconectCH_Java

            close(Obj.Conn);
            Obj.Conn = [];
        end

        function [Result, Host, Port]=getDbConfig(Obj, DbName)
            % Get configuration file for db
            %   The config file is stored in AstroPack/config/
            %   under file db.<DbType>.yml
            % Input  : - self.
            %          - Optional DbName. If provided, then will compare it
            %             to available DbName in config file and will
            %             return also te Host and Port for the specific
            %             DbName.
            % Output : - A structure with the db configuration.
            %          - Host name. This is extracted if DbName is provided
            %            as the second input and it is found in the config.
            %          - Like Host, but for the Port.
            % Author : Eran Ofek (Oct 2024)
            % Example: D=db.Db;
            %          S=D.getDbConfig();
            %          [~,Host,Port]=D.getDbConfig('test_db');
            
            arguments
                Obj
                DbName  = [];
            end
            
            Result = Obj.Config.Data.db.(Obj.DbType);
            if isempty(DbName)
                Host = [];
                Port = [];
            else
                Ind  = find(strcmp(Result.DbName, DbName));
                Host = Result.Host{Ind};
                Port = Result.Port{Ind};
            end
        end
    end
    
        
    methods 
        function [Result, Error] = query(Obj, Query, Args)
            %

            arguments
                Obj
                Query     = "SELECT * FROM test_db.test_table;";
                
                Args.Convert2String logical   = true;
                Args.IsExec logical           = false;
            end

            if strcmpi(Obj.DbType, 'clickhouse') && strcmpi(Obj.ConnType, 'java')
                [Result,Error] = queryCH_Java(Obj, Query, 'IsExec',Args.IsExec);
            else
                error('DbType=%s and ConnType=%s query is not supported', Obj.DbType, Obj.ConnType);
            end

            if Args.Convert2String && ~isempty(Result)
                Result = tools.table.table_cell2string(Result);
            end

        end

        function [Result, Error] = showDB(Obj, Args)
            % Show all databases
            %   Using the 'SHOW DATABSES;' query.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Convert2String' - Default is true.
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - A table with column name 'name' containing a list
            %            of all databases.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showDB

            arguments
                Obj                
                Args.Convert2String logical   = true;
                Args.ReturnString logical     = true;
            end

            Query = 'SHOW DATABASES;';
            [Result,Error] = query(Obj, Query, 'Convert2String',Args.Convert2String);

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end

        function [Result, Error] = showCurrentDB(Obj, Args)
            % Show current database (in use)
            %   Using the 'SELECT currentDatabase();' query.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Convert2String' - Default is true.
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - Current DB (string).
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showCurrentDB

            arguments
                Obj                
                Args.Convert2String logical   = true;
                Args.ReturnString logical     = true;
            end

            Query = 'SELECT currentDatabase();';
            [Result,Error] = query(Obj, Query, 'Convert2String',Args.Convert2String);

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end

        function [Error]=useDB(Obj, DbName)
            % Change database (DbName property will change)
            %   Uisng the USE <DbName> query
            % Input  : - self.
            %          - Database name (use showDB to see list of DBs).
            % Output : - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.useDB('test_db');


            Query = sprintf('USE %s',DbName);
            [~,Error] = query(Obj, Query, 'Convert2String',false, 'IsExec',true);

            if isempty(Error)
                Obj.DbName = DbName;
            end

        end

        function [Result, Error] = showTables(Obj, Args)
            %

            arguments
                Obj
                Args.ReturnString logical   = true;
            end

            Query = 'SHOW TABLES;';
            [Result, Error] = Obj.query(Query);

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end


        function [Result, Error] = describeTable(Obj, TableName, Args)
            %
            % Example: D.describeTable('test_table')
            %          D.describeTable('test_db.test_table')

            arguments
                Obj
                TableName
                Args.Convert2String logical   = true;
            end

            Query = sprintf('DESCRIBE %s;',TableName);
            [Result, Error] = Obj.query(Query, 'Convert2String',Args.Convert2String);

        end


        function [Result,Error]=queryCH_Java(Obj, Query, Args)
            % Java is x2 faster
            
            arguments
                Obj
                Query     = "SELECT * FROM test_db.test_table;";
                
                    % 'db' - 'SHOW DATABASES'
                    % '
                
                %"SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'";

                %SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db';

                Args.IsExec logical           = false;
            end
            
            Error = Obj.Conn.Message;
            if isempty(Error)
                % Execute the query
                if Args.IsExec
                    exec(Obj.Conn, Query);
                    Result = [];
                else
                    Result = fetch(Obj.Conn, Query);
                end
        
                % Convert result to a table if it's not already a table
                if ~istable(Result) && ~isempty(Result)
                    Result = struct2table(Result);
                end
                
                
            else
                Result = [];
            end
        
        end
        
        function Result=queryCH_Http(Obj, TableName, Query)
            %
            
            arguments
                Obj
                TableName = "test_db.users";
                Query     = "SELECT * FROM test_db.users;";
                
                %"SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'";

                %SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db';

                %"SELECT * FROM test_db.users;";
            end
       
                       
            % Define ClickHouse server URL for HTTP interface
            %URL = 'http://localhost:8123/';
            URL = sprintf('http://%s:%s/',Obj.Host, Obj.Port);
    
            QueryURL = sprintf("%s?user=%s&password=%s&query=%s", URL, Obj.User, Obj.Password, urlencode(Query));
      
            % Send HTTP request and fetch result as text
            try
                QueryResult = webread(QueryURL);
            catch ME
                error('Error querying ClickHouse: %s', ME.message);
            end

            Ncol = 3;
            
            Format = tools.string.str_duplicate('%s ',Ncol, '\n');
            CellResult = textscan(QueryResult, Format,'Delimiter','\t');
                
            
            ResultLines = strsplit(QueryResult, '\n');
            
            % Remove any empty lines from the result
            %ResultLines = ResultLines(~tools.cell.isempty_cell(ResultLines));
            
            
            %Result = cellfun(@(x) strsplit(x, '\t'), ResultLines, 'UniformOutput', false);
            %Result = table((vertcat(ResultCell{:})));
           
            Result = QueryResult;
        end
            
    end
        
        
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
