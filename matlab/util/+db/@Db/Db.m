% db.Db - A class for accessing databases
%
% Example: 
% D=db.Db;  % create object
% D.Conn;   % make Java connection
% D.showDB  % show all DBs
% D.useDB('test_db');   % set use for specific DB
% D.showCurrentDB       % show current DB
% D.showTables          % show Tables in current DB
% D.describeTable('test_table')   % show Table columns and teir properties
% [ColNames, ColTypes] = D.getColumns('test_table')
%
% T=D.query("SELECT * FROM test_db.test_table;")
% D.query("USE test_db;", 'IsExec',true)
% D.createTable('test_db',["id"; "name"; "age"], ["UInt32"; "String"; "UInt8"]);
% D.insert('test_db.users',T1);
% D.insert('test_db.users','file.csv');
%
% D.disconnectCH_Java % disconnect Java



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
                if isempty(Struct)
                    Val = [];
                else
                    Val        = Struct.DbName{1};
                end
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
            % Input  : * ...,key,val,...
            %            See code for options
            % Output : - Connection object.
            %          - Jar file name.
            % Author : Eran Ofek (Oct 2024)
            
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
    
    methods (Static) % static utilities
        function [OutString] = table2charDump(T,Args)
            % Format table in characters dump for database insert operation
            % Input  : - table.
            %          * ...,key,val,... 
            %            See code for options.
            % Output : - String
            % Author : Eran Ofek (2024 Oct) 
            % Example: db.Db.table2charDump(T)

            arguments
                T
                Args.NumFormat      = '%.18g';  % Adjust precision as needed
                Args.Delimiter      = ',';
                Args.LineStartChar  = '(';
                Args.LineEndChar    = '),';
                Args.LastLineEndChar= ')';
                Args.LineTerminator = ''; %'\n';
                Args.CharInQuote logical = true;
            end

            ColNames = T.Properties.VariableNames;

            % Convert the table to a cell array
            C = table2cell(T);
            SizeC = size(C);

            % Get the variable types for each column
            VarTypes = varfun(@class, T, 'OutputFormat', 'cell');

            % Initialize an empty string to store the result
            OutString = '';

            % Define a format for numeric precision


            % Iterate over each row in the table
            for I = 1:SizeC(1)
                RowString = '';  % To store the row as a tab-separated string

                % Iterate over each column in the row
                for J = 1:SizeC(2)
                    switch VarTypes{J}
                        case {'single','double'}
                            ValStr = sprintf(Args.NumFormat, C{I,J});
                        case {'char','string'}
                            if Args.CharInQuote
                                ValStr = sprintf('''%s''', C{I,J});
                            else
                                ValStr = sprintf('%s', C{I,J});
                            end
                        otherwise
                            % assume this is an integer or logical
                            ValStr = sprintf('%d',C{I,J});
                    end
                    if J==SizeC(2)
                        RowString = sprintf('%s%s',RowString,ValStr);
                    else
                        RowString = sprintf('%s%s%s ',RowString, ValStr, Args.Delimiter);
                    end            
                end
                if I==SizeC(1)
                    OutString = sprintf('%s %s %s %s %s', OutString, Args.LineStartChar, RowString, Args.LastLineEndChar, Args.LineTerminator);
                else
                    OutString = sprintf('%s %s %s %s %s', OutString, Args.LineStartChar, RowString, Args.LineEndChar, Args.LineTerminator);
                end
            end

        end

        function Result=concatDbTable(DbName, DbTable)
            % Concat DB name and Db Table to <DbName>.<DbTable> string.
            % Input  : - DbName. If empty, will return only DbTable.
            %          - DbTable.
            % Output : String of <DbName>.<DbTable>
            % Author : Eran Ofek (Oct 2024)
            % Example: db.Db.concatDbTable('test_db','test_table')
            %          db.Db.concatDbTable([],'test_table')

            if isempty(DbName)
                Result = DbTable;
            else
                Result = sprintf('%s.%s', DbName, DbTable);
            end
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
            
            if isfield(Obj.Config.Data,'db')
                Result = Obj.Config.Data.db.(Obj.DbType);
                if isempty(DbName)
                    Host = [];
                    Port = [];
                else
                    Ind  = find(strcmp(Result.DbName, DbName));
                    Host = Result.Host{Ind};
                    Port = Result.Port{Ind};
                end
            else
                Host = [];
                Port = [];
                Result = [];
            end

        end
    end
    
        
    methods % main commands
        function [Result, Error] = query(Obj, Query, Args)
            % Execute Query/Exec command on database/table and format the output.
            % Input  : - self.
            %          - A string with a query or exec command to execute.
            %          * ...,key,val,...
            %            'IsExec' - A logical indicating if to run the
            %                   Query string as a query (false) or exec
            %                   (true). If IsExec is true, then the output
            %                   Result will be empty.
            %                   Default is false.
            %            'Convert2String' - A logical indicating if to
            %                   convert the columns in cell arrays to
            %                   string array. Default is true.
            % Output : - Table with output result. If IsExec=true, then
            %            this is empty.
            %          - Error message. If ok, then this is empty.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.query("SELECT * FROM test_db.test_table;");
            %          D.query("SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'");
            %          D.query("SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db'");

            arguments
                Obj
                Query     = [];
                
                Args.IsExec logical           = false;
                Args.Convert2String logical   = true;
                
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

        function Error=createTable(Obj, TableName, ColNames, ColTypes, Args)
            % Create table
            % Input  : - self.
            %          - Table name.
            %          - Cell array or string array of column names.
            %          - Cell array or string arry of column types (one
            %            type per column name).
            %          * ...,key,val,...
            %            'Engine' - Search engine.
            %                   Ask ChatGPT for more options.
            %                   Default is 'MergeTree()'
            %            'OrderBy' - ORDER BY column name.
            %                   Default is 'id'.
            % Output : - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.createTable('test_db',["id"; "name"; "age"], ["UInt32"; "String"; "UInt8"]);
            
            arguments
                Obj
                TableName
                ColNames
                ColTypes
                Args.Engine  = 'MergeTree()';
                Args.OrderBy = 'id';
            end
           
            Ncol = numel(ColNames);
            Command = sprintf('CREATE TABLE %s (',TableName);
            for Icol=1:1:Ncol
                if Icol==Ncol
                    Command = sprintf('%s %s %s)', Command, ColNames{Icol}, ColTypes{Icol});
                else
                    Command = sprintf('%s %s %s,', Command, ColNames{Icol}, ColTypes{Icol});
                end
            end
            Command = sprintf('%s ENGINE = %s  ORDER BY %s', Command, Args.Engine, Args.OrderBy);
            
            [~,Error] = Obj.query(Command, 'IsExec',true);
            
        end
        
        function Error=insert(Obj, TableName, InputTable)
            % Insert operation to table in DB
            %   Can either insert a table object or a csv file.
            % Input  : - self.
            %          - Table name to which to insert the data.
            %          - data to insert. This is either a table which
            %            columns corresponds to the columns in the DB
            %            table, or this is a csv file name for bulk insert.
            % Output : - Error message. If empty, then ok.      
            % Author : Eran Ofek (Oct 2024)
            % Dxample: D.insert('test_db.users',T1)
            
            arguments
                Obj
                TableName
                InputTable    % table of csv file name
            end
            
            if ischar(InputTable) || isstring(InputTable)
                % Assume InputTable is a scv table
                %Command = sprintf('INSERT INTO %s FORMAT CSV FILE ''%s'';', TableName, InputTable);
                %[~,Error]   = Obj.query(Command, 'IsExec',true);
                
                Command = sprintf('clickhouse-client --user=%s --password=%s --query="INSERT INTO %s FORMAT CSV" < %s',...
                                  Obj.User, Obj.Password, TableName, InputTable);
                [~,Error] = system(Command);
                
            else
                ColNames    = InputTable.Properties.VariableNames;
                StrColNames = sprintf('%s, ',string(ColNames));
                StrColNames = StrColNames(1:end-2);
                ValuesStr   = db.Db.table2charDump(InputTable);
                Command     = sprintf('INSERT INTO %s (%s) VALUES %s', TableName, StrColNames, ValuesStr);

                [~,Error]   = Obj.query(Command, 'IsExec',true);
            end
            %         INSERT INTO test_db.users (id, name, age) VALUES (1, 'Alice', 30);
            % 
            %         INSERT INTO test_db.users (id, name, age) VALUES 
            % (1, 'Alice', 30),
            % (2, 'Bob', 25),
            % (3, 'Charlie', 35);
            % 
            % INSERT INTO test_db.users (id, name, age)
            % SELECT id, name, age FROM another_table;
            
        end
    
    end
    
    methods % DB, Tables information
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
            % Retuen all tables in current DB
            %       Use showCurrentDB to get the current DB.
            % Input  : - self.
            %          * ...,key,val,...
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - List of tables in DB.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showTables

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
            % Get table description (schema) with all column names and their properties.
            % Input  : - self.
            %          - Table name, or <database>.<table_name>
            %          * ...mkey,val,...
            %            'Convert2String' - A logical indicating if to
            %                   convert the columns in cell arrays to
            %                   string array. Default is true.
            % Author : Eran Ofek (Oct 2024)
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

        function [ColNames, ColTypes, Error] = getColumns(Obj, TableName, DbName)
            % Return all column names and their type in a table.
            % Input  : - self.
            %          - Table name.
            %          - DB name. If empty, then use current DB.
            %            Default is [].
            % Output : - A string array of column names.
            %          - A string array of types of columns.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: [ColNames, ColTypes]=D.getColumns('test_db')
            
            arguments
                Obj
                TableName
                DbName     = [];
            end

            if isempty(DbName)
                DbName = Obj.showCurrentDB;
            end

            Query = sprintf('SELECT name, type FROM system.columns WHERE table = ''%s'' AND database = ''%s''', TableName, DbName);
            [Tmp, Error] = Obj.query(Query, 'Convert2String',true);
            ColNames = Tmp.name;
            ColTypes = Tmp.type;
            
        end
     
    end
    
    methods % low level functions
        function [Result,Error]=queryCH_Java(Obj, Query, Args)
            % Query Clickhouse DB using Java interface
            % Input  : - self.
            %          - A string with a query or exec command to execute.
            %          * ...,key,val,...
            %            'IsExec' - A logical indicating if to run the
            %                   Query string as a query (false) or exec
            %                   (true). If IsExec is true, then the output
            %                   Result will be empty.
            %                   Default is false.
            % Output : - Output.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Notes  : Java is x2 faster compared to the http interface.
            % Example: D.queryCH_Java("SELECT * FROM test_db.test_table;");
            
            
            arguments
                Obj
                Query     = "SELECT * FROM test_db.test_table;";
                
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
            % Query Clickhoues using the http interface [NOT FINALIZED]
            
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
