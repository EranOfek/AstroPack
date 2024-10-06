% db.Db - A class for accessing databases
%

classdef Db < Component
    %
    
    properties       
        DbType   = "Clickhouse"; % 'Postgress'|'Clickhouse'
        DbName
        User     = "default"; %"";
        Password = "spotpot"; %[];  % if empty, then use the PasswordsManager class to find and populate the password
        Host     = [];
        Port     = [];
        
        ConnType = 'java';  % 'java'|'http'
        
    end
    
    properties (Hidden)
        Conn = []; % connectivity information (for ConnType=java)
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
    end
      
    methods (Static) % construction
        
        function Conn=connectCH_Java(Args)
            % Connect to ClickHouse DB via Java
            %   To close connection use: close(Conn)
            %
            
            arguments
                Args.DbName  = 'test_db';
                Args.Host    = 'localhost';
                Args.Port    = '8123';
                Args.User    = 'default';
                Args.Password = 'spotpot';
                Args.JarFile = '/home/eran/jdbc/clickhouse-jdbc-0.7.0-all.jar';
                Args.Driver  = 'com.clickhouse.jdbc.ClickHouseDriver';  % 'ru.yandex.clickhouse.ClickHouseDriver'
                Args.BaseURL = "jdbc:clickhouse";
            end
            
            JdbcURL = sprintf("%s://%s:%s/%s",Args.BaseURL, Args.Host, Args.Port, Args.DbName);
                
         
            % Add JDBC driver to the MATLAB Java path if it's not already added
            javaaddpath(Args.JarFile);
            if ~ismember(Args.JarFile, javaclasspath('-dynamic'))
                javaaddpath(Args.JarFile);
            end

            % Set up the JDBC connection
            Conn = database('', Args.User, Args.Password, Args.Driver, JdbcURL);
        end


    end
    
    
    methods % utilities
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
        function [Result,Error]=queryCH_Java(Obj, TableName, Query, Args)
            % Java is x2 faster
            
            arguments
                Obj
                TableName = [];
                Query     = "SELECT * FROM test_db.users;";
                
                %"SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'";

                %SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db';

                Args.Convert2String logical   = true;
            end
            
            Error = Obj.Conn.Message;
            if isempty(Error)
                % Execute the query
                Result = fetch(Obj.Conn, Query);
        
                % Close the connection
                %close(conn);
        
                % Convert result to a table if it's not already a table
                if ~istable(Result)
                    Result = struct2table(Result);
                end
                
                %if Args.Convert2String
                %    ColumnTypes = varfun(@class, Result, 'OutputFormat', 'cell');
                %    
                %end
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
