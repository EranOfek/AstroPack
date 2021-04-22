% PostgreSQL Database Class
%
% The native Matlab database functions requires installation of the Matlab
% Database Toolbox. To avoid dependency on it, we implement our database
% class using the Java packages.
%
% Some workaround is required to use the java interface.
%
% See:
% https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
%
%
% Postgresql driver must be installed, download page:
% 
% https://jdbc.postgresql.org/
% https://jdbc.postgresql.org/download.html
%
% We currently use postgresql-42.2.19.jar
%--------------------------------------------------------------------------



classdef SqlDb < handle %DbComponent
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        DatabaseName = 'pipeline';  % Database name
        UserName = 'postgres';      % Login user
        Password = 'pass';          % Login password
        Host = 'localhost';         % Host name or IP address
        Port = 5432;                % Post number, 5432 is Postgres default
        
        % Driver
        JarFile = 'postgresql-42.2.19.jar';
        Driver = [];                % Driver object
        Url = '';                   % Connection URL
        
        %Schema = '';
        Conn = [];  % Connection object, returned by connect()
        
        % Current SQL statement data
        SqlText = '';               % SQL text
        Statement = [];     % Prepared statement object
        ResultSet = [];     % Returned result-set
        Record = [];        % Current record
        
        %
        Key = '';
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = SqlDb()
            
            % Generate UUID, used with SqlDbManager
            %Key = Obj.needUuid();
        end
    end
    
    
    methods % Connect, disconnect
                
        function Result = open(Obj)
            % Connect to database
            Result = Obj.connect();
        end
        
            
        function Result = close(Obj)
            % Disconnect from database
            Result = Obj.disconnect();
        end
        
        
        function Result = connect(Obj)          
            % Connect to database specified by Host:Port:Database as UserName/Password
            
            % Add jar file to classpath (ensure it is present in your current dir)
            % Do we need this? javaclasspath('postgresql-9.0-801.jdbc4.jar');
            try
                javaclasspath(Obj.JarFile);
            catch
                
            end

            % Prepare username and password
            try                
                props = java.util.Properties;
                props.setProperty('user', Obj.UserName);
                props.setProperty('password', Obj.Password);
            catch
            end
            
            % Create the database connection
            try                
                Obj.Driver = org.postgresql.Driver;
            catch
            end
     
            % Connect
            try
                Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
                Obj.Conn = Obj.Driver.connect(Obj.Url, props);
            catch
            end

            Result = true;
        end
        
        
        
        function Result = disconnect(Obj)
            % Disconnect from database
            try
            catch
            end
            
            Result = true;
        end
    end
    
        
    methods
        
        function Result = test(Obj)                  
            
            Obj.SqlText = 'select * from raw_images';

            Obj.Statement = Obj.Conn.prepareStatement(Obj.SqlText);            
            Obj.ResultSet = Obj.Statement.executeQuery();

            % Read the results into an array of result structs
            count=0;
            result=struct;
            while Obj.ResultSet.next()
                disp(Obj.ResultSet.getString('ImageID'));
                %count=count+1;
                %result(count).var1=char(rs.getString(2));
                %result(count).var2=char(rs.getString(3));
                %...
            end

            Obj.ResultSet.close();
            disp('Done');
            
            %
%             x = 1;
% 
%             for i=1:10
%                 pk = ['pk_', string(i).char];
%                 sql = 'INSERT INTO raw_images(ImageID, RA_Center) VALUES(%s,%s);'
% 
%                 Obj.Statement = Obj.Conn.prepareStatement(sql);            
%                 Obj.ResultSet = Obj.Statement.executeQuery();                
%             end
            
        end
        
             
        function select(Obj, QueryText)
            % Execute SELECT query
            Obj.SqlText = sprintf('%s FROM %s', QueryText, Obj.Schema);
            Obj.Record = select(Obj.Conn, Text)
        end
        
        
        function selectWhere(Obj, QueryText, WhereText)
        end
        
        
        function exec(Obj, QueryText)
            % Execute query text
            Obj.SqlText = QueryText;
            
            Obj.Statement = Obj.Conn.prepareStatement(Obj.SqlText);
            
            Obj.ResultSet = Obj.Statement.executeQuery();
            
        end 
            
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            %io.msgLog(LogLevel.Test, "Started\n");
   
            
            % Test: Create database and tables
            
            % Test: Write data to tables
            
            % Test: Query tables         
            
 
            %io.msgLog(LogLevel.Test, "Passed")
            Result = true;
        end
    end    
        
    
end




function msgLog(varargin)
    %fprintf('fits: ');
    fprintf(varargin{:});
    fprintf('\n');
end

