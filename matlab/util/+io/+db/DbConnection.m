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

classdef DbConnection < Component
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        DatabaseName = 'pipeline'   % Database name
        UserName = 'postgres'       % Login user
        Password = 'pass'           % Login password
        Host = 'localhost'          % Host name or IP address
        Port = 5432                 % Post number, 5432 is Postgres default
                                    
        % Driver
        DriverName = 'postgres'     % 
        Driver = [ ]
        Url = ''                    % Connection URL
        Metadata = []               %
        
        ConnectionStr = ''          % 'jdbc:postgresql://localhost:5432/pipeline'
        Conn = []  % Connection object, returned by connect()
        IsOpen = false
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = DbConnection(varargin)
            
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'DbConnection created: %s', Obj.Uuid);
            
            if numel(varargin) > 0
                
            end
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'DbConnection deleted: %s', Obj.Uuid);
        end                
    end
    
    
    methods % Connect, disconnect
               
        function Result = open(Obj)          
            % Connect to database specified by Host:Port:Database as UserName/Password
            Obj.msgLog(LogLevel.Info, 'DbConnection.open');
            
            % Already open            
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'DbConnection.open: already open');
                return
            end
               
            % Setup driver
            if isempty(Obj.Driver)
                Obj.Driver = io.db.DbDriver.getDbDriver(Obj.DriverName);
            end
            
            % Open driver
            if ~Obj.Driver.IsOpen
                Obj.Driver.open();
            end

            % Prepare username and password
            try                
                Obj.msgLog(LogLevel.Debug, 'DbConnection.open: setProperty: %s/%s', Obj.UserName, Obj.Password);
                props = java.util.Properties;
                props.setProperty('user', Obj.UserName);
                props.setProperty('password', Obj.Password);
            catch
                Obj.msgLog(LogLevel.Error, 'DbConnection.open: setProperty failed');
            end
     
            % Connect
            try
                Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
                Obj.msgLog(LogLevel.Info, 'DbConnection.open: Url: %s', Obj.Url);
                Obj.Conn = Obj.Driver.Driver.connect(Obj.Url, props);
                Obj.IsOpen = true;
                Obj.msgLog(LogLevel.Error, 'DbConnection.open: connect OK');
            catch
            end

            % Get metadata
            try
                Obj.msgLog(LogLevel.Debug, 'DbConnection.open: calling getMetaData');
                Obj.Metadata = Obj.Conn.getMetaData();   
            catch
                Obj.msgLog(LogLevel.Error, 'DbConnection.open: getMetaData failed');
            end         
            
            Result = Obj.IsOpen;
        end
        
        
        
        function Result = close(Obj)
            % Disconnect from database
            
            Obj.msgLog(LogLevel.Info, 'DbConnection.close');
            try
                Obj.IsOpen = false;
            catch
            end
            
            Result = ~Obj.IsOpen;
        end
       
        
        function Result = newQuery(Obj)
            % Create new DbQuery instance
            
            Result = io.db.DbQuery(Obj)
        end
        
        
    end
    
       %----------------------------------------------------------------------
    methods(Static) % getDbConnection
        
        function Result = getConnectionKey()
            %Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Result = '';
        end
        
        
        function Result = getDbConnection(ConnKey)
            persistent Map
            if isempty(Map)
                Map = ComponentMap('DbConnection');
            end
            
            if isempty(ConnKey)
                ConnKey = 'Default';
            end
            
            Key = ConnKey;
            Comp = Map.find(Key);
            if isempty(Comp)
                Comp = io.db.DbConnection();
                Comp.MapKey = ConnKey;
                Map.add(Comp);
            else
            end
            Result = Comp;                         
        end
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'DbConnection test started');
   
            % Open/close connection
            Conn = io.db.DbConnection;            
            Conn.open();
            assert(Conn.IsOpen);        
            Conn.close();
            assert(~Conn.IsOpen);                        
            
            % Get/register connection, open, close
            Con = io.db.DbConnection.getDbConnection('test');
            assert(~isempty(Con));
            Con.open();
            assert(Con.IsOpen);
            Con.close();
            Con2 = io.db.DbConnection.getDbConnection('test');
            assert(~isempty(Con2));
            assert(~Con2.IsOpen);
                        
            % Done            
            io.msgStyle(LogLevel.Test, '@passed', 'DbConnection test passed');
            Result = true;
        end
    end    
        
    
end


