% DbConnection Class, connection to database on host, as use
% Wrapper for Java Connection class
% 'jdbc:postgresql://localhost:5432/pipeline'
%
% Used internally by DbQuery

% #functions (autogen)
% DbConnection - Constructor
% close - Disconnect from database,  @Todo
% delete - Destructor
% getConnectionKey - Create connection key f - @TBD Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
% getDbConnection - Search global (singleton) map of DbConnection for the specified connection key
% newQuery - Create new DbQuery instance linked to this connection
% open - Connect to database specified by Host:Port:Database as UserName/Password
% setupDefault -
% #/functions (autogen)
%

classdef DbConnection < Component
    
    % Properties
    properties (SetAccess = public)
        
        % Connection details provided by user
        DriverName = 'postgres'     % Driver name
        DatabaseName = 'pipeline'   % Database name
        UserName = 'postgres'       % Login user
        Password = 'pass'           % Login password
        Host = 'localhost'          % Host name or IP address
        Port = 5432                 % Post number, 5432 is Postgres default
                                    
        % Internal data and flags
        Url = ''                    % Connection URL: 'jdbc:postgresql://localhost:5432/pipeline'
        IsOpen = false              % True if connection is open
        
        % Objects
        Driver          = []        % DbDriver used by this connection
        
        % Java objects
        JavaConn        = []        % Java Connection object, returned by connect()
        JavaMetadata    = []        % Metadata

    end
    
    %--------------------------------------------------------
    methods % Constructor
        
        function Obj = DbConnection(Args)
            % Constructor
            arguments
                Args.DatabaseType = 'postgres'
            end            
            Obj.setName('DbConnection');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
        end
        
                
        function delete(Obj)
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    
    methods % Connect, disconnect
               
        function Result = open(Obj)
            % Connect to database specified by Host:Port:Database as UserName/Password
            
            PerfLog = io.FuncLog('DbConnection.open');
            Obj.msgLog(LogLevel.Info, 'open');
            Result = false;
            
            % Already open
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'open: already open');
                Result = true;
                return
            end
               
            % Setup driver
            if isempty(Obj.Driver)
                Obj.Driver = db.DbDriver.getDbDriver(Obj.DriverName);
                if ~Obj.Driver.IsLoaded
                    Obj.Driver.loadDriver();
                end
            end

            % Prepare URL
            Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Obj.msgLog(LogLevel.Info, 'open: Url: %s', Obj.Url);
                
            % Prepare username and password
            try
                Obj.msgLog(LogLevel.Debug, 'open: setProperty: %s/%s', Obj.UserName, Obj.Password);
                JavaProps = java.util.Properties;
                JavaProps.setProperty('user', Obj.UserName);
                JavaProps.setProperty('password', Obj.Password);
            catch
                Obj.msgLog(LogLevel.Error, 'open: setProperty failed');
            end
     
            % Connect to database
            try
                % Prepare URL from host/port/database
                Obj.JavaConn = Obj.Driver.JavaDriver.connect(Obj.Url, JavaProps);
                Obj.IsOpen = true;
                Obj.msgLog(LogLevel.Info, 'open: connect OK: %s', Obj.Url);
            catch
                Obj.msgLog(LogLevel.Error, 'open: JavaDriver.connect failed');
            end

            % Get connetion metadata
            try
                Obj.msgLog(LogLevel.Debug, 'open: calling getMetaData');
                Obj.JavaMetadata = Obj.JavaConn.getMetaData();
            catch
                Obj.msgLog(LogLevel.Error, 'open: getMetaData failed');
            end
            
            Result = Obj.IsOpen;
            Obj.msgLog(LogLevel.Info, 'open done');
        end
                
        
        function Result = close(Obj)
            % Disconnect from database, % @Todo
            
            Obj.msgLog(LogLevel.Info, 'close');
            if Obj.IsOpen
                Obj.IsOpen = false;
                try
                    Obj.JavaConn.close();
                catch
                    Obj.msgLog(LogLevel.Error, 'close: failed');
                end
            end
            
            Result = ~Obj.IsOpen;
        end
       
        
        function Result = newQuery(Obj)
            % Create new DbQuery instance linked to this connection
            Result = db.DbQuery(Obj);
        end
                
    end
    
    %----------------------------------------------------------------------
    methods(Static) % getDbConnection
        
        function Result = getConnectionKey()
            % Create connection key f - @TBD
            %Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Result = '';
        end
        
        
        function Result = getDbConnection(ConnKey)
            % Search global (singleton) map of DbConnection for the
            % specified connection key
            arguments
                ConnKey
            end
            
            persistent Map
            if isempty(Map)
                Map = ComponentMap('DbConnection');
            end
            
            if isempty(ConnKey)
                ConnKey = 'Default';
            end
            
            Key = ConnKey;
            Comp = Map.find(Key);
            
            % Not found, create new connection object
            if isempty(Comp)
                Comp = db.DbConnection();
                Comp.MapKey = ConnKey;
                Map.add(Comp);
                
            % Already exist in the map, just return it
            else
            end
            Result = Comp;
        end
    end
    
    

    % Unit test
    methods(Static)
        function Result = setupDefault()
            
            Con = db.DbConnection.getDbConnection('default');
            assert(~isempty(Con));
            Result = true;
        end
        
    end
        
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
         Result = unitTest()
    end
            
end
