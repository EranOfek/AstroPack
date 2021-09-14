% DbConnection Class, connection to database on host, as use
% Wrapper for Java Connection class
% 'jdbc:postgresql://localhost:5432/pipeline'
%
% Used internally by DbQuery

classdef DbConnection < HandleComponent
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        DatabaseName = 'pipeline'   % Database name
        UserName = 'postgres'       % Login user
        Password = 'pass'           % Login password
        Host = 'localhost'          % Host name or IP address
        Port = 5432                 % Post number, 5432 is Postgres default
                                    
        % Driver
        DriverName = 'postgres'     % Driver name
        Driver = []                 % DbDriver
        Url = ''                    % Connection URL
        Metadata = []               % Metadata
        
        ConnectionStr = ''          % 'jdbc:postgresql://localhost:5432/pipeline'
        Conn = []                   % Java Connection object, returned by connect()
        IsOpen = false              % True if connection is open        
    end
    
    %-------------------------------------------------------- 
    methods % Constructor
        
        function Obj = DbConnection(varargin)
            Obj.setName('DbConnection');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);            
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end                
    end
    
    
    methods % Connect, disconnect
               
        function Result = open(Obj)          
            % Connect to database specified by Host:Port:Database as UserName/Password
            
            f_ = io.FuncLog('DbConnection.open');
            Obj.msgLog(LogLevel.Info, 'open');
            
            % Already open            
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'open: already open');
                return
            end
               
            % Setup driver
            if isempty(Obj.Driver)
                Obj.Driver = db.DbDriver.getDbDriver(Obj.DriverName);
            end
            
            % Open driver
            if ~Obj.Driver.IsOpen
                Obj.Driver.open();
            end

            % Prepare username and password
            try                
                Obj.msgLog(LogLevel.Debug, 'open: setProperty: %s/%s', Obj.UserName, Obj.Password);
                props = java.util.Properties;
                props.setProperty('user', Obj.UserName);
                props.setProperty('password', Obj.Password);
            catch
                Obj.msgLog(LogLevel.Error, 'open: setProperty failed');
            end
     
            % Connect to database
            try
                Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
                Obj.msgLog(LogLevel.Info, 'open: Url: %s', Obj.Url);
                Obj.Conn = Obj.Driver.Driver.connect(Obj.Url, props);
                Obj.IsOpen = true;
                Obj.msgLog(LogLevel.Info, 'open: connect OK: %s', Obj.Url);
            catch
            end

            % Get metadata
            try
                Obj.msgLog(LogLevel.Debug, 'open: calling getMetaData');
                Obj.Metadata = Obj.Conn.getMetaData();   
            catch
                Obj.msgLog(LogLevel.Error, 'open: getMetaData failed');
            end         
            
            Result = Obj.IsOpen;
            Obj.msgLog(LogLevel.Info, 'open finished');
        end
                
        
        function Result = close(Obj)
            % Disconnect from database, % @Todo
            
            Obj.msgLog(LogLevel.Info, 'close');
            try
                Obj.IsOpen = false;
            catch
            end
            
            Result = ~Obj.IsOpen;
        end
       
        
        function Result = newQuery(Obj)
            % Create new DbQuery instance linked to this connection
            
            Result = db.DbQuery(Obj)
        end
                
    end
    
    %----------------------------------------------------------------------
    methods(Static) % getDbConnection
        
        function Result = getConnectionKey()
            %Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Result = '';
        end
        
        
        function Result = getDbConnection(ConnKey) %, Args)
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
            if isempty(Comp)
                Comp = db.DbConnection();
                Comp.MapKey = ConnKey;
                Map.add(Comp);
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
