% File:    DbConnection.m
% Class:   DbConnection
% Title:   Internal class used by DbQuery for database connection (using Java 'Connection' class)
% Author:  Chen Tishler
% Created: July 2021
%--------------------------------------------------------------------------
% Usage:
%
% This class is used **INTERNALLY* by DbQuery.
% Use the static function getDbConnection() to get an existing, or create 
% a new DbConnection object, settings will be loaded from the YAML 
% configuration file, in config/ or config/local/ folder.
% See more details in DbQuery documentation.
%
% Example:
%   DbConn = db.DbConnection.getDbConnection('unittest');
%
% Sample connection string:
%   'jdbc:postgresql://localhost:5432/pipeline'
%--------------------------------------------------------------------------
% Todo: 
%
% 1. Need to implement reconnection support in case of server disconnect.
%
%#docgen
%
% Methods:
%    DbConnection - Constructor Input:       'DriverName'      - Currently only 'postgres' is supported       'Host'            - Network host name or IP address       'DatabaseName'    - Database name, i.e. 'unittest'
%    close - Disconnect from database, % @Todo
%    delete - Destructor
%    isSharedPathAvail - Check if shared folder is available between server and client
%    newQuery - Create new DbQuery instance linked to this connection
%    open - Connect to database specified by Host:Port:Database as UserName/Password
%
% Methods: Static
%    findFieldIC - Search struct field name, ignore case Intput:  - Output:  - Example: -
%    getDbConnection - Search global (singleton) map of DbConnection for the specified connection key Since persistent data is visible only inside this function, we call the function with different option for find and to
%
%#/docgen


classdef DbConnection < Component
    
    % Properties
    properties (SetAccess = public)
        Db              = 'UnitTest'       % Database alias from Config.Data.Database.Items
        
        % Connection details provided by user
        % All values should be loaded from Config
        DriverName      = '' %postgres'    % Driver name
        DatabaseName    = '' %unittest'    % Database name
        UserName        = '' %postgres'    % Login user
        Password        = '' %pass'        % Login password
        Host            = '' %localhost'   % Host name or IP address
        Port            = 0  %5432         % Port number, 5432 is Postgres default
        DriverUrl       = '' %             % Connection URL: 'jdbc:postgresql://localhost:5432/pipeline'
        ServerSharePath = '' %             % Path to shared folder on the server, for COPY statements
        MountSharePath  = '' %             % Path to shared folder from client
        
        % Internal data and flags
        IsOpen          = false         % True if connection is open
        Driver          = []            % DbDriver used by this connection
        JavaConn        = []            % [Java object] Java Connection object, returned by connect()
        JavaMetadata    = []            % [Java object] Metadata
    end
    
    %--------------------------------------------------------
    methods % Constructor
        
        function Obj = DbConnection(Args)
            % Constructor
            % Input : -
            %         * Pairs of ...,key,val,...
            %           The following keys are available:                        
            %           'DriverName'      - Currently only 'postgres' is supported
            %           'Host'            - Network host name or IP address
            %           'DatabaseName'    - Database name, i.e. 'unittest'
            %           'UserName'        - Database user name
            %           'Password'        - Database user password
            %           'Port             - Port number, default is 5432
            %           'DriverUrl'       - Connection string ('jdbc:postgresql://localhost:5432/pipeline')
            %           'ServerSharePath' - Path to shared server folder (use NFS)
            % Output  : New DbConnection object
            % Author  : Chen Tishler (2021)
            % Example :
            %           % Create connection from configuration file Database.DbConnections.UnitTest.yml
            %           Conn = DbConnection('Db', 'UnitTest')
            %
            arguments
                % Use to specify database alias from Config.Data.Database.Items
                Args.Db = ''            % Database alias
                Args.UseConfig = true   % True to use configuration file, otherwise Args are used
                
                % Allow user to set value explicitly, when not empty they
                % override the values loaded from configuration
                Args.DriverName         = 'postgres' % Driver name
                Args.Host               % Network host name or IP address
                Args.DatabaseName       % Database name, i.e. 'unittest'
                Args.UserName           % Database user name
                Args.Password           % Database user password
                Args.Port               % Port number, default is 5432
                Args.DriverUrl          % Driver connection string (
                Args.ServerSharePath    %
            end
            
            % Check if already exists - Do not create another object
            if ~isempty(Args.Db)
                DbConn = db.DbConnection.getDbConnection(Args.Db, 'Create', false);
                if ~isempty(DbConn)
                    io.msgLog(LogLevel.Warning, 'DbConnection: Already created by alias, new connection was not created. Use "clear all": %s', Args.Db);
                    
                    % @Todo: Throw exception!
                    return;
                end
            end

            Obj.setName('DbConnection');
            Obj.needUuid();

            % Alias specified, set values from config
            if ~isempty(Args.Db) && Args.UseConfig
                Obj.Db = Args.Db;
                Alias = db.DbConnection.findFieldIC(Obj.Config.Data.Database.DbConnections, Obj.Db);
                if ~isempty(Alias)
                    Obj.Db = Alias;
                    Obj.msgLog(LogLevel.Info, 'Using db alias from config: %s', Obj.Db);
                    Item = Obj.Config.Data.Database.DbConnections.(Obj.Db);
                    Obj.setProps(Item);
                    
                    % On Windows use 'WinMountSharePath' from config
                    if tools.os.iswindows && isfield(Item, 'WinMountSharePath') && ~isempty(Item.WinMountSharePath)
                        Obj.MountSharePath = Item.WinMountSharePath;
                    end
                else
                    Obj.msgLog(LogLevel.Warning, 'Db alias not found in config, make sure this is on purpose: %s', Obj.Db);
                end
                
            % Alias not specified, use explict values
            else
                Obj.setProps(Args);
            end
            
            % Register
            db.DbConnection.getDbConnection('', 'DbConn', Obj, 'Register', true);
            Obj.msgLog(LogLevel.Debug, 'created: %s, uuid: %s', Obj.Db, Obj.Uuid);
        end
        
                
        function delete(Obj)
            % Destructor
            % Internally called by Matlab when the object is destroyed.
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    
    methods % Connect, disconnect
        
        function Result = open(Obj)
            % Connect to database specified by Host:Port:Database as UserName/Password
            % Input   : - DbConnection object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.open()
            
            %PerfLog = io.FuncLog('DbConnection.open');
            Obj.msgLog(LogLevel.Info, 'open');
            
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

            % Prepare DriverUrl (or use the specified one)
            if isempty(Obj.DriverUrl)
                Obj.DriverUrl = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            end
            Obj.msgLog(LogLevel.Info, 'open: Url: %s', Obj.DriverUrl);
                
            % Prepare username and password
            try
                Obj.msgLog(LogLevel.Debug, 'open: setProperty: %s/%s', Obj.UserName, Obj.Password);
                JavaProps = java.util.Properties;
                JavaProps.setProperty('user', Obj.UserName);
                JavaProps.setProperty('password', Obj.Password);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'open: setProperty failed');
            end
     
            % Connect to database
            try
                % Prepare URL from host/port/database
                Obj.JavaConn = Obj.Driver.JavaDriver.connect(Obj.DriverUrl, JavaProps);
                Obj.IsOpen = true;
                Obj.msgLog(LogLevel.Info, 'open: connect OK: %s', Obj.DriverUrl);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'open: JavaDriver.connect failed');
            end

            % Get connection metadata
            try
                Obj.msgLog(LogLevel.Debug, 'open: calling getMetaData');
                Obj.JavaMetadata = Obj.JavaConn.getMetaData();
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'open: getMetaData failed');
            end
            
            Result = Obj.IsOpen;
            Obj.msgLog(LogLevel.Info, 'open done');
        end
                
        
        function Result = close(Obj)
            % Disconnect from database
            % Input   : - DbConnection object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.close()
            
            Obj.msgLog(LogLevel.Info, 'close');
            if Obj.IsOpen
                Obj.IsOpen = false;
                try
                    Obj.JavaConn.close();
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'close: failed');
                end
            end
            
            Result = ~Obj.IsOpen;
        end
       
        
        function Result = newQuery(Obj)
            % Create new DbQuery instance linked to this connection
            % Input   : - DbConnection object
            % Output  : - New DbQuery object lnked to Obj DbConnection
            % Author  : Chen Tishler (2021)
            % Example : Q = Obj.newQuery()
            Result = db.DbQuery(Obj);
        end
        
        
        function Result = isSharedPathAvail(Obj)
            % Check if shared folder is available between server and client, by calling isfolder()
            % Input   : - DbConnection object
            % Output  : - true if shared folder is available
            % Author  : Chen Tishler (2021)
            % Examlpe : Avail = isSharedPathAvail()
            Result = false;
            if ~isempty(Obj.ServerSharePath) && ~isempty(Obj.MountSharePath)
                if isfolder(Obj.MountSharePath)
                    Result = true;
                end
            end            
        end
        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % getDbConnection
               
        function Result = getDbConnection(Alias, Args)
            % Search global (singleton) map of DbConnection for the specified connection key
            % Input   : - Database alias (i.e. name of configuration file, like 'unittest')
            %           * Pairs of ...,key,val,...
            %             The following keys are available:                        
            %             'create'   - true to create new DbConnection object
            %             'register' - true to register the object specified in 'DbConn' argument  
            %             'DbConn'   -
            % Output  : - DbConection object handle
            % Author  : Chen Tishler (2021)
            % Examlpe : Conn = DbConnection.getDbConnection('unittest')
            % Note    : Since persistent data is visible only inside this function,
            %           we call the function with different option for find and to
            %           register
            arguments
                Alias                       %
                Args.Create     = true      % True to create new object
                Args.Register   = false     % True to register the specified object in DbConn
                Args.DbConn     = []        % With Args.Register
            end

            if isempty(Alias) && isempty(Args.DbConn)
                error('getDbConnection: Must specify Db alias');
            end
            
            % Create persisent (singleton) map for all DbConnection(s)
            persistent Map
            if isempty(Map)
                Map = ComponentMap('Name', 'DbConnection');
            end
           
            % Register in map
            if ~isempty(Args.DbConn) && Args.Register
                Res = Map.find(Args.DbConn.Db);
                if isempty(Res)
                    io.msgLog(LogLevel.Debug, 'getDbConnection: Register: %s', Args.DbConn.Db);
                    Args.DbConn.MapKey = Args.DbConn.Db;
                    Map.add(Args.DbConn);
                end
                Result = Args.DbConn;
                return;
            end
            
            % Not found, create new connection object
            % Already exist in the map, just return it
            DbConn = Map.find(Alias);
            if isempty(DbConn) && Args.Create
                % This will call us again with Register=true
                io.msgLog(LogLevel.Debug, 'getDbConnection: Creating: %s', Alias);
                DbConn = db.DbConnection('Db', Alias);
            end
            Result = DbConn;
        end
    end
    

    methods(Static)
        function Result = findFieldIC(Struct, FieldName)
            % Search struct field name, ignore case. Used internally by DbConnection.
            % Used to search configuration files that may not have the exact
            % case as the specified alias (i.e. 'UnitTest' and 'unittest')
            % Input   : - Data of type 'struct'
            %           - Field name to look for
            % Output  : - Actual field name that exists in class
            % Author  : Chen Tishler (2021)
            % Example : Alias = db.DbConnection.findFieldIC(Obj.Config.Data.Database.DbConnections, Obj.Db);
            
            Result = '';
            List = fieldnames(Struct);
            for i=1:numel(List)
                if strcmpi(List{i}, FieldName)
                    Result = List{i};
                    break;
                end
            end
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
         Result = unitTest()
            % unitTest
    end
            
end
