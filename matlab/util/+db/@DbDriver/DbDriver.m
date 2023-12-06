% File:    DbDriver.m
% Class:   DbDriver
% Title:   Internaly used by DbConnection to load JDBC driver.
% Author:  Chen Tishler
% Created: July 2021
%--------------------------------------------------------------------------
% Description:
%
% Database Driver Class, currently supports PostgreSQL.
% DbDriver is used **INTERNALLY** by DbConnection, and SHOULD NOT be
% accessed by the user.
% DbDriver is based on Java package, current version is postgresql-42.3.1.jar
%
% We copy the jar file from its original loation in the repository
% to a target folder that is added by javaclasspath().
%
% The native MATLAB database functions require installation of MATLAB
% Database Toolbox. To avoid dependency on it, we implement our database
% classes using Java packages. Some workaround is required to use the java interface.
% See: https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
%
% PostgreSQL Java driver (jar file) must be installed, download page:
%       https://jdbc.postgresql.org/
%       https://jdbc.postgresql.org/download.html
%
% There should be only one driver object for each database type.
% For example, when working with Postgres, we need only one DbDriver for
% it. We hold a 'persistent' ComponentMap of all created DbDriver objects with
% the database type (i.e. 'postgres') as the key.
%
% Usage:
%   Drv = DbDriver.getDbDriver('postgres');
%   Drv.loadDriver();
%--------------------------------------------------------------------------
%
%#docgen
%
% Methods:
%    DbDriver - Constructor, currently only 'postgres' is supported
%    delete - Destructor, unload driver from memory
%    loadDriver - Load database driver library Calls copyDriverFile() to copy the library file to target folder Output: true if loaded successfully
%    unloadDriver - Unload database driver Output: true if unloaded successfully
%    validateConfig - NOT IMPLEMENTED YET Validate that we have all configuration params that we need
%
% Methods: Static
%    getDbDriver - Get singleton Map object that maps database type to DbDriver object Example: Driver = db.DbDriver.getDbDriver('postgres')
%
%#/docgen


classdef DbDriver < Component
    % Database Driver Class, internally used by DbConnection
    
    % Properties
    properties (SetAccess = public)        
        DatabaseType  = 'postgres'               % Database type, currently only Postgres is supported.
        PostgresJar   = 'postgresql-42.3.1.jar'  % Driver Java package file (.jar)       
        IsLoaded      = false                    % True when driver is loaded (open)
        JavaDriver    = []                       % Java Driver object, returned by org.postgresql.Driver
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbDriver(Args)
            % Constructor, currently only 'postgres' is supported
            % Input   : -
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'DatabaseType' - Database driver type, currently 'postgres' is
            %             supported.
            % Output  : New instance of DbDriver
            % Author  : Chen Tishler (2021)
            % Example : drv = DbDriver()
            arguments
                Args.DatabaseType = 'postgres'
            end
            Obj.setName('DbDriver');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
            
            % Currently only postgres is supported
            assert(strcmp(Args.DatabaseType, 'postgres'))
            Obj.DatabaseType = Args.DatabaseType;
        end
        
        
        function delete(Obj)
            % Destructor, unload driver from memory.
            % Internally called by Matlab when the object is destroyed.
            Obj.msgLog(LogLevel.Debug, 'deleted started: %s', Obj.Uuid);
            Obj.unloadDriver();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    
    methods % Connect, disconnect
                        
        function Result = loadDriver(Obj)
            % Load Java database driver library to memory.
            % Calls copyDriverFile() to copy the library file to target folder.
            % It is required because in some cases loading the jar file from
            % its location in the repository fails.
            % Input   : - DbDriver object
            % Output  : - true if loaded successfully
            % Author  : Chen Tishler (2021)
            % Example : drv.loadDriver()
            % Ref     : https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
            Obj.msgLog(LogLevel.Info, 'loadDriver');
            
            % Already open?
            if Obj.IsLoaded
                Obj.msgLog(LogLevel.Info, 'loadDriver: already loaded');
                Result = true;
                return
            end
                      
            % Postgres
            if strcmp(Obj.DatabaseType, 'postgres')
                
                % Copy driver .jar file from source folder to target folder
                tools.os.copyJavaJarToTempDir(fullfile(tools.os.getAstroPackExternalPath(), 'postgresql', Obj.PostgresJar));

                % Create Java driver object
                try
                    Obj.JavaDriver = org.postgresql.Driver;
                    Obj.IsLoaded = true;
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'open: Failed to get org.postgresql.Driver');
                end
                
            % Other type, currently not supported
            else
                Obj.msgLog(LogLevel.Error, 'openDriver: Database type not supported: %s', Obj.DatabaseType);
            end
            
            Result = Obj.IsLoaded;
        end
                
        
        function Result = unloadDriver(Obj)
            % Unload database driver from memory.
            % Input   : - DbDriver object
            % Output  : - true if unloaded successfully
            % Author  : Chen Tishler (2021)
            % Example : drv.unloadDriver()
            Obj.msgLog(LogLevel.Info, 'unloadDriver');
            if Obj.IsLoaded
                try
                    % Is this the correct function
                    % @Todo, do we need to call disconnect() or ???
                    clear Obj.JavaDriver;
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'unloadDriver: Exception');
                end
                Obj.JavaDriver = [];
                Obj.IsLoaded = false;
            end
            Result = true;
        end
    end
    
    
    methods(Static) % getDbDriver - Static Singleton
        
        function Result = getDbDriver(varargin)
            % Get singleton Map object that maps database type to DbDriver object.
            % Input   : - [Optional] database type as string, currently only 'postgres' is supported
            % Output  : - Instance of DbDriver object
            % Author  : Chen Tishler (2021)
            % Example : Driver = db.DbDriver.getDbDriver('postgres')
            persistent Map
            if isempty(Map)
                Map = ComponentMap('Name', 'DbDriver');
            end
            
            % Set default database type
            if numel(varargin) > 0
                DatabaseType = varargin{1};
            else
                DatabaseType = 'postgres';
            end
            
            % Search in map
            Comp = Map.find(DatabaseType);
            if isempty(Comp)
                % Not found, create new DbDriver object for this database type
                Comp = db.DbDriver();
                Comp.DatabaseType = DatabaseType;
                
                % Add to map
                Comp.MapKey = DatabaseType;
                Map.add(Comp);
            else
                % Already exist in map, just return Comp
            end
            Result = Comp;
        end
    end
    
    %----------------------------------------------------------------------
    methods(Static)
        Result = unitTest()
            % Unit test
    end
        
end
