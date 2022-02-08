%--------------------------------------------------------------------------
% File:    DbDriver.m
% Class:   DbDriver
% Title:   Internaly used by DbConnection to load JDBC driver.
% Author:  Chen Tishler
% Created: July 2021
%--------------------------------------------------------------------------
% Description:
%
% Database Driver Class, currently supports PostgreSQL
% DbDriver is used **internally** by DbConnection, and SHOULD NOT be
% accessed by the user.
% We currently use postgresql-42.3.1.jar
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
% For example, when working with Postgress, we need only one DbDriver for
% it. We hold a persistent ComponentMap of all created DbDriver objects with
% the database type (i.e. 'postgres') as the key.
%
% Usage:
%   Drv = DbDriver.getDbDriver('postgres');
%   Drv.loadDriver();
%--------------------------------------------------------------------------

% #functions (autogen)
% DbDriver - Constructor, currently only 'postgres' is supported
% copyDriverFile - Copy driver file from source folder to target path This is requried to call javaclasspath() Input:   FileName - Jar file name Output:  true on success Example: Obj.copyDriverFile(Obj.PostgresJar)
% delete - Destructor, unload driver from memory
% getDbDriver - Get singleton Map object that maps database type to DbDriver object Example: Driver = db.DbDriver.getDbDriver('postgres')
% loadDriver - Load database driver library Calls copyDriverFile() to copy the library file to target folder Output: true if loaded successfully
% unloadDriver - Unload database driver Output: true if unloaded successfully
% validateConfig - NOT IMPLEMENTED YET Validate that we have all configuration params that we need
% #/functions (autogen)
%

classdef DbDriver < Component
    % Database Driver Class, internally used by DbConnection
    
    % Properties
    properties (SetAccess = public)
        
        % Connection details
        DatabaseType = 'postgres'   % Currently we support only Postgres
        
        % Driver Java package file (.jar)
        PostgresJar = 'postgresql-42.3.1.jar'
        
        % We copy the jar file from its original loation in the repository
        % to a target folder that is added by javaclasspath()
        SourceJarFile = ''          % Full path of source jar file to be copied
        TargetJarFile = ''          % Full path of target jar file
        IsLoaded = false            % True when driver is open
        
        % Java objects
        JavaDriver = []             % Java Driver object, returned by org.postgresql.Driver
    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = DbDriver(Args)
            % Constructor, currently only 'postgres' is supported
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
            % Destructor, unload driver from memory
            Obj.msgLog(LogLevel.Debug, 'deleted started: %s', Obj.Uuid);
            Obj.unloadDriver();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    
    methods % Connect, disconnect
                        
        function Result = loadDriver(Obj)
            % Load database driver library
            % Calls copyDriverFile() to copy the library file to target folder
            % Output: true if loaded successfully
            
            % See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
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
                Obj.SourceJarFile = fullfile(tools.os.getAstroPackExternalPath(), 'postgresql', Obj.PostgresJar);
                Obj.TargetJarFile = fullfile(tools.os.getTempDir(), Obj.PostgresJar);
                Obj.msgLog(LogLevel.Info, 'copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
                if ~copyfile(Obj.SourceJarFile, Obj.TargetJarFile)
                    Obj.msgLog(LogLevel.Warning, '(already running? ignore this warning) cannot copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
                end
                javaaddpath(Obj.TargetJarFile);

                % Create Java driver object
                try
                    Obj.JavaDriver = org.postgresql.Driver;
                    Obj.IsLoaded = true;
                catch
                    Obj.msgLog(LogLevel.Error, 'open: Failed to get org.postgresql.Driver');
                end
                
            % Other type, currently not supported
            else
                Obj.msgLog(LogLevel.Error, 'openDriver: Database type not supported: %s', Obj.DatabaseType);
            end
            
            Result = Obj.IsLoaded;
        end
                
        
        function Result = unloadDriver(Obj)
            % Unload database driver
            % Output: true if unloaded successfully
            Obj.msgLog(LogLevel.Info, 'unloadDriver');
            if Obj.IsLoaded
                try
                    % Is this the correct function
                    % @Todo, do we need to call disconnect() or ???
                    clear Obj.JavaDriver;
                catch
                    Obj.msgLog(LogLevel.Error, 'unloadDriver: Exception');
                end
                Obj.JavaDriver = [];
                Obj.IsLoaded = false;
            end
            Result = true;
        end
        
                
        % @Todo:
        function Result = validateConfig(Obj)
            % NOT IMPLEMENTED YET
            % Validate that we have all configuration params that we need
            
            % @Todo: replace with real params
            % assert(~isempty(Obj.Config.Data.System.EnvFolders.ROOT));
            
            Result = true;
        end
    end
    
    
    methods(Static) % getDbDriver - Static Singleton
        
        function Result = getDbDriver(varargin)
            % Get singleton Map object that maps database type to DbDriver object
            % Example: Driver = db.DbDriver.getDbDriver('postgres')
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
