% Database Driver Class, currently supports PostgreSQL
% DbDriver is used **internally** by DbConnection, and SHOULD NOT be 
% accessed by the user.
% We currently use postgresql-42.2.19.jar
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
% DbDriver - Constructor
% copyDriverFile - Copy driver file from source folder to target path This is requried to call javaclasspath()
% delete - Destructor
% getDbDriver - Get singleton Map object that maps database type to DbDriver object
% loadDriver - Load database driver library See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
% unloadDriver - Unload database driver
% #/functions (autogen)
%

classdef DbDriver < Component
    % Database Driver Class, internally used by DbConnection
    
    % Properties
    properties (SetAccess = public)
        
        % Connection details
        DatabaseType = 'postgres'   % Currently we support only Postgres
        
        % Driver Java package file (.jar)
        PostgresJar = 'postgresql-42.2.19.jar'
        
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
            % Constructor            
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
            % Destructor
            Obj.msgLog(LogLevel.Debug, 'deleted started: %s', Obj.Uuid);
            Obj.unloadDriver();
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    
    methods % Connect, disconnect
                        
        function Result = loadDriver(Obj)
            % Load database driver library
            % See https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
            Obj.msgLog(LogLevel.Info, 'loadDriver');
            
            % Already open
            if Obj.IsLoaded
                Obj.msgLog(LogLevel.Info, 'loadDriver: already loaded');
                Result = true;
                return
            end
                      
            % Postgres
            if strcmp(Obj.DatabaseType, 'postgres')
                
                % Copy driver .jar file from source folder to target folder
                if Obj.copyDriverFile(Obj.PostgresJar)
                                
                    % Create Java driver object
                    try
                        Obj.JavaDriver = org.postgresql.Driver;
                        Obj.IsLoaded = true;
                    catch
                        Obj.msgLog(LogLevel.Error, 'open: Failed to get org.postgresql.Driver');
                    end
                end
                
            % Other type, currently not supported
            else
                Obj.msgLog(LogLevel.Error, 'openDriver: Database type not supported: %s', Obj.DatabaseType);
            end
            
            Result = Obj.IsLoaded;
        end
                
        
        function Result = unloadDriver(Obj)
            % Unload database driver
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
        
        
        function Result = copyDriverFile(Obj, FileName)
            % Copy driver file from source folder to target path
            % This is requried to call javaclasspath()
            Result = false;
            
            % Get full path and name of the file in which the call occurs
            MyFileName = mfilename('fullpath');
            [MyPath, ~, ~] = fileparts(MyFileName);
            Obj.SourceJarFile = fullfile(MyPath, FileName);
                        
            % Target is system temporary folder
            % On Linux we use /tmp, on Windows we assume
            % that we can use C:\Temp
            if tools.os.iswindows()
                Obj.TargetJarFile = fullfile('C:\Temp', FileName);
            else
                Obj.TargetJarFile = fullfile('/tmp', FileName);
            end
            
            Obj.msgLog(LogLevel.Info, 'copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
            if copyfile(Obj.SourceJarFile, Obj.TargetJarFile)
            else
                Obj.msgLog(LogLevel.Warning, '(already running?) cannot copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
            end
            
            % Add jar file to javaclasspath (ensure it is present in your current dir)
            try
                javaclasspath(Obj.TargetJarFile);
                Obj.msgLog(LogLevel.Info, 'driver OK');
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'javaclasspath failed: %s', Obj.TargetJarFile);
            end
        end
                
    end
    
    
    methods(Static) % getDbDriver - Static Singleton
        
        function Result = getDbDriver(varargin)
            % Get singleton Map object that maps database type to DbDriver object
            persistent Map
            if isempty(Map)
                Map = ComponentMap('DbDriver');
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
    methods(Static) % Unit test
        Result = unitTest()
    end
        
end
