% Database Driver Class, currently supports PostgreSQL
% DbDriver is used internally by DbQuery
%
% The native MATLAB database functions require installation of MATLAB
% Database Toolbox. To avoid dependency on it, we implement our database
% class using Java packages.
%
% Some workaround is required to use the java interface.
%
% See:
% https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
%
% Postgresql driver must be installed, download page:
% 
% https://jdbc.postgresql.org/
% https://jdbc.postgresql.org/download.html
%
% We currently use postgresql-42.2.19.jar
%--------------------------------------------------------------------------

classdef DbDriver < HandleComponent
    % Database Driver Class
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        DatabaseType = 'postgres'   % Currently we support only Postgres
        
        % Driver Java package file (.jar)
        PostgresJar = 'postgresql-42.2.19.jar'
        
        SourceJarFile = ''          % Source file
        TargetJarFile = ''          % Target
        Driver = []                 % Java Driver object, returned by org.postgresql.Driver
        IsOpen = false              %        
    end
    
    %-------------------------------------------------------- 
    methods % Constructor
        function Obj = DbDriver()            
            Obj.setName('DbDriver');
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
            % Load database driver library
            Obj.msgLog(LogLevel.Info, 'open');
            
            % Already open
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'open: already open');                
                Result = true;
                return
            end
                      
            % Postgres
            if strcmp(Obj.DatabaseType, 'postgres')
                
                % Copy driver .jar file from source folder to target folder
                if Obj.copyDriverFile(Obj.PostgresJar)
                                
                    % Create Java driver object
                    try                
                        Obj.Driver = org.postgresql.Driver;
                        Obj.IsOpen = true;
                    catch
                        Obj.msgLog(LogLevel.Error, 'open: Failed to get org.postgresql.Driver');
                    end
                end
                
            % Other type
            else
                Obj.msgLog(LogLevel.Error, 'open: Database type not supported: %s', Obj.DatabaseType);
            end
            
            Result = Obj.IsOpen;
        end
        
        
        function Result = copyDriverFile(Obj, FileName)        
            % Copy driver file from source folder to target path
            Result = false;
            
            % Get full path and name of the file in which the call occurs
            MyFileName = mfilename('fullpath');       
            [MyPath, ~, ~] = fileparts(MyFileName);            
            Obj.SourceJarFile = fullfile(MyPath, FileName);
                        
            % Target is temporary folder
            if tools.os.iswindows()
                Obj.TargetJarFile = fullfile('C:\Temp', FileName);
            else
                Obj.TargetJarFile = fullfile('/tmp', FileName);
            end
            
            Obj.msgLog(LogLevel.Info, 'copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
            if copyfile(Obj.SourceJarFile, Obj.TargetJarFile)
            else
                Obj.msgLog(LogLevel.Error, 'cannot copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
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
                
        
        function Result = close(Obj)
            % Unload database driver
            Obj.msgLog(LogLevel.Info, 'close');
            try
                Obj.Driver = [];
                Obj.IsOpen = false;
            catch
            end
            
            Result = true;
        end
    end
    
    
    methods(Static) % getDbDriver
        
        function Result = getDbDriver(DatabaseType)
            % Get singleton Map object that maps database type to DbDriver object
            persistent Map
            if isempty(Map)
                Map = ComponentMap('DbDriver');
            end
            
            % Set default database type
            if isempty(DatabaseType)
                DatabaseType = 'postgres';
            end
            
            % Search in map
            Comp = Map.find(DatabaseType);
            if isempty(Comp)
                % Not found, create new DbDriver object for this database type
                Comp = db.DbDriver();
                Comp.DatabaseType = DatabaseType;
                Comp.MapKey = DatabaseType;
                Map.add(Comp);
            else
                % Already exist
            end
            Result = Comp;                         
        end
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Unit test
        Result = unitTest()
    end    
        
end
