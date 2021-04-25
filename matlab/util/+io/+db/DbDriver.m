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


classdef DbDriver < Component
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        DatabaseType = 'postgres'   % Currently we support only Postgres
        
        % Driver
        PostgresJar = 'postgresql-42.2.19.jar'
        
        SourceJarFile = ''
        TargetJarFile = ''
        Driver = []                % Driver object
        IsOpen = false
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = DbDriver()
            
            % Generate UUID, used with SqlDbManager
            %Key = Obj.needUuid();
        end
    end
    
    
    methods % Connect, disconnect
                        
        function Result = open(Obj)          
            %
            Obj.msgLog(LogLevel.Info, 'DbDriver: open');
            
            % Already open
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'DbDriver.open: already open');                
                Result = true;
                return
            end
                      
            if strcmp(Obj.DatabaseType, 'postgres')
                if Obj.copyDriverFile(Obj.PostgresJar)
                                
                    % Create the database connection
                    try                
                        Obj.Driver = org.postgresql.Driver;
                        Obj.IsOpen = true;
                    catch
                        Obj.msgLog(LogLevel.Error, 'DbDriver.open: Failed to get org.postgresql.Driver');
                    end
                end
            else
                Obj.msgLog(LogLevel.Error, 'DbDriver.open: Database type not supported: %s', Obj.DatabaseType);
            end
            
            Result = Obj.IsOpen;
        end
        
        
        function Result = copyDriverFile(Obj, FileName)        
            % Copy driver file to target path
            Result = false;
            
            % Get full path and name of the file in which the call occurs
            MyFileName = mfilename('fullpath');       
            [MyPath, ~, ~] = fileparts(MyFileName);            
            Obj.SourceJarFile = fullfile(MyPath, FileName);
                        
            if tools.os.iswindows()
                Obj.TargetJarFile = fullfile('C:\Temp', FileName);
            else
                Obj.TargetJarFile = fullfile('/tmp', FileName);
            end
            
            Obj.msgLog(LogLevel.Info, 'DbDriver: Copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
            if copyfile(Obj.SourceJarFile, Obj.TargetJarFile)
            else
                Obj.msgLog(LogLevel.Error, 'DbDriver: Cannot copy file %s to %s', Obj.SourceJarFile, Obj.TargetJarFile);
            end
            
            % Add jar file to classpath (ensure it is present in your current dir)
            % Do we need this? javaclasspath('postgresql-9.0-801.jdbc4.jar');
            try
                javaclasspath(Obj.TargetJarFile);
                Obj.msgLog(LogLevel.Info, 'DbDriver: driver OK');
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'DbDriver: javaclasspath failed: %s', Obj.TargetJarFile);                
            end
            
        end
                
        
        function Result = close(Obj)
            % Disconnect from database
            
            Obj.msgLog('DbDriver: close');
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
            persistent Manager
            if isempty(Manager)
                Manager = CompRegManager;
            end
            
            if isempty(DatabaseType)
                DatabaseType = 'postgres';
            end
            
            Key = DatabaseType;
            Comp = Manager.getComp(Key);
            if isempty(Comp)
                Comp = io.db.DbDriver();
                Comp.DatabaseType = DatabaseType;
                Comp.RegKey = DatabaseType;
                Manager.add(Comp);
            else
            end
            Result = Comp;                         
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgLog(LogLevel.Test, "DbDriver test started\n");
               
            % Test: Open/close driver
            Driver = io.db.DbDriver;
            Driver.open();
            assert(Driver.IsOpen); 
            Driver.close();
            assert(~Driver.IsOpen);
            
            % Get/register driver, open, close
            Drv = io.db.DbDriver.getDbDriver('postgres');
            assert(~isempty(Drv));
            Drv.open();
            assert(Drv.IsOpen);
            Drv.close();
            Drv2 = io.db.DbDriver.getDbDriver('postgres');
            assert(~isempty(Drv2));
            assert(~Drv2.IsOpen);
            
            % Done
            io.msgLog(LogLevel.Test, "DbDriver test passed")
            Result = true;
        end
    end    
        
    
end

