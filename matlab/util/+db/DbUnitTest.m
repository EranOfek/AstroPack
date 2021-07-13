

classdef DbUnitTest < Component
    
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
        function Obj = DbUnitTest()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end        
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            
            disp(db.Glb);
            
            % Create database from XLSX
            
            % Insert
            
            % Update
            
            % Delete
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'DbDriver test passed')
            Result = true;
        end
    end    
        
    
end

