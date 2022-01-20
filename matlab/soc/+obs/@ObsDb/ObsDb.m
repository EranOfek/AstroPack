
classdef ObsDb < Component
    
    % Properties
    properties (SetAccess = public)            
        
        %Conn io.db.DbConnectinn
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsDb()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsDb created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'ObsDb deleted: %s', Obj.Uuid);
        end        
    end
    
    
    methods %
                        
  
    end
    
    
    methods(Static)
        
        function Result = getObsDb()
            persistent Db
            if isempty(Db)
                Db = ObsDb();
            end
     
            Result = Db;                         
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    end    
        
end
