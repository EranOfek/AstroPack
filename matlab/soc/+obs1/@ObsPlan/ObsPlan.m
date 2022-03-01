
classdef ObsPlan < Component
    
    % Properties
    properties (SetAccess = public)            
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsPlan()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsPlan created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %Obj.msgLog(LogLevel.Debug, 'ObsPlan deleted: %s', Obj.Uuid);
        end        
    end
    
    
    methods % Connect, disconnect

  
    end
    
    
    methods(Static)
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %

    end    
            
end
