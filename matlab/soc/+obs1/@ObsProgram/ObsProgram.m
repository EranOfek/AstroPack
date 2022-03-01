
classdef ObsProgram < Component
    
    % Properties
    properties (SetAccess = public)            
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsProgram()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsProgram created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %
        end        
    end
    
    
    methods %
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    end    
            
end
