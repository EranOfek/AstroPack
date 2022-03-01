%
%--------------------------------------------------------------------------

classdef ObsOptics < Component
    
    % Properties
    properties (SetAccess = public)            

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsOptics()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsOptics created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %
        end        
    end
    
    
    methods %
    end
    
    
    methods(Static) %
        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
            
    end    
    
end
