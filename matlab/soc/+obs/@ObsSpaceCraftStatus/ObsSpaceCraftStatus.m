%
%--------------------------------------------------------------------------

classdef ObsSpaceCraftStatus < Component
    
    % Properties
    properties (SetAccess = public)            
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsSpaceCraftStatus()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsSpaceCraftStatus created: %s', Obj.Uuid);
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
