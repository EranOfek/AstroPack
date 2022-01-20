%
%--------------------------------------------------------------------------

classdef ObsAstroMaps < Component
    
    % Properties
    properties (SetAccess = public)            
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsAstroMaps()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsAstroMaps created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %
        end        
    end
    
    
    methods % Connect, disconnect
                        
    end      
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    end
end

