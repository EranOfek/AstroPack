%
%--------------------------------------------------------------------------

classdef ObsPlannerManager < Component
    
    % Properties
    properties (SetAccess = public)            
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsPlannerManager()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsPlannerManager created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %Obj.msgLog(LogLevel.Debug, 'ObsPlannerManager deleted: %s', Obj.Uuid);
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
