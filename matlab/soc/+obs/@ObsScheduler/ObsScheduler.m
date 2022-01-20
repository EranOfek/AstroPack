%
%--------------------------------------------------------------------------

classdef ObsScheduler < Component
    
    % Properties
    properties (SetAccess = public)                           
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsScheduler()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsScheduler created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'DbDriver deleted: %s', Obj.Uuid);
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

