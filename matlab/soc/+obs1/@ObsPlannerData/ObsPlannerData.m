% Planner data record

classdef ObsPlannerData < Component
    
    % Properties
    properties (SetAccess = public)
        Query = []
        KeyField = ''
    end
    
    %-------------------------------------------------------- 
    methods % Constructor            
        function Obj = ObsPlannerData(varargin)           
        end
        
        
        % Destructor
        function delete(Obj)
            %io.msgLog(LogLevel.Debug, 'ObsPlannerData deleted');
        end
    end

    
    methods % Main functions
    
        
    end
    
    %----------------------------------------------------------------------   
    methods(Static) % Unit test
                         
        Result = unitTest();
            %
    end
        
end
