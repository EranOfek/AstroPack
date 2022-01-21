%
%

classdef Scheduler < Component
    
    % Properties
    properties (SetAccess = public)

    end
    
    %--------------------------------------------------------
    methods % Constructor
        function Obj = Scheduler(Args)
            % Constructor
            arguments
                Args.ColNames = [];  % Required when Data is Cell or Matrix
            end
        end            
      
        
        % Destructor
        function delete(Obj)
            %
        end
    end

    
    methods % Main functions
        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Unit test
                         
        Result = unitTest()
            % Unit-Test
            
    end
        
end
