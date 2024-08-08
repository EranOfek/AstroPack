classdef ProductsDb < PlannerBase
    properties (Access = private)
        % Property 
        ClickDb = []        % ClickHouse database
    end
    
    methods
        function Obj = PlannerDb()
            % Constructor
        end
        
        function delete(Obj)
            % Destructor
            
        end
    end
end

