classdef PlannerDb < PlannerBase
    properties (Access = private)
        % Property 
        Db = [] % Postgres database
        ClickHouseDb = [] % ClickHouse database
    end
    
    methods
        function Obj = PlannerDb()
            % Constructor
        end
        
        function delete(Obj)
            % Destructor
            
        end


        function List = getPlans(Obj)
        end


        function List = getPlanTargets(Obj, PlanPk)
        end        


        function Result = SavePlan(Obj)
        end


        function Result = SubmitPlan(Obj)
        end        


    end
end

