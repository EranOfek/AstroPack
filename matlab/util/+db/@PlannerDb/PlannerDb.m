
classdef PlannerDb < Component

    % Properties
    properties (SetAccess = public)
        Query = []                                  % DbQuery object
        PlansTableName = 'scheduler.plans'          %
        TargetsTableName = 'scheduler.targets'      %
    end


    methods
        function Obj = PlannerDb(Args)
            % Create new DbQuery obeject
            %
            % Input : - 
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %             'Host'      - Host name
            %             'Database'  - Database name
            %             'UserName'  - User name
            %             'Password'  - Password
            %             'Port'      - Port number
            %
            % Output   : - New instance of LastDb object
            % Author   : Chen Tishler (02/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   Q = DbQuery()
            %
            %
            Obj.setName('PlannerDb');
            
            % Create DbQuery object
            Obj.msgLog(LogLevel.Info, 'PlannerDb created');
            Obj.Query = db.DbQuery('SocDb');
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));            
        end
    end


    methods
        
        function Result = addPlan(Obj, Plan, Targets)
            % Insert Plan with multiple Targets
            % Input :  - PlannerDb object
            %          - Plan data in struct. Must match database fields
            %          - Array of structs with Target fields that match the database
            %
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (03/2023)
            % Example : createTables()
            arguments
                Obj
                Plan
                Targets
            end
            
            Result = Obj.Query.insert(Plan, 'TableName', Obj.PlansTableName, 'ColumnsOnly', true);
            
            % Currently we do not support RETURNING in DbQuery.m, so
            % after Insert, we select the maximum PK
            DataSet = Obj.Query.select('*', 'TableName', Obj.PlansTableName, 'Order', 'pk DESC', 'Limit', 1);
            PlanPK = DataSet.Data(1).pk;
            
            % Add targets
            for i=1:numel(Targets)
                Target = Targets(i);
                Target.trg_plan_pk = PlanPK;
                TargetPK = Obj.addTarget(Target);
            end
            
        end
            
        
        function Result = addTarget(Obj, Target)
            % Insert single Target 
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (03/2023)
            % Example : createTables()
            arguments
                Obj
                Target
            end
            
            % Insert Target fields
            Obj.Query.insert(Target, 'TableName', Obj.TargetsTableName, 'ColumnsOnly', true);
            DataSet = Obj.Query.select('*', 'TableName', Obj.TargetsTableName, 'Order', 'pk DESC', 'Limit', 1);
            TargetPK = DataSet.Data(1).pk;            
            
            Result = TargetPK;
        end

        
        function Result = updatePlan(Obj, PlanPK, Plan)
            % @TODO - Update Plan data (not its Targets)
            % Input :  - PlannerDb object
            %          - 
            %          -
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (03/2023)
            % Example : createTables()
            arguments
                Obj
                PlanPK
                Plan
            end
            
            % Insert Target fields
            Obj.Query.update(Target, 'TableName', Obj.PlansTableName, 'ColumnsOnly', true, 'Where', sprintf('pk = %d', TargetPk));
            
            Result = TargetPK;
        end
        
        
        function Result = updateTarget(Obj, TargetPK, Target)            
            % @TODO - Update Target Data
            % Input :  - LastDb object
            %          * Pairs of ...,key,val,...
            %            The following keys are available:
            % Output  : True on success
            % Author  : Chen Tishler (03/2023)
            % Example : createTables()
            arguments
                Obj
                TargetPK
                Target
            end
            
            % Insert Target fields
            Obj.Query.update(Target, 'TableName', Obj.TargetsTableName, 'ColumnsOnly', true, 'Where', sprintf('pk = %d', TargetPk));
            
            Result = TargetPK;
        end
                
    end

    
    methods(Static)
        Result = unitTest()
            % PlannerDb Unit-Test
    end

end

