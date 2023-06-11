
classdef PlannerBackend < Component

    % Properties
    properties (SetAccess = public)
        Query = []                                  % DbQuery object
        PlansTableName = 'scheduler.plans'          %
        TargetsTableName = 'scheduler.targets'      %
    end


    methods
        function Obj = PlannerBackend()
            % Create new DbQuery obeject
            %
            % Input : - 
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %
            % Output   : - New instance of PlannerBackend object
            % Author   : Chen Tishler (05/2023)
            % Examples :
            %   % Create query object width default connection parameters
            %   Q = PlannerBackend()
            %
            %
            arguments

            end            
            Obj.setName('PlannerBackend');
            
            % Create object
            Obj.msgLog(LogLevel.Info, 'PlannerBackend created');
            Obj.Query = db.DbQuery('SocDb');
            
            % Query database version, to verify that we have a connection
            pgver = Obj.Query.getDbVersion();
            Obj.msgLog(LogLevel.Info, 'Connected, Postgres version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));            
        end
    end


    methods
        
        function Result = run(Obj)
            % Backend processing loop
            % Author  : Chen Tishler (05/2023)
            % Example : run()           
            while true
                Obj.process();
                pause(0.1);
            end                        
        end
            
        
        function process(Obj)
            % Backend processing
            % Author  : Chen Tishler (05/2023)
            % Example : process()
            
            % Select pending operations
            DataSet = Obj.Query.select('*', 'TableName', TableName, 'Where', sprintf('xxhash = %d', Args.Xxhash));
            
            % Process the operations
            for i=1:numel(DataSet)
                Obj.processOp(DataSet(i).Data);
            end
        end
        
        function Result = process(Obj, Data)
            Result = true;
        end

    end

    
    methods(Static)
        Result = unitTest()
            % Unit-Test
    end

end
