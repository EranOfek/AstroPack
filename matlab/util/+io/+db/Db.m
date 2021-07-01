
classdef Db < Component
    
    % Properties
    properties (SetAccess = public)            
        DbUnitTest io.db.DbConnection
        DbPipeline io.db.DbConnection
        DbLast io.db.DbConnection
        DbPlanner io.db.DbConnection        
        DbSoc io.db.DbConnection
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Db()
            Obj.setName('Db');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'Db created: %s', Obj.Uuid);
        end
      
        
        function Result = setup(Obj)
            Obj.DbUnitTest = io.db.DbConnection.getDbConnection('unittest');
            Obj.DbUnitTest.DatabaseName = 'unittest';
            
            Obj.DbPipeline = io.db.DbConnection.getDbConnection('pipeline');
            Obj.DbPipeline.DatabaseName = 'pipeline';            
            
            Obj.DbLast = io.db.DbConnection.getDbConnection('lastdb');
            Obj.DbLast.DatabaseName = 'lastdb';                        
            
            Obj.DbPlanner = io.db.DbConnection.getDbConnection('planner');
            Obj.DbPlanner.DatabaseName = 'planner';            
            
            Obj.DbSoc = io.db.DbConnection.getDbConnection('soc');
            Obj.DbSoc.DatabaseName = 'soc';                        

            Result = true;
        end
        
    end
    
    
    
    methods(Static) %
        
        function Result = getDb()
            persistent Db
            if isempty(Db)
                Db = io.db.Db;
                Db.setup();
            end
            Result = Db;
        end
            
        
        function Result = getUnitTest(Obj)        
            Db = io.db.Db.getDb();
            Result = Db.DbUnitTest;            
        end
        
        
        function Result = getPipeline(Obj)        
            Db = io.db.Db.getDb();
            Result = Db.DbPipeline;            
        end        
        
        
        function Result = getLast(Obj)        
            Db = io.db.Db.getDb();
            Result = Db.DbLast;            
        end        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'Db test started\n');
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'Db test passed')
            Result = true;
        end
    end    
            
end

