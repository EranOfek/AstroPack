
%global Glb
%Glb = [];


classdef Db < HandleComponent
    
    % Properties
    properties (SetAccess = public)            
        DbUnitTest db.DbConnection
        DbPipeline db.DbConnection
        DbLast db.DbConnection
        DbPlanner db.DbConnection        
        DbSoc db.DbConnection
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
            Obj.DbUnitTest = db.DbConnection.getDbConnection('unittest');
            Obj.DbUnitTest.DatabaseName = 'unittest';
            
            Obj.DbPipeline = db.DbConnection.getDbConnection('pipeline');
            Obj.DbPipeline.DatabaseName = 'pipeline';            
            
            Obj.DbLast = db.DbConnection.getDbConnection('lastdb');
            Obj.DbLast.DatabaseName = 'lastdb';                        
            
            Obj.DbPlanner = db.DbConnection.getDbConnection('planner');
            Obj.DbPlanner.DatabaseName = 'planner';            
            
            Obj.DbSoc = db.DbConnection.getDbConnection('soc');
            Obj.DbSoc.DatabaseName = 'soc';                        

            Result = true;
        end
        
    end
    
    
    
    methods(Static) %
        
        function Result = getDb()
            persistent Db
            if isempty(Db)
                Db = db.Db;
                Db.setup();
            end
            Result = Db;
        end
            
        
        function Result = getUnitTest(Obj)        
            Db = db.Db.getDb();
            Result = Db.DbUnitTest;            
        end
        
        
        function Result = getPipeline(Obj)        
            Db = db.Db.getDb();
            Result = Db.DbPipeline;            
        end        
        
        
        function Result = getLast(Obj)        
            Db = db.Db.getDb();
            Result = Db.DbLast;            
        end        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'Db test started\n');
            
            Glb = Db;
            
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'Db test passed')
            Result = true;
        end
    end    
            
end

