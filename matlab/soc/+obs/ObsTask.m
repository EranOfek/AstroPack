
classdef ObsPlan < Component
    
    % Properties
    properties (SetAccess = public)            
        
        Conn io.db.DbConnectinn
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsPlan()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsPlan created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'ObsPlan deleted: %s', Obj.Uuid);
        end        
    end
    
    
    methods % Connect, disconnect
                        
        function Result = open(Obj)          
            
            % Open/close connection
            Obj.Conn = io.db.DbConnection;            
            Obj.Conn.DatabaseName = 'planner';
            Obj.Conn.open();
            assert(Obj.Conn.IsOpen);        
                           
            %
            Obj.msgLog(LogLevel.Info, 'PlannerDb: open');
            
            % Already open
            if Obj.IsOpen
                Obj.msgLog(LogLevel.Info, 'DbDriver.open: already open');                
                Result = true;
                return
            end

            
            Result = Obj.IsOpen;
        end
        
  
    end
    
    
    methods(Static) % getDbDriver
        
        function Result = getPlannerDb()
            persistent Db
            if isempty(Db)
                Db = PlannerDb();
            end
     
            Result = Db;                         
        end
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PlannerDb test started\n');
               
            % Test: Open/close driver
            Driver = io.db.DbDriver;
            Driver.open();
            assert(Driver.IsOpen); 
            Driver.close();
            assert(~Driver.IsOpen);
    
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'PlannerDb test passed')
            Result = true;
        end
    end    
        
    
end

