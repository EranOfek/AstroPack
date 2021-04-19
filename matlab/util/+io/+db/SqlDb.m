% FITS Database Class
%--------------------------------------------------------------------------


classdef SqlDb < DbComponent
    % Properties
    properties (SetAccess = public)            
        DataSource = '';
        UserName = 'postgres';
        Password = 'pass';
        Driver = 'org.postgresql.Driver';
        Url = 'jdbc:postgresql://localhost:5432/avionics';
        Schema = 'public';
        Conn = 0;
        
        SqlText = ''
        Record = []
        
        DbName = ''
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = SqlDb()
            
        end
    end
    
    
    methods
        
        
        function Result = open(Obj)
        end
        
            
        function Result = close(Obj)
        end
        
        
        function Result = connect(Obj)
            
            Obj.Conn = database(Cbj.DataSource, Obj.UserName, Obj.Password, Obj.Driver, Obj.Url);           
            switch isopen(Obj.Conn)
                case 1
                    msgLog('Database connected OK')
                    Result = true;
                otherwise
                    msgLog('Database connected FAILED')
                    
                    % Exception?
                    error('Failed to connection with database')
                    Result = false;
            end            
            
        end
        
        
        function disconnect()
        end
        
        
        function select(Obj, QueryText)
            Obj.SqlText = sprintf('%s FROM %s', QueryText, Obj.Schema);
            Obj.Record = select(Obj.Conn, Text)
        end
        
        
        function selectWhere(Obj, QueryText, WhereText)
        end
        
        
        function exec(Obj, QueryText)
        end 
            
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgLog(LogLevel.Test, "Started\n");
   
            
            % Test: Create database and tables
            
            % Test: Write data to tables
            
            % Test: Query tables         
            
 
            io.msgLog(LogLevel.Test, "Passed")
            Result = true;
        end
    end    
        
    
end





function msgLog(varargin)
    %fprintf('fits: ');
    fprintf(varargin{:});
    fprintf('\n');
end



