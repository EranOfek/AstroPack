% PostgreSQL Database Class
%
%--------------------------------------------------------------------------

%
% https://stackoverflow.com/questions/2698159/how-can-i-access-a-postgresql-database-from-matlab-with-without-matlabs-database
%


classdef SqlDb < handle %DbComponent
    % Properties
    properties (SetAccess = public)            
        
        UserName = 'postgres';
        Password = 'pass';
        Host = 'localhost';
        Port = 5432;
        
        JarFile = 'postgresql-42.2.19.jar';
        Driver = [];
        Url = '';
        Schema = '';
        Conn = [];
        
        SqlText = '';
        Record = [];
        ResultSet = [];
        Statement = [];
        
        
        DatabaseName = 'pipeline';
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

            % Add jar file to classpath (ensure it is present in your current dir)
            %javaclasspath('postgresql-9.0-801.jdbc4.jar');
            javaclasspath(Obj.JarFile);

            % Username and password you chose when installing postgres
            props = java.util.Properties;
            props.setProperty('user', Obj.UserName);
            props.setProperty('password', Obj.Password);

            % Create the database connection (port 5432 is the default postgres chooses
            % on installation)
            Obj.Driver = org.postgresql.Driver;

            Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Obj.Conn = Obj.Driver.connect(Obj.Url, props);

        end
        
        
        function Result = test(Obj)                  
            
            Obj.SqlText = 'select * from raw_images';

            Obj.Statement = Obj.Conn.prepareStatement(Obj.SqlText);            
            Obj.ResultSet = Obj.Statement.executeQuery();

            % Read the results into an array of result structs
            count=0;
            result=struct;
            while Obj.ResultSet.next()
                disp(Obj.ResultSet.getString('ImageID'));
                %count=count+1;
                %result(count).var1=char(rs.getString(2));
                %result(count).var2=char(rs.getString(3));
                %...
            end

            Obj.ResultSet.close();
           
            
            x = 1;

            for i=1:10
                pk = ['pk_', string(i).char];
                sql = 'INSERT INTO raw_images(ImageID, RA_Center) VALUES(%s,%s);'

                Obj.Statement = Obj.Conn.prepareStatement(sql);            
                Obj.ResultSet = Obj.Statement.executeQuery();                
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
            Obj.SqlText = QueryText;
            
            Obj.Statement = Obj.Conn.prepareStatement(Obj.SqlText);
            
            Obj.ResultSet = Obj.Statement.executeQuery();
            
        end 
            
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            %io.msgLog(LogLevel.Test, "Started\n");
   
            
            % Test: Create database and tables
            
            % Test: Write data to tables
            
            % Test: Query tables         
            
 
            %io.msgLog(LogLevel.Test, "Passed")
            Result = true;
        end
    end    
        
    
end





function msgLog(varargin)
    %fprintf('fits: ');
    fprintf(varargin{:});
    fprintf('\n');
end



