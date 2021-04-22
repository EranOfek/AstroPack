% DbQuery
%
% https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%--------------------------------------------------------------------------

classdef DbQuery < Component
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        ConnectionStr = ''
        Conn = []
        
        % Current SQL statement data
        SqlText = ''               % SQL text
        Statement = []     % Prepared statement object
        ResultSet = []     % Returned result-set
        Record = []        % Current record

        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = DbQuery(varargin)
            
            if numel(varargin) == 1
                Conn = varargin{1};
            elseif numel(varargin) == 0
                Conn = io.db.DbConnection.getDbConnection('');
            else
                error('DbQuery: Unknown parameters');
            end
            
            Obj.Conn = Conn;

            % Debug
            Obj.SqlText = 'select * from raw_images';
        end
    end
    
    
    methods % Connect, disconnect
                               
        function Result = open(Obj, varargin)
            %
            Obj.msgLog(LogLevel.Info, 'DbQuery: open');            
            
            % Need connection
            if isempty(Obj.Conn)
                error('DbQuery.open: No connection');
            end
               
            % Open connection
            if ~Obj.Conn.IsOpen
                Obj.Conn.open();
                if ~Obj.Conn.IsOpen
                    error('DbQuery.open: Open connection failed');
                end
            end            
               
            % Set SQL text
            if numel(varargin) == 1
                Obj.SqlText = varargin{1};
            end
                
            % Debug
            Obj.msgLog(LogLevel.Debug, 'DbQuery.open: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);            
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: prepareStatement failed: %s', Obj.SqlText);
            end
            
            try
                Obj.ResultSet = Obj.Statement.executeQuery();
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: executeQuery failed: %s', Obj.SqlText);
            end

            % Read the results into an array of result structs

            while Obj.ResultSet.next()
                ImageID = Obj.getField('ImageID');
                disp(ImageID);
                %count=count+1;
                %result(count).var1=char(rs.getString(2));
                %result(count).var2=char(rs.getString(3));
                %...
            end

            Obj.ResultSet.close();
            disp('Done');
            
            %
%             x = 1;
% 
%             for i=1:10
%                 pk = ['pk_', string(i).char];
%                 sql = 'INSERT INTO raw_images(ImageID, RA_Center) VALUES(%s,%s);'
% 
%                 Obj.Statement = Obj.Conn.prepareStatement(sql);            
%                 Obj.ResultSet = Obj.Statement.executeQuery();                
%             end
            
        end
        
             
        function select(Obj, QueryText)
            % Execute SELECT query
            Obj.SqlText = sprintf('%s FROM %s', QueryText, Obj.Schema);
            Obj.Record = select(Obj.Conn, Text)
        end
        
        
        function selectWhere(Obj, QueryText, WhereText)
        end
        
        
        function exec(Obj, QueryText)
            % Execute query text
            
            Obj.SqlText = QueryText;
            
            Obj.Statement = Obj.Conn.prepareStatement(Obj.SqlText);
            
            Obj.ResultSet = Obj.Statement.executeQuery();
            
        end 
        
        
        function Result = next(Obj)
            try
                Obj.ResultSet.next();
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.next failed');
            end                
        end
        
        
        function Result = prev(Obj)
            try
                Obj.ResultSet.previous();
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.prev failed');
            end                
        end        
        
            
        function Result = getField(Obj, FieldName)
            % Get string field
            
            if Obj.isField(FieldName)
                try 
                    Result = Obj.ResultSet.getString(FieldName);
                catch
                    Obj.msgLog(LogLevel.Error, 'getString failed: %s', FieldName);
                end
            end
        end
        
        
        function Result = isField(Obj, FieldName)        
            % Check if field exists
            
            if isempty(Obj.ResultSet)
                Obj.msgLog(LogLevel.Error, 'Query is not open (ResultSet is empty)');
                Result = '';
            else
                try 
                    Result = Obj.ResultSet.getString(FieldName);
                catch
                    Obj.msgLog(LogLevel.Error, 'Field not found: %s', FieldName);
                end
            end
        end
        
        
        function Result = newRecord(Obj)
            % Create new empty record associated with this query
            
            Result = io.db.DbQuery(Obj);
        end
        
        
        function Result = getRecord(Obj)
            % Get current record from ResultSet as DbRecord
            
            Result = DbRecord;
            
            
            
            
        end
        
        
        function Result = insertRecord(Obj, Rec)
            % Insert new record
        end
        
        
        function Result = updateRecord(Obj, Rec)
            % Update record
        end        
        function 
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgLog(LogLevel.Test, "DbQuery test started")
   
            Q = io.db.DbQuery();
            Q.open('select * from raw_images');
            
            % Test: Create database and tables
            
            % Test: Write data to tables
            
            % Test: Query tables         
            
 
            io.msgLog(LogLevel.Test, "DbQuery test passed")
            Result = true;
        end
    end    
        
    
end




function msgLog(varargin)
    %fprintf('fits: ');
    fprintf(varargin{:});
    fprintf('\n');
end

