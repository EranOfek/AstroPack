% DbQuery - Database query based on Java
%
% https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%--------------------------------------------------------------------------
% Use unittest__tables from GDrive to test
%

classdef DbQuery < Component
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        ConnectionStr   = ''
        Conn            = []        % DbConnection
        ConnName        = ''
        
        % Current SQL statement data
        SqlText         = ''        % SQL text
        Statement       = []        % Prepared statement object
        
        ResultSet       = []        % Returned result-set
        Record          = []        % Current record
        Metadata        = []        
        ColCount        = 0         %
        ColNames        = []        %
        ColType         = []        %
        IsOpen          = false     % Last result of query() 
        ExecOk          = false     % Last result of exec()
        Eof             = true      % True when cursor reached last record of current ResultSet
        Toc             = 0         % Time of last operation
    end
    
    %-------------------------------------------------------- 
    methods % Constructor
        
        % Constructor    
        function Obj = DbQuery(varargin)
            % Create new DbQuery obeject
            
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
            
            %
            if numel(varargin) == 1 
                
                % Connection object is specified
                if strcmp(class(varargin{1}), 'db.DbConnection')                    
                    Conn = varargin{1};
                    
                % Connection name is specified
                elseif ischar(varargin{1}) || isa(varargin{1}, 'string')
                    Obj.ConnName = varargin{1};
                    Conn = db.DbConnection.getDbConnection(Obj.ConnName);
                end                
                
            % Use default connection
            elseif numel(varargin) == 0
                Conn = db.DbConnection.getDbConnection('');
            else
                error('DbQuery: Unknown parameters');
            end

            Obj.DebugMode = true;
            Obj.Conn = Conn;
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end                        
    end
    
    
    methods % open, close, query, exec
                         
        function Result = openConn(Obj)
            % Open connection, throw exception on failure
            Result = false; 
            if isempty(Obj.Conn)
                error('DbQuery.query: No connection');
            end
            
            if ~Obj.Conn.IsOpen
                Obj.Conn.open();
                if Obj.Conn.IsOpen
                    Result = true;
                else
                    error('DbQuery.openConn: Open connection failed');
                end
            end
        end

                
        function Result = close(Obj)
            % Close current query
            Result = Obj.clear();
        end

        
        function Result = query(Obj, varargin)
            % Run SELECT query, for other statements use exec() 
            % @Todo: Replace varargin with arguments block?
            
            % Run SELECT statement (using java calls)          
            Obj.msgLog(LogLevel.Info, 'query');
            Result = false;            
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
            
            % Set SQL text
            if numel(varargin) == 1
                Obj.SqlText = varargin{1};
            end              
        
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'query: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);            
            catch
                Obj.msgLog(LogLevel.Error, 'query: prepareStatement failed: %s', Obj.SqlText);
            end
            
            % executeQuery
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.ResultSet = Obj.Statement.executeQuery();                
                Obj.IsOpen = true;                

                % Get metadata (@Todo: Make it Optional?)
                Obj.getMetadata();

                % Get first result record
                Obj.next();
                Result = true;
            catch
                Obj.IsOpen = false;
                Obj.msgLog(LogLevel.Error, 'query: executeQuery failed: %s', Obj.SqlText);                
            end           
            
            Obj.Toc = toc();            
            Obj.msgLog(LogLevel.Info, 'query time: %.6f', Obj.Toc);
        end
        

        function Result = exec(Obj, varargin)
            % Execute SQL statement (that does not return data)
            Obj.msgLog(LogLevel.Info, 'exec');            
            Result = false;
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
            
            % Set SQL text
            if numel(varargin) >= 1
                Obj.SqlText = varargin{1};
            end              
        
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'exec: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);            
            catch
                Obj.msgLog(LogLevel.Error, 'exec: prepareStatement failed: %s', Obj.SqlText);
            end
            
            % See https://www.codota.com/code/java/methods/java.sql.PreparedStatement/setBigDecimal
            if numel(varargin) >= 1
                try
                catch
                end
            end              
                        
            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.ResultSet = Obj.Statement.executeUpdate();                             
                Obj.ExecOk = true;                
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'exec: executeQuery failed: %s', Obj.SqlText);                
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Info, 'exec time: %.6f', Obj.Toc);
        end
               
    end        


    methods % Record, Fields
        
        function Result = next(Obj)
            % Move cursor to next record
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.ResultSet.next();
                Result = ~Obj.Eof;
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.next failed');
            end                
        end
        
        
        function Result = prev(Obj)
            % Move cursor to previous record
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.ResultSet.previous();
                Result = ~Obj.Eof;
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.prev failed');
            end                
        end        
        
        
        function Result = getField(Obj, FieldName)
            % Get field value from current ResultSet, when FieldName is
            % numeric, it is used as column index
            
            if isnumeric(FieldName)
                ColIndex = FieldName;
            else
                ColIndex = Obj.getFieldIndex(lower(FieldName));
            end
                
            if ColIndex > 0
                try 
                    Type = Obj.ColType{ColIndex};
                    
                    switch Type
                        case { 'Float', 'Double' }
                            Result = Obj.ResultSet.getDouble(ColIndex);
                        case { 'Long', 'Integer', 'Short', 'BigDecimal' }
                            Result = double(Obj.ResultSet.getDouble(ColIndex));
                        case 'Boolean'
                            Result = logical(Obj.ResultSet.getBoolean(ColIndex));
                        case 'String'
                            Result = char(Obj.ResultSet.getString(ColIndex));
                        otherwise % case { 'Date', 'Time', 'Timestamp' }
                            Result = char(Obj.ResultSet.getString(ColIndex));
                    end
                    if Obj.DebugMode
                        Obj.msgLog(LogLevel.Debug, 'getField %s = %s', string(FieldName).char, string(Result).char);
                    end
                    
                catch
                    Obj.msgLog(LogLevel.Error, 'getField failed: %s', string(FieldName).char);
                end
            else
                Obj.msgLog(LogLevel.Error, 'getField failed: Field not found: %s', string(FieldName).char);
            end
        end
        
        
        function Result = isField(Obj, FieldName)        
            % Check if field exists            
            if isempty(Obj.ResultSet)
                Obj.msgLog(LogLevel.Error, 'Query is not open (ResultSet is empty)');
                Result = '';
            else
                try 
                    if ~isempty(Obj.Metadata)
                        Index = getFieldIndex(FieldName);
                        Result = (Index > 0);
                    else
                        Result = Obj.ResultSet.getString(FieldName);
                        Result = true;
                    end
                catch
                    Obj.msgLog(LogLevel.Error, 'Field not found: %s', FieldName);
                end
            end
        end
        
        
        function Result = getFieldIndex(Obj, FieldName)
            % Get field index in ColNames{}            
            Result = find(strcmp(Obj.ColNames, FieldName));
        end
         
        
        function Result = getFieldType(Obj, FieldName)
            % Get field type            
            if isnumeric(FieldName)
                Index = FieldName;
            else
                Index = Obj.getFieldIndex(FieldName);
            end
                
            if Index > 0                
                Result = Obj.ColType{Index};
            else
            end
        end  
                    
        
        function Result = getFieldList(Obj)
            % Get fields list of current ResultSet as celarray
            Result = Obj.ColNames;
        end
              

        function Result = getTableFieldList(Obj, TableName)
            % Get fields list of specified table as celarray

            % Select single record from table
            % @Todo: Check how to get it without slect
            Text = ['SELECT * from ', TableName, ' LIMIT 1'];
            Obj.query(Text);
            
            % @Todo This still does not work 
            % Obj.getMetadata(TableName);
            
            % Loop over all columns in the row
            Result = Obj.ColNames;
        end        
        
        
        function Result = getFieldNames(Obj, Rec)
            if isstruct(Rec)
                Result = fieldnames(Rec);
            else %if isa(Rec, 'DbRecord')
                Result = Rec.getFieldNames();
            end
        end
        
        
        function Result = getRecord(Obj)
            % Get current record from ResultSet as DbRecord
            % NOTE: Field names are loaded in lower-case (because Postgres
            % creates field names lower-cased)
            
            % Create new record object
            Rec = db.DbRecord(Obj);
            
            % Loop over all columns in the row
            for ColIndex = 1 : Obj.ColCount                
                FieldName = Obj.ColNames{ColIndex};
                Value = Obj.getField(ColIndex);                           
                addprop(Rec, FieldName);
                Rec.(FieldName) = Value;
            end
            
            Result = Rec;        
        end
        
                        
        function Result = getStruct(Obj)
            % Get current record from ResultSet as struct
            % NOTE: Field names are loaded in lower-case (because Postgres
            % creates field names lower-cased)
            
            % Create new record object
            Rec = struct;
            
            % Loop over all columns in the row
            for ColIndex = 1 : Obj.ColCount                
                FieldName = Obj.ColNames{ColIndex};
                Value = Obj.getField(ColIndex);                           
                Rec.(FieldName) = Value;
            end
            
            Result = Rec;                    
        end
        
    end
        
    
    methods % Select
        
        function select(Obj, Fields, TableName, Args)
            % Execute: SELECT Fields FROM TableName
            
            arguments 
                Obj
                Fields
                TableName
                Args.Where = ''
                Args.Order = ''
                Args.Limit = -1
            end
            
            % Select
            Obj.SqlText = sprintf('SELECT %s FROM %s', Fields, TableName);  %.char;
            
            % Where
            if ~isempty(Args.Where)
                Obj.SqlText = [Obj.SqlText, ' WHERE ', Args.Where];
            end
            
            % Order
            if ~isempty(Args.Order)
                Obj.SqlText = [Obj.SqlText, ' ORDER BY ', Args.Order];
            end
            
            % Limit            
            if Args.Limit > -1
                Obj.SqlText = [Obj.SqlText, ' LIMIT ', string(Args.Limit).char];
            end
            
            % Open
            Obj.query();            
        end
        
        
        function Result = selectWhere(Obj, Fields, TableName, Where)
            % SELECT the specified fields from table, using where clause
            Obj.SqlText = sprintf('%s FROM %s WHERE %s', Fields, TableName, Where).char;
            Result = Obj.query();            
        end
        
        
        function Result = selectCount(Obj, TableName, varargin)
            % Select number of records with optionally where clause
            if numel(varargin) > 0
                Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s WHERE %s', TableName, varargin{1});
            else
                Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', TableName);
            end
            Obj.query();            
            Result = Obj.getField('count');
        end
       
        
        function Result = loadAll(Obj, Args)
            % Load entire ResultSet to memory, might be time/memory consuming!
            % @Todo?: add arg max row
            % @Todo: load to Table (instead?)
            % 
            
            arguments
                Obj
                Args.MaxRows = Inf
                Args.OutType = 'table'      % 'table', 'cell', 'struct', 'mat', 'AstroTable', 'AstroCatalog'
            end
            
            FL_ = io.FuncLog('loadAll');
            tic();
            
            % Initialize
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadAll, ColumnCount = %d', Obj.ColCount);
            Result = cell(0, Obj.ColCount);
            
            % Loop over all ResultSet rows (records)
            RowIndex = 1;
            while ~Obj.Eof
                
                % Loop over all columns in the row
                for ColIndex = 1 : Obj.ColCount
                    Value = Obj.getField(ColIndex);
                    Result{RowIndex, ColIndex} = Value;
                end
                
                if ~Obj.next()
                    break
                end
                
                if RowIndex > Args.MaxRows
                    break
                end
                
                RowIndex = RowIndex + 1;                
            end         
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadAll, RowCount = %d, Time: %.6f', RowIndex, Obj.Toc);
        end
        
        
        function Result = loadTable(Obj, Args)
            % Load entire ResultSet to memory, might be time/memory consuming!
            % @Todo?: add arg max row
            % @Todo: load to Table (instead?)
            
            arguments
                Obj
                Args.MaxRows = Inf
            end
            
            FL_ = io.FuncLog('loadTable');
            tic();
            
            % Initialize
            Obj.msgLog(LogLevel.Debug, 'loadTable, ColCount = %d', Obj.ColCount);
            
            % Create empty table
            Fields = Obj.ColNames;
            Result = table(Fields);
            
            % Loop over all ResultSet rows (records)
            RowIndex = 1;
            while ~Obj.Eof
                
                % Loop over all columns in the row
                Record = {};
                for ColIndex = 1 : Obj.ColCount
                    Value = Obj.getField(ColIndex);
                    Record{1, ColIndex} = Value;
                end
                
                % Append row to table
                Result = [Result;Record];
                
                if ~Obj.next()
                    break
                end
                
                if RowIndex > Args.MaxRows
                    break
                end
                
                RowIndex = RowIndex + 1;                
            end         
            
            Obj.Time = toc();
            Obj.msgLog(LogLevel.Debug, 'loadTable, RowCount = %d, Time: %.6f', RowIndex, Obj.Time);
        end
                                    
    end % Select
    
    
                   
    methods % Insert
        
        function Result = insert(Obj, TableName, Keys, Values)
            % Insert new record to table, Keys and Values are celarray
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, i).char;
                        
            SqlKeys = '';
            SqlValues = '';
            for i = 1:numel(Keys)
                SqlKeys = [SqlKeys, string(SqlKeys).char];
                SqlValues = [SqlValues, '?'];
                if i < numel(Keys)
                    SqlKeys = [SqlKeys ','];
                    SqlValues = [SqlValues ','];
                end
            end
            
            % Prepare statement
            Sql = sprintf("INSERT INTO %s (%s) VALUES (%s)", TableName, SqlKeys, SqlValues);
            Result = Obj.exec(Sql, Values);
        end
        
                
        function Result = getFieldTable(Obj)
            % Get fields as empty table
            
            Fields = Obj.getFieldList();
            Result = table(Fields);
        end
        
    end
    
    
    methods                    
                    
        function Result = newRecord(Obj)
            % Create new empty record associated with this query
            
            Result = db.DbQuery(Obj);
        end
        

        function Result = insertRecord(Obj, TableName, Rec, Args)
            % Insert DbRecord or struct fields to specified table
            % Todo: support struct array
            arguments
                Obj
                TableName
                Rec                     % DbRecord or struct
                Args.FieldMap = []      % Optional field map
                Args.BatchSize = 1      % Number of records per commit operation - @TODO
            end
            
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;
                       
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Info, 'DbQuery: insertRecord');            
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
                                 
            % Iterate struct array
            RecordCount = numel(Rec);
            for i=1:RecordCount
            
                % Prepare SQL statement
                % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
                FieldNames = Obj.getFieldNames(Rec);
                %disp(FieldNames);          
                [SqlFields, SqlValues] = Obj.makeInsertFieldsText(FieldNames, Args.FieldMap);

                % 
                Obj.SqlText = ['INSERT INTO ', string(TableName).char, ' (', SqlFields, ') VALUES (', SqlValues, ')'];
                Obj.msgLog(LogLevel.Debug, 'insertRecord: SqlText: %s', Obj.SqlText);

                % Prepare query
                Obj.msgLog(LogLevel.Debug, 'insertRecord: %s', Obj.SqlText);
                try
                    Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
                catch
                    Obj.msgLog(LogLevel.Error, 'insertRecord: prepareStatement failed: %s', Obj.SqlText);
                end

                % Iterate struct fields
                Obj.setStatementValues(FieldNames, Rec, Args.FieldMap);

                % Execute
                % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
                try
                    Obj.ResultSet = Obj.Statement.executeUpdate();
                    Obj.ExecOk = true;                
                    Result = true;
                catch
                    Obj.msgLog(LogLevel.Error, 'insertRecord: executeQuery failed: %s', Obj.SqlText);                
                end
            end
          
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Info, 'insertRecord time: %.6f', Obj.Toc);  
                  
            Result = true;
        end
                     
               
        function Result = insertCell(Obj, TableName, Cell, Args)
            
            % Insert DbRecord or struct fields to specified table
            arguments
                Obj
                TableName
                Cell cell               % Cell array, key, value pairs
                Args.FieldMap = []      % Optional field map
                Args.ExFields struct = []     % Additional fields as struct
            end

            
            % Get list of table's fields 
            FieldNames = Obj.getTableFieldList(TableName);
            CellSize = size(Cell);
            Rec = struct;
            
            % @Todo
            % Rec.recid = Component.newUuid();

            % Iterate all fields
            NumKeys = CellSize(1);
            for f = 1:NumKeys

                % Add field name and value to record
                Key = Cell{f, 1};
                if isempty(Key) || ~ischar(Key)
                    continue
                end

                Value = Cell{f, 2};

                % Convert key to valid database field name
                FieldName = Obj.getValidFieldName(Key);

                if ~strcmp(Key, FieldName)
                    Obj.msgLog(LogLevel.Debug, 'Key: %s, Field: %s', Key, FieldName);
                end

                FieldName = lower(FieldName);
                if any(strcmpi(FieldNames, FieldName))
                    if ~isfield(Rec, FieldName)
                        Rec.(FieldName) = Value;
                        Obj.msgLog(LogLevel.Debug, '%s = %s', FieldName, string(Value));
                    else
                        Obj.msgLog(LogLevel.Debug, 'field already exist: %s', FieldName);
                    end
                else
                    % Field does not exist in database
                end                               
            end
            
            % Additional fields
            ExFieldNames = fieldnames(Args.ExFields);
            for i = 1:numel(ExFieldNames)
                FieldName = ExFieldNames{i};
                Value = Args.ExFields.(FieldName);
                FieldName = lower(FieldName);
                
                if ~isfield(Rec, FieldName)                
                    Rec.(FieldName) = Value;
                else
                    Obj.msgLog(LogLevel.Debug, 'field already exist: %s', FieldName);
                end
            end

            Result = Obj.insertRecord(TableName, Rec);
        end
        
    end
    
    
    methods % Update        
        function Result = updateRecord(Obj, TableName, Rec, WhereRec, Args)
            % Update record
            arguments
                Obj
                TableName
                Rec                     % DbRecord or struct
                WhereRec
                Args.FieldMap = []      % Optional field map
            end
            
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;
                       
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Info, 'DbQuery: updateRecord');            
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
                                                      
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            FieldNames = Obj.getFieldNames(Rec);
            WhereFieldNames = Obj.getFieldNames(WhereRec);
            disp(FieldNames);          
            SqlFields = Obj.makeUpdateFieldsText(FieldNames, Args.FieldMap);
            WhereFields = Obj.getFieldNames(WhereRec);
            Where = Obj.makeWhereFieldsText(WhereFieldNames, ' AND ', Args.FieldMap);
            
            % 
            Obj.SqlText = ['UPDATE ', string(TableName).char, ' SET ', SqlFields, ' WHERE ', Where];
            Obj.msgLog(LogLevel.Debug, 'updateRecord: SqlText: %s', Obj.SqlText);
            
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'updateRecord: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'updateRecord: prepareStatement failed: %s', Obj.SqlText);
            end
                      
            % Iterate struct fields
            Obj.setStatementValues(FieldNames, Rec, Args.FieldMap);
            Obj.setStatementValues(WhereFieldNames, WhereRec, Args.FieldMap, 'FieldIndex', numel(FieldNames)+1);
             
            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.ResultSet = Obj.Statement.executeUpdate();                             
                Obj.ExecOk = true;                
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'updateRecord: executeQuery failed: %s', Obj.SqlText);                
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Info, 'updateRecord time: %.6f', Obj.Toc);  
                  
            Result = true;
        end                
    end
    
    
    methods % Delete
        function Result = deleteRecord(Obj, TableName, Rec, Args)
            % Delete record by fields specified in Rec
            arguments
                Obj
                TableName
                Rec                     % DbRecord or struct
                Args.FieldMap = []      % Optional field map
            end                        
            
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;
                       
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Info, 'DbQuery: insertRecord');            
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
                                                      
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            FieldNames = Obj.getFieldNames(Rec);
            FieldMap = struct;
            Where = Obj.makeWhereFieldsText(FieldNames, 'AND', FieldMap);
            
            % 
            Obj.SqlText = ['DELETE FROM ', string(TableName).char, ' WHERE ', Where];
            Obj.msgLog(LogLevel.Debug, 'deleteRecord: SqlText: %s', Obj.SqlText);
            
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'deleteRecord: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'deleteRecord: prepareStatement failed: %s', Obj.SqlText);
            end
                      
            % Iterate struct fields
            Obj.setStatementValues(FieldNames, Rec, FieldMap);
             
            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.ResultSet = Obj.Statement.executeUpdate();                             
                Obj.ExecOk = true;                
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'deleteRecord: executeQuery failed: %s', Obj.SqlText);                
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Info, 'deleteRecord time: %.6f', Obj.Toc);  
                  
            Result = true;            
        end
    end

    %----------------------------------------------------------------------
    methods
        
        function Result = createDatabase(Obj, DbName, Args)
            % Create database
            arguments
                Obj
                DbName              %
                Args.Fields = []    %
            end
            
        end
    end
    
    %======================================================================
    %                         Internal Functions
    %======================================================================
    
    methods
        
        function Result = clear(Obj)
            % Clear current statement and ResultSet
            Obj.clearResultSet();
            if ~isempty(Obj.ResultSet)
                Obj.ResultSet.close();
                Obj.IsOpen = false;
            end
            
            if ~isempty(Obj.Statement)
                Obj.Statement.close();
                Obj.Statement = [];
                Obj.IsOpen = false;
            end
                                           
            Obj.ExecOk = false;
            Result = ~Obj.IsOpen;
        end
        

        function Result = clearResultSet(Obj)
            % Clear current ResultSet and related data
            Obj.ResultSet = [];
            Obj.Record = [];
            Obj.Metadata = [];        
            Obj.ColCount = 0;
            Obj.ColNames  = [];
            Obj.ColType  = [];
            Result = true;
        end
        
        
        function Result = getMetadata(Obj, varargin)
            % Get metadata of current result-set or specified table
            Obj.ColCount = 0;
            Obj.ColNames = {};
            Obj.ColType = {};          
            Result = false;
            try
                if numel(varargin) == 1
                    TableName = varargin{1};
                    Obj.Metadata = Obj.Conn.Conn.getMetaData();
                    
                    null = libpointer;
                    Obj.ResultSet = Obj.Metadata.getColumns(null, null, TableName, null);                    
                else
                    Obj.Metadata = Obj.Statement.getMetaData();
                end
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
            end            

            try
                % http://docs.oracle.com/javase/7/docs/api/java/sql/Types.html
                Obj.ColCount = Obj.Metadata.getColumnCount();
                Obj.msgLog(LogLevel.Debug, 'DbQuery.getMetadata: ColumnCount = %d', Obj.ColCount);
                %data = cell(0, Obj.ColCount);
                for ColIndex = Obj.ColCount : -1 : 1
                    Obj.ColNames{ColIndex} = char(Obj.Metadata.getColumnLabel(ColIndex));
                    Obj.ColType{ColIndex}  = char(Obj.Metadata.getColumnClassName(ColIndex));  
                end
                
                % Remove 'java.lang.' frm field types, leave 'Double' etc.
                Obj.ColType = regexprep(Obj.ColType, '.*\.',''); 
                Result = true;

            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
            end            

        end
               
        
        function [SqlFields, SqlValues] = makeInsertFieldsText(Obj, FieldNames, FieldMap)
            % Prepare SQL text from cell array 
            % "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"
            SqlFields = '';
            SqlValues = '';
            
            if Obj.DebugMode
                disp(FieldNames);
            end
            
            % Iterate struct fields            
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
                
                % Optionally replace field name from mapping
                if ~isempty(FieldMap) && isfield(FieldMap, FieldName)
                    FieldName = FieldMap.(FieldName);
                end
            
                % 
                if numel(SqlFields) > 0
                    SqlFields = [SqlFields, ',', FieldName]; %#ok<AGROW>
                    SqlValues = [SqlValues, ',?']; %#ok<AGROW>
                else
                    SqlFields = [SqlFields, FieldName]; %#ok<AGROW>
                    SqlValues = [SqlValues, '?']; %#ok<AGROW>
                end
            end
            
            Obj.msgLog(LogLevel.Debug, 'makeInsertFieldsText: %s', SqlFields);
        end      
                    
        
        function SqlFields = makeUpdateFieldsText(Obj, FieldNames, FieldMap)                   
            % Prepare SQL text from cell array 
            % "UPDATE master_table set RecID=?,FInt=? WHERE..."
            SqlFields = '';

            % Iterate struct fields
            disp(FieldNames);
            
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
            
                % Optionally replace field name from mapping
                if ~isempty(FieldMap) && isfield(FieldMap, FieldName)
                    FieldName = FieldMap.(FieldName);
                end
                
                % 
                if numel(SqlFields) > 0
                    SqlFields = [SqlFields, ',' FieldName, '=?']; %#ok<AGROW>
                else
                    SqlFields = [SqlFields, FieldName, '=?']; %#ok<AGROW>
                end
            end
            
            Obj.msgLog(LogLevel.Debug, 'makeUpdateFieldsText: %s', SqlFields);
        end      
            
        
        
        function SqlFields = makeWhereFieldsText(Obj, FieldNames, Operand, FieldMap)                   
            % Prepare SQL text from cell array 
            % "WHERE RecID=? AND FInt=?..."
            arguments
                Obj
                FieldNames
                Operand         % 'AND' / 'OR'
                FieldMap
            end
            
            SqlFields = '';

            % Iterate struct fields
            disp(FieldNames);
            
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
            
                % Optionally replace field name from mapping
                if ~isempty(FieldMap) && isfield(FieldMap, FieldName)
                    FieldName = FieldMap.(FieldName);
                end
                
                % 
                if numel(SqlFields) > 0
                    SqlFields = [SqlFields, ' ', Operand, ' ', FieldName, '=?']; %#ok<AGROW>
                else
                    SqlFields = [SqlFields, FieldName, '=?']; %#ok<AGROW>
                end
            end
            
            Obj.msgLog(LogLevel.Debug, 'makeWhereFieldsText: %s', SqlFields);
        end      
            
        
        function Result = setStatementValues(Obj, FieldNames, Rec, FieldMap, Args)
            % Set statement values from specified DbRecord or struct
            arguments
                Obj
                FieldNames
                Rec
                FieldMap
                Args.FieldIndex = 1
            end
            
            
            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            Obj.msgLog(LogLevel.Debug, 'setStatementValues: setting values');
            Index = Args.FieldIndex;
            for i = 1:numel(FieldNames)
                f = FieldNames{i};
                
                % Get value
                val = Rec.(f);
                
                if isa(val, 'int8') || isa(val, 'uint8') || ...
                   isa(val, 'int16') || isa(val, 'uint16') || ...
                   isa(val, 'int32') || isa(val, 'uint32')
                    Obj.msgLog(LogLevel.Debug, 'integer: %s = %d', f, val);
                    Obj.Statement.setInt(Index, val);
                elseif isa(val, 'int64') || isa(val, 'uint64')
                    Obj.msgLog(LogLevel.Debug, 'int64: %s = %d', f, val);
                    Obj.Statement.setLong(Index, val);
                elseif isa(val, 'logical')
                    Obj.msgLog(LogLevel.Debug, 'bool: %s = %d', f, val);
                    Obj.Statement.setBoolean(Index, val);
                elseif isa(val, 'float') || isa(val, 'single') || isa(val, 'double')
                    Obj.msgLog(LogLevel.Debug, 'double: %s = %f', f, val);
                    Obj.Statement.setDouble(Index, val);
                elseif isa(val, 'char')
                    Obj.msgLog(LogLevel.Debug, 'char: %s = %s', f, val);
                    Obj.Statement.setString(Index, val);
                else
                    % Other not supported (yet?)
                    Obj.msgLog(LogLevel.Debug, 'setStatementValues: ERROR: other type - not supported: %s', f);
                end
                Index = Index+1;
            end           
            Result = true;
        end

        
        function Result = getValidFieldName(Obj, Str)
            for i=1:numel(Str)
                Ch = Str(i);
                %Valid = isletter(Ch) || isnumeric(Ch) || Ch == '_';
                Valid = isstrprop(Ch, 'alpha') || isstrprop(Ch, 'digit') || Ch == '_';                
                if ~Valid
                    Str(i) = '_';
                end
            end
            Result = Str;
        end
    end

        
    %======================================================================
    
    %======================================================================
    % Unit test
    methods(Static)
        function Result = unitTest()
            % Unit-Test
            % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
            % On Linux, use DataGrip by JetBrains 
            io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
            io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
               
            % ---------------------------------------------- Connect
            % NOTE: Database 'unittest' should exist
            
            % Create database connection
            %Conn = db.DbConnection;
            %Conn.DatabaseName = 'unittest';
            %Conn.open();

            Conn = db.Db.getUnitTest();
            
            % Query Postgres version, result should be similar to
            % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
            Q = db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
       
            
            % ---------------------------------------------- getTableFieldList 
            Q = db.DbQuery(Conn);
            Fields = Q.getTableFieldList('master_table');
            disp(Fields);
            
            % ---------------------------------------------- Select
            % NOTE: At this point, we assume that tables master_table and
            % details_table exist and are not empty
                       
            % Select two fields from table, using LIMIT
            Q = db.DbQuery(Conn);
            Q.query('SELECT count(*) FROM master_table');
            count = Q.getField('count');
            if count > 0
            
                Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
                assert(Q.ColCount == 2);
                
                % Get fields list as celarray
                fields = Q.getFieldList();
                assert(all(size(fields)) > 0);
                
                % Get fields list as empty table
                tab = Q.getFieldTable();
                assert(all(size(tab)) > 0);
                
                % Get entire record (Note: Field names are lower-case only)
                Rec = Q.getRecord();
                assert(~isempty(Rec.recid));
                assert(~isempty(Rec.fint));

                % Load entire result set
                Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
                B = Q.loadAll();
                assert(~isempty(B));

                % Load entire result set to memory
                Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
                Data = Q.loadAll();
                assert(size(Data, 2) == 2);

%                 % Load as table
%                 Q.query('SELECT RecId, FInt FROM master_table LIMIT 5');
%                 Tab = Q.loadTable();
%                 sz = size(Tab)
%                 assert(sz(1) > 1);
%                 assert(sz(2) > 1);
                
                % Select all fields from table, using LIMIT
                Q.query(['SELECT * FROM master_table LIMIT 10']);

                % Load current record to memory
                delete(Rec);
                Rec = [];
                assert(isempty(Rec));
                Rec = Q.getRecord();
                assert(~isempty(Rec));

                % Get all fields (Note: Field names are lower-case only)
                % All these fields should exist in table 'master_table'
                RecID = Rec.recid;
                assert(~isempty(Rec.recid));            
                InsertTime = Rec.inserttime;
                UpdateTime = Rec.updatetime;
                FInt = Rec.fint;
                FBigInt = Rec.fbigint;
                FBool = Rec.fbool;
                FDouble = Rec.fdouble;
                FTimestamp = Rec.ftimestamp;
                FString = Rec.fstring;

                % Try to access undefined field
                catched = false;
                try
                    temp = Rec.abcabcabcabc;
                catch
                    catched = true;
                end
                assert(catched);

                % Test select() function
                % select RecId, FInt, FBigInt from master_table where recid != ''
                Q.select('RecID, Fint', 'master_table', 'where', 'Fint > 0');
                Rec2 = Q.getRecord();
                assert(~isempty(Rec2));  
            else
                io.msgStyle(LogLevel.Test, '@warn', 'Table master_table is empty, select tests are skipped');
            end
            
            % @Todo - fails on changes of 22/06/2021 !!!
            Result = true;
            
            % ---------------------------------------------- insertRecord: struct
                                 
            % Create struct with different types of fields
            s = struct;            
            s.recid = Component.newUuid();
            Q.insertRecord('master_table', s);            
                       
            % int32
            s = struct;            
            s.recid = Component.newUuid();
            s.fint = int32(1);
            Q.insertRecord('master_table', s);            
            
            % bool
            s = struct;            
            s.recid = Component.newUuid();
            s.fbool = true;
            Q.insertRecord('master_table', s);             
            
            % bigint            
            s = struct;            
            s.recid = Component.newUuid();            
            s.fbigint = int64(3);
            Q.insertRecord('master_table', s);             
            
            % double
            s = struct;            
            s.recid = Component.newUuid();            
            s.fdouble = double(5);
            Q.insertRecord('master_table', s);                         
            
            % string
            s = struct;            
            s.recid = Component.newUuid();                        
            s.fstring = 'abcd';
            Q.insertRecord('master_table', s);             
                       
            % Insert struct with field mapping
            s = struct;            
            s.recid = Component.newUuid();
            s.fintTest = int32(1);
            map = struct;
            map.fintTest = 'fint';            
            Q.insertRecord('master_table', s, 'FieldMap', map);
            
            % ---------------------------------------------- insertRecord: DbRecord
            r = db.DbRecord;
            r.addProp('recid', Component.newUuid());
            r.addProp('fint', 3);
            Q.insertRecord('master_table', r);
            
            % ---------------------------------------------- Insert with plain SQL text
            
            % Insert records            
            io.msgLog(LogLevel.Test, 'testing INSERT...');
            InsertCount = 1;
            for i = 1:InsertCount
                % Prepare statement
                uuid = Component.newUuid();
                sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, i).char;
                Q.exec(sql);
                assert(Q.ExecOk);
            end           
            
            % Make sure all records were inserted
            Q.query('SELECT count(*) FROM master_table');
            count = Q.getField('count');
            assert(count > InsertCount);

            % Insert batch
            % See: https://www.tutorialspoint.com/how-to-generate-multiple-insert-queries-via-java
            TestBatch = false;
            if (TestBatch)
                InsertCount = 1;
                sql = '';
                for i = 1:InsertCount
                    % Prepare statement
                    uuid = Component.newUuid();
                    sql2 = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d);", uuid, i).char;
                    sql = [sql sql2];
                end           
                Q.exec(sql);
                assert(Q.ExecOk);            
            end
            
            % Test insert() function
            
            % ---------------------------------------------- Update
            
            % string
            s = struct;            
            s.recid = Component.newUuid();                        
            s.fstring = 'Original Text';
            Q.insertRecord('master_table', s);             
            
            u = struct;
            u.fstring = 'My new TEXT';
            where = struct;
            where.recid = s.recid;
            Q.updateRecord('master_table', u, where);                         
            %function Result = updateRecord(Obj, TableName, Rec, WhereRec, Args)
            
            Q.deleteRecord('master_table', where);
            
            io.msgLog(LogLevel.Test, 'testing UPDATE...');
            UpdateCount = 100;
            uuid = Component.newUuid();
            sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            Q.exec(sql);
            for i = 1:UpdateCount
                sql = sprintf("UPDATE master_table SET FInt=%d WHERE RecID='%s'", i, uuid);
                Q.exec(sql);
                
                sql = sprintf("SELECT RecID,FInt from master_table where RecID='%s'", uuid).char;
                Q.query(sql);
                val = Q.getField('FInt');
                assert(val == i);
            end            
            
            count2 = Q.selectCount('master_table');
            assert(count2 == count+1);
            
            % ---------------------------------------------- Delete
            sql = sprintf("DELETE FROM master_table WHERE RecID='%s'", uuid);
            Q.exec(sql);           
            count2 = Q.selectCount('master_table');
            assert(count2 == count);
            
            % ---------------------------------------------- Create database
            %
 
            io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
            Result = true;
        end
                
    end                
end

