% DbQuery - SQL Database query
%
% This class provides SQL functionality, currently tested with PostgreSQL v13.
%
% Functionality
%
% Related Classes:
%   DbDriver - Internally used to load Java package for Postgres
%   DbConnection - Used to connect to specific database on local or remote
%   server.
%   DbRecord - Class with dynamic properties, used as struct to store
%   values of database record.
%
% References:
%   https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%
% Unit-Test:
%   Use unittest__tables from GDrive to test
%

% #functions (autogen)
% DbQuery - Create new DbQuery obeject
% clear - Clear current statement and ResultSet
% clearResultSet - Clear current ResultSet and related data
% close - Close current query
% copyFrom - Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
% copyTo - Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
% createDatabase - Create database
% delete -
% deleteRecord - Delete record by fields specified in Rec
% exec - Execute SQL statement (that does not return data) Example: exec('INSERT ...')
% getField - Get field value from current ResultSet, when FieldName is numeric, it is used as column index Example: Value = getField('MyFieldName')
% getFieldIndex - Get field index by field name, search in ColNames{}
% getFieldList - Get fields list of current ResultSet as celarray
% getFieldNames -
% getFieldNamesOfType -
% getFieldTable - Get fields as empty table
% getFieldType - Get field type
% getMetadata - Get metadata of current result-set or specified table
% getRecord - Get current record from ResultSet as DbRecord NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)
% getStruct - Get current record from ResultSet as struct NOTE: Field names are loaded in lower-case (because Postgres creates field names lower-cased)
% getTableFieldList - Get fields list of specified table as celarray
% getValidFieldName -
% insert - Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;
% insertCell -
% insertRecord - Insert DbRecord or struct fields to specified table Todo: support struct array
% isField - Check if field exists by name
% loadAll - Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)
% loadTable - Load entire ResultSet to memory, might be time/memory consuming! @Todo?: add arg max row @Todo: load to Table (instead?)
% makeInsertFieldsText - Prepare SQL text from cell array "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"
% makeUpdateFieldsText - Prepare SQL text from cell array "UPDATE master_table set RecID=?,FInt=? WHERE..."
% makeWhereFieldsText - Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..."
% newRecord - Create new empty record associated with this query
% next - Move cursor to next record, return false if reached end of data
% openConn - Open connection, throw exception on failure
% prev - Move cursor to previous record, return false if reached end of data
% query - Run SELECT query, for other statements use exec() @Todo: Replace varargin with arguments block? Example: Result = query('SELECT COUNT(*) from master_table')
% select - Execute: SELECT Fields FROM TableName Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')
% selectCount - Select number of records with optionally where clause
% selectWhere - SELECT the specified fields from table, using where clause
% setStatementValues - Set statement values from specified DbRecord or struct
% updateRecord - Update record
% #/functions (autogen)
%

classdef DbQuery < Component
    
    % Properties
    properties (SetAccess = public)
        
        % Connection details
        ConnectionStr   = ''        %
        Conn            = []        % DbConnection
        ConnName        = ''        % Key to ConnectionList 
        
        % Current SQL statement data
        SqlText         = ''        % SQL text

        TableName       = '';       %        
        PrimaryKey                  % char or celarray
        
        % Separate props to 
        

        Record          = []        % DbRecord - Current record

        % Metadata
        ColCount        = 0;        % Number of columns
        ColNames        = [];       % cell
        ColType         = [];       % cell
        
        % Flags and statistics
        IsOpen          = false;    % Last result of query()
        ExecOk          = false;    % Last result of exec()
        Eof             = true;     % True when cursor reached last record of current ResultSet
        Toc             = 0;        % Time of last operation

        
        % Internals
        Statement       = []        % Java object - Prepared statement object
        Metadata        = []        % Java object, set by getMetadata()
        ResultSet       = []        % Java object, returned result-set from SELECT
    end
    
    %----------------------------------------------------------------------
    methods % Constructor
        
        % Constructor
        function Obj = DbQuery(Args)
            % Create new DbQuery obeject
            arguments
                Args.Connection         %
                Args.TableName          %
                Args.PrimaryKey         %
            end
            
            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = true;
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
            
            % Connection: Default/char/DbConnection
            if isempty(Args.Connection)
                Obj.Conn = db.DbConnection.getDbConnection('');
            elseif isa(Args.Connection, char)
                Obj.Conn = db.DbConnection.getDbConnection(Args.Connection);
            elseif isa(Args.Connection, db.DbConnection)
                Obj.Conn = Args.Connection;
            else
                error('Unknown type for Args.Connection');
            end                
        end
        
        
        % Destructor
        function delete(Obj)
            Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end
    
    %----------------------------------------------------------------------    
    methods % High-level: Select
        
        function Result = select(Obj, Fields, Args)
            % Execute SELECT Fields FROM TableName and load results to memory
            % Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')
            arguments
                Obj                     %
                Fields                  %
                Args.TableName = ''     %
                Args.Where = ''         %
                Args.Order = ''         %
                Args.Limit = -1         %
            end
            
            Result = [];
            Res = Obj.runSelect(Fields, Args);
            if Res
                Result = Obj.loadResultSet();
            end
        end
                
        
        function Result = runSelect(Obj, Fields, Args)
            % Execute: SELECT Fields FROM TableName
            % Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')
            arguments
                Obj                     %
                Fields                  %
                Args.TableName = ''     %
                Args.Where = ''         %
                Args.Order = ''         %
                Args.Limit = -1         %
            end
            
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            % Prepare Select
            Obj.SqlText = sprintf('SELECT %s FROM %s', Fields, Args.TableName);
            
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
            
            % Run query
            Obj.query();
        end
        
        
        function Result = selectCount(Obj, Args)
            % Select number of records with optionally where clause
            arguments
                Obj                     %
                Args.TableName = ''     %
                Args.Where = ''         %
            end
            
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            % Prepare Select
            Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', Fields, Args.TableName);
            
            % Where
            if ~isempty(Args.Where)
                Obj.SqlText = [Obj.SqlText, ' WHERE ', Args.Where];
            end                      
            
            % Run query and return result
            Obj.query();
            Result = Obj.getField('count');
        end
       
        
        function Result = loadResultSet(Obj, Args)
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
    
    
    %----------------------------------------------------------------------    
    methods % High-level: Insert, Update, Delete
              
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
            Obj.msgLog(LogLevel.Debug, 'DbQuery: insertRecord');
            T1 = tic();
            
            % Need connection, clear current query
            Obj.openConn();
            
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            FieldNames = Obj.getFieldNames(Rec);
            FieldNamesCount = numel(FieldNames);
            [SqlFields, SqlValues] = Obj.makeInsertFieldsText(FieldNames, Args.FieldMap);

            %
            RecSqlText = ['INSERT INTO ', string(TableName).char, ' (', SqlFields, ') VALUES (', SqlValues, ');'];
            Obj.msgLog(LogLevel.Debug, 'insertRecord: SqlText: %s', RecSqlText);
                
            % Iterate struct array
            RecordCount = numel(Rec);
            RecIndex = 1;
            LastBatchCount = 0;
            BatchNum = 0;
            while RecordCount > 0
                 
                if RecordCount > Args.BatchSize
                    BatchCount = Args.BatchSize;
                else
                    BatchCount = RecordCount;
                end
                RecordCount = RecordCount - BatchCount;
                BatchNum = BatchNum + 1;
                
                Obj.clear();
                
                if BatchCount ~= LastBatchCount
                    Obj.SqlText = repmat(RecSqlText, 1, BatchCount);
                    LastBatchCount = BatchCount;
                end
               
                % Prepare query
                %Obj.msgLog(LogLevel.Debug, 'insertRecord: %s', Obj.SqlText);
                try
                    Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
                catch
                    Obj.msgLog(LogLevel.Error, 'insertRecord: prepareStatement failed: %s', Obj.SqlText);
                end

                % Iterate struct fields
                T2 = tic();
                FieldIndex = 1;
                for i = 1:BatchCount
                    Obj.setStatementValues(FieldNames, Rec(RecIndex), Args.FieldMap, 'FieldIndex', FieldIndex);
                    RecIndex = RecIndex + 1;
                    FieldIndex = FieldIndex + FieldNamesCount;
                end
                Toc2 = toc(T2);
                Obj.msgLog(LogLevel.Debug, 'insertRecord (%d) prepare time: %f, BatchCount: %d', BatchNum, Toc2, BatchCount);
            
                % Execute
                % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
                T3 = tic();
                try
                    Obj.ResultSet = Obj.Statement.executeUpdate();
                    Obj.ExecOk = true;
                catch
                    Obj.msgLog(LogLevel.Error, 'insertRecord: executeQuery failed: %s', Obj.SqlText);
                    Obj.ExecOk = false;
                    break;
                end
                Toc3 = toc(T3);
                Obj.msgLog(LogLevel.Debug, 'insertRecord (%d) executeUpdate time: %f, BatchCount: %d', BatchNum, Toc3, BatchCount);
            end
          
            Obj.Toc = toc(T1);
            Obj.msgLog(LogLevel.Debug, 'insertRecord time: %f', Obj.Toc);
            Result = Obj.ExecOk;
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
    methods % High-level: 
    end
    
    
    %----------------------------------------------------------------------    
    methods % High-level: Utilities
        
        
        function Result = createDatabase(Obj, DbName, Args)
            % Create database
            arguments
                Obj
                DbName              %
                Args.Fields = []    %
            end
            
        end
        
        
        function Result = copyTo(Obj, TableName, FileName, Args)
            % Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html
            % https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
            arguments
                Obj
                TableName           %
                FileName            %
                Args.Fields = ''    %
                Args.Csv = true     %
            end
                                                          
            Obj.msgLog(LogLevel.Debug, 'DbQuery: copyTo');
            
            AFields = '';
            if ~isempty(Args.Fields)
                AFields = [' (', Args.Fields, ') '];
            end
            
            % Prepare SQL
            ASql = ['COPY ', string(TableName).char, string(AFields).char, ' TO ''', string(FileName).char, ''' DELIMITER '','' CSV HEADER'];
            
            Result = Obj.exec(ASql);
            Obj.msgLog(LogLevel.Debug, 'copyTo time: %f', Obj.Toc);
            Result = Obj.ExecOk;
        end
        
        
        function Result = copyFrom(Obj, TableName, FileName, Args)
            % Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html
            % https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
            arguments
                Obj
                TableName           %
                FileName            %
                Args.Fields = ''    %
                Args.Csv = true     %
            end
                                                          
            Obj.msgLog(LogLevel.Debug, 'DbQuery: copyFrom');
            
            AFields = '';
            if ~isempty(Args.Fields)
                AFields = [' (', Args.Fields, ') '];
            end
            
            % Prepare SQL
            ASql = ['COPY ', string(TableName).char, string(AFields).char, ' FROM ''', string(FileName).char, ''' DELIMITER '','' CSV HEADER;'];
                            
            Result = Obj.exec(ASql);
            Obj.msgLog(LogLevel.Debug, 'copyFrom time: %f', Obj.Toc);
        end
              
    end
    
    %======================================================================
    %                      Low-Level & Internal Functions
    %======================================================================
    
    methods % Low-level: open, close, query, exec
                         
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
            % Example:
            %   Result = query('SELECT COUNT(*) from master_table')
            
            % Run SELECT statement (using java calls)
            Obj.msgLog(LogLevel.Debug, 'query');
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
            Obj.msgLog(LogLevel.Debug, 'query time: %.6f', Obj.Toc);
        end
        

        function Result = exec(Obj, varargin)
            % Execute SQL statement (that does not return data)
            % Example: exec('INSERT ...')
            
            Obj.msgLog(LogLevel.Debug, 'exec');
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
            Obj.msgLog(LogLevel.Debug, 'exec time: %.6f', Obj.Toc);
        end
               
    end
    %----------------------------------------------------------------------

    methods % Low-level: Record, Fields
        
        function Result = next(Obj)
            % Move cursor to next record, return false if reached end of data
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
            % Move cursor to previous record, return false if reached end of data
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
            % Example:
            %    Value = getField('MyFieldName')
            
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
            % Check if field exists by name
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
            % Get field index by field name, search in ColNames{}
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
        
       
                
        function Result = getFieldTable(Obj)
            % Get fields as empty table
            
            Fields = Obj.getFieldList();
            Result = table(Fields);
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
        
        
        function Result = getMetadata(Obj, Args)
            % Get metadata of the specified table or the current result-set
            arguments
                Obj
                Args.TableName = '';        % Table name, if not specified 
            end
            
            % Clear metadata
            Obj.ColCount = 0;
            Obj.ColNames = {};
            Obj.ColType = {};
            Result = false;
            try
                
                % Query metadata of specified table
                if ~isempty(Args.TableName)
                    Obj.Metadata = Obj.Conn.Conn.getMetaData();                  
                    null = libpointer;
                    Obj.ResultSet = Obj.Metadata.getColumns(null, null, Args.TableName, null);
                    
                % Get metadata of current statement
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
               
        
        function [SqlFields, SqlValues] = makeInsertFieldsText(Obj, FieldNames, Args)
            arguments
                Obj
                FieldNames
                Args.FieldMap = [];     % Optional struct.FieldName = ActualFieldName;
            end
            
            % Prepare SQL text from cell array of field names
            % "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"
            SqlFields = '';
            SqlValues = '';
            
            if Obj.DebugMode
                %disp(FieldNames);
            end
            
            % Iterate struct fields
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
                
                % Optionally replace field name from mapping
                if ~isempty(Args.FieldMap) && isfield(Args.FieldMap, FieldName)
                    FieldName = Args.FieldMap.(FieldName);
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
            arguments
                Obj
                FieldNames
                Args.FieldMap = [];     % Optional struct.FieldName = ActualFieldName;
            end

            
            SqlFields = '';

            % Iterate struct fields
            disp(FieldNames);
            
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
            
                % Optionally replace field name from mapping
                if ~isempty(Args.FieldMap) && isfield(Args.FieldMap, FieldName)
                    FieldName = Args.FieldMap.(FieldName);
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
            
        
        function Result = setStatementValues(Obj, FieldNames, Rec, Args)
            % Set statement values from specified DbRecord or struct
            arguments
                Obj                     %
                FieldNames              %
                Rec                     %
                Args.FieldMap           %
                Args.FieldIndex = 1     %
            end            
            
            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            %Obj.msgLog(LogLevel.DebugEx, 'setStatementValues: setting values');
            % int8, uint8, int16, uint16, int32, uint32 -> setInt()
            % int64, uint64 -> setLong()
            % logical -> setBoolean()
            % float, single, double -> setDouble()
            % char -> setString()
            Index = Args.FieldIndex;
            for i = 1:numel(FieldNames)
                f = FieldNames{i};
                
                % Get value
                val = Rec.(f);
                
                if isa(val, 'int8') || isa(val, 'uint8') || ...
                   isa(val, 'int16') || isa(val, 'uint16') || ...
                   isa(val, 'int32') || isa(val, 'uint32')
                    %Obj.msgLog(LogLevel.DebugEx, 'integer: %s = %d', f, val);
                    Obj.Statement.setInt(Index, val);
                elseif isa(val, 'int64') || isa(val, 'uint64')
                    %Obj.msgLog(LogLevel.DebugEx, 'int64: %s = %d', f, val);
                    Obj.Statement.setLong(Index, val);
                elseif isa(val, 'logical')
                    %Obj.msgLog(LogLevel.DebugEx, 'bool: %s = %d', f, val);
                    Obj.Statement.setBoolean(Index, val);
                elseif isa(val, 'float') || isa(val, 'single') || isa(val, 'double')
                    %Obj.msgLog(LogLevel.DebugEx, 'double: %s = %f', f, val);
                    Obj.Statement.setDouble(Index, val);
                elseif isa(val, 'char')
                    %Obj.msgLog(LogLevel.DebugEx, 'char: %s = %s', f, val);
                    Obj.Statement.setString(Index, val);
                else
                    % Other not supported (yet?)
                    Obj.msgLog(LogLevel.Warn, 'setStatementValues: ERROR: other type - not supported: %s', f);
                end
                Index = Index+1;
            end
            Result = true;
        end

        
        function Result = getValidFieldName(Obj, Str)
            % Convert specified table field name to valid Matlab
            % property/struct-field name, replace non-valid chars with '_'
            % Example: getValidFieldName('') -> 
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
        
                
        function Result = getFieldNamesOfType(Obj, FieldType)
            % Get cell array field names that match the specified field type
            arguments
                Obj
                FieldType       % Specified field type
            end            
            
            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            Obj.msgLog(LogLevel.Debug, 'getFieldNamesOfType: %s', FieldType);
            
            Result = {};
            for ColIndex = 1:Obj.ColCount
                if strcmp(Obj.ColType{ColIndex}, FieldType)
                    Result{end+1} = Obj.ColNames{ColIndex};
                end
            end
        end
        
    end
    
    %======================================================================
    
    methods(Static) % Unit-Tests

        Result = unitTest()
            % Unit-Test
        
        Result = perfTest()
            % Performance Test
              
        Result = stressTest()
            % Stree Test @Todo
               
        Result = unitTestDev()
            % Development helper to test specific functinality
                            
    end
end
