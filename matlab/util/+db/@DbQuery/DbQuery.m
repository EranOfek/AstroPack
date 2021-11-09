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
% copyFrom - Import records from file to table Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
% copyTo - Export records from table to file Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
% createDatabase - Create database
% delete -
% exec - Execute SQL statement (that does not return data) Example: exec('INSERT ...')
% getDbVersion - Query Postgres version, result should be similar to 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
% getField - Get field value from current ResultSet, when FieldName is numeric, it is used as column index Example: Value = getField('MyFieldName')
% getFieldIndex - Get field index by field name, search in ColNames{}
% getFieldList - Get fields list of current ResultSet as celarray
% getFieldNamesOfType - Get cell array field names that match the specified field type
% getFieldType - Get field type
% getMetadata - Get metadata of the specified table or the current result-set
% getTableFieldList - Get fields list of specified table as celarray
% getValidFieldName - Convert specified table field name to valid Matlab property/struct-field name, replace non-valid chars with '_' Example: getValidFieldName('') ->
% insertCell -
% insertDbRecord - Simple insert, all arguments are char Insert new record to table, Keys and Values are celarray sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('s', d)", uuid, i).char;
% isField - Check if field exists by name
% loadResultSet - Load ResultSet to DbRecord array Might be time and memory consuming!
% makeInsertFieldsText -
% makeUpdateFieldsText - Prepare SQL text from cell array "UPDATE master_table set RecID=?,FInt=? WHERE..."
% makeWhereFieldsText - Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..."
% next - Move cursor to next record, return false if reached end of data
% openConn - Open connection, throw exception on failure
% prev - Move cursor to previous record, return false if reached end of data
% query - Run SELECT query, for other statements use exec() If no char argument is specified, use the current Obj.SqlText @Todo: Support query with params Example: Result = query('SELECT COUNT(*) from master_table')
% select - Execute SELECT Fields FROM TableName and load results to memory Obj.select('Field', 'Table', 'Where', '...', 'Order', '...')
% selectCount - Select number of records with optionally where clause
% setConnection - Set connection Connection argument may be either: - Empty string: use default connetion() - Non empty string: used as connection key for DbConnection.getDbConnection - DbConnection object
% setStatementValues - Set statement values from specified DbRecord or struct
% updateRecord - Update record
% #/functions (autogen)
%

classdef DbQuery < Component
    
    % Properties
    properties (SetAccess = public)
        
        % Connection details
        Conn            = []        % DbConnection
        
        % Current SQL statement data
        SqlText         = ''        % SQL text

        TableName       = '';       % Current table name
        PrimaryKey                  % Primary Key(s) used when TableName is not empty - char or celarray
        FieldMap        = [];       % struct
        PrimaryKeyFunc  = [];       % function(DbQuery, DbRecord, First, Last)
        InsertUseCopy   = 5000;     % Above this number of records, insert() uses copyFrom()
        
        
        % Metadata from last select
        ColCount        = 0;        % Number of columns
        ColNames        = [];       % cell
        ColType         = [];       % cell

        
        % Flags and statistics
        IsOpen          = false;    % Last result of query()
        ExecOk          = false;    % Last result of exec()
        Eof             = true;     % True when cursor reached last record of current ResultSet
        Toc             = 0;        % Time of last operation
        
        % Internals
        JavaStatement   = []        % Java object - Prepared statement object
        JavaMetadata    = []        % Java object, set by getMetadata()
        JavaResultSet   = []        % Java object, returned result-set from SELECT
    end
    
    %----------------------------------------------------------------------
    methods % Constructor
        
        % Constructor
        function Obj = DbQuery(DbTableOrConn, Args)
            % Create new DbQuery obeject
            % Input:
            %   DbTableOrConn - Database alias from Database.yml, with
            %   optional table name, for example: 'UnitTest'
            %
            % Examples:
            %   % Create query object for 'UnitTest' database alias 'UnitTest'
            %   Q = DbQuery('UnitTest')
            %
            %   % Create query object for 'UnitTest' database and table
            %   'master_table'
            %   Q = DbQuery(UnitTest:master_table')
            %
            %   % Create query object for custom database connection (not
            %   from Database.yml)
            %   MyConn = DbConnection('Db', 'MyAlias', ...)
            %   Q.DbQuery(MyConn)
            %
            arguments
                DbTableOrConn   = []        % DbAlias / DbAlias:TableName / DbConnection object
                Args.TableName              % Set TableName when not included in DbTable parameter
                Args.PrimaryKey             % Primary key(s)
            end
            
            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = true;
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
                        
            % Set connection
            Obj.setConnection(DbTableOrConn);
            
            % Override TableName and set other properties
            Obj.setProps(Args);

        end
        
        
        % Destructor
        function delete(Obj)
            Obj.clear();
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
                Fields                  % Comma-separated field names to select (i.e. 'recid,fint')
                Args.TableName = ''     % Table name, if not specified, Obj.TableName is used
                Args.Where = ''         % Where condition (excluding WHERE keyword)
                Args.Order = ''         % Order by clause  (excluding ORDER BY keyword)
                Args.Limit = -1         % Maximum number of records (LIMIT)
                Args.Load = true        % True to load entire result set
                Args.Convert = ''       % Optional conversion, otherwise DbRecord is returned: 'table', 'cell', 'mat', 'AstroTable', 'AstroCatalog'
                Args.UseCopy = false    % @Todo (not implemented yet): True to use copyTo() instead of SELECT
            end
            
            Result = [];
                       
            % Use speified TableName or Obj.TableName
            if ~isempty(Args.TableName)
                Obj.TableName = Args.TableName;
            end
            assert(~isempty(Obj.TableName));
            
            % Prepare Select
            Obj.SqlText = sprintf('SELECT %s FROM %s', Fields, Obj.TableName);
            
            % Where
            % @Todo: Support Where with external values
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

            if Args.UseCopy
                Obj.msgLog(LogLevel.Warning, 'select: UseCopy is not implemented yet, using SELECT');
            end
            
            % Run query
            Res = Obj.query();
            if Res
                if Args.Load
                    Result = Obj.loadResultSet();
                    
                    % Optional conversion
                    Conv = lower(Args.Convert);
                    if strcmp(Conv, 'table')
                        Result = Result.convert2table();
                    elseif strcmp(Conv, 'cell')
                        Result = Result.convert2cell();
                    elseif strcmp(Conv, 'mat')
                        Result = Result.convert2mat();
                    elseif strcmp(Conv, 'astrotable')
                        Result = Result.convert2AstroTable();
                    elseif strcmp(Conv, 'astrocatalog')
                        Result = Result.convert2AstroCatalog();                        
                    end
                else
                    Result = true;
                end
            end
        end
                               
        
        function Result = loadResultSet(Obj, Args)
            % Load ResultSet to DbRecord array
            % Might be time and memory consuming!
            % Input:
            %
            % Output:
            %
            %
            
            arguments
                Obj
                Args.MaxRows = Inf
                Args.OutType = 'table'      % 'table', 'cell', 'struct', 'mat', 'AstroTable', 'AstroCatalog'
            end
            
            PerfLog = io.FuncLog('loadResultSet');
            tic();
            
            % Initialize
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadResultSet, ColumnCount = %d', Obj.ColCount);
            Result = db.DbRecord;
            Result.ColCount = Obj.ColCount;
            Result.ColNames = Obj.ColNames;
            Result.ColType  = Obj.ColType;
            
            % Loop over all ResultSet rows (records)
            RowIndex = 1;
            while ~Obj.Eof
                
                % Loop over all columns in the row
                for ColIndex = 1 : Obj.ColCount
                    FieldName = Obj.ColNames{ColIndex};
                    FieldValue = Obj.getField(ColIndex);
                    Result.Data(RowIndex).(FieldName) = FieldValue;
                end
                
                % Move to next record
                if ~Obj.next()
                    break
                end
                
                % Stop on specified limit
                if RowIndex > Args.MaxRows
                    break
                end
                
                RowIndex = RowIndex + 1;
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadResultSet, RowCount = %d, Time: %.6f', RowIndex, Obj.Toc);
        end
                                            
    end % Select
    
    
    %----------------------------------------------------------------------
    methods % High-level: Insert
        
        function Result = insert(Obj, Rec, Args)
            % Simple insert, all arguments are char
            % Insert new record to table, Keys and Values are celarray
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, i).char;
            arguments
                Obj
                Rec                         %
                Args.TableName = ''         % Table name, if not specified, Obj.TableName is used
                Args.FieldNames = []        % Comma-separated field names (i.e. 'recid,fint')
                Args.FieldMap = []          % Optional field map
                Args.ExFields struct = []   % Additional fields as struct
                Args.BatchSize = 1          % Number of records per commit operation
                Args.PrimaryKeyFunc = []    % Called to generate primary key if required
                Args.UseCopy = 0            % When number of records is above this value, copyFrom() is used
            end
            
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Debug, 'DbQuery: insert');
            Result = false;
            
            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
                            
            % Convert from input type
            if ~isa(Rec, 'db.DbRecord')
                Rec = db.DbRecord(Rec);
            end

            % Generate primary keys using user function
            if isempty(Args.PrimaryKeyFunc)
                Args.PrimaryKeyFunc = Obj.PrimaryKeyFunc;
            end
            if ~isempty(Args.PrimaryKeyFunc)
                Args.PrimaryKeyFunc(Obj, Rec, 1, numel(Rec.Data));
            end
            
            if isempty(Args.UseCopy)
                Args.UseCopy = Obj.InsertUseCopy;
            end
            
            % Need connection, clear current query
            if ~Obj.openConn()
                return;
            end

            %
            RecordCount = numel(Rec.Data);            
            
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            FieldNames = fieldnames(Rec.Data);
            [SqlFields, SqlValues] = Obj.makeInsertFieldsText(FieldNames, 'FieldMap', Args.FieldMap);
            
             if Args.UseCopy > 0 && RecordCount > Args.UseCopy
                 CsvFileName = sprintf('%s.csv', tempname);
                 Obj.msgLog(LogLevel.Warning, 'insert: UseCopy is not implemented yet, using INSERT');
 
                 %
                 %Rec.writeCsv(CsvFileName, Rec.Data, 'Header', Rec.Data);

                 %
                 %Result = Obj.copyFrom(Args.TableName, FileName);
                 %return;
             end
           
            
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            T1 = tic();

            %
            RecSqlText = ['INSERT INTO ', string(Args.TableName).char, ' (', SqlFields, ') VALUES (', SqlValues, ');'];
            Obj.msgLog(LogLevel.Debug, 'insert: SqlText: %s', RecSqlText);

            % Iterate struct array and insert in batchs
            RecIndex = 1;
            LastBatchSize = 0;
            BatchNum = 0;
            while RecordCount > 0

                % Update counters
                if RecordCount > Args.BatchSize
                    BatchSize = Args.BatchSize;
                else
                    BatchSize = RecordCount;
                end
                RecordCount = RecordCount - BatchSize;
                BatchNum = BatchNum + 1;

                % Prepare INSERTs for each line in the batch
                if BatchSize ~= LastBatchSize
                    Obj.SqlText = repmat(RecSqlText, 1, BatchSize);
                    LastBatchSize = BatchSize;
                end

                % Clear current statement
                if ~isempty(Obj.JavaStatement)
                    Obj.JavaStatement.close();
                    Obj.JavaStatement = [];
                end

                try
                    Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
                catch
                    Obj.msgLog(LogLevel.Error, 'insert: prepareStatement failed: %s', Obj.SqlText);
                end

                % Iterate struct fields
                T2 = tic();
                Obj.setStatementValues(Rec, RecIndex, BatchSize);  %Args.FieldMap, 'FirstIndex', FieldIndex);
                RecIndex = RecIndex + BatchSize;
                Toc2 = toc(T2);
                
                Obj.msgLog(LogLevel.Debug, 'insert (%d) prepare time: %f, BatchCount: %d', BatchNum, Toc2, BatchSize);

                % Execute
                % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
                T3 = tic();
                try
                    Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                    Obj.ExecOk = true;
                catch
                    Obj.msgLog(LogLevel.Error, 'insert: executeQuery failed: %s', Obj.SqlText);
                    Obj.ExecOk = false;
                    break;
                end
                Toc3 = toc(T3);
                Obj.msgLog(LogLevel.Debug, 'insert (%d) executeUpdate time: %f, BatchCount: %d', BatchNum, Toc3, BatchSize);
            end
            
            Obj.Toc = toc(T1);
            Obj.msgLog(LogLevel.Debug, 'insert time: %f', Obj.Toc);
            Result = Obj.ExecOk;
        end
                            
    end
    
    
    methods % High-level: Update, Delete
        
        function Result = update(Obj, Rec, Args)
            % Update record
            arguments
                Obj
                Rec                     % DbRecord or struct
                Args.TableName
                Args.Where
                Args.FieldMap = []      % Optional field map
            end
            
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;
%
%             % Execute SQL statement (using java calls)
%             Obj.msgLog(LogLevel.Info, 'DbQuery: updateRecord');
%             tic();
%
%             % Use speified TableName or Obj.TableName
%             if isempty(Args.TableName)
%                 Args.TableName = Obj.TableName;
%             end
%             assert(~isempty(Args.TableName));
%
%
%             % Need connection, clear current query
%             Obj.openConn();
%             Obj.clear();
%
%             % Prepare SQL statement
%             % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
%             FieldNames = Obj.getFieldNames(Rec);
%             WhereFieldNames = Obj.getFieldNames(WhereRec);
%             disp(FieldNames);
%             SqlFields = Obj.makeUpdateFieldsText(FieldNames, Args.FieldMap);
%             WhereFields = Obj.getFieldNames(WhereRec);
%             Where = Obj.makeWhereFieldsText(WhereFieldNames, ' AND ', Args.FieldMap);
%
%             %
%             Obj.SqlText = ['UPDATE ', string(Args.TableName).char, ' SET ', SqlFields, ' WHERE ', Where];
%             Obj.msgLog(LogLevel.Debug, 'updateRecord: SqlText: %s', Obj.SqlText);
%
%             % Prepare query
%             Obj.msgLog(LogLevel.Debug, 'updateRecord: %s', Obj.SqlText);
%             try
%                 Obj.JavaStatement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
%             catch
%                 Obj.msgLog(LogLevel.Error, 'updateRecord: prepareStatement failed: %s', Obj.SqlText);
%             end
%
%             % Iterate struct fields
%             Obj.setStatementValues(FieldNames, Rec, Args.FieldMap);
%             Obj.setStatementValues(WhereFieldNames, WhereRec, Args.FieldMap, 'FieldIndex', numel(FieldNames)+1);
%
%             % Execute
%             % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
%             try
%                 Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
%                 Obj.ExecOk = true;
%                 Result = true;
%             catch
%                 Obj.msgLog(LogLevel.Error, 'updateRecord: executeQuery failed: %s', Obj.SqlText);
%             end
%
%             Obj.Toc = toc();
%             Obj.msgLog(LogLevel.Info, 'updateRecord time: %.6f', Obj.Toc);
%
%             Result = true;
        end
   

        function Result = deleteRecord(Obj, Args)
            % Delete record by fields specified in Rec
            % Note that we cannot use 'delete' as function name because it
            % is a reserved keyword.
            arguments
                Obj
                Args.TableName = '';    %
                Args.Where = '';        % Exact where clause
                Args.Rec                % DbRecord or struct
            end

            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;

            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Info, 'DbQuery: deleteRecord');
            tic();

            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();

            % Prepare SQL statement
            % sql = sprintf("DELETE FROM master_table WHERE key1=... and key2=...).char;
            if ~isempty(Args.Where)
            else
            end
            
            FieldNames = Obj.getFieldNames(Rec);
            FieldMap = struct;
            Where = Obj.makeWhereFieldsText(FieldNames, 'AND', FieldMap);

            %
            Obj.SqlText = ['DELETE FROM ', string(TableName).char, ' WHERE ', Where];
            Obj.msgLog(LogLevel.Debug, 'deleteRecord: SqlText: %s', Obj.SqlText);

            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'deleteRecord: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'deleteRecord: prepareStatement failed: %s', Obj.SqlText);
            end

            % Iterate struct fields
            Obj.setStatementValues(FieldNames, Rec, FieldMap);

            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
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
            Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', Args.TableName);
            
            % Where
            if ~isempty(Args.Where)
                Obj.SqlText = [Obj.SqlText, ' WHERE ', Args.Where];
            end
            
            % Run query and return result
            Obj.query();
            Result = Obj.getField('count');
        end

        
        function Result = createDatabase(Obj, DbName, Args)
            % Create database
            arguments
                Obj
                DbName              %
                Args.Fields = []    %
            end
            
        end
        
        
        function Result = copyFrom(Obj, TableName, FileName, Args)
            % Import records from file to table
            % Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html
            % https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
            arguments
                Obj
                TableName           % Table name
                FileName            % Input file name (CSV)
                Args.Fields = ''    % Fields list, default is to use CSV headers as field names and import all fields
                Args.Csv = true     % true=Csv file,
            end
                                                          
            Obj.msgLog(LogLevel.Debug, 'DbQuery: copyFrom');
            
            AFields = '';
            if ~isempty(Args.Fields)
                AFields = [' (', Args.Fields, ') '];
            end
            
            % Prepare SQL:
            % COPY tablename field1, field2 FROM 'filename' DELIMITER ',' CSV HEADER
            ASql = ['COPY ', string(TableName).char, string(AFields).char, ' FROM ''', string(FileName).char, ''' DELIMITER '','' CSV HEADER;'];

            % Execute
            Result = Obj.exec(ASql);
            Obj.msgLog(LogLevel.Debug, 'copyFrom time: %f', Obj.Toc);
        end
        
        
        function Result = copyTo(Obj, TableName, FileName, Args)
            % Export records from table to file
            % Copy statement, see https://www.postgresql.org/docs/9.2/sql-copy.html
            % https://www.postgresqltutorial.com/export-postgresql-table-to-csv-file/
            arguments
                Obj
                TableName           % Table name
                FileName            % Output file name
                Args.Fields = ''    % Fields list, if empty all fileds are exported
                Args.Csv = true     % true to use CSV format
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
              
    end
    
    %======================================================================
    %                      Low-Level & Internal Functions
    %======================================================================
    
    methods % Low-level: open, close, query, exec
                       
        function Result = setConnection(Obj, DbTableOrConn)
            % Set connection
            % Connection argument may be either:
            %   - Empty string: use default connetion()
            %   - Non empty string: used as connection key for DbConnection.getDbConnection
            %   - DbConnection object
            %
            
            Result = true;
            if isempty(DbTableOrConn)
                %Obj.Conn = db.DbConnection.getDbConnection('');
                
            elseif isa(DbTableOrConn, 'char')
                [Split, Delimiter] = split(DbTableOrConn, ':');
                if numel(Split) >= 1
                    Obj.Conn = db.DbConnection.getDbConnection(Split{1});                    
                end
                
                if numel(Split) >= 2
                    Obj.TableName = Split{2};
                end
                
            elseif isa(DbTableOrConn, 'db.DbConnection')
                Obj.Conn = DbTableOrConn;
            else
                Result = false;
                error('setConnection: Unknown type');
            end
        end

        
        function Result = openConn(Obj)
            % Open connection, throw exception on failure
            Result = false;
            if isempty(Obj.Conn)
                error('DbQuery.query: No connection');
            end
            
            if ~Obj.Conn.IsOpen
                if ~Obj.Conn.open()
                    error('DbQuery.openConn: Open connection failed');
                end
            end
            Result = Obj.Conn.IsOpen;
        end

                
        function Result = close(Obj)
            % Close current query
            Result = Obj.clear();
        end

        
        function Result = query(Obj, varargin)
            % Run SELECT query, for other statements use exec()
            % If no char argument is specified, use the current Obj.SqlText
            % @Todo: Support query with params
            % Example:
            %   Result = query('SELECT COUNT(*) from master_table')
            
            % Run SELECT statement (using java calls)
            % Obj.msgLog(LogLevel.Debug, 'query');
            Result = false;
            tic();
            
            % Need connection
            if ~Obj.openConn()
                return;
            end
            
            % Clear current query and metadata
            Obj.clear();
            
            % Set SQL text
            if numel(varargin) == 1
                Obj.SqlText = varargin{1};
            end
        
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'query: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'query: prepareStatement failed: %s', Obj.SqlText);
            end
            
            % executeQuery
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeQuery();
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
            
            %Obj.msgLog(LogLevel.Debug, 'exec');
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
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
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
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                Obj.ExecOk = true;
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'exec: executeQuery failed: %s', Obj.SqlText);
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Debug, 'exec time: %.6f', Obj.Toc);
        end
               
    end
    %======================================================================
    %
    %======================================================================
    methods % Low-level: Record, Fields
        
        function Result = next(Obj)
            % Move cursor to next record, return false if reached end of data
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.JavaResultSet.next();
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
                Obj.Eof = ~Obj.JavaResultSet.previous();
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
                            Result = Obj.JavaResultSet.getDouble(ColIndex);
                        case { 'Long', 'Integer', 'Short', 'BigDecimal' }
                            Result = double(Obj.JavaResultSet.getDouble(ColIndex));
                        case 'Boolean'
                            Result = logical(Obj.JavaResultSet.getBoolean(ColIndex));
                        case 'String'
                            Result = char(Obj.JavaResultSet.getString(ColIndex));
                        otherwise % case { 'Date', 'Time', 'Timestamp' }
                            Result = char(Obj.JavaResultSet.getString(ColIndex));
                    end
                    if Obj.DebugMode
                        %Obj.msgLog(LogLevel.Debug, 'getField %s = %s', string(FieldName).char, string(Result).char);
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
            if isempty(Obj.JavaResultSet)
                Obj.msgLog(LogLevel.Error, 'Query is not open (ResultSet is empty)');
                Result = '';
            else
                try
                    if ~isempty(Obj.JavaMetadata)
                        Index = getFieldIndex(FieldName);
                        Result = (Index > 0);
                    else
                        Result = Obj.JavaResultSet.getString(FieldName);
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
            % @Todo: Check how to get it without select, or what we do when
            % the table is empty?
            Text = ['SELECT * from ', TableName, ' LIMIT 1'];
            Obj.query(Text);
            
            % @Todo This still does not work
            % Obj.getMetadata(TableName);
            
            % Loop over all columns in the row
            Result = Obj.ColNames;
        end

    end
    %======================================================================
    %                         Internal Functions
    %======================================================================
    
    methods
        
        function Result = clear(Obj)
            % Clear current statement and ResultSet
            
            Obj.clearResultSet();
            if ~isempty(Obj.JavaResultSet)
                Obj.JavaResultSet.close();
                Obj.IsOpen = false;
            end
            
            if ~isempty(Obj.JavaStatement)
                Obj.JavaStatement.close();
                Obj.JavaStatement = [];
                Obj.IsOpen = false;
            end
                                           
            Obj.ExecOk = false;
            Result = ~Obj.IsOpen;
        end
        

        function Result = clearResultSet(Obj)
            % Clear current ResultSet and related data
            Obj.JavaResultSet = [];
            Obj.JavaMetadata = [];
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
                    Obj.JavaMetadata = Obj.Conn.Conn.getMetaData();
                    null = libpointer;
                    Obj.JavaResultSet = Obj.JavaMetadata.getColumns(null, null, Args.TableName, null);
                    
                % Get metadata of current statement
                else
                    Obj.JavaMetadata = Obj.JavaStatement.getMetaData();
                end
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
            end

            try
                % http://docs.oracle.com/javase/7/docs/api/java/sql/Types.html
                Obj.ColCount = Obj.JavaMetadata.getColumnCount();
                Obj.msgLog(LogLevel.Debug, 'DbQuery.getMetadata: ColumnCount = %d', Obj.ColCount);
                %data = cell(0, Obj.ColCount);
                for ColIndex = Obj.ColCount : -1 : 1
                    Obj.ColNames{ColIndex} = char(Obj.JavaMetadata.getColumnLabel(ColIndex));
                    Obj.ColType{ColIndex}  = char(Obj.JavaMetadata.getColumnClassName(ColIndex));
                end
                
                % Remove 'java.lang.' from field types, leave 'Double' etc.
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
                    
        
        function SqlFields = makeUpdateFieldsText(Obj, FieldNames, Args)
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
            
        
        function Result = setStatementValues(Obj, Rec, FirstRecord, RecordCount, Args)
            % Set statement values from specified DbRecord or struct
            arguments
                Obj                     %
                Rec                     % DbRecord
                FirstRecord
                RecordCount
                Args.FieldNames = [];   % cell
                Args.FieldMap = []      % Optional
                Args.StartIndex = 1     % Index of field in current JavaStatement
            end
            
            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            %Obj.msgLog(LogLevel.DebugEx, 'setStatementValues: setting values');
            % int8, uint8, int16, uint16, int32, uint32 -> setInt()
            % int64, uint64 -> setLong()
            % logical -> setBoolean()
            % float, single, double -> setDouble()
            % char -> setString()
            
            if isempty(Args.FieldNames)
                Args.FieldNames = fieldnames(Rec.Data);
            end
            
            Index = Args.StartIndex;
            for DataIndex = FirstRecord:FirstRecord+RecordCount-1

                for FieldNo = 1:numel(Args.FieldNames)
                    FieldName = Args.FieldNames{FieldNo};

                    % Get value
                    Value = Rec.Data(DataIndex).(FieldName);

                    if isa(Value, 'int8') || isa(Value, 'uint8') || ...
                       isa(Value, 'int16') || isa(Value, 'uint16') || ...
                       isa(Value, 'int32') || isa(Value, 'uint32')
                        %Obj.msgLog(LogLevel.DebugEx, 'integer: %s = %d', f, Value);
                        Obj.JavaStatement.setInt(Index, Value);
                    elseif isa(Value, 'int64') || isa(Value, 'uint64')
                        %Obj.msgLog(LogLevel.DebugEx, 'int64: %s = %d', f, Value);
                        Obj.JavaStatement.setLong(Index, Value);
                    elseif isa(Value, 'logical')
                        %Obj.msgLog(LogLevel.DebugEx, 'bool: %s = %d', f, Value);
                        Obj.JavaStatement.setBoolean(Index, Value);
                    elseif isa(Value, 'float') || isa(Value, 'single') || isa(Value, 'double')
                        %Obj.msgLog(LogLevel.DebugEx, 'double: %s = %f', f, Value);
                        Obj.JavaStatement.setDouble(Index, Value);
                    elseif isa(Value, 'char')
                        %Obj.msgLog(LogLevel.DebugEx, 'char: %s = %s', f, Value);
                        Obj.JavaStatement.setString(Index, Value);
                    else
                        % Other not supported (yet?)
                        Obj.msgLog(LogLevel.Warn, 'setStatementValues: ERROR: other type - not supported: %s', FieldName);
                    end
                    Index = Index+1;
                end
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

        
        function Result = getDbVersion(Obj)
            % Query Postgres version, result should be similar to
            % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
            Result = [];
            Obj.query('SELECT version()');
            if Obj.ColCount == 1
                Result = Obj.getField('version');
            end
            Valid = contains(Result, 'PostgreSQL') && contains(Result, 'build');
            if ~Valid
                Obj.msgLog(LogLevel.Warning, 'getDbVersion: Invalid result: %s', Result);
            end
        end
        
    end
    
    
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
