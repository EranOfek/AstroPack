%--------------------------------------------------------------------------
% File:    DbRecord.m
% Class:   DbRecord
% Title:   Data container that holds struct array of database table rows.
% Author:  Chen Tishler
% Created: July 2021
%--------------------------------------------------------------------------
% Description:
%
% DbQuery - SQL Database query
%
% Basic usage:
%
%   Select and load records, automatically convert to output type:
%
%     Q = db.DbQuery('unittest:master_table');
%     Mat = Q.select('fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'OutType', 'mat', 'Limit', 100000);
%
%
%   Insert single record
%
%   Insert records with callback to add columns not in input:
%
%
%     function Result = makePK(Q, Rec, First, Last)
%        for i=First:Last
%            Rec.Data(i).recid = sprintf('PK_%s', Rec.newKey());
%        end
%        Result = true;
%     end
%
%     Mat = rand(10, 3);
%     R = db.DbRecord(Mat, 'ColNames', 'fdouble1,fdouble2,fdouble3');
%     Q.insert(R, 'InsertRecFunc', @makePK, 'BatchSize', 10000);
%
%
% PostgreSQL V14 - Installation instructions for Linux:
%
%     https://techviewleo.com/how-to-install-postgresql-database-on-ubuntu/
%
% Create database on remote server (password: 'Passw0rd')
%
%     psql -h gauss -p 5432 -U admin -W -d postgres -f unittest.sql
%
% Using COPY:
% Still need to find a solution for this:
% https://gpdb.docs.pivotal.io/6-9/admin_guide/load/topics/g-loading-data-with-copy.html
% The COPY source file must be accessible to the postgres process on the master host.
% Specify the COPY source file name relative to the data directory on the master host, or specify an absolute path.
%
%--------------------------------------------------------------------------

%#docgen
%
% Methods:
%    DbQuery - Create new DbQuery obeject Input: DbTableOrConn - Database alias from Database.yml, with optional table name, for example: 'UnitTest'
%    delete -
%    deleteRecord - Delete record by fields specified in Rec Note that we cannot use 'delete' as function name because it is a reserved keyword. Intput: Args.TableName - Args.Where -
%    exec - Execute SQL statement (that does not return data), for SELECT, use query() Input: char-array - SQL text. If not specified, Obj.SqlText is used Output: true on success Example: Obj.exec('INSERT (recid,fint) INTO master_table VALUES (''MyUuid'',1)'
%    getColumn - Get field value from current ResultSet, when ColumnName is numeric, it is used as column index Input: ColumnName Output: Column value: double/integer/char/ Example: Value = Obj.getColumn('MyFieldName')
%    getColumnIndex - Get column index by column name, search in ColNames{} Input: ColumnName - Output: DbRecord Example: Index = Obj.getColumnIndex('')
%    getColumnList - Get columns list of current ResultSet as celarray Input: - Output: Example: ColNames = Obj.getColumnList()
%    getColumnType - Get column type Input: ColumnName - Output: char - Example: -
%    getTableColumnList - Get columns list of specified table as cell array Input: TableName Output: Cell array Example: = Obj.getTableColumnList('master_table')
%    getTableIndexList - Get list of index names of specified table Input: TableName Output: Cell array Example: = Obj.getTableIndexList('master_table')
%    getTablePrimaryKey - Get primary key column names of specified table Input: TableName Output: Cell array Example: = Obj.getTablePrimaryKey('master_table')
%    getTablesList - Get columns list of specified table as cell array Input: - Output: Cell array Example: = Obj.getTablesList()
%    insert - Simple insert, all arguments are char Insert new record to table, Keys and Values are celarray Input: Rec - Data to insert 'TableName' - Table name, if not specified, Obj.TableName is used 'CsvFileName' - When non-empty, use copyFrom and insert data from this file instead of Rec
%    isColumn - Check if field exists by name Input: ColumnName Output: true if current result set Example: IsColumn = Obj.isColumn('MyFieldName')
%    loadResultSet - Helper function for select() - Load ResultSet to DbRecord array Might be time and memory consuming! Input: 'MaxRows' - Optional limit of number of rows to load to memory Output: DbRecord object: ColCount, ColNames, ColType, Data(i) Example: Obj.loadResultSet();
%    next - Move cursor to next record, return false if reached end of data Input: - Output: true on sucess Example: Obj.next()
%    prev - Move cursor to previous record, return false if reached end of data Input: - Output: true on sucess Example: Obj.prev()
%    query - Run SELECT query, do NOT load any data. For non-SELECT statements, use exec() Input: char-array - SQL text. If not specified, Obj.SqlText is used Output: true on success, use loadResultSet() to load the data Example: Obj.query('SELECT COUNT(*) FROM master_table');
%    select - Execute SELECT Columns FROM TableName and load results to memory Input: - Columns - Comma-separated field names to select (i.e. 'recid,fint') 'TableName' - Table name, if not specified, Obj.TableName is used 'Where' - Where condition (excluding WHERE keyword) 'Order' - Order by clause (excluding ORDER BY keyword)
%    selectColumn - Get column from specified table as cell array Input: SqlText - SELECT query text ColumnName - Column name to select from query result Output: Cell array Example: = Obj.selectColumn('SELECT COUNT(*) FROM master_table', 'count')
%    selectCount - Select number of records with optionally WHERE clause Intput: Args.TableName - If empty, use Obj.TableName Args.Where - Example: 'Flag == 1' Output: integer - COUNT returned by query Example: Count = Obj.selectCount()
%    update - Update record Intput: SetColumns - Note that string values must be enclosed by single ' for example: 'MyField=''MyValue''' 'TableName' - 'Where' -
%    writeResultSetToCsvFile - Write Obj.JavaResultSet returned by select() to CSV file using Java CSVWriter obejct Input: CsvFileName Output: true on sucess Example: Obj.writeResultSetToCsvFile('/tmp/test1.csv');
%
% Methods: Hidden
%    clear - Clear current statement and ResultSet Input: - Output: true on sucess Example: Obj.clear()
%    clearResultSet - Clear current ResultSet and related data Input: - Output: - Example: Obj.clearResultSet()
%    close - [Internal Use] Close current query Intput: - Output: true on sucess Example: -
%    columnName2matlab - Convert specified table column name to valid MATLAB property/ struct-field name, replace non-valid chars with '_' Input: Str - char array, i.e. 'My:Field' Output: char array, i.e. 'My_Field' Example: ColumnNames = getValidColumnName('My:Field') -> 'My_Field'
%    getColumnNamesOfType - Get cell array field names that match the specified field type Input: ColumnType: 'Integer', 'Double', etc. Output: Cell array with list of all fields Example: ColNames = Q.getColumnNamesOfType('Double');
%    getDbVersion - Query Postgres version, note that DbQuery must be linked to DbConnection Input: - Output: char-array, such as 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit' Example: Ver = DbQuery.getDbVersion()
%    getMetadata - Get metadata of the specified table or the current result-set Input: - 'TableName' - Output: true on success Example: -
%    getSharedFileName - Prepare file names for server and client Input: FileName - Output: ServerFileName - ClientFileName - Exampe: -
%    makeInsertColumnsText - Input: FieldNames - 'TableColumnList' 'FieldMap' - Output: SqlFields - SqlValues -
%    makeUpdateColumnsText - Prepare SQL text from cell array Input: ColumnNames - 'ColumnMap' - @Todo Future Output: char-array Example: -
%    makeWhereColumnsText - Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..." Input: ColumnNames - Operand - ColumnMap -
%    openConn - [Internal Use] Open connection, throw exception on failure Input: - Output: true on sucess Example: Obj.openConn()
%    setConnection - [Internal Use] Set connection Input: DbTableOrConn - Output: true on sucess Example: Obj.setConnection('unittest')
%    setStatementValues - Set statement values from specified DbRecord or struct Input: Rec - FirstRecord - RecordCount - 'ColumnNames' -
%    writeBinaryFile - Get cell array field names that match the specified field type Input: Rec - DbRecord with data Output: true on sucess Example: ColNames = Q.getColumnNamesOfType('Double'); @Todo - Implement this function in MEX
%
%#/docgen

classdef DbQuery < Component

    % Properties
    properties (SetAccess = public)

        % Connection details
        Conn            = []        % DbConnection

        % Current SQL statement data
        SqlText         = ''        % SQL text

        TableName       = '';       % Current table name
        PrimaryKey                  % Primary Key(s) used when TableName is not empty - char or celarray
        ColumnMap        = [];      % struct - @Todo
        InsertRecFunc   = [];       % function(DbQuery, DbRecord, First, Last)
        InsertBatchSize = 100;      %
        InsertUseCopyThreshold = 5000;     % Above this number of records, insert() uses copyFrom()

        % Metadata from last select
        ColCount        = 0;        % Number of columns
        ColNames        = [];       % cell
        ColType         = [];       % cell

        % Flags and statistics
        IsOpen          = false;    % Last result of query()
        ExecOk          = false;    % Last result of exec()
        Eof             = true;     % True when cursor reached last record of current ResultSet
        Toc             = 0;        % Time of last operation
        PerfLog         = false;    % True to log performance times
                
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
                Args.InsertRecFunc          %
            end

            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = true;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

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
    methods % High-level: Select/Insert/Update/Delete

        function Result = select(Obj, Columns, Args)
            % Execute SELECT Columns FROM TableName and load results to memory
            % Input:   - Columns - Comma-separated field names to select (i.e. 'recid,fint')
            %            'TableName' - Table name, if not specified, Obj.TableName is used
            %            'Where'     - Where condition (excluding WHERE keyword)
            %            'Order'     - Order by clause  (excluding ORDER BY keyword)
            %            'Limit'     - Maximum number of records (LIMIT)
            %            'Load'      - true to load entire result set
            %            'OutType'   - Optional conversion, otherwise DbRecord is
            %                           returned: 'table', 'cell', 'mat', 'AstroTable', 'AstroCatalog',
            %                           'AstroHeader'
            %            'UseCopy'   - @Todo (not implemented yet): True to use copyTo() instead of SELECT
            %            'TempName'  - @Todo
            %            'CsvFileName' - Select to specified output CSV file, instead of
            %                            result-set (using COPY TO)
            % Output:  true on sucess
            % Example: -
            % Obj.select('Column1', 'Table', 'Where', '...', 'Order', '...')
            arguments
                Obj                     %
                Columns                 % Comma-separated field names to select (i.e. 'recid,fint')
                Args.TableName = ''     % Table name, if not specified, Obj.TableName is used
                Args.Where = ''         % Where condition (excluding WHERE keyword)
                Args.Order = ''         % Order by clause  (excluding ORDER BY keyword)
                Args.Limit = -1         % Maximum number of records (LIMIT)
                Args.Load = true        % true to load entire result set
                Args.OutType = ''       % Optional conversion, otherwise DbRecord is returned: 'table', 'cell', 'mat', 'AstroTable', 'AstroCatalog'
                Args.UseCopy = false    % True to use COPY TO
                Args.UseCsv = false     % True to write resultset to CSV file and then load it
                Args.CsvFileName = ''   % Select to CSV file instead of result-set
            end

            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            assert(~isempty(Columns));

            % Prepare Select
            Obj.SqlText = sprintf('SELECT %s FROM %s', Columns, Args.TableName);

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

            %
            % Now Obj.SqlText is ready
            %
            
            %-----------------------------------------------------
            % SELECT to output CSV file (using COPY TO or CSVWriter, when shared folder is not available)
            if ~isempty(Args.CsvFileName)
                % Shared folder is available, use it and then move the result to our target
                if Obj.Conn.isSharedPathAvail()
                    [ServerFileName, ClientFileName] = Obj.getSharedFileName(Args.CsvFileName);
                    Obj.SqlText = sprintf('COPY (%s) TO ''%s'' CSV HEADER', Obj.SqlText, ServerFileName);
                    %Obj.SqlText = ['COPY (', Obj.SqlText, ') TO ''', ServerFileName, ''' CSV HEADER'];
                    Res = Obj.exec();
                    if Res
                        if ~strcmpi(ClientFileName, Args.CsvFileName)
                            if isfile(Args.CsvFileName)
                                delete(Args.CsvFileName);
                            end
                            movefile(ClientFileName, Args.CsvFileName);
                        end
                        Result = true;
                    end
                    
                % Shared folder is not avilable, use select and write the results
                else
                    Res = Obj.query();
                    if Res
                        Obj.writeResultSetToCsvFile(Args.CsvFileName);
                    end
                end
            
            %-----------------------------------------------------
            % SELECT using COPY TO, load result-set or return file name
            
            % Use COPY TO statement to temporary csv file, and read file
            elseif Args.UseCopy
                
                % Generate UUID.csv or make sure that we only take the filename part
                TempFileName = sprintf('%s.csv', Component.newUuid());
                
                % Prepare file names
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                Obj.SqlText = sprintf('COPY (%s) TO ''%s'' CSV HEADER', Obj.SqlText, ServerFileName);
                %Obj.SqlText = ['COPY (', Obj.SqlText, ') TO ''', ServerFileName, ''' CSV HEADER'];
                Res = Obj.exec();
                if Res
                    if Args.Load
                        tic();
                        Result = db.DbRecord(ClientFileName);
                        Obj.msgStyle(LogLevel.Debug, 'blue', 'DbRecord from file: RowCount = %d, Time: %.6f', numel(Result.Data), toc());
                        if ~isempty(Args.OutType)
                            Result = Result.convert2(Args.OutType);
                        end
                    else
                        Result = ClientFileName;
                    end
                else
                    Result = false;
                end
            %-----------------------------------------------------
            % SELECT to result-set, optionally load it
            else
                % Run query
                Res = Obj.query();
                if Res
                    if Args.Load
                        
                        % Load by writing temporary CSV file and constructing DbRecord
                        % from it
                        if Args.UseCsv
                            TempFileName = sprintf('%s.csv', Component.newUuid());
                            CsvFileName = fullfile(tools.os.getTempDir(), TempFileName);
                            Obj.writeResultSetToCsvFile(CsvFileName);
                            Result = db.DbRecord(CsvFileName);
                            if ~isempty(Args.OutType)
                                Result = Result.convert2(Args.OutType);
                            end
                        
                        % Load with loadResultSet()
                        else
                            Result = Obj.loadResultSet();
                            if ~isempty(Args.OutType)
                                Result = Result.convert2(Args.OutType);
                            end
                        end
                    else
                        Result = true;
                    end
                end
            end
        end
        %------------------------------------------------------------------

        function Result = insert(Obj, Rec, Args)
            % Simple insert, all arguments are char
            % Insert new record to table, Keys and Values are celarray
            % Input:   Rec             - Data to insert
            %          'TableName'     - Table name, if not specified, Obj.TableName is used
            %          'CsvFileName'   - When non-empty, use copyFrom and insert data from this file instead of Rec
            %
            %          These arguments are used only when CsvFileName is
            %          empty and Rec is used as data source
            %          'ColNames'      - Comma-separated field names (i.e. 'recid,fint')
            %          'ColumnMap'     - Optional field map
            %          'BatchSize'     - Number of records per operation
            %          'InsertRecFunc' - Called to generate primary key if required
            %          'InsertRecArgs' - Arguments to InsertRecFunc
            %          'UseCopyThreshold' - When number of records is above this value, copyFrom() is used
            %          'CsvFileName'      - Specify CSV file as source of data
            %          'BinaryFileName'   - When using COPY, name of Binary file @TODO - NOT IMPLEMENTED YET!
            % Output:  DbRecord
            % Example: -
            
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, i).char;
            arguments
                Obj
                Rec                         % db.DbRecord, struct array, table, cell array, matrix,
                                            % AstroTable, AstroCatalog, AstroHeader
                Args.TableName = ''         % Table name, if not specified, Obj.TableName is used
                Args.ColNames = []          % Comma-separated field names (i.e. 'recid,fint')
                Args.ColumnMap = []         % Optional field map
                Args.BatchSize = []         % Number of records per operation
                Args.InsertRecFunc = []     % Called to generate primary key if required
                Args.InsertRecArgs = {}     % Arguments to InsertRecFunc
                Args.UseCopyThreshold = []  % When number of records is above this value, copyFrom() is used
                Args.ColumnsOnly = false;   % When true, ignore fields that has no matching columns in the table
                Args.CsvFileName = ''       % When using COPY, name of CSV file
                Args.BinaryFileName = ''    % NOT IMPLEMENTED YET! When using COPY, name of Binary file @TODO - NOT IMPLEMENTED YET!
            end

            % Execute SQL statement (using java calls)
            %Obj.msgLog(LogLevel.Debug, 'DbQuery: insert');
            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            %
            % Insert directly from CSV file, primary key must be included
            %
            if ~isempty(Args.CsvFileName)
                
                if ~isfile(Args.CsvFileName)
                    Obj.msgLog(LogLevel.Error, 'insert: csv file not found: %s', Args.CsvFileName);
                    return;
                end
                
                if ~Obj.Conn.isSharedPathAvail()
                    Obj.msgLog(LogLevel.Error, 'insert: shared path is not available');
                    return;
                end
                
                
                % Get columns list from CSV file
                fid = fopen(Args.CsvFileName);
                AColNames = strsplit(fgetl(fid), ',');
                fclose(fid);
                Columns = strjoin(AColNames, ',');
 
                % Check 
                TempFileName = sprintf('%s.csv', Component.newUuid());
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                
                % Check if CsvFileName is already on shared folder
                [CsvPath, ~, ~] = fileparts(Args.CsvFileName);
                [ClientPath, ~, ~] = fileparts(ClientFileName);
                if strcmp(CsvPath, ClientPath)
                    [ServerFileName, ~] = Obj.getSharedFileName(Args.CsvFileName);
                else
                   % Copy CsvFileName to shared folder, only if we need to copy it                    
                    copyfile(Args.CsvFileName, ClientFileName);
                    Obj.msgLog(LogLevel.Debug, 'insert: Inserting CSV file, copied to: %s', ClientFileName);
                end

                Obj.SqlText = sprintf('COPY %s (%s) FROM ''%s'' DELIMITER '','' CSV HEADER;', Args.TableName, Columns, ServerFileName);
                Result = Obj.exec();
                
                return;
            end
            
            %---------------------------------------------------------
            % See:
            %   https://nickb.dev/blog/disecting-the-postgres-bulk-insert-and-binary-format
            %   https://www.postgresql.org/docs/14/sql-copy.htm
            %   https://stackoverflow.com/questions/758945/whats-the-fastest-way-to-do-a-bulk-insert-into-postgres?rq=1
            %   https://www.bytefish.de/blog/pgbulkinsert_bulkprocessor.html
            %   https://github.com/PgBulkInsert/PgBulkInsert
            if ~isempty(Args.BinaryFileName)
                Obj.msgLog(LogLevel.Error, 'insert: BinaryFileName option is not supported yet!!!');

                if ~isfile(Args.BinaryFileName)
                    Obj.msgLog(LogLevel.Error, 'insert: file not found: %s', Args.BinaryFileName);
                    return;
                end
                
                if ~Obj.Conn.isSharedPathAvail()
                    Obj.msgLog(LogLevel.Error, 'insert: shared path is not available');
                    return;
                end
                                
                % Get columns list from Rec file
                AColNames = fieldnames(Rec.Data);
                Columns = strjoin(AColNames, ',');
 
                TempFileName = sprintf('%s.dat', Component.newUuid());
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                copyfile(Args.CsvFileName, ClientFileName);
                Obj.msgLog(LogLevel.Debug, 'insert: Inserting Binary file, copied to: %s', ClientFileName);

                Obj.SqlText = sprintf('COPY %s (%s) FROM ''%s'' BINARY', Args.TableName, Columns, ServerFileName);
                Result = Obj.exec();
                
                return;
                
            end
            
            %---------------------------------------------------------
            
            if isempty(Args.BatchSize)
                Args.BatchSize = Obj.InsertBatchSize;
            end

            % Convert from input type
            if ~isa(Rec, 'db.DbRecord')
                if isnumeric(Rec) || iscell(Rec)
                    Rec = db.DbRecord(Rec, 'ColNames', Args.ColNames);
                else
                    Rec = db.DbRecord(Rec);
                end
            end

            % Generate primary keys using user function
            if isempty(Args.InsertRecFunc)
                Args.InsertRecFunc = Obj.InsertRecFunc;
            end
            if ~isempty(Args.InsertRecFunc)
                Args.InsertRecFunc(Obj, Rec, 1, numel(Rec.Data), Args.InsertRecArgs{:});
            end

            % When not specified, use class's default
            if isempty(Args.UseCopyThreshold)
                Args.UseCopyThreshold = Obj.InsertUseCopyThreshold;
            end

            % Need connection, clear current query
            if ~Obj.openConn()
                return;
            end

            %
            RecordCount = numel(Rec.Data);

            % Get table columns list
            if Args.ColumnsOnly
                TableColumnList = Obj.getTableColumnList(Args.TableName);
            else
                TableColumnList = [];
            end
            
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            ColumnNames = fieldnames(Rec.Data);
            [SqlColumns, SqlValues] = Obj.makeInsertColumnsText(ColumnNames, 'ColumnMap', Args.ColumnMap, 'TableColumnList', TableColumnList);
            %--------------------------------------------------------
            % INSERT INTO statement
            
            % Use COPY FROM
            if Args.UseCopyThreshold > 0 && RecordCount >= Args.UseCopyThreshold
                TempFileName = sprintf('%s.csv', Component.newUuid());
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                Rec.writeCsv(ClientFileName);
                
                Obj.msgLog(LogLevel.Debug, 'insert: Using COPY FROM');

                Obj.SqlText = ['COPY ', string(Args.TableName).char, ' FROM ''', string(ServerFileName).char, ''' DELIMITER '','' CSV HEADER;'];
                Result = Obj.exec();

            
                return;
            end


            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            T1 = tic();

            %
            RecSqlText = ['INSERT INTO ', string(Args.TableName).char, ' (', SqlColumns, ') VALUES (', SqlValues, ');'];
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
                Obj.setStatementValues(Rec, RecIndex, BatchSize, 'TableColumnList', TableColumnList);  %Args.ColumnMap, 'FirstIndex', ColumnIndex);
                RecIndex = RecIndex + BatchSize;
                Toc2 = toc(T2);

                if Obj.PerfLog
                    Obj.msgLog(LogLevel.Debug, 'insert (%d) prepare time: %f, BatchCount: %d', BatchNum, Toc2, BatchSize);
                end

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
                if Obj.PerfLog
                    Obj.msgLog(LogLevel.Debug, 'insert (%d) executeUpdate time: %f, BatchCount: %d', BatchNum, Toc3, BatchSize);
                end
            end

            Obj.Toc = toc(T1);

            if Obj.PerfLog
                Obj.msgLog(LogLevel.Debug, 'insert time: %f', Obj.Toc);
            end

            Result = Obj.ExecOk;
        end
        %------------------------------------------------------------------

        function Result = update(Obj, SetColumns, Args)
            % Update record
            % Intput:  SetColumns   - Note that string values must be enclosed by single '
            %                         for example: 'MyField=''MyValue'''
            %          'TableName'  -
            %          'Where'      -
            %          'ColumnMap'  - @Todo - for future use
            % Output:  true on success
            % Example: Obj.update('TableName', 'MyTable', 'MyField=1', 'Where', 'TheFlag = 1')
            
            arguments
                Obj
                SetColumns              % SQL statement, i.e. 'FInt=1', etc.
                Args.TableName = ''     % Table name
                Args.Where = ''         % Where condition
                %Args.ColumnMap = []    % Optional field map, for future use
            end

            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;

            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Debug, 'DbQuery: updateRecord');
            tic();

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));


            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();

            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            %FieldNames = Obj.getFieldNames(Rec);
            %WhereFieldNames = Obj.getFieldNames(WhereRec);
            %disp(FieldNames);
            %SqlFields = Obj.makeUpdateFieldsText(FieldNames, Args.FieldMap);
            %WhereFields = Obj.getFieldNames(WhereRec);
            %Where = Obj.makeWhereFieldsText(WhereFieldNames, ' AND ', Args.FieldMap);

            %
            if ~isempty(Args.Where)
                Obj.SqlText = ['UPDATE ', string(Args.TableName).char, ' SET ', string(SetColumns).char, ' WHERE ', string(Args.Where).char];
            else
                Obj.SqlText = ['UPDATE ', string(Args.TableName).char, ' SET ', string(SetColumns).char];
            end
            
            Obj.msgLog(LogLevel.Debug, 'update: SqlText: %s', Obj.SqlText);

            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'updateRecord: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'updateRecord: prepareStatement failed: %s', Obj.SqlText);
            end

            % Iterate struct fields
            %Obj.setStatementValues(ColumnNames, Rec, Args.ColumnMap);
            %Obj.setStatementValues(WhereColumnNames, WhereRec, Args.ColumnMap, 'ColumnIndex', numel(ColumnNames)+1);

            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                Obj.ExecOk = true;
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'update: executeQuery failed: %s', Obj.SqlText);
            end

            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Perf, 'update time: %.6f', Obj.Toc);

            Result = true;
        end


        function Result = deleteRecord(Obj, Args)
            % Delete record by fields specified in Rec
            % Note that we cannot use 'delete' as function name because it
            % is a reserved keyword.
            % Intput:  Args.TableName -
            %          Args.Where     -
            %
            % Output:  true on success
            % Example: Obj.deleteRecord('TableName', 'MyTable', 'Where', 'TheFlag = 1')
            
            arguments
                Obj
                Args.TableName = '';    % Table name, if empty, Obj.TableName is used
                Args.Where = '';        % Optional WHERE clause
            end

            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Debug, 'DbQuery: deleteRecord');
            tic();

            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();

            % Prepare SQL statement
            % sql = sprintf("DELETE FROM master_table WHERE key1=... and key2=...).char;
            if ~isempty(Args.Where)
                Obj.SqlText = ['DELETE FROM ', string(Args.TableName).char, ' WHERE ', string(Args.Where).char];
            else
                Obj.SqlText = ['DELETE FROM ', string(Args.TableName).char];
            end

            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'deleteRecord: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch
                Obj.msgLog(LogLevel.Error, 'deleteRecord: prepareStatement failed: %s', Obj.SqlText);
            end

            % Iterate struct fields
            %Obj.setStatementValues(ColumnNames, Rec, ColumnMap);

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
            Obj.msgLog(LogLevel.Perf, 'deleteRecord time: %.6f', Obj.Toc);

            Result = true;
        end

    end

    %----------------------------------------------------------------------
    
    methods % High-level: Run query / Execute statement(s)

        function Result = query(Obj, varargin)
            % Run SELECT query, do NOT load any data. For non-SELECT statements, use exec()
            % Input:   char-array - SQL text. If not specified, Obj.SqlText is used
            % Output:  true on success, use loadResultSet() to load the data
            % Example: Obj.query('SELECT COUNT(*) FROM master_table');
            
            % Obj.msgLog(LogLevel.Debug, 'query');
            Result = false;
            tic();

            % Need connection
            if ~Obj.openConn()
                return;
            end

            % Clear current query and metadata
            Obj.clear();

            % Set SQL text, or use current Obj.SqlText
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
            Obj.msgStyle(LogLevel.Debug, 'blue', 'query time: %.6f', Obj.Toc);
        end


        function Result = exec(Obj, varargin)
            % Execute SQL statement (that does not return data), for SELECT, use query()
            % Input:   char-array - SQL text. If not specified, Obj.SqlText is used
            % Output:  true on success
            % Example: Obj.exec('INSERT (recid,fint) INTO master_table VALUES (''MyUuid'',1)'

            %Obj.msgLog(LogLevel.Debug, 'exec');
            Result = false;
            tic();

            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();

            % Set SQL text, or use current Obj.SqlText
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
            Obj.msgStyle(LogLevel.Debug, 'blue', 'exec time: %.6f', Obj.Toc);
        end
        
    end
    
    
    methods % High-level: Utilities
        
        function Result = selectCount(Obj, Args)
            % Select number of records with optionally WHERE clause
            % Intput:  Args.TableName - If empty, use Obj.TableName
            %          Args.Where     - Example: 'Flag == 1'
            % Output:  integer        - COUNT returned by query
            % Example: Count = Obj.selectCount()
            % Example: Count = Obj.selectCount('TableName', 'MyTable')
            % Example: Count = Obj.selectCount('TableName', 'MyTable', 'Where', 'Flag == 1')
            
            arguments
                Obj                     %
                Args.TableName = ''     % If empty, use Obj.TableName
                Args.Where = ''         % Example: 'Flag == 1'
            end

            % TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));

            % Prepare Select
            Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', Args.TableName);

            % Where
            if ~isempty(Args.Where)
                Obj.SqlText = [Obj.SqlText, ' WHERE ', string(Args.Where).char];
            end

            % Run query and return result
            Obj.query();
            Result = Obj.getColumn('count');
        end


        function Result = loadResultSet(Obj, Args)
            % Helper function for select() - Load ResultSet to DbRecord array
            % Might be time and memory consuming!
            % Input:  'MaxRows' - Optional limit of number of rows to load to memory
            % Output:  DbRecord object: ColCount, ColNames, ColType, Data(i)
            % Example: Obj.loadResultSet();

            arguments
                Obj
                Args.MaxRows = Inf      % Maximum numer of rows to select
            end

            %PerfLog = io.FuncLog('loadResultSet');
            tic();

            % Initialize
            %Obj.msgLog(LogLevel.Debug, 'DbQuery.loadResultSet, ColumnCount = %d', Obj.ColCount);
            Result = db.DbRecord;
            Result.ColCount = Obj.ColCount;
            Result.ColNames = Obj.ColNames;
            Result.ColType  = Obj.ColType;

            % Loop over all ResultSet rows (records)
            RowIndex = 1;
            while ~Obj.Eof

                % Loop over all columns in the row
                for ColIndex = 1 : Obj.ColCount
                    ColumnName = Obj.ColNames{ColIndex};
                    ColumnValue = Obj.getColumn(ColIndex);
                    Result.Data(RowIndex).(ColumnName) = ColumnValue;
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
            if Obj.PerfLog
                Obj.msgStyle(LogLevel.Debug, 'blue', 'DbQuery.loadResultSet, RowCount = %d, Time: %.6f', RowIndex, Obj.Toc);
            end
        end

        
        function Result = writeResultSetToCsvFile(Obj, CsvFileName)
            % Write Obj.JavaResultSet returned by select() to CSV file using
            % Java CSVWriter obejct
            % Input:   CsvFileName
            % Output:  true on sucess
            % Example: Obj.writeResultSetToCsvFile('/tmp/test1.csv');
            % See: https://stackoverflow.com/questions/60756995/write-a-sql-resultset-to-a-csv-file
            %
            % Note: Seems that there is a bug in CSVWriter, one row is missing from the
            % file @Todo @Bug @Chen
            
            Result = false;
            
            % Copy jar file only once
            persistent Init;
            if isempty(Init)
                Init = true;
                tools.os.copyJavaJarToTempDir(fullfile(tools.os.getAstroPackExternalPath(), 'opencsv', 'opencsv-5.5.2.jar'));
            end
                        
            try
                % Create file string
                File = java.io.FileWriter(CsvFileName);
                
                % Write CSV file using java CSVWriter object
                % https://github.com/jlawrie/opencsv/blob/master/src/au/com/bytecode/opencsv/CSVWriter.java
                % http://opencsv.sourceforge.net/apidocs/com/opencsv/CSVWriter.html
                % CSVWriter(Writer writer,
                %   char separator,
                %   char quotechar,
                %   char escapechar,
                %   String lineEnd)
                
                % When creating with CSVWriter(File), the default quotechar is '"'
                % which result in busy lines like:
                % "a","","","10","0","","1.0","","N","0.0","0.0"
                % So we send empty quotechar (see NO_QUOTE_CHARACTER in CSVWriter.java)
                Writer = com.opencsv.CSVWriter(File, ',', char(0), char(0), newline);
                Writer.writeAll(Obj.JavaResultSet, true);
                Writer.close();
                File.close();
                Result = true;
            catch Ex
                Obj.msgLogEx(Ex, 'writeResultSetToCsvFile failed: %s', CsvFileName);
            end
                    
        end
        
    end

    %======================================================================
    %
    %======================================================================
    methods % Low-level: Record, Fields

        function Result = next(Obj)
            % Move cursor to next record, return false if reached end of data
            % Input:   -
            % Output:  true on sucess
            % Example: Obj.next()
            
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
            % Input:   -
            % Output:  true on sucess
            % Example: Obj.prev()
            
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.JavaResultSet.previous();
                Result = ~Obj.Eof;
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.prev failed');
            end
        end


        function Result = getColumn(Obj, ColumnName)
            % Get field value from current ResultSet, when ColumnName is
            % numeric, it is used as column index
            % Input:   ColumnName
            % Output:  Column value: double/integer/char/
            % Example: Value = Obj.getColumn('MyFieldName')
            
            % Example:
            %    Value = getColumn('MyFieldName')

            if isnumeric(ColumnName)
                ColIndex = ColumnName;
            else
                ColIndex = Obj.getColumnIndex(lower(ColumnName));
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
                        %Obj.msgLog(LogLevel.Debug, 'getColumn %s = %s', string(ColumnName).char, string(Result).char);
                    end

                catch
                    Obj.msgLog(LogLevel.Error, 'getColumn failed: %s', string(ColumnName).char);
                end
            else
                Obj.msgLog(LogLevel.Error, 'getColumn failed: Column not found: %s', string(ColumnName).char);
            end
        end


        function Result = isColumn(Obj, ColumnName)
            % Check if field exists by name
            % Input:   ColumnName
            % Output:  true if current result set
            % Example: IsColumn = Obj.isColumn('MyFieldName')
            
            if isempty(Obj.JavaResultSet)
                Obj.msgLog(LogLevel.Error, 'Query is not open (ResultSet is empty)');
                Result = '';
            else
                try
                    if ~isempty(Obj.JavaMetadata)
                        Index = Obj.getColumnIndex(ColumnName);
                        Result = (Index > 0);
                    else
                        Result = Obj.JavaResultSet.getString(ColumnName);
                        Result = true;
                    end
                catch
                    Obj.msgLog(LogLevel.Error, 'Column not found: %s', ColumnName);
                end
            end
        end


        function Result = getColumnIndex(Obj, ColumnName)
            % Get column index by column name, search in ColNames{}
            % Input:   ColumnName -
            % Output:  DbRecord
            % Example: Index = Obj.getColumnIndex('')
            
            Result = find(strcmp(Obj.ColNames, ColumnName));
        end


        function Result = getColumnType(Obj, ColumnName)
            % Get column type
            % Input:   ColumnName -
            % Output:  char -
            % Example: -
            
            if isnumeric(ColumnName)
                Index = ColumnName;
            else
                Index = Obj.getColumnIndex(ColumnName);
            end

            if Index > 0
                Result = Obj.ColType{Index};
            else
            end
        end


        function Result = getColumnList(Obj)
            % Get columns list of current ResultSet as celarray
            % Input:   -
            % Output:
            % Example: ColNames = Obj.getColumnList()
            
            Result = Obj.ColNames;
        end


        function Result = getTablesList(Obj)
            % Get list of all tables in current database
            % Input:   -
            % Output:  Cell array
            % Example: = Obj.getTablesList()
            Text = 'SELECT table_name FROM information_schema.tables WHERE table_schema = ''public'' ORDER BY table_name';
            Result = Obj.selectColumn(Text, 'table_name');
        end

        
        function Result = isTableExist(Obj, TableName)
            % Check if specified table exists in current database
            % Input:   TableName - Table name to search
            % Output:  true if table exists
            % Example: = Obj.isTableExist('MyTable')
            Text = sprintf('SELECT table_name FROM information_schema.tables WHERE table_schema = ''public'' AND table_name = ''%s'' ORDER BY table_name', lower(TableName));
            List = Obj.selectColumn(Text, 'table_name');
            Result = any(strcmpi(List, TableName));
        end
        
        
        function Result = getTableColumnList(Obj, TableName)
            % Get columns list of specified table as cell array
            % Input:   TableName
            % Output:  Cell array
            % Example: = Obj.getTableColumnList('master_table')
            Text = sprintf('SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ''%s'' ORDER BY column_name', TableName);
            Result = Obj.selectColumn(Text, 'column_name');
        end
                

        function Result = isColumnExist(Obj, TableName, ColumnName)
            % Check if column Get columns list of specified table as cell array
            % Input:   TableName
            %          ColumnName
            % Output:  true if column exist
            % Example: = Obj.isColumnExist('master_table', 'my_column')
            Text = sprintf('SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ''%s'' AND column_name = ''%s'' ORDER BY column_name', lower(TableName), lower(ColumnName));
            List = Obj.selectColumn(Text, 'column_name');
            Result = any(strcmpi(List, ColumnName));
        end
        
        
        function Result = getTablePrimaryKey(Obj, TableName)
            % Get primary key column names of specified table
            % Input:   TableName
            % Output:  Cell array
            % Example: = Obj.getTablePrimaryKey('master_table')
            Text = ['SELECT c.column_name, c.data_type '...
                'FROM information_schema.table_constraints tc '...
                'JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) '...
                'JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema '...
                'AND tc.table_name = c.table_name AND ccu.column_name = c.column_name '...
                'WHERE constraint_type = ''PRIMARY KEY'' and tc.table_name = ''' TableName ''''];
            Result = Obj.selectColumn(Text, 'column_name');
        end

        
        function Result = getTableIndexList(Obj, TableName)
            % Get list of index names of specified table
            % Input:   TableName
            % Output:  Cell array
            % Example: = Obj.getTableIndexList('master_table')
            Text = sprintf('SELECT * FROM pg_indexes WHERE tablename = ''%s''', TableName);
            Result = Obj.selectColumn(Text, 'indexname');
        end
        
        
        function Result = selectColumn(Obj, SqlText, ColumnName)
            % Get column from specified table as cell array
            % Input:   SqlText - SELECT query text
            %          ColumnName - Column name to select from query result
            % Output:  Cell array
            % Example: = Obj.selectColumn('SELECT COUNT(*) FROM master_table', 'count')
            
            Result = {};
            if Obj.query(SqlText)
                while ~Obj.Eof
                    Value = Obj.getColumn(ColumnName);
                    Result{end+1} = Value;
                    if ~Obj.next()
                        break
                    end
                end
            end
        end

    end
    
    %======================================================================
    %                      Low-Level & Internal Functions
    %======================================================================

    methods(Hidden=true)

        function Result = setConnection(Obj, DbTableOrConn)
            % [Internal Use] Set connection
            % Input:   DbTableOrConn -
            % Output:  true on sucess
            % Example: Obj.setConnection('unittest')
            
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
            % [Internal Use] Open connection, throw exception on failure
            % Input:   -
            % Output:  true on sucess
            % Example: Obj.openConn()
            
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
            % [Internal Use] Close current query
            % Intput:  -
            % Output:  true on sucess
            % Example: -
            
            Result = Obj.clear();
        end
        

        function [ServerFileName, ClientFileName] = getSharedFileName(Obj, FileName)
            % Prepare file names for server and client
            % Input:  FileName -
            % Output: ServerFileName -
            %         ClientFileName -
            % Exampe: -

            % Prepare path, use ServerShareFileName in the COPY statement
            % so the file name will be local on the server, we will be able
            % to access over the network using ClientShareFileName
            % Note that our server should be Linux
            
            [~, FName, Ext] = fileparts(FileName);
            ServerFileName = sprintf('%s%s%s%s', Obj.Conn.ServerSharePath, '/', FName, Ext);
            ClientFileName = fullfile(Obj.Conn.MountSharePath, strcat(FName, Ext));
            %ClientFileName = sprintf('%s%s%s%s', Obj.Conn.MountSharePath, filesep, FName, Ext);
        end
        
        
        function Result = clear(Obj)
            % Clear current statement and ResultSet
            % Input:   -
            % Output:  true on sucess
            % Example: Obj.clear()
            
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
            % Input:   -
            % Output:  -
            % Example: Obj.clearResultSet()
            
            Obj.JavaResultSet = [];
            Obj.JavaMetadata = [];
            Obj.ColCount = 0;
            Obj.ColNames  = [];
            Obj.ColType  = [];
            Result = true;
        end


        function Result = getMetadata(Obj, Args)
            % Get metadata of the specified table or the current result-set
            % Input:   -
            %          'TableName' -
            % Output:  true on success
            % Example: -
            
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
                %Obj.msgLog(LogLevel.Debug, 'DbQuery.getMetadata: ColumnCount = %d', Obj.ColCount);
                %data = cell(0, Obj.ColCount);
                for ColIndex = Obj.ColCount : -1 : 1
                    Obj.ColNames{ColIndex} = char(Obj.JavaMetadata.getColumnLabel(ColIndex));
                    Obj.ColType{ColIndex}  = char(Obj.JavaMetadata.getColumnClassName(ColIndex));
                end

                % Remove 'java.lang.' from column types, leave 'Double' etc.
                Obj.ColType = regexprep(Obj.ColType, '.*\.','');
                Result = true;

            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
            end

        end


        function [SqlColumns, SqlValues] = makeInsertColumnsText(Obj, ColumnNames, Args)
            % Input:   FieldNames -
            %          'TableColumnList'
            %          'FieldMap' -
            % Output:  SqlFields  -
            %          SqlValues  -
            % Example: [SqlFields, SqlValues] = Obj.makeInsertColumnsText('fint,fdouble')
            
            arguments
                Obj
                ColumnNames
                Args.TableColumnList = [];   %
                Args.ColumnMap = [];         % Optional struct.ColumnName = ActualColumnName;
            end

            % Prepare SQL text from cell array of field names
            % "INSERT INTO master_table(RecID, FInt) VALUES (?,?)"
            SqlColumns = '';
            SqlValues = '';

            if Obj.DebugMode
                %disp(ColumnNames);
            end

            %CheckColumns = false;
            %if Args.TableName
            %    CheckColumns = true;
            %    ColumnList = Obj.getTableColumnList(Args.TableName);
            %end
            
            % Iterate struct fields
            for i = 1:numel(ColumnNames)
                ColumnName = ColumnNames{i};

                % When Args.TableColumnList is specified, ignore columns not in list
                if ~isempty(Args.TableColumnList)
                    if ~any(strcmpi(Args.TableColumnList, ColumnName))
                        continue;
                    end
                end

                    
                % Optionally replace field name from mapping
                if ~isempty(Args.ColumnMap) && isfield(Args.ColumnMap, ColumnName)
                    ColumnName = Args.ColumnMap.(ColumnName);
                end

                %
                if numel(SqlColumns) > 0
                    SqlColumns = [SqlColumns, ',', ColumnName]; %#ok<AGROW>
                    SqlValues = [SqlValues, ',?']; %#ok<AGROW>
                else
                    SqlColumns = [SqlColumns, ColumnName]; %#ok<AGROW>
                    SqlValues = [SqlValues, '?']; %#ok<AGROW>
                end
            end

            %Obj.msgLog(LogLevel.Debug, 'makeInsertColumnsText: %s', SqlFields);
        end


        function SqlColumns = makeUpdateColumnsText(Obj, ColumnNames, Args)
            % Prepare SQL text from cell array
            % Input:   ColumnNames -
            %          'ColumnMap' - @Todo Future
            % Output:  char-array
            % Example: -
            
            % "UPDATE master_table set RecID=?,FInt=? WHERE..."
            arguments
                Obj
                ColumnNames             %
                Args.ColumnMap = [];    % Optional struct.ColumnName = ActualColumnName;
            end


            SqlColumns = '';

            % Iterate struct fields
            disp(ColumnNames);

            for i = 1:numel(ColumnNames)
                ColumnName = ColumnNames{i};

                % Optionally replace field name from mapping
                if ~isempty(Args.ColumnMap) && isfield(Args.ColumnMap, ColumnName)
                    ColumnName = Args.ColumnMap.(ColumnName);
                end

                %
                if numel(SqlColumns) > 0
                    SqlColumns = [SqlColumns, ',' ColumnName, '=?']; %#ok<AGROW>
                else
                    SqlColumns = [SqlColumns, ColumnName, '=?']; %#ok<AGROW>
                end
            end

            Obj.msgLog(LogLevel.Debug, 'makeUpdateColumnText: %s', SqlColumns);
        end


        function SqlColumns = makeWhereColumnsText(Obj, ColumnNames, Operand, ColumnMap)
            % Prepare SQL text from cell array
            % "WHERE RecID=? AND FInt=?..."
            % Input:   ColumnNames -
            %          Operand    -
            %          ColumnMap   -
            % Output:  DbRecord
            % Example: -
            
            arguments
                Obj
                ColumnNames     %
                Operand         % 'AND' / 'OR'
                ColumnMap       %
            end

            SqlColumns = '';

            % Iterate struct fields
            disp(ColumnNames);

            for i = 1:numel(ColumnNames)
                ColumnName = ColumnNames{i};

                % Optionally replace field name from mapping
                if ~isempty(ColumnMap) && isfield(ColumnMap, ColumnName)
                    ColumnName = ColumnMap.(ColumnName);
                end

                %
                if numel(SqlColumns) > 0
                    SqlColumns = [SqlColumns, ' ', Operand, ' ', ColumnName, '=?']; %#ok<AGROW>
                else
                    SqlColumns = [SqlColumns, ColumnName, '=?']; %#ok<AGROW>
                end
            end

            Obj.msgLog(LogLevel.Debug, 'makeWhereColumnsText: %s', SqlColumns);
        end


        function Result = setStatementValues(Obj, Rec, FirstRecord, RecordCount, Args)
            % Set statement values from specified DbRecord or struct
            % Input:   Rec               -
            %          FirstRecord       -
            %          RecordCount       -
            %          'ColumnNames'     -
            %          'ColumnMap'       -
            %          'StartIndex'      -
            %          'TableColumnList' -
            % Output:
            % Example:
            arguments
                Obj                     %
                Rec                     % DbRecord
                FirstRecord             %
                RecordCount             %
                Args.ColumnNames = [];  % cell
                Args.ColumnMap = []     % Optional
                Args.StartIndex = 1     % Index of field in current JavaStatement
                Args.TableColumnList = [];
            end

            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            % Obj.msgLog(LogLevel.DebugEx, 'setStatementValues: setting values');
            % int8, uint8, int16, uint16, int32, uint32 -> setInt()
            % int64, uint64 -> setLong()
            % logical -> setBoolean()
            % float, single, double -> setDouble()
            % char -> setString()

            if isempty(Args.ColumnNames)
                Args.ColumnNames = fieldnames(Rec.Data);
            end

            Index = Args.StartIndex;
            for DataIndex = FirstRecord:FirstRecord+RecordCount-1

                for ColumnNo = 1:numel(Args.ColumnNames)
                    ColumnName = Args.ColumnNames{ColumnNo};
                    
                    % When Args.TableColumnList is specified, ignore columns not in list
                    if ~isempty(Args.TableColumnList)
                        if ~any(strcmpi(Args.TableColumnList, ColumnName))
                            continue;
                        end
                    end
                    
                    % Get value
                    Value = Rec.Data(DataIndex).(ColumnName);

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
                        Obj.msgLog(LogLevel.Warn, 'setStatementValues: ERROR: other type - not supported: %s', ColumnName);
                    end
                    Index = Index+1;
                end
            end
            Result = true;
        end


        function Result = columnName2matlab(Obj, Str)
            % Convert specified table column name to valid MATLAB property/
            % struct-field name, replace non-valid chars with '_'
            % Input:   Str - char array, i.e. 'My:Field'
            % Output:  char array, i.e. 'My_Field'
            % Example: ColumnNames = getValidColumnName('My:Field') -> 'My_Field'
            for i=1:numel(Str)
                Ch = Str(i);
                Valid = isstrprop(Ch, 'alpha') || isstrprop(Ch, 'digit') || Ch == '_';
                if ~Valid
                    Str(i) = '_';
                end
            end
            Result = Str;
        end


        function Result = getColumnNamesOfType(Obj, ColumnType)
            % Get cell array field names that match the specified field type
            % Input:   ColumnType: 'Integer', 'Double', etc.
            % Output:  Cell array with list of all fields
            % Example: ColNames = Q.getColumnNamesOfType('Double');
            
            arguments
                Obj
                ColumnType      % Specified column type
            end

            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            Obj.msgLog(LogLevel.Debug, 'getColumnNamesOfType: %s', ColumnType);

            Result = {};
            for ColIndex = 1:Obj.ColCount
                if strcmp(Obj.ColType{ColIndex}, ColumnType)
                    Result{end+1} = Obj.ColNames{ColIndex};
                end
            end
        end


        function Result = getDbVersion(Obj)
            % Query Postgres version, note that DbQuery must be linked to DbConnection
            % Input:   -
            % Output:  char-array, such as 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
            % Example: Ver = DbQuery.getDbVersion()
            
            Result = [];
            Obj.query('SELECT version()');
            if Obj.ColCount == 1
                Result = Obj.getColumn('version');
            end
            Valid = contains(Result, 'PostgreSQL');
            if ~Valid
                Obj.msgLog(LogLevel.Warning, 'getDbVersion: Invalid result: %s', Result);
            end
        end
        
        
        function Result = writeBinaryFile(Obj, Rec, FileName)
            % Get cell array field names that match the specified field type
            % Input:   Rec - DbRecord with data
            % Output:  true on sucess
            % Example: ColNames = Q.getColumnNamesOfType('Double');
            % @Todo - Implement this function in MEX
            arguments
                Obj
                Rec             % DbRecord
                FileName        %
            end

            Fid = fopen(FileName, 'w');
            
            % Write file header
            % Postgres contains various header information of 15 bytes followed by an
            % optional header extension, which we mark as having a length of 0.
            
            % The file header consists of 15 bytes of fixed fields, followed by a
            % variable-length header extension area.
            %
            % The fixed fields are:
            %   - Signature
            %        11-byte sequence PGCOPY\n\377\r\n\0 � note that the zero byte
            %        is a required part of the signature. (The signature is designed
            %        to allow easy identification of files that have been munged by
            %        a non-8-bit-clean transfer. This signature will be changed by
            %        end-of-line-translation filters, dropped zero bytes, dropped high
            %        bits, or parity changes.)
            %
            %   - Flags field
            %        32-bit integer bit mask to denote important aspects of the file format.
            %        Bits are numbered from 0 (LSB) to 31 (MSB). Note that this field is
            %        stored in network byte order (most significant byte first), as are
            %        all the integer fields used in the file format. Bits 16�31 are reserved
            %        to denote critical file format issues; a reader should abort if it finds an
            %        unexpected bit set in this range. Bits 0�15 are reserved to signal
            %        backwards-compatible format issues; a reader should simply ignore any
            %        unexpected bits set in this range. Currently only one flag bit is defined,
            %        and the rest must be zero:
            %
            %        Bit 16 - If 1, OIDs are included in the data; if 0, not. Oid system
            %        columns are not supported in PostgreSQL anymore, but the format still
            %        contains the indicator.
            %
            %   - Header extension area length
            %        32-bit integer, length in bytes of remainder of header, not including self.
            %        Currently, this is zero, and the first tuple follows immediately.
            %        Future changes to the format might allow additional data to be present in
            %        the header. A reader should silently skip over any header extension data it
            %        does not know what to do with.
            %
            
            fwrite(Fid, 'PGCOPY');
            fwrite(Fid, char(10), 'int8');
            fwrite(Fid, char(0xFF), 'int8');
            fwrite(Fid, char(13), 'int8');
            fwrite(Fid, char(10), 'int8');
            fwrite(Fid, char(0), 'int8');
            fwrite(Fid, 0, 'int32');
            fwrite(Fid, 0, 'int32');

            % Write file data
            % for every row of data:
            %     write the number of columns to be written (short)
            %     for every column in row:
            %           write the amount of data in bytes about to be inserted (integer)
            %           write data (length varies by type and contents)
            %
            
            ARowCount = numel(Rec.Data);
            AColNames = fieldnames(Rec.Data);
            AColCount = numel(AColNames);
            for RowNum=1:ARowCount
                fwrite(Fid, int32(AColCount), 'int32');
                for ColNum=1:AColCount
                    % @Todo
                    %fwrite(Fid,
                    %fwrite(Fid,
                end
            end
            
            fclose(Fid);
        end
        
    end
    
    
    methods(Static) % Unit-Tests

        Result = unitTest()
            % Unit-Test

        Result = perfTest()
            % Performance Test - test timing of various operations

        Result = stressTest()
            % Stress Test - run continuous operations that create large
            % amount of data

        Result = examples()
            % Examples
    end
end
