% File:    DbQuery.m
% Class:   DbQuery
% Title:   SQL Database query component for Postgres
% Author:  Chen Tishler
% Created: July 2021
% Updated: July 2022
% Note:    For more documentation see .md files in +db/doc/
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
%--------------------------------------------------------------------------
%
%#docgen
%
% Methods:
%    DbQuery - Create new DbQuery obeject Input: DbTableOrConn - Database alias from Database.yml, with optional table name, for example: 'UnitTest'
%    addColumn - Add single or multiple columns to table Input: Output: Example: Obj.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0') Refs: https://www.postgresqltutorial.com/postgresql-add-column/
%    addIndex - Add single index to table, may include one or multiple fields Input: TableName - Table name to be altered IndexName - Unique index name, usually composed as
%    addUser - Add database user Input: UserName - User name Password - Pasword string 'DatabaseName' - If specified user will be granted only for this database 'Permission' - 'read', 'write', 'full'
%    createConnectionConfig - Create database connection in config/local/Database.DbConnections.UnitTest.yml Input: 'FileName' = '' % 'DatabaseName' = '' % 'Host' = 'localhost' % Host name or IP address
%    createDb - Create database Input: 'XlsFileName' - When specified, 'DatabaseName' -
%    createTable - Create database table, @Todo Input: Output: Example: db.DbQuery.createTable('unittest', ) Refs: https://www.postgresql.org/docs/8.0/sql-createuser.html
%    delete -
%    deleteRecord - Delete record by fields specified in Rec Note that we cannot use 'delete' as function name because it is a reserved keyword. Intput: Args.TableName - Args.Where -
%    exec - Execute SQL statement (that does not return data), for SELECT, use query() Input: char-array - SQL text. If not specified, Obj.SqlText is used Output: true on success Example: Obj.exec('INSERT (recid,fint) INTO master_table VALUES (''MyUuid'',1)'
%    getColumn - Get field value from current ResultSet, when ColumnName is numeric, it is used as column index Input: ColumnName Output: Column value: double/integer/char/ Example: Value = Obj.getColumn('MyFieldName')
%    getColumnIndex - Get column index by column name, search in ColNames{} Input: ColumnName - Output: DbRecord Example: Index = Obj.getColumnIndex('')
%    getColumnList - Get columns list of current ResultSet as celarray Input: - Output: Example: ColNames = Obj.getColumnList()
%    getColumnType - Get column type Input: ColumnName - Output: char - Example: -
%    getDbList - Get list of databases Input: - Output: Cell array with list of databases Example: List = Obj.getDbList() Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    getTableColumnNames - Get columns list of specified table as cell array Input: TableName Output: Cell array Example: = Obj.getTableColumnList('master_table')
%    getTableIndexList - Get list of index names of specified table Input: TableName Output: Cell array Example: = Obj.getTableIndexList('master_table')
%    getTablePrimaryKey - Get primary key column names of specified table Input: TableName Output: Cell array Example: = Obj.getTablePrimaryKey('master_table')
%    getTablesList - Get columns list of specified table as cell array Input: - Output: Cell array Example: = Obj.getTablesList()
%    getUserList - Get columns list of specified table as cell array Input: - Output: Cell array Example: List = Obj.getUserList() Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    insert - Simple insert, all arguments are char Insert new record to table, Keys and Values are celarray Input: Rec - Data to insert 'TableName' - Table name, if not specified, Obj.TableName is used 'CsvFileName' - When non-empty, use copyFrom and insert data from this file instead of Rec
%    isColumn - Check if field exists by name Input: ColumnName Output: true if current result set Example: IsColumn = Obj.isColumn('MyFieldName')
%    isDbExist - Check if database exists Input: DbName Output: true if exists Example: List = Obj.isDbExist('my_database') Refs: https://www.postgresqltutorial.com/postgresql-list-users/
%    loadResultSet - Helper function for select() - Load ResultSet to DbRecord array Might be time and memory consuming! Input: 'MaxRows' - Optional limit of number of rows to load to memory Output: DbRecord object: ColCount, ColNames, ColType, Data(i) Example: Obj.loadResultSet();
%    next - Move cursor to next record, return false if reached end of data Input: - Output: true on sucess Example: Obj.next()
%    prev - Move cursor to previous record, return false if reached end of data Input: - Output: true on sucess Example: Obj.prev()
%    query - Run SELECT query, do NOT load any data. For non-SELECT statements, use exec() Input: char-array - SQL text. If not specified, Obj.SqlText is used Output: true on success, use loadResultSet() to load the data Example: Obj.query('SELECT COUNT(*) FROM master_table');
%    removeUser - Remove user Input: UserName to remove Output: true on success Example: db.DbQuery.removeUser('robert') Refs: https://www.postgresql.org/docs/9.4/sql-dropuser.html
%    select - Execute SELECT Columns FROM TableName and load results to memory Input: - Columns - Comma-separated field names to select (i.e. 'recid,fint') 'TableName' - Table name, if not specified, Obj.TableName is used 'Where' - Where condition (excluding WHERE keyword) 'Order' - Order by clause (excluding ORDER BY keyword)
%    selectColumn - Get column from specified table as cell array Input: SqlText - SELECT query text ColumnName - Column name to select from query result Output: Cell array Example: = Obj.selectColumn('SELECT COUNT(*) FROM master_table', 'count')
%    selectTableRowCount - Select number of records with optionally WHERE clause Intput: Args.TableName - If empty, use Obj.TableName Args.Where - Example: 'Flag == 1' Output: integer - COUNT returned by query Example: Count = Obj.selectTableRowCount()
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
%    makeInsertColumnsText - Input: FieldNames - 'TableColumnList' - Output: SqlFields - SqlValues -
%    makeUpdateColumnsText - Prepare SQL text from cell array Input: ColumnNames - Output: char-array Example: -
%    makeWhereColumnsText - Prepare SQL text from cell array "WHERE RecID=? AND FInt=?..." Input: ColumnNames - Operand
%    openConn - [Internal Use] Open connection, throw exception on failure Input: - Output: true on sucess Example: Obj.openConn()
%    runPsql - Run 'psql' external utility with command line parameters. Input: XlsFileName Output: Example: db.DbQuery.runPsql( psql -h gauss -p 5432 -U admin -W -d postgres -f unittest_postgres.sql
%    setConnection - [Internal Use] Set connection Input: DbTableOrConn - Output: true on sucess Example: Obj.setConnection('unittest')
%    setStatementValues - Set statement values from specified DbRecord or struct Input: Rec - FirstRecord - RecordCount - 'ColumnNames' -
%    writeBinaryFile - Get cell array field names that match the specified field type Input: Rec - DbRecord with data Output: true on sucess Example: ColNames = Q.getColumnNamesOfType('Double'); @Todo - Implement this function in MEX
%    xlsx2sql - Convert XLSX file downloaded from Google Drive to SQL file Note: Requires ULTRASAT repository and ULTRASAT_PATH environment var to be set correctly. Note: python3 (python3.exe on Windows) should be on system PATH Input: XlsFileName
%
% Methods: Static
%    startGui - Run gui utility - @TODO - Currently DO NOT USE Input: - Output: - Example: db.DbQuery.startGui
%
%#/docgen

classdef DbQuery < Component

    % Properties
    properties (SetAccess = public)
        Conn            = []        % DbConnection component to that links to the specific host/database/user
        SqlText         = ''        % Current SQL text
        TableName       = '';       % Current table name

        % 'Insert' related
        InsertRecFunc   = [];       % function(DbQuery, DbRecord, First, Last)
        InsertBatchSize = 100;      % Batch size when inserting multiple rows using INSERT statement
        InsertUseCopyThreshold = 5000; % Above this number of records, insert() uses copyFrom()

        % Metadata from last select
        ColCount        = 0;        % Number of columns in current dataset
        ColNames        = [];       % Column names of current dataset (cell)
        ColType         = [];       % Column types of current dataset (cell)

        % Flags and statistics
        IsOpen          = false;    % Last result of query()
        ExecOk          = false;    % Last result of exec()
        Eof             = true;     % True when cursor reached last record of current ResultSet
        Toc             = 0;        % Time of last operation
        PerfLog         = false;    % True to log performance times
                
        % Internals
        JavaStatement   = []        % [Java object] Prepared statement object
        JavaMetadata    = []        % [Java object] set by getMetadata()
        JavaResultSet   = []        % [Java object] returned result-set from SELECT
        
        % Admin
        Shell           = ''        % Linux shell, 'tcsh' or 'bash', empty for auto detect by $SHELL env        
        Host            = ''        % Host name or IP address
        Port            = 5432      % Port number, default if 5432
        DatabaseName    = ''        % Use 'postgres' when creating databases or for general functionality
        UserName        = ''        % User name for connection
        Password        = ''        % Password for connection
        
    end

    %----------------------------------------------------------------------
    methods % Constructor

        % Constructor
        function Obj = DbQuery(DbTableOrConn, Args)
            % Create new DbQuery obeject
            % Input : - DbConnection object, or database alias from Database.yml.
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %             These arguments are used when DbTableOrConn is empty:
            %             'Host'      - Host name
            %             'Database'  - Database name
            %             'UserName'  - User name
            %             'Password'  - Password
            %             'Port'      - Port number
            %
            %             'TableName' - Set current table name, when not set, it must be
            %                           specified by each function (insert/select/etc.)
            %             'InsertRecFunc' - Default function used with insert(). See help of insert()
            %
            % Output   : - New instance of DbQuery object
            % Author   : Chen Tishler (2021)
            % Examples :
            %   % Create query object for 'UnitTest' database alias 'UnitTest'
            %   Q = DbQuery('UnitTest')
            %
            %   % Create query object for 'UnitTest' database and table
            %   'master_table'
            %   Q = DbQuery('UnitTest:master_table')
            %
            %   % Create query object for custom database connection (not
            %   from Database.yml)
            %   MyConn = DbConnection('Db', 'MyAlias', ...)
            %   Q.DbQuery(MyConn)
            %
            arguments
                DbTableOrConn   = []  % DbAlias / DbAlias:TableName / DbConnection object
                Args.TableName        % Set TableName when not included in DbTable parameter
                Args.InsertRecFunc    % Default function used with insert(). See help of insert()
                
                % These arguments are used when both DbQuery and DbCon are NOT set:
                Args.Host          = ''        % Host name or IP address
                Args.Port          = 5432      % Port number
                Args.DatabaseName  = ''        % Use 'postgres' to when creating databases or for general
                Args.UserName      = ''        % User name
                Args.Password      = ''        % Password
                
            end

            % Setup component
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.DebugMode = false;
            %Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);

            % Set connection
            if ~isempty(DbTableOrConn)
                if isa(DbTableOrConn, 'db.DbQuery')
                    if ~isempty(Args.DatabaseName)
                        %Obj.setProps(Args);
                        AConn = DbTableOrConn.Conn;
                        NewCon = db.DbConnection('Host', AConn.Host, 'Port', AConn.Port, ...
                            'DatabaseName', Args.DatabaseName, 'UserName', AConn.UserName, 'Password', AConn.Password);
                
                        Obj.setConn(NewCon);
                    else
                        Obj.setConnection(DbTableOrConn.Conn);
                    end
                else
                    Obj.setConnection(DbTableOrConn);
                end                

            % Create new connection from other arguments
            elseif ~isempty(Args.Host)
                NewCon = db.DbConnection('Host', Args.Host, 'Port', Args.Port, ...
                    'DatabaseName', Args.DatabaseName, 'UserName', Args.UserName, 'Password', Args.Password);
                
                Obj.setConn(NewCon);
                
            end

            % Override TableName and set other properties
            Obj.setProps(Args);
        end

        
        function delete(Obj)
            % Destructor            
            % Internally called by Matlab when the object is destroyed.            
            Obj.clear();
            %Obj.msgLog(LogLevel.Debug, 'deleted: %s', Obj.Uuid);
        end
    end

    %----------------------------------------------------------------------
    methods % High-level: Select/Insert/Update/Delete

        function Result = select(Obj, Columns, Args)
            % Execute SELECT Columns FROM TableName and optionally load results to memory
            % Input :  - DbQuery object
            %          - Cellarray or comma-separated field names to select (i.e. 'recid,fint')
            %          * Pairs of ...,key,val,...
            %            The following keys are available:            
            %            'TableName' - Table name, if not specified, Obj.TableName is used
            %            'Where'     - Where condition (excluding WHERE keyword)
            %            'Order'     - Order by clause  (excluding ORDER BY keyword)
            %            'Limit'     - Maximum number of records (LIMIT), -1 for no limit
            %            'Load'      - true to load entire result set
            %            'OutType'   - Optional conversion, otherwise DbRecord is
            %                          returned: 'table', 'cell', 'mat', 'AstroTable', 'AstroCatalog',
            %                          'AstroHeader'
            %            'UseCopy'   - @Todo (not implemented yet): True to use copyTo() instead of SELECT
            %            'CsvFileName' - Select to specified output CSV file, instead of
            %                            result-set (using Postgres' "COPY TO" statement or CsvWriter 
            %                            if shared folder is not available)
            %            'CsvDelimiter' - Csv columns delimiter character
            %            'CsvNull'      - Value for null fields
            %
            % Output  : - DbRecord object or other data type according to 'OutType' argument.
            % Author  : Chen Tishler (2021)
            % Example : DataSet = Obj.select('Column1', 'Table', 'Where', 'X=1', 'Order', 'Column1')
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
                Args.CsvDelimiter = ',' % Csv columns delimiter character
                Args.CsvNull = 'NaN'    % Value for null fields
            end

            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            assert(~isempty(Columns));

            if iscell(Columns)
                Columns = strjoin(Columns, ',');
            end
            
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

            % Now Obj.SqlText is ready
            
            % SELECT to output CSV file (using COPY TO or CSVWriter, when shared folder is not available)
            if ~isempty(Args.CsvFileName)
                % Shared folder is available, use it and then move the result to our target
                if Obj.Conn.isSharedPathAvail()
                    [ServerFileName, ClientFileName] = Obj.getSharedFileName(Args.CsvFileName);                    
                    Obj.SqlText = sprintf('COPY (%s) TO ''%s'' WITH (FORMAT CSV, HEADER, DELIMITER ''%s'', NULL ''%s'');', ...
                        Obj.SqlText, ServerFileName, Args.CsvDelimiter, Args.CsvNull);
                    %Obj.SqlText = sprintf('COPY (%s) TO ''%s'' CSV HEADER', Obj.SqlText, ServerFileName);
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
                    % @Todo - should we use next=false??? (24/03/2022)
                    Res = Obj.query(Obj.SqlText);  %, 'next', false);
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
                Obj.SqlText = sprintf('COPY (%s) TO ''%s'' WITH (FORMAT CSV, HEADER, DELIMITER ''%s'', NULL ''%s'');', ...
                        Obj.SqlText, ServerFileName, Args.CsvDelimiter, Args.CsvNull);
                %Obj.SqlText = sprintf('COPY (%s) TO ''%s'' CSV HEADER', Obj.SqlText, ServerFileName);
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
                Res = Obj.query(Obj.SqlText);
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
            % INSERT new row(s) to database table.
            % Data source can be specified as actual data in Rec, or from external file.
            % (Note that BinaryFileName is not supported yet).
            % Input :   - DbQuery object
            %           - Data to insert, DbRecord or other type supported by its constructor
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %            'TableName'    - Table name, if not specified, Obj.TableName is used
            %            'CsvFileName'  - When non-empty, use COPY FROM and insert data from 
            %                             this CSV file instead of Rec.
            %
            %            These arguments are used only when CsvFileName is
            %            empty and Rec is used as data source:
            %
            %            'ColNames'        - Comma-separated or cell array column names (i.e. 'recid,fint')
            %            'BatchSize'       - Number of records per operation
            %            'InsertRecFunc'   - Called to generate primary key if required, see below.
            %            'InsertRecArgs'   - Arguments to InsertRecFunc
            %            'UseCopyThreshold'- When number of records is above this value, copyFrom() is used
            %            'CsvFileName'     - Specify CSV file as source of data
            %            'CsvDelimiter'    - Csv column delimiter character
            %            'CsvNull'         - Value for null fields
            %            'BinaryFileName'  - NOT IMPLEMENTED YET - When using COPY, name of Binary file @TODO
            %            'Returning'       - @TODO - RETURNING - Need to add support
            %
            % Output   : - true on success.
            % Author   : Chen Tishler (2021)
            % Examples : 
            %
            %   Insert matrix of 3 columns:
            %
            %       DoubleColumns = 'fdouble1,fdouble2,fdouble3';
            %       Cols = numel(strsplit(DoubleColumns, ','));
            %       Mat = rand(1000, Cols);
            %       Q.insert(Mat, 'ColNames', DoubleColumns);
            %
            %   Insert matrix with user-function to generate primary key,
            %   make_recid will be called from insert()
            %
            %       Mat = rand(10, 2);
            %       Q.insert(Mat, 'ColNames', 'fdouble1,fdouble2', 'InsertRecFunc', @make_recid, 'BatchSize', 10000);
            %
            arguments
                Obj
                Rec                         % db.DbRecord, struct array, table, cell array, matrix,
                                            % AstroTable, AstroCatalog, AstroHeader
                Args.TableName = ''         % Table name, if not specified, Obj.TableName is used
                Args.ColNames = []          % Comma-separated field names (i.e. 'recid,fint')
                Args.BatchSize = []         % Number of records per operation
                Args.InsertRecFunc = []     % Called to generate primary key if required
                Args.InsertRecArgs = {}     % Arguments to InsertRecFunc
                Args.UseCopyThreshold = []  % When number of records is above this value, copyFrom() is used
                Args.ColumnsOnly = false;   % When true, ignore fields that has no matching columns in the table
                Args.CsvFileName = ''       % When using COPY, name of CSV file
                Args.CsvDelimiter = ','     % Csv columns delimiter
                Args.CsvNull = 'NaN'        % Csv values for Null column
                Args.BinaryFileName = ''    % NOT IMPLEMENTED YET! When using COPY, name of Binary file @TODO - NOT IMPLEMENTED YET!
                Args.Returning = ''         % NOT IMPLEMENTED YET!
            end

            % Execute SQL statement (using java calls)
            %Obj.msgLog(LogLevel.Debug, 'DbQuery: insert');
            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            % Insert directly from CSV file, primary key must be included
            if ~isempty(Args.CsvFileName)                
                if ~isfile(Args.CsvFileName)
                    Obj.msgLog(LogLevel.Error, 'insert: csv file not found: %s', Args.CsvFileName);
                    return;
                end
                
                % Make sure that shared path is available, otherwise we cannot insert
                % diretly from file
                if ~Obj.Conn.isSharedPathAvail()
                    Obj.msgLog(LogLevel.Error, 'insert: shared path is not available');
                    return;
                end               
                
                % Get columns list from CSV file
                fid = fopen(Args.CsvFileName);
                AColNames = strsplit(fgetl(fid), ',');
                fclose(fid);
                Columns = strjoin(AColNames, ',');
 
                % Get server and client pathes
                TempFileName = sprintf('%s.csv', Component.newUuid());
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                
                % Check if CsvFileName is already on shared folder
                [CsvPath, ~, ~] = fileparts(Args.CsvFileName);
                [ClientPath, ~, ~] = fileparts(ClientFileName);
                NeedDelete = false;
                
                % Csv file is already in the shared folder
                if strcmp(CsvPath, ClientPath)
                    [ServerFileName, ~] = Obj.getSharedFileName(Args.CsvFileName);
                    
                % Csv file is not in the shared folder, need to copy it
                else
                   % Copy CsvFileName to shared folder, only if we need to copy it                    
                    copyfile(Args.CsvFileName, ClientFileName);
                    NeedDelete = true;
%                     Obj.msgLog(LogLevel.Debug, 'insert: Inserting CSV file, copied to: %s', ClientFileName);
                end

                % Prepare COPY FROM statement
                Obj.SqlText = sprintf('COPY %s (%s) FROM ''%s'' WITH (FORMAT CSV, HEADER, DELIMITER ''%s'', NULL ''%s'');', ...            
                    Args.TableName, Columns, ServerFileName, Args.CsvDelimiter, Args.CsvNull);
                %Obj.SqlText = sprintf('COPY %s (%s) FROM ''%s'' DELIMITER '','' CSV HEADER;', Args.TableName, Columns, ServerFileName);
                t1 = tic();
                Result = Obj.exec();
                t2 = toc(t1);
%                 Obj.msgLog(LogLevel.Debug, 'insert: COPY FROM: %0.6f', t2);
                
                % Delete temporary file
                if NeedDelete
                    delete(ClientFileName);
                end
                
                % We done here
                Result = true;
                return;
            end
            
            %---------------------------------------------------------
            % Binary file - NOT IMPLEMENTED YET
            %
            % See:
            %   https://nickb.dev/blog/disecting-the-postgres-bulk-insert-and-binary-format
            %   https://www.postgresql.org/docs/14/sql-copy.htm
            %   https://stackoverflow.com/questions/758945/whats-the-fastest-way-to-do-a-bulk-insert-into-postgres?rq=1
            %   https://www.bytefish.de/blog/pgbulkinsert_bulkprocessor.html
            %   https://github.com/PgBulkInsert/PgBulkInsert
            %
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
%                 Obj.msgLog(LogLevel.Debug, 'insert: Inserting Binary file, copied to: %s', ClientFileName);

                Obj.SqlText = sprintf('COPY %s (%s) FROM ''%s'' BINARY', Args.TableName, Columns, ServerFileName);
                Result = Obj.exec();
                
                return;
                
            end
            
            %---------------------------------------------------------           
            % Get batch size
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

            % Threadshold, when not specified, use class's default
            if isempty(Args.UseCopyThreshold)
                Args.UseCopyThreshold = Obj.InsertUseCopyThreshold;
            end

            % Need connection, clear current query
            if ~Obj.openConn()
                Obj.msgLog(LogLevel.Error, 'Connection is not open');
                return;
            end

            % Get number of rows in input data
            RecordCount = numel(Rec.Data);

            % Get table columns list
            if Args.ColumnsOnly
                TableColumnList = Obj.getTableColumnNames(Args.TableName);
            else
                TableColumnList = [];
            end
            
            % Prepare SQL statement, i.e.: 'INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1)'
            ColumnNames = fieldnames(Rec.Data);
            [SqlColumns, SqlValues] = Obj.makeInsertColumnsText(ColumnNames, 'TableColumnList', TableColumnList);

            % If there are more rows that threadshold, and we can use COPY FROM,
            % create Csv file and use it.           
            if Args.UseCopyThreshold > 0 && RecordCount >= Args.UseCopyThreshold
                TempFileName = sprintf('%s.csv', Component.newUuid());
                [ServerFileName, ClientFileName] = Obj.getSharedFileName(TempFileName);
                Rec.writeCsv(ClientFileName);                
%                 Obj.msgLog(LogLevel.Debug, 'insert: Using COPY FROM');
                Obj.SqlText = ['COPY ', string(Args.TableName).char, ' FROM ''', string(ServerFileName).char, ''' DELIMITER '','' CSV HEADER;'];
                Result = Obj.exec();            
                return;
            end

            % Insert using 'INSERT ...' statement(s)            
            % Now we are going to ise INSERT INTO 
            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            T1 = tic();
            RecSqlText = ['INSERT INTO ', string(Args.TableName).char, ' (', SqlColumns, ') VALUES (', SqlValues, ')'];
%             Obj.msgLog(LogLevel.Debug, 'insert: SqlText: %s', RecSqlText);

            % @Todo - RETURNING
            if ~isempty(Args.Returning)
                RecSqlText = [RecSqlText, ' RETURNING ', Args.Returning];
            end
            RecSqlText = [RecSqlText, ';'];
            ReturningResults = [];
                        
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

                % Prepare the actual Java object that represent the statement
                try
                    Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'insert: prepareStatement failed: %s', Obj.SqlText);
                end

                % Set statement values from input data
                T2 = tic();
                Obj.setStatementValues(Rec, RecIndex, BatchSize, 'TableColumnList', TableColumnList);
                RecIndex = RecIndex + BatchSize;
                Toc2 = toc(T2);
                if Obj.PerfLog
                    Obj.msgLog(LogLevel.Debug, 'insert (%d) prepare time: %f, BatchCount: %d', BatchNum, Toc2, BatchSize);
                end

                % Execute the statement
                % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
                T3 = tic();
                try                   
                    % RETURNING - Collect the return data
                    if ~isempty(Args.Returning)
                        Obj.JavaResultSet = Obj.JavaStatement.executeQuery();
                        if Obj.JavaResultSet.next()
                            ret = Obj.JavaResultSet.getInt(1);
                            ReturningResults(end+1) = ret;
                        end
                        
                    % Without 
                    else
                        Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();                        
                    end
            
                    Obj.ExecOk = true;
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'insert: executeQuery failed: %s', Obj.SqlText);
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
            if ~isempty(Args.Returning)            
                Result = ReturningResults(1);
            end
        end
        %------------------------------------------------------------------

        function Result = update(Obj, SetColumns, Args)
            % Update table record(s)
            % Input   : - DbQuery object
            %           - SetColumns   - The part of UPDATE statement that includes
            %                          field values, as FieldName=Value,...
            %                          Note that string values must be enclosed by single '
            %                          for example: 'MyField=''MyValue'''
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableName'  - Table name to update
            %             'Where'      - Where condition expression, i.e. (FlagField=1')
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.update('MyField=1', 'TableName', 'MyTable', 'Where', 'TheFlag = 1')
            
            arguments
                Obj
                SetColumns              % SQL statement, i.e. 'FInt=1', etc.
                Args.TableName = ''     % Table name
                Args.Where = ''         % Where condition
            end

            % Use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;

            % Execute SQL statement (using java calls)
            %Obj.msgLog(LogLevel.Debug, 'DbQuery: updateRecord');
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
            if ~isempty(Args.Where)
                Obj.SqlText = ['UPDATE ', string(Args.TableName).char, ' SET ', string(SetColumns).char, ' WHERE ', string(Args.Where).char];
            else
                Obj.SqlText = ['UPDATE ', string(Args.TableName).char, ' SET ', string(SetColumns).char];
            end
            
            %Obj.msgLog(LogLevel.Debug, 'update: SqlText: %s', Obj.SqlText);

            % Prepare query
            %Obj.msgLog(LogLevel.Debug, 'updateRecord: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'updateRecord: prepareStatement failed: %s', Obj.SqlText);
            end

            % Iterate struct fields
            %Obj.setStatementValues(ColumnNames, Rec);
            %Obj.setStatementValues(WhereColumnNames, WhereRec, 'ColumnIndex', numel(ColumnNames)+1);

            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                Obj.ExecOk = true;
                Result = true;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'update: executeQuery failed: %s', Obj.SqlText);
            end

            Obj.Toc = toc();
            %Obj.msgLog(LogLevel.Perf, 'update time: %.6f', Obj.Toc);
        end


        function Result = deleteRecord(Obj, Args)
            % Delete record by fields specified in Rec
            % Note that we cannot use 'delete' as function name because it
            % is a reserved keyword.
            % Input   : - DbQuery object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableName' - Table name to delete from
            %             'Where'     - The WHERE clause with conditional expression 
            %
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.deleteRecord('TableName', 'MyTable', 'Where', 'TheFlag = 1')
            %
            arguments
                Obj
                Args.TableName = '';    % Table name, if empty, Obj.TableName is used
                Args.Where = '';        % Optional WHERE clause, if not specified, 
                                        % ALL ROWS from the table will be deleted!!!
            end

            Result = false;

            % Use speified TableName or Obj.TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));
            
            % Execute SQL statement (using java calls)
            %Obj.msgLog(LogLevel.Debug, 'DbQuery: deleteRecord');
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
            %Obj.msgLog(LogLevel.Debug, 'deleteRecord: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'deleteRecord: prepareStatement failed: %s', Obj.SqlText);
            end

            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                Obj.ExecOk = true;
                Result = true;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'deleteRecord: executeQuery failed: %s', Obj.SqlText);
            end

            Obj.Toc = toc();
            %Obj.msgLog(LogLevel.Perf, 'deleteRecord time: %.6f', Obj.Toc);
        end
    end

    %----------------------------------------------------------------------
    
    methods % Low-level - FOR INTERNAL USE : Run query / Execute statement(s)

        function Result = query(Obj, ASqlText, Args)
            % Run any SELECT query, results should be accessed by calling getColumn()
            % For non-SELECT statements, use exec(), see below
            %
            % Input   : - DbQuery object
            %           - SQL text. If not specified, Obj.SqlText is used
            % Output  : - true on success, use loadResultSet() to load the data
            % Author  : Chen Tishler (2021)
            % Example : Obj.query('SELECT COUNT(*) FROM master_table');
            arguments
                Obj
                ASqlText = ''
                Args.Next = true    % true to call next so first data row is available to access its columns
            end
            
            Result = false;
            tic();

            % Need connection
            if ~Obj.openConn()
                Obj.msgLog(LogLevel.Error, 'query: connection is not open');
                return;
            end

            % Clear current query and metadata
            Obj.clear();

            % Set SQL text, or use current Obj.SqlText
            if ~isempty(ASqlText)
                Obj.SqlText = ASqlText;
            end

            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'query: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'query: prepareStatement failed: %s', Obj.SqlText);
            end

            % executeQuery
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeQuery();
                Obj.IsOpen = true;

                % Get metadata (@Todo: Make it Optional?)
                Obj.getMetadata();

                % Get first result record
                if Args.Next
                    Obj.next();
                end
                
                Result = true;
            catch Ex
                Obj.IsOpen = false;
                Obj.msgLogEx(LogLevel.Error, Ex, 'query: executeQuery failed: %s', Obj.SqlText);
            end

            Obj.Toc = toc();
            Obj.msgStyle(LogLevel.Debug, 'blue', 'query time: %.6f', Obj.Toc);
        end


        function Result = exec(Obj, ASqlText)
            % Execute any non-select SQL statement (that does not return data), 
            % for SELECT, use query() above
            %
            % Input   : - DbQuery object
            %           - SQL text to execute. If not specified, Obj.SqlText is used
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.exec('INSERT (recid,fint) INTO master_table VALUES (''MyUuid'',1)'
            arguments
                Obj
                ASqlText = ''
            end

            %Obj.msgLog(LogLevel.Debug, 'exec');
            Result = false;
            tic();

            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();

            % Set SQL text, or use current Obj.SqlText
            if ~isempty(ASqlText)
                Obj.SqlText = ASqlText;
            end

            % Prepare query
            %Obj.msgLog(LogLevel.Debug, 'exec: %s', Obj.SqlText);
            try
                Obj.JavaStatement = Obj.Conn.JavaConn.prepareStatement(Obj.SqlText);
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'exec: prepareStatement failed: %s', Obj.SqlText);
            end

            % @Todo - Check if we need to do something with BigDecimal
            % See https://www.codota.com/code/java/methods/java.sql.PreparedStatement/setBigDecimal

            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.JavaResultSet = Obj.JavaStatement.executeUpdate();
                Obj.ExecOk = true;
                Result = true;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'exec: executeQuery failed: %s', Obj.SqlText);
            end
            Obj.Toc = toc();
            Obj.msgStyle(LogLevel.Debug, 'blue', 'exec time: %.6f', Obj.Toc);
        end        
    end
    
    %----------------------------------------------------------------------
    
    methods % High-level: Utilities
        
        function Result = selectTableRowCount(Obj, Args)
            % Select number of records in table, with optional WHERE clause.
            % Input   : - DbQuery object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableName' - Table name, if empty, use Obj.TableName
            %             'Where'     - Optoinal WHERE cluse, example: 'Flag == 1'
            %             'Fast'      - When true, get ESTIMATED number of records which is
            %                           much faster, to be used with large tables.
            %                           See https://wiki.postgresql.org/wiki/Count_estimate
            % Output  : - integer     - COUNT returned by query
            % Author  : Chen Tishler (2021)
            % Example : Count = Obj.selectTableRowCount()
            %           Count = Obj.selectTableRowCount('TableName', 'MyTable')
            %           Count = Obj.selectTableRowCount('TableName', 'MyTable', 'Where', 'Flag == 1')           
            arguments
                Obj                     %
                Args.TableName = ''     % If empty, use Obj.TableName
                Args.Where = ''         % Example: 'Flag == 1'
                Args.Fast = false       % When true, get ESTIMATED number of records
            end

            % TableName
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            assert(~isempty(Args.TableName));

            % SELECT estimated
            if Args.Fast && isempty(Args.Where)
                % See https://wiki.postgresql.org/wiki/Count_estimate
                Obj.SqlText = sprintf('SELECT reltuples AS estimate FROM pg_class WHERE relname = ''%s''', Args.TableName);                
                Obj.query();
                Result = Obj.getColumn('estimate');
                
                % -1 might be returned on empty table
                if Result < 0
                    Result = 0;
                end
                
            % SELECT COUNT(*)
            else
                Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', Args.TableName);

                % Where
                if ~isempty(Args.Where)
                    Obj.SqlText = [Obj.SqlText, ' WHERE ', string(Args.Where).char];
                end

                % Run query and return result
                Obj.query();
                Result = Obj.getColumn('count');
            end
        end


        function Result = loadResultSet(Obj, Args)
            % Helper function for select() - Load ResultSet to DbRecord array
            % Might be time and memory consuming, depeding on size of data returned
            % by SELECT.
            %
            % Input   : - DbQuery object 
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'MaxRows' - Optional limit of number of rows to load to memory
            % Output  : - New DbRecord object
            % Author  : Chen Tishler (2021)
            % Example : Obj.loadResultSet();

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
            %
            % NOTE: from unknown reason CSVWriter object does not always work, @Todo (24/03/2022)
            %
            % Input   : - DbQuery object
            %           - CsvFileName - name of Csv file
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.writeResultSetToCsvFile('/tmp/test1.csv');
            %
            % See: https://stackoverflow.com/questions/60756995/write-a-sql-resultset-to-a-csv-file
            %
            % Note: Seems that there is a bug in CSVWriter, one row is missing from the
            % file @Todo @Bug @Chen - Maybe we need to remove the call to next() from
            % query() ????
            %             
            Result = false;
            
            % Copy opencsv jar file (Java package) to temporary folder (only once)
            persistent Init;
            if isempty(Init)
                Init = true;
                tools.os.copyJavaJarToTempDir(fullfile(tools.os.getAstroPackExternalPath(), 'opencsv', 'opencsv-5.5.2.jar'));
            end
                        
            try
                % Create file writer object
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
                
                % Create CsvWriter Java object
                % This line fails on some cases, check when/why? @Todo (24/03/2022)
                Writer = com.opencsv.CSVWriter(File, ',', char(0), char(0), newline);
                
                Writer.writeAll(Obj.JavaResultSet, true);
                Writer.close();
                File.close();
                Result = true;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'writeResultSetToCsvFile failed: %s', CsvFileName);
            end                    
        end        
    end

    %======================================================================
    %
    %======================================================================
    methods % Low-level: Record, Fields

        function Result = next(Obj)
            % Move cursor to next record of JavaResultSet, return false if reached end of data
            % Input   : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.next()            
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.JavaResultSet.next();
                Result = ~Obj.Eof;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'DbQuery.next failed');
            end
        end


        function Result = prev(Obj)
            % Move cursor to previous record of JavaResultSet, return false if reached end of data
            % Input   : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.prev()            
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.JavaResultSet.previous();
                Result = ~Obj.Eof;
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'DbQuery.prev failed');
            end
        end


        function Result = getColumn(Obj, ColumnName)
            % Get field value from current ResultSet, when ColumnName is
            % numeric, it is used as column index istead of column name
            %
            % Input   : - DbQuery object
            %           - ColumnName - name of column of index of column
            % Output  : - Column value: double/integer/char, depends on column data type
            % Author  : Chen Tishler (2021)
            % Example : Value = Obj.getColumn('MyFieldName')

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

                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'getColumn failed: %s', string(ColumnName).char);
                end
            else
                Obj.msgLog(LogLevel.Error, 'getColumn failed: Column not found: %s', string(ColumnName).char);
            end
        end


        function Result = isColumn(Obj, ColumnName)
            % Check if column exists by name
            % Input   : - DbQuery object
            %           - ColumnName
            % Output  : - true if ColumnName exists in current result set
            % Author  : Chen Tishler (2021)
            % Example : IsColumn = Obj.isColumn('my_column_name')
            
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
                catch Ex
                    Obj.msgLogEx(LogLevel.Error, Ex, 'Column not found: %s', ColumnName);
                end
            end
        end


        function Result = getColumnIndex(Obj, ColumnName)
            % Get column index by column name, search in Obj.ColNames{}
            % Input   : - DbQuery object
            %           - Name of culumn to be search for
            % Output  : - Integer - index of column
            % Author  : Chen Tishler (2021)
            % Example : Index = Obj.getColumnIndex('my_column_name')            
            Result = find(strcmp(Obj.ColNames, ColumnName));
        end


        function Result = getColumnType(Obj, ColumnName)
            % Get column type from column name or column index
            % Input   : - DbQuery object
            %           - ColumnName - column name or column index
            % Output  : - Data type name (char)
            % Author  : Chen Tishler (2021)
            % Example : getColumnType('my_column_name')            
            
            if isnumeric(ColumnName)
                Index = ColumnName;
            else
                Index = Obj.getColumnIndex(ColumnName);
            end

            if Index > 0
                Result = Obj.ColType{Index};
            else
                Result = '';
            end
        end


        function Result = getColumnList(Obj)
            % Get columns list of current ResultSet, as celarray
            % Input   : - DbQuery object
            % Output  : - Cell-array with list of columns
            % Author  : Chen Tishler (2021)
            % Example : ColNames = Obj.getColumnList()          
            Result = Obj.ColNames;
        end


        function Result = getTablesList(Obj)
            % Get list of all tables in current database
            % Input   : - DbQuery object
            % Output  : - Cell array
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getTablesList()
            Text = 'SELECT table_name FROM information_schema.tables WHERE table_schema = ''public'' ORDER BY table_name';
            Result = Obj.selectColumn(Text, 'table_name');
        end
        
        
        function Result = getSchemasList(Obj)
            % Get list of all tables in current database
            % Input   : - DbQuery object
            % Output  : - Cell array
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getTablesList()
            Text = 'SELECT schema_name FROM information_schema.schemata';
            Result = Obj.selectColumn(Text, 'schema_name');
        end        

        
        function Result = getPartitionTree(Obj, Args)
            % Get list of table's Partitions
            % Partitioning refers to splitting what is logically one large 
            % table into smaller physical pieces.
            % See: https://www.postgresql.org/docs/current/ddl-partitioning.html
            %
            % Input   : - DbQuery object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableName' - name of table
            % Output  : - Cell array with list of partitions
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getPartitionTree('TableName', 'my_table')
            arguments
                Obj
                Args.TableName = '';
            end
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            Text = sprintf('SELECT * FROM pg_partition_tree(''%s'')', Args.TableName);
            Result = Obj.selectColumn(Text, 'relid');
        end
        

        function Result = createPartition(Obj, PartitionName, Args)
            % Create new table partition
            % Input   : - DbQuery object
            %           - PartitionName - name of new partition to create
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableName'  - Table name, if empty, Obj.TableName is used
            %             'Type'       - 'range' is the only type currently supported
            %             'RangeStart' - Start value of range partition 
            %             'RangeEnd'   - End value of range partition
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.CreatePartition('my_table_part1', 'TableName', 'my_table',
            %             'Type', 'range', 'RangeStart', 100, 'RangeEnd', 200);
            arguments
                Obj
                PartitionName           % Name of new partition
                Args.TableName = '';    % Table name
                Args.Type               % Currently only 'range' is supported
                Args.RangeStart         % Start of range
                Args.RangeEnd           % End of range
            end
            if isempty(Args.TableName)
                Args.TableName = Obj.TableName;
            end
            if strcmp(Args.Type, 'range')
                Text = sprintf('CREATE TABLE %s PARTITION OF %s FOR VALUES FROM (%f) TO (%f)', PartitionName, Args.TableName, Args.RangeStart, Args.RangeEnd);
                Result = Obj.exec(Text);
            else
                Obj.msgLog(LogLevel.Error, 'CreatePartition: unsupported partition type: %s', Args.Type);
                Result = false;
            end
        end

        
        function Result = isTableExist(Obj, TableName)
            % Check if specified table exists in current database
            % Input   : - DbQuery object
            %           - Table name to search
            % Output  : - true if table exists
            % Author  : Chen Tishler (2021)
            % Example : Obj.isTableExist('MyTable')
            [Schema, TableName] = db.DbQuery.getSchemaTable(TableName);
            Text = sprintf('SELECT table_name FROM information_schema.tables WHERE table_schema = ''%s'' AND table_name = ''%s'' ORDER BY table_name', lower(Schema), lower(TableName));
            List = Obj.selectColumn(Text, 'table_name');
            Result = any(strcmpi(List, TableName));
        end
        
        
        function Result = getTableColumnNames(Obj, TableName)
            % Get columns list of specified table as cell array
            % Input   : - DbQuery object
            %           - TableName
            % Output  : - Cell array with list of column names
            % Author  : Chen Tishler (2021)
            % Example : Obj.getTableColumnNames('master_table')
            Text = sprintf('SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ''%s'' ORDER BY column_name', TableName);
            Result = Obj.selectColumn(Text, 'column_name');
        end
                

        function Result = isColumnExist(Obj, TableName, ColumnName)
            % Check if column exist in the specified table
            % Input   : - DbQuery object
            %           - Table name
            %           - Column name
            % Output  : - true if column exist
            % Author  : Chen Tishler (2021)
            % Example : Obj.isColumnExist('master_table', 'my_column')
            % @Todo Schema = ...
            %  sql = "SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '{}' AND TABLE_NAME = '{}' AND column_name = '{}' ORDER BY column_name".\
            % format(self.get_schema(table_name).lower(), self.get_table(table_name).lower(), column_name.lower())
            %Text = sprintf('SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ''%s'' AND column_name = ''%s'' ORDER BY column_name', lower(TableName), lower(ColumnName));
            [Schema, TableName] = db.DbQuery.getSchemaTable(TableName);
            Text = sprintf('SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = ''%s'' AND TABLE_NAME = ''%s'' AND column_name = ''%s'' ORDER BY column_name', ...
                lower(Schema), lower(TableName), lower(ColumnName));
            
            List = Obj.selectColumn(Text, 'column_name');
            Result = any(strcmpi(List, ColumnName));
        end
        
        
        function Result = getTablePrimaryKey(Obj, TableName)
            % Get primary key column names of specified table
            % Input   : - DbQuery object
            %           - TableName
            % Output  : - Cell array with name(s) of primary key columns
            % Author  : Chen Tishler (2021)
            % Example : Obj.getTablePrimaryKey('master_table')
            Text = ['SELECT c.column_name, c.data_type '...
                'FROM information_schema.table_constraints tc '...
                'JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) '...
                'JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema '...
                'AND tc.table_name = c.table_name AND ccu.column_name = c.column_name '...
                'WHERE constraint_type = ''PRIMARY KEY'' and tc.table_name = ''' TableName ''''];
            Result = Obj.selectColumn(Text, 'column_name');
        end

        
        function Result = getTableIndexList(Obj, TableName)
            % Get list of index NAMES of specified table
            % Input   : - DbQuery object
            %           - TableName
            % Output  : - Cell array with list of index names
            % Author  : Chen Tishler (2021)
            % Example : Obj.getTableIndexList('master_table')
            Text = sprintf('SELECT * FROM pg_indexes WHERE tablename = ''%s''', TableName);
            Result = Obj.selectColumn(Text, 'indexname');
        end
        
        
        function Result = selectColumn(Obj, SqlText, ColumnName)
            % Get all rows of specified column from table as cell array.
            % Used for example by getTablesList()
            % Input   : - DbQuery object
            %           - SELECT query text
            %           - Column name to select from query result
            % Output  : - Cell array with returned values
            % Author  : Chen Tishler (2021)            
            % Example : Obj.selectColumn('SELECT COUNT(*) FROM master_table', 'count')
            
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
            % Input   : - DbQuery object
            %           - DbTableOrConn - See below
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.setConnection('unittest')
            
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
            % Input   : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.openConn()
            
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
            % Intput  : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : -
            
            Result = Obj.clear();
        end
        

        function [ServerFileName, ClientFileName] = getSharedFileName(Obj, FileName)
            % Prepare file names for server and client, based on folder sharing in 
            % configuration file.
            %
            % Input   : - DbQuery object
            %           - FileName - File name on client computer
            % Output  : - ServerFileName - File name to be accessed from server computer
            %           - ClientFileName - File name to be accessed from client computer
            % Author  : Chen Tishler (2021)
            % Example : [ServerFileName, ClientFileName] = Obj.getSharedFileName(Args.CsvFileName);

            % Prepare path, use ServerShareFileName in the COPY statement
            % so the file name will be local on the server, we will be able
            % to access over the network using ClientShareFileName
            % Note that our server should be Linux            
            [~, FName, Ext] = fileparts(FileName);
            ServerFileName = sprintf('%s%s%s%s', Obj.Conn.ServerSharePath, '/', FName, Ext);
            ClientFileName = fullfile(Obj.Conn.MountSharePath, strcat(FName, Ext));
        end
        
        
        function Result = clear(Obj)
            % Clear current statement and ResultSet
            % Input   : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.clear()
            
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
            % Input   : - DbQuery object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.clearResultSet()
            
            Obj.JavaResultSet = [];
            Obj.JavaMetadata = [];
            Obj.ColCount = 0;
            Obj.ColNames  = [];
            Obj.ColType  = [];
            Result = true;
        end


        function Result = getMetadata(Obj, Args)
            % query the metadata of the specified table or the current result-set
            % Input   : - DbQuery object
            %           - table name
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.getMetadata();
            
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
            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
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

            catch Ex
                Obj.msgLogEx(LogLevel.Error, Ex, 'DbQuery.open: getMetaData failed: %s', Obj.SqlText);
            end
        end


        function [SqlColumns, SqlValues] = makeInsertColumnsText(Obj, ColumnNames, Args)
            % Input   : - DbQuery object
            %           - ColumnNames - cell array with list of column names
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'TableColumnList' - When non-empty, ignore values from ColumnNames
            %             which are not in this list
            % Output  : - SqlColumns - Part of the INSERT statement for names, for example, 'ColName1,ColName2' 
            %           - SqlValues  - Part of the INSERT statement for values, for example, '?,?'
            % Author  : Chen Tishler (2021)
            % Example : [SqlColumns, SqlValues] = Obj.makeInsertColumnsText('fint,fdouble')
            
            arguments
                Obj
                ColumnNames
                Args.TableColumnList = [];   %
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
            %    ColumnList = Obj.getTableColumnNames(Args.TableName);
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


        function SqlColumns = makeUpdateColumnsText(Obj, ColumnNames)
            % *** CURRENTLY UNUSED ***
            % Prepare SQL text from cell array
            % Input   : - DbQuery object
            %           - ColumnNames -
            % Output  : - char-array
            % Author  : Chen Tishler (2021)
            % Example : -
            
            % "UPDATE master_table set RecID=?,FInt=? WHERE..."
            arguments
                Obj
                ColumnNames             %
            end

            SqlColumns = '';

            % Iterate struct fields
            disp(ColumnNames);

            for i = 1:numel(ColumnNames)
                ColumnName = ColumnNames{i};

                %
                if numel(SqlColumns) > 0
                    SqlColumns = [SqlColumns, ',' ColumnName, '=?']; %#ok<AGROW>
                else
                    SqlColumns = [SqlColumns, ColumnName, '=?']; %#ok<AGROW>
                end
            end

            %Obj.msgLog(LogLevel.Debug, 'makeUpdateColumnText: %s', SqlColumns);
        end


        function SqlColumns = makeWhereColumnsText(Obj, ColumnNames, Operand)
            % *** CURRENTLY UNUSED ***
            % Prepare SQL text from cell array, for example "WHERE RecID=? AND FInt=?..."
            % % Used internally by DbQuery.
            % Input   : - DbQuery object
            %           - ColumnNames -
            %           - Operand    - 'AND' ' / 'OR'
            % Output  : - char containing SQL text such as "WHERE RecID=? AND FInt=?..."
            % Author  : Chen Tishler (2021)
            % Example : makeWhereColumnsText(ColumnList, 'OR')
           
            arguments
                Obj
                ColumnNames     %
                Operand         % 'AND' / 'OR'
            end

            SqlColumns = '';

            % Iterate struct fields
            disp(ColumnNames);

            for i = 1:numel(ColumnNames)
                ColumnName = ColumnNames{i};

                %
                if numel(SqlColumns) > 0
                    SqlColumns = [SqlColumns, ' ', Operand, ' ', ColumnName, '=?']; %#ok<AGROW>
                else
                    SqlColumns = [SqlColumns, ColumnName, '=?']; %#ok<AGROW>
                end
            end

            %Obj.msgLog(LogLevel.Debug, 'makeWhereColumnsText: %s', SqlColumns);
        end


        function Result = setStatementValues(Obj, Rec, FirstRecord, RecordCount, Args)
            % Set statement values from specified DbRecord or struct.
            % Used internally by DbQuery.
            % Input   : - DbQuery object
            %           - Rec               - DbRecord object with data
            %           - FirstRecord       - Index of first row to process
            %           - RecordCount       - Number of rows to process
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            
            %             'ColumnNames'     - List of column names
            %             'StartIndex'      - Index of first column in current JavaStatement
            %             'TableColumnList' - When not empty, ignore columns not in this list
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : setStatementValues(Rec, 1, 1)
            arguments
                Obj                     %
                Rec                     % DbRecord
                FirstRecord             %
                RecordCount             %
                Args.ColumnNames = [];  % cell
                Args.StartIndex = 1     % Index of first column in current JavaStatement
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
                    elseif isa(Value, 'datetime')
                        d = Value;
                        Sec = d.Second;
                        V = java.sql.Timestamp(d.Year-1900, d.Month-1, d.Day, d.Hour, d.Minute, floor(Sec), d.Second-floor(Sec));
                        Obj.JavaStatement.setTimestamp(Index, V);                        
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
            % Convert specified string to valid MATLAB property name or
            % valid struct-field name, replace non-valid chars with '_'
            % Input   : - DbQuery object
            %           - char array with column name, i.e. 'My:Field'
            % Output  : - char array, i.e. 'My_Field'
            % Author  : Chen Tishler (2021)
            % Example : ColumnNames = getValidColumnName('My:Field') -> 'My_Field'
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
            % Input   : - DbQuery object
            %           - ColumnType - Type such as 'Integer', 'Double', etc, according 
            %                          to MATLAB data types
            % Output  : - Cell array with list of all columns that match the specified type.
            % Author  : Chen Tishler (2021)
            % Example : ColNames = Q.getColumnNamesOfType('Double');
            
            arguments
                Obj
                ColumnType      % Specified column type
            end

            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            %Obj.msgLog(LogLevel.Debug, 'getColumnNamesOfType: %s', ColumnType);

            Result = {};
            for ColIndex = 1:Obj.ColCount
                if strcmp(Obj.ColType{ColIndex}, ColumnType)
                    Result{end+1} = Obj.ColNames{ColIndex};
                end
            end
        end


        function Result = getDbVersion(Obj)
            % Query Postgres version, note that DbQuery must be linked to DbConnection
            % Input   : - DbQuery object
            % Output  : - char-array, such as 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
            % Author  : Chen Tishler (2021)
            % Example : Ver = DbQuery.getDbVersion()
            
            Result = [];
            Valid = false;
            Obj.query('SELECT version()');
            if Obj.ColCount == 1
                Result = Obj.getColumn('version');
            
                % Check that the output is valid
                Valid = contains(Result, 'PostgreSQL');
            end
            
            if ~Valid
                Obj.msgLog(LogLevel.Warning, 'getDbVersion: Invalid result: %s', Result);
            end            
        end
        
        
        function Result = writeBinaryFile(Obj, Rec, FileName)
            % @Todo *** CURRENTLY UNUSED - NOT COMPLETED YET ***
            % Write DbRecord object to binary file for COPY FROM opeation 
            % For internal use only.
            % 
            % Input   : - DbQuery object
            %           - Rec - DbRecord filled with struct array (Rec.Data)
            %           - FileName - Output file name
            % Output  : - true on success
            % Example : writeBinaryFile(Rec, '/tmp/myfile.dat')
            % Author  : Chen Tishler (2021)
            % @Todo - Implement this function in MEX?
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
    
    %----------------------------------------------------------------------
    
    methods % Database creating & modification
        
        function Result = createSchema(Obj, SchemaName)
            % Create database, from .XLSX or .SQL file
            %
            % Input : - DbQuery object
            %         - Schema name to create
            %
            % Output  : true on success
            % Author  : Chen Tishler (2022)
            % Example : createSchema('myschema')
            SqlText = sprintf('CREATE SCHEMA IF NOT EXISTS %s;', SchemaName);
            Result = Obj.exec(SqlText);           
        end

        
        function Result = createTablespace(Obj, TablespaceName, Path)
            % Create table space on specified path.
            % See: https://www.postgresql.org/docs/current/manage-ag-tablespaces.html
            % The location must be an existing, empty directory that is owned by the 
            % PostgreSQL operating system user. All objects subsequently created within 
            % the tablespace will be stored in files underneath this directory.
            % The location must not be on removable or transient storage, as the cluster 
            % might fail to function if the tablespace is missing or lost.
            %
            % Create new folder:
            %   sudo mkdir /data2/pgdata
            %   sudo chown postgres:postgres /data2/pgdata
            %   sudo chmod 700 /data2/pgdata
            %
            % Input : - DbQuery object
            %         - Tablespace name to create
            %         - Path to folder owned by PostgreSQL operating system user
            %
            % Output  : true on success
            % Author  : Chen Tishler (2022)
            % Example : createTablespace('myspace')
            SqlText = sprintf('CREATE TABLESPACE %s LOCATION ''%s'';', TablespaceName, Path);
            Result = Obj.exec(SqlText);           
        end
        
        
        function Result = createDb(Obj, Args)
            % Create database, from .XLSX or .SQL file
            %
            % Input : - DbQuery object
            %         * Pairs of ...,key,val,...
            %           The following keys are available:            			            
            %           'DatabaseName' - Database name, create empty database (without XLS or SQL)
            %           'CheckExist'   - True (default) to check if database already exists, and ignore
            %           'Drop'         - True to drop (delete) the database before creating it (default is false)
            %           'XlsxFileName' - .xlsx file from Google Sheets - Specify XLSX file
            %           'SqlFileName'  - .sql file name - Specify SQL script to run
            %           'Tablespace'   - The name of the tablespace that will be associated with the new database, 
            %                            or DEFAULT to use the template database's tablespace. This tablespace will 
            %                            be the default tablespace used for objects created in this database. 
            %                            See CREATE TABLESPACE for more information.
            %
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : createDb(DatabaseName='MyDb');
            % Instructions:
            %    1. Download database definition from Google Sheet by selecting
            %       File -> Download -> (XLS)
            %
            
            arguments
                Obj
                Args.DatabaseName   = ''        % Database name, create empty database (without XLS or SQL)
                Args.XlsxFileName   = ''        %
                Args.SqlFileName    = ''        %                
                Args.CheckExist     = true      % When true, check if database exist before executing 
                Args.Tablespace     = ''        % 
            end
            
            Result = false;
                      
            % 
            if ~isempty(Args.DatabaseName)
                if Args.CheckExist
                    if Obj.isDbExist(Args.DatabaseName)
                        Result = true;
                        return;
                    end
                end
                
                SqlText = sprintf('CREATE DATABASE %s WITH TEMPLATE = template0 ENCODING = ''UTF8''', Args.DatabaseName);
                
                % Add tablespace
                if ~isempty(Args.Tablespace)
                    SqlText = strcat(SqlText, ' TABLESPACE ', Args.Tablespace);
                end
                
                Result = Obj.exec(SqlText);
                return;
            end           
        
            % Execute SQL file using psql
            if ~isempty(Args.SqlFileName)
                if isfile(Args.SqlFileName)
                    Obj.runPsql('SqlFileName', Args.SqlFileName);
                    Result = true;
                else
                    Obj.msgLog(LogLevel.Error, 'createDb: Input SQL file not found: %s', Args.SqlFileName);
                end
                
            % Extract database definition from XLS file and execute the
            % resulting SQL file using psql
            elseif ~isempty(Args.XlsxFileName)
                if isfile(Args.XlsxFileName)
                    SqlFileName = Obj.xlsx2sql(Args.XlsxFileName);
                    if ~isempty(SqlFileName) && isfile(SqlFileName)
                        Obj.runPsql('SqlFileName', SqlFileName);
                        Result = true;
                    end
                else
                    Obj.msgLog(LogLevel.Error, 'createDb: Input XLSX file not found: %s', Args.XlsxFileName);
                end               

            % Create empty database
            elseif ~isempty(Args.DatabaseName)
                SqlText = sprintf('CREATE DATABASE %s WITH TEMPLATE = template0 ENCODING = ''UTF8'';', Args.DatabaseName);
                Result = Obj.exec(SqlText);
            end
            
        end
             
        
        function Result = createConnectionConfig(Obj, Args)
            % Create database connection in config/local/Database.DbConnections.UnitTest.yml
            % Input : - DbQuery object
            %         * Pairs of ...,key,val,...
            %           The following keys are available:            			            
            %           'FileName'        = ''                %
            %           'DatabaseName'    = ''                %
            %           'Host'            = 'localhost'       % Host name or IP address
            %           'Port'            = 5432              %
            %           'DriverName'      = 'postgres'        % Driver name
            %           'UserName'        = ''                % Login user
            %           'Password'        = ''                % Login password
            %           'ServerSharePath' = '' %              % Path to shared folder on the server, for COPY statements
            %           'MountSharePath'  = ''
            %           'WinMountSharePath'  = ''
            % Output : - Configuration file name on success
            % Author : Chen Tishler (2021)
            % Example: db.DbQuery.createConnectionConfig('DatabaseName', 'unittest5', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
            % Database.DbConnections.UnitTest.yml
            
            arguments
                Obj
                Args.FileName        = ''                %
                Args.DatabaseName    = ''                %
                Args.Host            = 'localhost'       % Host name or IP address
                Args.Port            = 5432              %
                Args.DriverName      = 'postgres'        % Driver name
                Args.UserName        = ''                % Login user
                Args.Password        = ''                % Login password
                Args.ServerSharePath = '/var/samba/pgshare'   % Path to shared folder on the server, for COPY statements
                Args.MountSharePath  = '/media/gauss_pgshare' %
                Args.WinMountSharePath  = 'S:\'               %
            end
            
            % Prepare file name if not specified
            Result = [];
            if isempty(Args.FileName)
                ConfigPath = fullfile(tools.os.getAstroPackConfigPath(), 'local');
                Args.FileName = fullfile(ConfigPath, strcat('Database.DbConnections.', Args.DatabaseName, '.yml'));
            end
            
            % Create file
            Fid = fopen(Args.FileName, 'wt');
            if Fid > -1
                fprintf(Fid, '# %s\n\n',                        Args.FileName);
                fprintf(Fid, 'DatabaseName    : ''%s''\n',      Args.DatabaseName);
                fprintf(Fid, 'Host            : ''%s''\n',      Args.Host);
                fprintf(Fid, 'Port            : %d\n',          Args.Port);
                fprintf(Fid, 'DriverName      : ''postgres''\n');
                fprintf(Fid, 'UserName        : ''%s''\n',      Args.UserName);
                fprintf(Fid, 'Password        : ''%s''\n',      Args.Password);
                fprintf(Fid, 'ServerSharePath : ''%s''\n',      Args.ServerSharePath);
                fprintf(Fid, 'MountSharePath  : ''%s''\n',      Args.MountSharePath);
                fprintf(Fid, 'WinMountSharePath : ''%s''\n',    Args.WinMountSharePath);
                fclose(Fid);
                
                if isfile(Args.FileName)
                    Result = Args.FileName;
                end
            end            
        end
              
        
        function Result = createTable(Obj, Args)
            % Create database table, @Todo
            % Input   : - DbQuery object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			                        
            %             'SqlText'       - SQL text to execute
            %             'SqlFileName'   - File name of SQL script to execute
            %             'TableName'     - Table name, if not specified, Obj.TableName is used
            %             'PrimaryKeyDef' - Definition of primary key (SQL)
            %             'AutoPk'        - Name of auto-increment primary
            %                               key field (BIGINT), if specified, we use Postgres'
            %                               IDENTITY COLUMN or UUID (when UuidPk=true)
            %             'UuidPk'        - True to create primary key as UUID instead of IDENTITY, default is false
            %             'CheckExist'    - True (default) to check if database already exists, and ignore
            %             'Drop'          - True to drop (delete) the table before creating it (default is false)            
            %             'Tablespace'    - Specify TABLESPACE name to for the new table
            %
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.createTable('TableName', 'my_table', 'AutoPk', 'pk')
            % Refs    : https://www.postgresql.org/docs/8.0/sql-createuser.html
            % SQL     : [DROP TABLE IF EXISTS customers CASCADE;]
            %           CREATE TABLE customers (
            %             id SERIAL PRIMARY KEY,
            %             customer_name VARCHAR NOT NULL
            %           );
            %
            arguments
                Obj
                Args.SqlText        = ''
                Args.SqlFileName    = ''
                Args.TableName      = ''
                Args.PrimaryKeyDef  = ''
                Args.AutoPk         = ''        
                Args.UuidPk         = false
                Args.CheckExist     = true
                Args.Drop           = false;    
                Args.Tablespace     = '';
            end

            Result = false;
            SqlText = '';
            
            %
            if ~isempty(Args.TableName) && Args.CheckExist && ~Args.Drop
                if Obj.isTableExist(Args.TableName)
                    Result = true;
                    return;
                end
            end
            
            % Drop existing table
            if ~isempty(Args.TableName) && Args.Drop
                if Obj.isTableExist(Args.TableName)
                    SqlText = sprintf('DROP TABLE IF EXISTS %s;', Args.TableName);
                    Result = Obj.exec(SqlText);
                end
            end
            
                   
            % SQL text
            if ~isempty(Args.SqlText)
                SqlText = Args.SqlText;
                
            % SQL file name
            elseif ~isempty(Args.SqlFileName)
                SqlText = fileread(Args.SqlFileName);

            % Table name with auto-increment primary key
            elseif ~isempty(Args.TableName) && ~isempty(Args.AutoPk)
                if Args.UuidPk
                    SqlText = sprintf('CREATE TABLE %s (%s UUID DEFAULT gen_random_uuid() PRIMARY KEY);', Args.TableName, Args.AutoPk); 
                else
                    SqlText = sprintf('CREATE TABLE %s (%s BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY);', Args.TableName, Args.AutoPk); 
                end
                
            % Table name with Primary key
            elseif ~isempty(Args.TableName) && ~isempty(Args.PrimaryKeyDef)
                SqlText = sprintf('CREATE TABLE %s (%s PRIMARY KEY)', Args.TableName, Args.PrimaryKeyDef);
            end
            
            % Add TABLESPACE
            if ~isempty(Args.Tablespace)
                SqlText = strcat(SqlText, ' TABLESPACE ', Args.Tablespace);
            end
            
            if ~isempty(SqlText)
                Result = Obj.exec(SqlText);
            end
        end
        
        
        function Result = dropTable(Obj, TableName)
            % Drop database
            %
            % Input : - DbQuery object
            %         - Table name to drop            
            % Output  : true on success
            % Author  : Chen Tishler (2022)
            % Example : dropTable('MyTable');
            
            SqlText = sprintf('DROP TABLE IF EXISTS %s;', TableName);
            Result = Obj.exec(SqlText);            
        end
        
        
        function Result = addColumn(Obj, TableName, ColumnName, DataType, ColumnDef, Args)
            % Add single or multiple columns to table
            % Input   : - DbQuery object
            %           - Table name
            %           - Column name char
            %           - DataType   - Data field type, INTEGER, etc. see
            %             https://www.postgresql.org/docs/current/datatype.html
            %           - Additional text for column definition, i.e. DEFAULT ...
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			                        
            %             'CheckExist'    - True (default) to check if column already exists
            %             'Index'         - True to create index
            %             'Comment'       - Comment text, specifiy 'NULL' to remove existing comment            
            % Output  : true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.addColumn('master_table', 'MyColA', 'INTEGER', 'DEFAULT 0')
            % Refs    : https://www.postgresqltutorial.com/postgresql-add-column/
            % SQL     : ALTER TABLE table_name ADD COLUMN column_name1 data_type constraint;
            arguments
                Obj
                TableName               %
                ColumnName              %
                DataType                %
                ColumnDef               %
                Args.CheckExist = true  %
                Args.Comment = ''       %
                Args.Index = false      % 
            end

            DType = DataType;
                
            % Fix data types
            if strcmpi(DType, 'double')
                DType = 'DOUBLE PRECISION';
            elseif strcmpi(DType, 'float') || strcmpi(DType, 'single')
                DType = 'REAL';
            end

            SqlText = sprintf('ALTER TABLE %s ADD COLUMN %s %s %s;', TableName, ColumnName, DType, ColumnDef);          
            Result = Obj.exec(SqlText);
            if Args.Index
                Obj.addIndexOnColumn(TableName, ColumnName);
            end
            
            % Add/remove column comment
            if ~isempty(Args.Comment)
                if strcmp(Args.Comment, 'NULL')
                    SqlText = sprintf('COMMENT ON COLUMN %s.%s IS NULL;', TableName, ColumnName);
                else
                    SqlText = sprintf('COMMENT ON COLUMN %s.%s IS ''%s''', TableName, ColumnName, Args.Comment);
                end
                Result = Obj.exec(SqlText);
            end
        end

        
        function Result = getColumnComment(Obj, DbName, TableName, ColumnName)
            % Query column comment
            % Input   : - DbQuery object
            %           - Database name
            %           - Table name
            %           - Column name
            % Output  : Column comment char array
            % Author  : Chen Tishler (2022)
            % Example : Obj.getColumnComment('mydb1', 'mytable1', 'f2')
            SqlText = strcat('SELECT cols.column_name, (SELECT pg_catalog.col_description(c.oid, cols.ordinal_position::int) ', ...
                ' FROM pg_catalog.pg_class c WHERE c.oid = (SELECT cols.table_name::regclass::oid) AND c.relname = cols.table_name ', ...
                ' ) as column_comment FROM information_schema.columns cols WHERE cols.table_catalog = ''%s'' AND ', ...
                ' cols.table_name = ''%s'' AND cols.column_name = ''%s''');

            SqlText = sprintf(SqlText, DbName, TableName, ColumnName);
            Result = Obj.selectColumn(SqlText, 'column_comment');                   
        end
        
        
        function Result = getTableColumnsComments(Obj, DbName, TableName)
            % Query comments of all columns of table, return dict of [column_name] = comment_comment
            % Input   : - DbQuery object
            %           - Database name
            %           - Table name
            % Output  : Struct array, for each column: column_name, column_comment
            % Author  : Chen Tishler (2022)
            % Example : Obj.getColumnComment('mydb1', 'mytable1', 'f2')        

            SqlText = strcat('SELECT cols.column_name, (SELECT pg_catalog.col_description(c.oid, cols.ordinal_position::int) ', ...
                ' FROM pg_catalog.pg_class c WHERE c.oid = (SELECT cols.table_name::regclass::oid) AND c.relname = cols.table_name ', ...
                ' ) as column_comment FROM information_schema.columns cols WHERE cols.table_catalog = ''%s'' AND cols.table_name = ''%s''');

            SqlText = sprintf(SqlText, DbName, TableName);
            Result = Obj.query(SqlText);
            Result = Obj.loadResultSet();
            Result = Result.Data;
        end
        
        
        function Result = isIndexExist(Obj, TableName, IndexName)
            % Check if specified index exists
            % Input   : - DbQuery object
            %           - Table name to be altered
            %           - Index name
            % Output  : - true if index exsits
            % Author  : Chen Tishler (2022)
            % Example : isIndexExist('master_table', 'idx_master_table_str1')          
            [Schema, TableName] = db.DbQuery.getSchemaTable(TableName);
            SqlText = sprintf('SELECT * FROM pg_indexes WHERE schemaname = ''%s'' AND tablename = ''%s''', Schema, TableName);
            List = Obj.selectColumn(SqlText, 'indexname');
            Result = any(strcmpi(List, IndexName));
        end
       
        
        function Result = addIndex(Obj, TableName, IndexName, Columns, Args)
            % Add single index to table, may include one or multiple fields
            % Input   : - DbQuery object
            %           - Table name to be altered
            %           - Unique index name, usually composed as TableName_idx_FieldNames, 
            %             for example 'master_table_idx_FDouble2'
            %           - Column names, comma separated
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			                        
            %             'CheckExist'    - True (default) to check if index already exists
            %             'Tablespace'    - Tablespace name
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : addIndex('master_table', 'master_table_idx_FDouble2', 'USING btree (FDouble2)');
            % Refs    : https://www.postgresql.org/docs/9.1/sql-createindex.html
            %           https://www.postgresqltutorial.com/postgresql-indexes/postgresql-create-index/
            % SQL     : CREATE INDEX index_name ON table_name [USING method]
            %            (
            %               column_name [ASC | DESC] [NULLS {FIRST | LAST }],
            %               ...
            %            );
            arguments
                Obj                 %
                TableName           %
                IndexName           %
                Columns             %
                Args.CheckExist = true %
                Args.Tablespace = ''
            end
                        
            if Args.CheckExist
                if Obj.isIndexExist(TableName, IndexName)
                    Result = true;
                    return
                end
            end
            
            SqlText = sprintf('CREATE INDEX %s ON %s USING BTREE(%s)', IndexName, TableName, Columns);
            
            if ~isempty(Args.Tablespace)
                SqlText = strcat(SqlText, ' TABLESPACE ', Args.Tablespace);
            end
            
            Result = Obj.exec(SqlText);
        end
        

        function Result = addIndexOnColumn(Obj, TableName, ColumnName, Args)
            % Add single index to table, on specified column, index name is generated as 'idx_TableName_ColumnName'
            % Input   : - DbQuery object
            %           - Table name to be altered
            %           - Column name
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			                        
            %             'CheckExist'    - True (default) to check if index already exists
            %             'Tablespace'    - Tablespace name
            % Output  : - true on success
            % Author  : Chen Tishler (2022)
            % Example : addIndexOnColumn('master_table', 'fdouble2')
            arguments
                Obj                      %
                TableName                %
                ColumnName               %
                Args.CheckExist = true   %
                Args.Tablespace = ''
            end
                        
            IndexName = sprintf('%s_idx_%s', TableName, ColumnName);
            Result = Obj.addIndex(TableName, IndexName, ColumnName, 'CheckExist', Args.CheckExist, 'Tablespace', Args.Tablespace);
        end
        
        
        function Result = getTablespaceList(Obj)
            % Get list of tablespaces
            % Input   : - DbQuery object
            % Output  : - Cell array with list of tablespaces
            % Author  : Chen Tishler (2022)
            % Example : List = Obj.getTablespaceList()
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = 'SELECT spcname FROM pg_tablespace';
            Result = Obj.selectColumn(Text, 'spcname');
        end
        
        function Result = getDbList(Obj)
            % Get list of databases
            % Input   : - DbQuery object
            % Output  : - Cell array with list of databases
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getDbList()
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = 'SELECT datname FROM pg_database WHERE datistemplate = false';
            Result = Obj.selectColumn(Text, 'datname');
        end
        
        
        function Result = isDbExist(Obj, DbName)
            % Check if the specified database exists
            % Input   : - DbQuery object
            %           - DbName - database name to check for
            % Output  : - true if exists
            % Author  : Chen Tishler (2021)
            % Example : Obj.isDbExist('my_database_name')
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = sprintf('SELECT datname FROM pg_database WHERE datistemplate = false AND datname = ''%s''', lower(DbName));
            List = Obj.selectColumn(Text, 'datname');
            Result = any(strcmpi(List, DbName));
        end
        
    end
    
    %----------------------------------------------------------------------
    methods % User Management
        
        % https://www.postgresql.org/docs/14/user-manag.html
        %
        % PostgreSQL manages database access permissions using the concept of roles.
        % A role can be thought of as either a database user, or a group of database
        % users, depending on how the role is set up. Roles can own database objects
        % (for example, tables and functions) and can assign privileges on those
        % objects to other roles to control who has access to which objects.
        % Furthermore, it is possible to grant membership in a role to another role,
        % thus allowing the member role to use privileges assigned to another role.
        %
        % The concept of roles subsumes the concepts of "users" and "groups".
        % In PostgreSQL versions before 8.1, users and groups were distinct kinds
        % of entities, but now there are only roles. Any role can act as a user,
        % a group, or both.
        %
        % Database roles are conceptually completely separate from operating
        % system users. In practice it might be convenient to maintain a
        % correspondence, but this is not required. Database roles are global
        % across a database cluster installation (and not per individual database).
        %
        % Every connection to the database server is made using the name of
        % some particular role, and this role determines the initial access
        % privileges for commands issued in that connection.
        % The role name to use for a particular database connection is indicated
        % by the client that is initiating the connection request in an
        % application-specific fashion. For example, the psql program uses
        % the -U command line option to indicate the role to connect as.
        % Many applications assume the name of the current operating system
        % user by default (including createuser and psql).
        % Therefore it is often convenient to maintain a naming correspondence
        % between roles and operating system users.
        %
        % CREATE ROLE admin WITH LOGIN SUPERUSER CREATEDB CREATEROLE PASSWORD 'Passw0rd';
        %
                
        function Result = addUser(Obj, UserName, Password, Args)
            % Add database user
            % Input   : - DbQuery object
            %           - User name
            %           - Password string
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'DatabaseName' - If specified user will be granted only for this database
            %             'Permission'   - 'read', 'write', 'full'
            % Output  : true on sucess
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.addUser('robert', 'pass123')
            % Refs    : https://www.postgresql.org/docs/8.0/sql-createuser.html
            %           https://stackoverflow.com/questions/760210/how-do-you-create-a-read-only-user-in-postgresql
            % SQL     : CREATE USER user user_name WITH ENCRYPED PASSWORD 'mypassword';
            %           GRANT ALL PRIVILEGES ON DATABASE sample_db TO user_name;
            %
            % @Todo   : Need to research and learn more about creating read-only users
            %
            arguments
                Obj                     %
                UserName                %
                Password                %
                Args.DatabaseName = ''  %
                Args.Permission = ''    %
            end
            
            % 1. Create new user
            SqlText = sprintf('CREATE USER %s WITH PASSWORD ''%s''', UserName, Password);
            Result = Obj.exec(SqlText);
            
            % 2. Grant the CONNECT access
            SqlText = sprintf('GRANT CONNECT ON DATABASE %s TO %s', Args.DatabaseName, UserName);
            Result = Obj.exec(SqlText);
            
            % 3. Grant full access
            if ~isempty(Args.DatabaseName) && (strcmp(Args.Permission, 'write') || strcmp(Args.Permission, 'full'))
                SqlText = sprintf('GRANT ALL PRIVILEGES ON DATABASE %s TO %s', Args.DatabaseName, UserName);
                Result = Obj.exec(SqlText);
            end
            
            % Create read-only user on specified database
            % https://ubiq.co/database-blog/how-to-create-read-only-user-in-postgresql/
            if ~isempty(Args.DatabaseName) && strcmp(Args.Permission, 'read')
                
                % Assign permission to this read only user
                SqlText = sprintf('GRANT USAGE ON SCHEMA public TO %s', UserName);
                Result = Obj.exec(SqlText);
                
                % Assign permission to this read only user
                SqlText = sprintf('GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s', UserName);
                Result = Obj.exec(SqlText);
                
                % Assign permissions to read all newly tables created in the future
                SqlText = sprintf('ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO %s', UserName);
                Result = Obj.exec(SqlText);
            end
        end
        
        
        function Result = removeUser(Obj, UserName)
            % Remove specified user from database users list
            %
            % NOTE: This function may fail if there are dependecies between
            % the user and other objects such as tables. Use pgAdmin to remove users
            % and dependecies.
            %
            % Input   : - DbQuery object
            %           - UserName to remove
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.removeUser('robert')
            % Refs    : https://www.postgresql.org/docs/9.4/sql-dropuser.html
            % SQL     : DROP USER [ IF EXISTS ] name [, ...]
            arguments
                Obj                 %
                UserName            %
            end

            SqlText = sprintf('DROP USER IF EXISTS %s', UserName);
            Result = Obj.exec(SqlText);
        end
        
        
        function Result = getUserList(Obj)
            % Get database users list. 
            % Note that users are per server, and may be used with multiple databases.
            % Input   : - DbQuery object
            % Output  : - Cell array with names of database users
            % Author  : Chen Tishler (2021)
            % Example : List = Obj.getUserList()
            % Refs    : https://www.postgresqltutorial.com/postgresql-list-users/
            
            Text = [...
                'SELECT usename AS role_name, '...
                '  CASE '...
                '     WHEN usesuper AND usecreatedb THEN '...
                '       CAST(''superuser, create database'' AS pg_catalog.text) '...
                '     WHEN usesuper THEN '...
                '        CAST(''superuser'' AS pg_catalog.text) '...
                '     WHEN usecreatedb THEN '...
                '        CAST(''create database'' AS pg_catalog.text) '...
                '     ELSE '...
                '        CAST('''' AS pg_catalog.text) '...
                '  END role_attributes '...
                'FROM pg_catalog.pg_user '...
                'ORDER BY role_name desc; '...
                ];
            
            Result = Obj.selectColumn(Text, 'role_name');
        end

    end
    %----------------------------------------------------------------------
    
    methods(Hidden)
        
        function Result = setConn(Obj, Conn)
            % Set connection data from the specified DbConnection object
            % Input   : - DbQuery object
            %           - Conn - DbConnection object
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : Obj.setConn(DbCon);
            
            Obj.Conn = Conn;
            Obj.Host = Conn.Host;
            Obj.Port = Conn.Port;
            Obj.DatabaseName = Conn.DatabaseName;
            Obj.UserName = Conn.UserName;
            Obj.Password = Conn.Password;
            Result = true;
        end
               
        
        function Result = xlsx2sql(Obj, XlsxFileName)
            % Convert XLSX file downloaded from Google Drive to SQL file, using xlsx2sql.py
            % Note: Requires ULTRASAT repository and ULTRASAT_PATH environment
            %       var to be set correctly.
            % Note: python3 (python3.exe on Windows) should be on PATH ('System' PATH on Windows)
            % Input   : - DbQuery object
            %           - XlsFileName
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.xlsx2sql('c:\temp\_xls\unittest.xlsx')
            arguments
                Obj             % 
                XlsxFileName    %
            end
            
            Result = '';
            if ~isfile(XlsxFileName)
                return;
            end
                
            PWD = pwd;
            try
               
                % Search xlsx2sql.py in Ultrasat folder
                Path = tools.os.getUltrasatPath();
                flist = [];
                if ~isempty(Path) && isfolder(Path)
                    flist = dir(fullfile(Path, '**\xlsx2sql.py'));
                    if numel(flist) == 0
                        Path = tools.os.getAstroPackPath();
                        if ~isempty(Path) && isfolder(Path)
                            flist = dir(fullfile(Path, '**\xlsx2sql.py'));                        
                        end
                    end
                end
                if numel(flist) > 0
                    PyScript = fullfile(flist(1).folder, flist(1).name);
                else
                    io.msgLog(LogLevel.Error, 'xlsx2sql.py: xlsx2sql.py not found in source code folders');
                    return;
                end
                    
                
                % Prepare path to Python script
                [Path, FName] = fileparts(XlsxFileName);
                cd(Path);
                if ~isfile(PyScript)
                    io.msgLog(LogLevel.Info, 'xlsx2sql.py: File not found: %s', 'xlsx2sql.py');
                    return;
                end
                
                % Prepare command line, assume we have 'python3' installed
                % Note: python3 (python3.exe on Windows) should be on
                % SYSTEM PATH (not USER PATH).
                % See: https://stackoverflow.com/questions/47539201/python-is-not-recognized-windows-10
                % For example: Add both C:\Python38 and C:\Python38\Scripts
                Args = sprintf('-x %s', XlsxFileName);
                Obj.msgLog(LogLevel.Info, 'xlsx2sql.py: %s', Args);
                
                % Execute python3 xlsx2sql.py, this may take a while...
                Obj.msgLog(LogLevel.Info, 'xlsx2sql.py: Executing %s', PyScript);
                
                [Status, Output] = tools.os.runPython(PyScript, 'ArgsStr', Args);
                if Status ~= 0
                    Obj.msgLog(LogLevel.Error, 'xlsx2sql.py: FAILED to execute, make sure that python3 is found on your PATH: %s', PyScript);
                end
                
                % Prepare file name of generated SQL
                SqlFileName = sprintf('%s%s%s_postgres.sql', FName, filesep, FName);
                
                % Check that SQL file was created in current folder
                if isfile(SqlFileName)
                    SqlFileName = fullfile(pwd, SqlFileName);
                    if isfile(SqlFileName)
                        Result = SqlFileName;
                    else
                        io.msgLog(LogLevel.Error, 'xlsx2sql.py: SQL file was not generated in current folder: %s', SqlFileName);
                        SqlFileName = '';
                    end
                else
                    Obj.msgLog(LogLevel.Error, 'xlsx2sql.py: SQL file was not generated: %s', SqlFileName);
                    SqlFileName = '';
                end
                
            catch Ex
                Obj.MsgLogEx(LogLevel.Error, Ex, 'xlsx2sql');
            end
            cd(PWD);
        end
        
        
        function Result = runPsql(Obj, Args)
            % Run the 'psql' external utility with command line parameters.
            % Input   : - DbQuery object
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'Host'          - Host name or IP address to connect
            %             'Port'          - Port number, 0 to use default
            %             'DatabaseName'  - Database name, use 'postgres' to when creating databases or for general
            %             'UserName'      - User name
            %             'Password'      - Password
            %             'SqlFileName'   - SQL file name to be executed
            %             'Params'        - Additional parameters for command line            
            %
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.runPsql(
            %               'psql -h gauss -p 5432 -U admin -W -d postgres -f unittest_postgres.sql')
            % Note    : psql (psql.exe on Windows) must be on the system PATH
            %           i.e. Add C:\Program Files\PostgreSQL\14\bin to SYSTEM PATH
            %
            arguments
                Obj
                Args.Host          = ''        % Host name or IP address to connect
                Args.Port          = 0         % Port number, 0 to use default
                Args.DatabaseName  = ''        % Database name, use 'postgres' to when creating databases or for general
                Args.UserName      = ''        % User name
                Args.Password      = ''        % Password
                Args.SqlFileName   = ''        % SQL file name to be executed
                Args.Params        = ''        % Additional parameters for command line
            end

            Result = false;
            
            if isempty(Args.Host)
                Args.Host = Obj.Host;
            end
            
            if Args.Port == 0
                Args.Port = Obj.Port;
            end

            if isempty(Args.DatabaseName)
                Args.DatabaseName = Obj.DatabaseName;
            end

            if isempty(Args.UserName)
                Args.UserName = Obj.UserName;
            end
        
            if isempty(Args.Password)
                Args.Password = Obj.Password;
            end

            try
                                
                % Prepare command line
                Cmd = sprintf('psql -h %s -p %d -U %s -w', Args.Host, Args.Port, Args.UserName);
                
                % -d
                if ~isempty(Args.DatabaseName)
                    Cmd = sprintf('%s -d %s', Cmd, Args.DatabaseName);
                end
                
                % -f
                if ~isempty(Args.SqlFileName)
                    Cmd = sprintf('%s -f %s', Cmd, Args.SqlFileName);
                end
                
                % Additional params
                if ~isempty(Args.Params)
                    Cmd = sprintf('%s %s', Cmd, Args.Params);
                end
                
                % Password
                if ~isempty(Args.Password)
                    
                    % Windows - note that we MUST NOT have a spaces next to '&&'
                    if tools.os.iswindows()
                        Cmd = sprintf('set PGPASSWORD=%s&&%s', Args.Password, Cmd);
                        
                    % Linux - use 'export'
                    else
                        
                        % Check which shell we use
                        if isempty(Obj.Shell)
                            Obj.Shell = getenv('SHELL');
                        end
                        
                        % bash / tcsh
                        if contains(Obj.Shell, 'tcsh')
                            Cmd = sprintf('setenv PGPASSWORD ''%s'' ; %s', Args.Password, Cmd);
                        else
                            Cmd = sprintf('export PGPASSWORD=''%s'' ; %s', Args.Password, Cmd);
                        end
                    end
                end
                
                Obj.msgLog(LogLevel.Info, 'psql: system( %s )', Cmd);
                [Status, Output] = system(Cmd);
                Obj.msgLog(LogLevel.Info, 'psql: %d', Status);
                Obj.msgLog(LogLevel.Info, 'psql: %s', Output);
                if Status ~= 0
                    Obj.msgLog(LogLevel.Error, 'runPsql: FAILED to execute, make sure that psql is found on your PATH: %s', Cmd);
                end
                Result = true;
            catch Ex
                Obj.msgLogEx(LogLevel.Info, Ex, 'psql');
            end
        end
                
    end

    %----------------------------------------------------------------------
    
    methods(Static)
        
        function Result = startGui()
            % Run gui utility process - @TODO - Not completed - DO NOT USE!
            % Input   : -
            % Output  : - true on success
            % Author  : Chen Tishler (2021)
            % Example : db.DbQuery.startGui

            Result = false;
            try
                                
                % Prepare command line
                Path = fullfile(tools.os.getUltrasatPath(), 'python', 'utils', 'utils_gui');
                if ~isfolder(Path)
                    io.msgLog(LogLevel.Error, 'DbQuery.startGui: Path not found: %s', Path);
                    return;
                end
                
                % Set utility name
                if tools.os.islinux
                    Cmd = sprintf('%s%s%s', Path, filesep, 'utils_gui');
                else
                    Cmd = sprintf('%s%s%s', Path, filesep, 'utils_gui.exe');
                end
 
                % Found - run it
                if isfile(Cmd)
                    io.msgLog(LogLevel.Info, 'DbQuery.startGui: system( %s )', Cmd);
                    [Status, Output] = system(Cmd);
                    %io.msgLog(LogLevel.Info, 'startGui: %d', Status);
                    %io.msgLog(LogLevel.Info, 'startGui: %s', Output);
                    Result = true;
                    
                % File not found
                else
                    io.msgLog(LogLevel.Error, 'DbQuery.startGui: File not found, use Lazarus to compile the project: %s', Cmd);
                end
                
            catch Ex
                io.msgLogEx(LogLevel.Error, Ex, 'DbQuery.startGui');
            end
        end
        
    end
    
    %----------------------------------------------------------------------
    properties(Constant)
        ReservedDatabaseWords = { 'group', 'dec' };
    end
    
     
    methods(Static) % Static
    
        function Result = camelToSnake(Name)
            % Convert camelCase to snake_case
            
            % Place an underscore between letters of adjacent lower case and upper case
            Result = regexprep(Name, '([a-z])([A-Z])','$1_${lower($2)}');           
            
            % Lower case first letter if upper case                        
            Result = regexprep(Result, '^([A-Z])', '${lower($1)}');           
        end
        
        
        function Result = snakeToCamel(Name)
            % Convert snake_case to camelCase (may not work for some cases, need to test)
            
            % Capitialize letters with that have an underscore preceding it
            Result = regexprep(Name, '_([a-z])','${upper($1)}');

            % Remove underscore for numbers that have an underscore preceding
            Result = regexprep(Result,'_([0-9])', '$1');

            % Option to upper or lower case the first letter
            Result = regexprep(Result, '^([a-z])', '${upper($1)}');
            Result = regexprep(Result, '^([A-Z])', '${lower($1)}');
        end
        
    
        function Result = NameToColumnName(ColumnName)
            % Normalize column name: convert camelCase to snake_case
            Result = db.DbQuery.camelToSnake(ColumnName);
            Result = strrep(Result, '__', '_');
            if any(contains(db.DbQuery.ReservedDatabaseWords, Result))
                Result = strcat('f_', Result);
            end
        end
        
    
        function Result = ColumnNameToName(Word)
            % 
            Result = Word;
            if startsWith(Word, 'f_')
                Word = Word(3:end);
                if any(contains(db.DbQuery.ReservedDatabaseWords, Word))
                    Result = Word;
                end
            end
        end
    
        
        function [Schema, TableName] = getSchemaTable(TableName, DefaultSchema)
            % Split schema.table_name to schema, table_name. When schema name is not specified, use the default ('public')            
            arguments
                TableName
                DefaultSchema = 'public'
            end           

            s = strsplit(TableName, '.');
            if numel(s) == 1
                Schema = DefaultSchema;
                TableName = lower(s{1});
            else
                Schema = lower(s{1});
                TableName = lower(s{2});
            end
        end
        
        
        function Result = getSchema(TableName, DefaultSchema)
            % Extract schema from schema.table_name. When schema name is not specified, use the default ('public')            
            arguments
                TableName
                DefaultSchema = 'public'
            end            
            [Schema, TN] = db.DbQuery.getSchemaTable(TableName, DefaultSchema);
            Result = Schema;
        end
        
        
        function Result = getTable(TableName)
            % Extract table_name from schema.table_name
            [Schema, TN] = db.DbQuery.getSchemaTable(TableName);
            Result = TN;
        end        
    end
    
    %----------------------------------------------------------------------
    methods(Static) % Static

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
