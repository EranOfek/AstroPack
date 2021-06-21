% DbQuery
%
% https://www.tutorialspoint.com/java-resultset-movetoinsertrow-method-with-example
%--------------------------------------------------------------------------
% Use unittest__tables from GDrive to test
%

classdef DbQuery < Component
    
    % Properties
    properties (SetAccess = public)            
        
        % Connection details
        ConnectionStr = ''
        Conn = []
        ConnName = ''
        
        % Current SQL statement data
        SqlText = ''        % SQL text
        Statement = []      % Prepared statement object
        
        ResultSet = []      % Returned result-set
        Record = []         % Current record
        Metadata = []        
        ColCount = 0     %
        ColNames  = []   %
        ColType  = []    %
        IsOpen = false      % query() 
        ExecOk = false      % exec()
        Eof = true          %
        Toc = 0             %
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = DbQuery(varargin)
            % Create new DbQuery obeject
            
            Obj.setName('DbQuery');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);
            
            %
            if numel(varargin) == 1 
                
                % Connection object is specified
                if strcmp(class(varargin{1}), 'io.db.DbConnection')                    
                    Conn = varargin{1};
                    
                % Connection name is specified
                elseif strcmp(class(varargin{1}), 'char') || strcmp(class(varargin{1}), 'string')
                    Obj.ConnName = varargin{1};
                    Conn = io.db.DbConnection.getDbConnection(Obj.ConnName);
                end                
                
            % Use default connection
            elseif numel(varargin) == 0
                Conn = io.db.DbConnection.getDbConnection('');
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
    
    
    methods % open, close
                         
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

            
        function Result = query(Obj, varargin)
            % Run SELECT query
            % @Todo: Replace varargin with arguments block
            
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
            
            % Execute
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
            % Execute SQL statement (using java calls)
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
        
        
        function Result = clear(Obj)
            % Clear current statement and resultset
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
            % Clear current ResultSet            
            Obj.ResultSet = [];
            Obj.Record = [];
            Obj.Metadata = [];        
            Obj.ColCount = 0;
            Obj.ColNames  = [];
            Obj.ColType  = [];
            Result = true;
        end
        
        
        function Result = close(Obj)
            % Close current query
            Result = Obj.clear();
        end
        
        
        function Result = getMetadata(Obj, varargin)
            % Get metadata of current result-set or of specified table 
            Obj.ColCount = 0;
            Obj.ColNames = {};
            Obj.ColType = {};
            TableName = '';
            
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
        
        
        function selectWhere(Obj, Fields, TableName, Where)
            % Execute: SELECT Fields FROM TableName WHERE Where
            Obj.SqlText = sprintf('%s FROM %s WHERE %s', Fields, TableName, Where).char;
            Obj.query();            
        end
        
        
        function Result = selectCount(Obj, TableName)
            Obj.SqlText = sprintf('SELECT COUNT(*) FROM %s', TableName);
            Obj.query();            
            Result = Obj.getField('count');
        end
        
            
        function Result = next(Obj)
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
            Result = false;
            Obj.Eof = true;
            try
                Obj.Eof = ~Obj.ResultSet.previous();
                Result = ~Obj.Eof;
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.prev failed');
            end                
        end        
        
        
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
        
        
        function Result = getField(Obj, FieldName)
            % Get string field
            
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
            % Get fields list as celarray
            
            % Loop over all columns in the row
            Result = Obj.ColNames;
        end
        
        
        function Result = getFieldTable(Obj)
            % Get fields as empty table
            
            Fields = Obj.getFieldList();
            Result = table(Fields);
        end
        

        function Result = getTableFieldList(Obj, TableName)
            % Get fields list as celarray

            % Select single record from table
            % @Todo: Check how to get it without slect
            Text = ['SELECT * from ', TableName, ' LIMIT 1'];
            Obj.query(Text);
            
            % This still does not work @Todo
            % Obj.getMetadata(TableName);
            
            % Loop over all columns in the row
            Result = Obj.ColNames;
        end
        
    end
    
    
    methods

                    
                    
        function Result = newRecord(Obj)
            % Create new empty record associated with this query
            
            Result = io.db.DbQuery(Obj);
        end
        
        
        function Result = getRecord(Obj)
            % Get current record from ResultSet as DbRecord
            % NOTE: Field names are loaded in lower-case (because Postgres
            % creates field names lower-cased)
            
            % Create new record object
            Rec = io.db.DbRecord(Obj);
            
            % Loop over all columns in the row
            for ColIndex = 1 : Obj.ColCount
                
                FieldName = Obj.ColNames{ColIndex};
                Value = Obj.getField(ColIndex);                           
                addprop(Rec, FieldName);
                Rec.(FieldName) = Value;
            end
            
            Result = Rec;        
                        
        end
        

        function Result = insertStruct(Obj, TableName, Struct) %, Args)
            arguments
                Obj
                TableName
                Struct
                %Args
            end
            
            % Insert struct, use all fields that exist in the table
            % See: https://www.programcreek.com/java-api-examples/?class=java.sql.Statement&method=executeUpdate
            Result = false;
                       
            % Execute SQL statement (using java calls)
            Obj.msgLog(LogLevel.Info, 'DbQuery: insertStruct');            
            tic();
            
            % Need connection, clear current query
            Obj.openConn();
            Obj.clear();
                                                      
            % Prepare SQL statement
            % sql = sprintf("INSERT INTO master_table(RecID, FInt) VALUES ('%s', %d)", uuid, 1).char;
            SqlFields = '';
            SqlValues = '';

            % Iterate struct fields
            FieldNames = fieldnames(Struct);
            disp(FieldNames);
            
            for i = 1:numel(FieldNames)
                FieldName = FieldNames{i};
            
                % 
                if numel(SqlFields) > 0
                    SqlFields = [SqlFields ',' FieldName];
                    SqlValues = [SqlValues ',?'];
                else
                    SqlFields = [SqlFields FieldName];
                    SqlValues = [SqlValues '?'];
                end
            end
            
            % 
            Obj.SqlText = ['INSERT INTO ', string(TableName).char, ' (', SqlFields, ') VALUES (', SqlValues, ')'];
                        
            % Prepare query
            Obj.msgLog(LogLevel.Debug, 'DbQuery.exec: %s', Obj.SqlText);
            try
                Obj.Statement = Obj.Conn.Conn.prepareStatement(Obj.SqlText);            
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.exec: prepareStatement failed: %s', Obj.SqlText);
            end
                      
            % Iterate struct fields
            % See https://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html
            for i = 1:numel(FieldNames)
                f = FieldNames{i};
                val = Struct.(f);
                
                if isa(Struct.(f), 'integer')
                    Obj.msgLog(LogLevel.Debug, 'integer: %s', f);
                    Obj.Statement.setInt(i, val);
                elseif isa(Struct.(f), 'uint64')
                    Obj.msgLog(LogLevel.Debug, 'uint64: %s', f);
                    
                elseif isa(Struct.(f), 'float')
                    Obj.msgLog(LogLevel.Debug, 'float: %s', f);
                elseif isa(Struct.(f), 'double')
                    Obj.msgLog(LogLevel.Debug, 'double: %s', f);
                    Obj.Statement.setDouble(i, val);
                elseif isa(Struct.(f), 'single')
                    Obj.msgLog(LogLevel.Debug, 'single: %s', f);
                    Obj.Statement.setSingle(i, val);
                elseif isa(Struct.(f), 'char')
                    Obj.msgLog(LogLevel.Debug, 'char: %s', f);
                    Obj.Statement.setString(i, val);
                else
                    % Other not supported (yet?)
                    Obj.msgLog(LogLevel.Debug, 'other - not supported: %s', f);
                end
            end           
             
            % Execute
            % See: https://www.enterprisedb.com/edb-docs/d/jdbc-connector/user-guides/jdbc-guide/42.2.8.1/executing_sql_commands_with_executeUpdate().html
            try
                Obj.ResultSet = Obj.Statement.executeUpdate();                             
                Obj.ExecOk = true;                
                Result = true;
            catch
                Obj.msgLog(LogLevel.Error, 'DbQuery.open: executeQuery failed: %s', Obj.SqlText);                
            end
            
            Obj.Toc = toc();
            Obj.msgLog(LogLevel.Info, 'DbQuery.exec time: %.6f', Obj.Toc);  
                  
            Result = true;
        end
        

            
                
%                 
%                 % Iterate keys
%                 for f=1:DataSize(1)
%                     Key = HeaderData{i, 1};
%                     
%                     % Check if there is a field matching the header key
%                     if any(contains(FieldNames, Key))
%                         
%                         % 
%                         if numel(SqlFields) > 0:
%                             SqlText = [SqlText ',' Key];
%                             SqlValues = [SqlValues ',' Value];
%                         else
%                             SqlText = [SqlText Key];
%                             SqlValues = [SqlValues Value];
%                         end
%                     else
%                     end
%                     
%                 end
%                 
                % Now we have keys and values
    
                
                
        function Result = insertRecord(Obj, TableName, Rec)
            % Insert new record
            Result = false;
            
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
        
        
        function Result = updateRecord(Obj, Rec)
            % Update record
            Result = false;
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
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadTable, ColCount = %d', Obj.ColCount);
            
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
            Obj.msgLog(LogLevel.Debug, 'DbQuery.loadTable, RowCount = %d, Time: %.6f', RowIndex, Obj.Time);
        end
        
        
        
        
        function Result = createDatabase(Obj)
            % Create database
        end
            
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'DbQuery test started')
            io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');
               
            % ---------------------------------------------- Connect
            % NOTE: Database 'unittest' should exist
            
            % Create database connection
            %Conn = io.db.DbConnection;
            %Conn.DatabaseName = 'unittest';
            %Conn.open();

            Conn = io.db.Db.getUnitTest();
            
            % Query Postgres version, result should be similar to
            % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'
            Q = io.db.DbQuery(Conn);
            Q.query('SELECT version()');
            assert(Q.ColCount == 1);
            pgver = Q.getField('version');
            io.msgLog(LogLevel.Test, 'Version: %s', pgver);
            assert(contains(pgver, 'PostgreSQL'));
       
            
            % ---------------------------------------------- getTableFieldList 
            Q = io.db.DbQuery(Conn);
            Fields = Q.getTableFieldList('master_table');
            disp(Fields);
            
            % ---------------------------------------------- Select
            % NOTE: At this point, we assume that tables master_table and
            % details_table exist and are not empty
                       
            % Select two fields from table, using LIMIT
            Q = io.db.DbQuery(Conn);
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
            
            
            
            % ---------------------------------------------- insertStruct             
            
            % Create struct with different types of fields
            s = struct;            
            s.recid = Component.newUuid();
            s.fint = int32(1);
            
%             s.int1 = int32(1);
%             s.uint1 = int32(2);
%             s.bigint1 = int64(3);
%             s.single1 = single(4);
%             s.double1 = double(5);
%             s.char1 = 'abcd';
            
            Q = io.db.DbQuery;
            
            %Q.insertStruct('master_table', s);            
            
            

            
            
            % ---------------------------------------------- Insert
            
            % Insert records            
            io.msgLog(LogLevel.Test, 'testing INSERT...');
            InsertCount = 100;
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
                InsertCount = 0;
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

            
            % ---------------------------------------------- Create and delete database       
            %
            
 
            io.msgStyle(LogLevel.Test, '@passed', 'DbQuery test passed')
            Result = true;
        end
        
        
    end                
end
