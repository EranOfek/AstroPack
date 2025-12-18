% db.Db - A class for accessing databases
%
% Example: 
% D=db.Db;  % create object
% D.Conn;  % or D.connect % make Java connection
% D.showDB  % show all DBs
% D.useDB('test_db');   % set use for specific DB
% D.showCurrentDB       % show current DB
% D.showTables          % show Tables in current DB
% D.describeTable('test_table')   % show Table columns and teir properties
% [ColNames, ColTypes] = D.getColumns('test_table')
%
% T=D.query("SELECT * FROM test_db.test_table;")
% D.query("USE test_db;", 'IsExec',true)
% D.createTable('test_db',["id"; "name"; "age"], ["UInt32"; "String"; "UInt8"]);
% D.insert('test_db.users',T1);
% D.insert('test_db.users','file.csv');
%
% D.disconnectCH_Java % disconnect Java
%
% LAST example:
% D=db.Db;
% D.User = 'last_user';
% D.Password = <>; % use the usual LAST password to employ the DB as a read-only user
% D.Conn;
% D.useDB('last');
% D.showCurrentDB
% D.showTables
% [ColNames, ColTypes]=D.getColumns('visit_images')
% T=D.query("SELECT top 10 * FROM last.visit_images;");
% T=D.query("SELECT top 5 * FROM last.proc_src;");
% T=D.query("SELECT count(*) FROM last.visit_src;")
% T=D.query("SELECT count(*) FROM last.visit_asteroids WHERE distmp < 1.5;")

classdef Db < Component
    %
    
    properties       
        DbType   = "Clickhouse"; % 'Postgress'|'Clickhouse'
        DbName   = []; %"test_db";
        User     = {"LASTDB_User","last_user"} %"default"; %""; If cell array then Project,User in PasswordsManager
        Password = ""; %[];  % if empty, then use the PasswordsManager class to find and populate the password
        Host     = "10.150.28.18"; % "socsrv" %"localhost"; %[];  % or '10.23.1.25' for last0
        Port     = "8123"; %[];
        
        ConnType = 'java';  % 'java'|'http'                        
    end
    
    properties (Hidden)
        Conn      = [];        % connectivity information (for ConnType=java)
        DataTypes = cell(2,0); % data types in the DB tables 
    end
    
    properties (Hidden, Constant)
       
    end
    
    
    methods % Constructor
       
        %function Obj = Db(Args)
        %    % Constructor for db.Db
        %end
             
        function delete(Obj)
            % Destractor for db.Db

            Obj.disconnect;
        end

        function Obj=connect(Obj)
            % connect

            Obj.Conn;
        end

        function [IsConn,ReadOnly]=isConnected(Obj)
            % Check if object is connected.
            % Input  : - self.
            % Output : - A logical indicating if DB is connected.
            %          - A logical indicating if connection is read only.
            % Author : Eran Ofek (Jun 2025)
            % Example: DB.isConnected

            IsConn = ~isempty(Obj.Conn);

            if nargout>1
                switch Obj.Conn.ReadOnly
                    case 'off'
                        ReadOnly = false;
                    otherwise
                        ReadOnly = true;
                end
            end
        end
        

    end
    
    methods % setter/getters
        function Val=get.DbName(Obj)
            % Getter for DbName
            
            Val = Obj.DbName;
            if isempty(Val)
                Struct     = Obj.getDbConfig;
                if isempty(Struct)
                    Val = [];
                else
                    Val        = Struct.DbName{1};
                end
                Obj.DbName = Val; 
            end            
            
        end
        
        function Val=get.Host(Obj)
            % Getter for Host
            
            Val = Obj.Host;
            if isempty(Val)
                [~,Val]    = Obj.getDbConfig(Obj.DbName);
                Obj.Host   = Val; 
            end    
        end
        
        function Val=get.Port(Obj)
            % Getter for Port
            
            Val = Obj.Port;
            if isempty(Val)
                [~,~,Val]  = Obj.getDbConfig(Obj.DbName);
                Obj.Port  = Val; 
            end    
        end
       
        function Val=get.Conn(Obj)
            % Getter for Conn (Java connection)
           
            Val = Obj.Conn;
            if isempty(Val)
                Obj.User;
                Val = db.Db.connectCH_Java('DbName',Obj.DbName, 'Host',Obj.Host, 'Port',Obj.Port, 'User',Obj.User, 'Password',Obj.Password);
                Obj.Conn = Val;
            end
        end

        
        function Val=get.User(Obj)
            % Getter for User (set Password if needed)

            if iscell(Obj.User)
                PM = PasswordsManager;
                [User, Pass] = PM.getUserPassword(Obj.User{:});
                Obj.User     = User;
                Obj.Password = Pass;
                Val = User;
            else
                if contains(Obj.User,'/')
                    switch Obj.User
                        case 'euclid/user'
                            Obj.User = {'LASTDB_User','last_user'};
                            Obj.Host = '10.150.28.18';
                        case 'euclid/root'
                            Obj.User = {'LASTDB_Root','default'};
                            Obj.Host = '10.150.28.18';
                        case 'last0/user'
                            Obj.User = {'LASTDB_User','last_user'};
                            Obj.Host = '10.23.1.25';
                        case 'last0/root'
                            Obj.User = {'LASTDB_Root','default'};
                            Obj.Host = '10.23.1.25';
                        otherwise
                            error('Unidentified user name pattern');
                    end
                else
                    Val = Obj.User;
                end
            end
        end

        function Val=get.Password(Obj)
            % Getter for Password
           
            if iscell(Obj.User)
                PM = PasswordsManager;
                [User, Pass] = PM.getUserPassword(Obj.User{:});
                Obj.User     = User;
                Obj.Password = Pass;
                Val = Obj.Password;
            else
                Val = Obj.Password;
            end
        end
    end
      
    methods (Static) % construction
        
        function [Conn, JarFile]=connectCH_Java(Args)
            % Connect to ClickHouse DB via Java
            %   To close connection use: close(Conn)
            % Input  : * ...,key,val,...
            %            See code for options
            % Output : - Connection object.
            %          - Jar file name.
            % Author : Eran Ofek (Oct 2024)
            
            arguments
                Args.DbName  = 'test_db';
                Args.Host    = 'localhost';
                Args.Port    = '8123';
                Args.User    = []; %'default';
                Args.Password = []; 
                Args.JarFile = []; % '/home/sasha/matlab/data/Java/Clickhouse/clickhouse-jdbc-0.3.2-patch11-all.jar'; % []; %'/home/eran/jdbc/clickhouse-jdbc-0.7.0-all.jar';
                Args.Driver  = 'com.clickhouse.jdbc.ClickHouseDriver';  % 'ru.yandex.clickhouse.ClickHouseDriver'
                Args.BaseURL = "jdbc:clickhouse";
                Args.Timeout = 3600.*1000;
            end
            
            if isempty(Args.JarFile)
                I = Installer;
                JarDir = I.getDataDir(I.Items.ClickHouseJar);                
                PWD = pwd;
                cd(JarDir);
                Fd = dir('*.jar');
                %JarFile = sprintf('%s%s%s',JarDir,filesep,Fd.name);
                JarFile = tools.cell.sprintf_Cell2Cell("%s%s%s",JarDir,filesep,{Fd.name});
                cd(PWD);
            else
                JarFile = Args.JarFile;
            end
            if ischar(JarFile)
                JarFile = string(JarFile);
            end
            if contains(JarFile,'~')
                for I=1:1:numel(JarFile)
                    JarFile(I) = tools.os.relPath2absPath(JarFile{I});
                end
            end


            % Check if Jar file exist
            for I=1:1:numel(JarFile)
                if ~isfile(JarFile(I))
                    fprintf('Jar file: %s not found\n',JarFile(I));
                    fprintf('Use I=Installer; I.install(''ClickHouseJar'')\n');
                    error('Jar file not found');
                else
                    % Add JDBC driver to the MATLAB Java path if it's not already added
                    javaaddpath(JarFile(I));
                    if ~ismember(JarFile(I), javaclasspath('-dynamic'))
                        javaaddpath(Args.JarFile);
                    end
                end

            end

            JdbcURL = sprintf("%s://%s:%s/%s?protocol=native&use_object_types=true",Args.BaseURL, Args.Host, Args.Port, Args.DbName);
            %JdbcURL = sprintf("%s://%s:%s/%s?socket_timeout=%d&dataTransferTimeout=%d",Args.BaseURL, Args.Host, Args.Port, Args.DbName, Args.Timeout, Args.Timeout);
                
         
            
            
            %Props = javaObject('com.clickhouse.client.config.ClickHouseProperties');
            %Props.setSocketTimeout(int64(Args.Timeout));       % 600 000 ms = 10 min
            %Props.setDataTransferTimeout(int64(Args.Timeout));  % same

            % Set up the JDBC connection
            Conn = database('', Args.User, Args.Password, Args.Driver, JdbcURL); %, 'Properties', Props);
            if ~isempty(Conn.Message) && contains(Conn.Message, 'Error')
                Conn.Message
                Conn = [];
            end
        end


    end
    
    methods (Static) % static utilities
        function [OutString] = table2charDump(T,Args)
            % Format table in characters dump for database insert operation
            % Input  : - table.
            %          * ...,key,val,... 
            %            See code for options.
            % Output : - String
            % Author : Eran Ofek (2024 Oct) 
            % Example: db.Db.table2charDump(T)

            arguments
                T
                Args.NumFormat      = '%.18g';  % Adjust precision as needed
                Args.Delimiter      = ',';
                Args.LineStartChar  = '(';
                Args.LineEndChar    = '),';
                Args.LastLineEndChar= ')';
                Args.LineTerminator = ''; %'\n';
                Args.CharInQuote logical = true;
            end

            ColNames = T.Properties.VariableNames;

            % Convert the table to a cell array
            C = table2cell(T);
            SizeC = size(C);

            % Get the variable types for each column
            VarTypes = varfun(@class, T, 'OutputFormat', 'cell');

            % Initialize an empty string to store the result
            OutString = '';

            % Define a format for numeric precision


            % Iterate over each row in the table
            for I = 1:SizeC(1)
                RowString = '';  % To store the row as a tab-separated string

                % Iterate over each column in the row
                for J = 1:SizeC(2)
                    switch VarTypes{J}
                        case {'single','double'}
                            ValStr = sprintf(Args.NumFormat, C{I,J});
                        case {'char','string'}
                            if Args.CharInQuote
                                ValStr = sprintf('''%s''', C{I,J});
                            else
                                ValStr = sprintf('%s', C{I,J});
                            end
                        otherwise
                            % assume this is an integer or logical
                            ValStr = sprintf('%d',C{I,J});
                    end
                    if J==SizeC(2)
                        RowString = sprintf('%s%s',RowString,ValStr);
                    else
                        RowString = sprintf('%s%s%s ',RowString, ValStr, Args.Delimiter);
                    end            
                end
                if I==SizeC(1)
                    OutString = sprintf('%s %s %s %s %s', OutString, Args.LineStartChar, RowString, Args.LastLineEndChar, Args.LineTerminator);
                else
                    OutString = sprintf('%s %s %s %s %s', OutString, Args.LineStartChar, RowString, Args.LineEndChar, Args.LineTerminator);
                end
            end

        end

        function FileName=table2csv(T, Args)
            % Convert table to csv file
            % Input  : - A table.
            %          * ...,key,val,...
            %            'FileName' - File name to write.
            %                   Default is tempname
            %            See code for additional options.
            % Output : - File Name.
            % Author : Eran Ofek (Oct 2024)

            arguments
                T  %table
                % Write table
                Args.FileName         = tempname; % /home/eran/output.csv';
                Args.FileType         = 'text';        % see writetable for optoins
                Args.WriteVarNames    = {};
                Args.Delimiter        = ',';
%                 Args.LineEnding       = '\r\n';
                Args.WriteVariableNames logical  = true;
                Args.QuoteStrings                = true; % 'minimal';
                Args.WriteMode                   = 'overwrite';
                Args.writetableArgs              = {};
                %Args.DeleteFile logical          = false;  % delete file after Db insertion
                Args.LowerCaseHeaders            = true; 
                Args.ReplaceNaN                  = true;  
            end

            FileName = Args.FileName;

            % write table to csv file
            if Args.LowerCaseHeaders
                T.Properties.VariableNames = lower(T.Properties.VariableNames); % the DB sometimes require lowercase headers
            end
            
            writetable(T, Args.FileName, 'FileType',Args.FileType,...
                                 'Delimiter',Args.Delimiter,...
                                 'WriteVariableNames',Args.WriteVariableNames,...
                                 'QuoteStrings',Args.QuoteStrings,...
                                 'WriteMode',Args.WriteMode,...
                                 Args.writetableArgs{:});
%                                  'LineEnding',Args.LineEnding,...  

            % replace NaN values with 'NULL's
            if Args.ReplaceNaN
%                 Command = sprintf('sed -i ''s/\\b[Nn][Aa][Nn]\\b/null/g'' %s',Args.FileName); % this does not work with CH DB
                Command = sprintf('sed -i ''s/\\b[Nn][Aa][Nn]\\b//g'' %s',Args.FileName); % looks like CH DB needs empty instead of NULL in the CSV 
                [~,Error] = system(Command);
            end

        end
        
        function Result=concatDbTable(DbName, DbTable)
            % Concat DB name and Db Table to <DbName>.<DbTable> string.
            % Input  : - DbName. If empty, will return only DbTable.
            %          - DbTable.
            % Output : String of <DbName>.<DbTable>
            % Author : Eran Ofek (Oct 2024)
            % Example: db.Db.concatDbTable('test_db','test_table')
            %          db.Db.concatDbTable([],'test_table')

            if isempty(DbName)
                Result = DbTable;
            else
                Result = sprintf('%s.%s', DbName, DbTable);
            end
        end
    end

    methods (Static) % construct query static utilities
        function [WhereClause] = genWhereClause(Const, Args)
            % Generate SQL WHERE clause from constraints
            % Input  : - Constraints.
            %            Either a cell array of {fieldname, constraints}
            %            or structure array of constraints with fields: 'Field' and
            %            'Range'.
            %            Range can be one of the following:
            %            A string or char in this case will perform a LIKE
            %            operation.
            %            A single element numeric - will perform equality.
            %            Two elements range - will perform in range check.
            %            Three elements range - will look in range of
            %            Range(1)-Range(2) to Range(1)+Range(3).
            %            Note that you can search for multiple ranges by specifying
            %            the field name more then once.
            %          * ...,key,val,... 
            %            'AddWhere' - Add 'WHERE' in the begining of clause.
            %                   Default is true.
            %            'Operator' - Operator between constraints.
            %                   Default is 'AND'.
            % Output : - A string containing the where clause.
            % Author : Eran Ofek (2024 Dec) 
            % Example: R=db.Db.genWhereClause({'fieldid','1456%'; 'camnum',[1 2]; 'mount',1})
        
            arguments
                Const
                Args.AddWhere logical = true;
                Args.Operator         = 'AND';
            end
            FieldField = 'Field';
            ConstField = 'Range';
        
        
            if isstruct(Const)
                Column = {Const.(FieldField)};
                Range  = {Const.(ConstField)};
            else
                Column = Const(:,1);
                Range  = Const(:,2);
            end
        
            Ncol = numel(Column);
            if Args.AddWhere
                WhereClause = 'WHERE';
            else
                WhereClause = '';
            end
            for Icol=1:1:Ncol
                ColRange = Range{Icol};
        
                if isnumeric(ColRange)
                    switch numel(ColRange)
                        case 0
                            % do nothing
                        case 1
                            % equal constraint
                            if floor(ColRange)==ColRange
                                % integer
                                WhereClause = sprintf("%s %s=%d", WhereClause, Column{Icol}, ColRange);
                            else
                                WhereClause = sprintf("%s %s=%18.15g", WhereClause, Column{Icol}, ColRange);
                            end
                        case 2
                            % range constraint
                            WhereClause = sprintf("%s %s>=%18.15g AND %s<=%18.15g", WhereClause, Column{Icol}, ColRange(1), Column{Icol}, ColRange(2));
                        case 3
                            % center +/- constraint
                            R1 = ColRange(1) - ColRange(2);
                            R2 = ColRange(1) + ColRange(3);
                            WhereClause = sprintf('%s %s>=%18.15g AND %s<=%18.15g', WhereClause, Column{Icol}, R1, Column{Icol}, R2);
                        otherwise
                            error('Unknown number of elements in Range in elemnet %d',Icol);
                    end
                elseif ischar(ColRange) || isstring(ColRange)
                    % assume string comparison using LIKE
                    % convert to string
                    ColRange = string(ColRange);
                    % reColRangeplace ' with ''
                    ColRange = strrep(ColRange,"'","''");
                    WhereClause = sprintf("%s %s LIKE '%s'", WhereClause, Column{Icol}, ColRange);
                elseif iscell(ColRange)
                    error('No treatment yet');
                else
                    error('Unknown Range format option');
                end
                if Icol<Ncol
                    WhereClause = sprintf('%s %s', WhereClause, Args.Operator);
                end
            end
        
        end

        function [Result] = genQuery(TableName, Columns, Constr, Args)
            % Generate an SQL query programmatically from constructs.
            % Input  : - TableName (e.g., 'last_visits').
            %          - Columns in the SELECT clause.
            %            Either a string containing a column names to
            %            select (e.g., '*' | 'col1, col2'), OR a cell
            %            (or strings) array of column names (e.g.,
            %            {'col1','col2'}).
            %          - Constraints. Either a char array of constraints
            %            (e.g., 'ra>1').
            %            Alternatively, a cell array of {fieldname, constraints}
            %            or structure array of constraints with fields: 'Field' and
            %            'Range'.
            %            Range can be one of the following:
            %            A string or char in this case will perform a LIKE
            %            operation.
            %            A single element numeric - will perform equality.
            %            Two elements range - will perform in range check.
            %            Three elements range - will look in range of
            %            Range(1)-Range(2) to Range(1)+Range(3).
            %            Note that you can search for multiple ranges by specifying
            %            the field name more then once.
            %            See: db.Db.genWhereClause for details.
            %          * ...,key,val,...
            %            'Top' - (Top clause) Number of lines to retrieve.
            %                   If empty, will retrieve all lines.
            %                   Default is [].
            %            'SortBy' - Column name to sort by. If empty do not
            %                   add the ORDER BY clause.
            %                   Note that in case SortBy and Top are given,
            %                   then the sorting is done prior to TOP.
            %                   Default is [].
            %            'SortOrder' - Sort order: 'ASC'|'DESC'.
            %                   Default is 'ASC'.
            %            'Operator' - Operator to use in db.Db.genWhereClause
            %                   Default is 'AND'.
            %            'Join' - Join clause of the form: 
            %                   [INNER | LEFT | RIGHT] JOIN table2 ON table1.column = table2.column
            %            'AddAfterWhere' - Add string after where.
            %                   Default is ''.
            % Output : - A full query clause.
            % Author : Eran Ofek (Dec 2024)
            % Example: db.Db.genQuery('last_vistits')
            %          db.Db.genQuery('last_vistits', {'ra','dec'}, 'mag_psf<15')
            %          db.Db.genQuery('last_vistits', {'ra','dec'}, {'mag_psf',[15 16]; 'camnum',1; 'ra',[1 0.1 0.2]},'Top',10)

            arguments
                TableName          % cell/strings for multiple tables
                Columns        = '*';
                Constr         = '';
                Args.Top       = [];
                Args.SortBy    = [];
                Args.SortOrder = 'ASC';  % or 'DESC'
                Args.Operator  = 'AND';
                Args.Join      = '';
                Args.AddAfterWhere = '';
            end

            if ischar(Columns)
                SelectClause = Columns;
            else
                % string or cell
                SelectClause = join(string(Columns),', ');
            end

            if isempty(Args.Top)
                TopClause = '';
            else
                TopClause = sprintf('top %d',Args.Top);
            end

            if ischar(TableName)
                FromClause = TableName;
            else
                FromClause = join(string(TableName),', ');
            end

            if ischar(Constr)
                WhereClause = Constr;
            else
                WhereClause = db.Db.genWhereClause(Constr, 'AddWhere',false, 'Operator',Args.Operator);
            end

            if isempty(Args.SortBy)
                SortClause = '';
            else
                SortClause = sprintf('ORDER BY %s %s', Args.SortBy, Args.SortOrder);
            end

            if isempty(WhereClause)
                Result = sprintf("SELECT %s %s FROM %s %s %s", TopClause, SelectClause, FromClause, Args.Join, SortClause);
            else
                Result = sprintf("SELECT %s %s FROM %s %s WHERE %s %s", TopClause, SelectClause, FromClause, Args.Join, WhereClause, SortClause);
            end

            Result = sprintf('%s %s',Result, Args.AddAfterWhere);

        end
       
        function Result = convertClass2DB_Class(Type, TypeDB)
            % Convert matlab class names to DB class names.
            % Input  : - A cell array or string array of matlab class names
            % Output : - A string array of DB class names.
            % Author : Eran Ofek (Mar 2025)
            % Example: R=db.Db.convertClass2DB_Class({'int8','uint32','single'})

            arguments
                Type
                TypeDB = 'ClickHouse';
            end

            switch lower(TypeDB)
                case 'clickhouse'
                    TypeM  = ["int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "logical", "single", "double", "string", "char"];
                    TypeDB = ["Int8", "In16", "Int32", "Int64", "UInt8", "UInt16", "UInt32", "UInt64", "UInt8",    "Float32", "Float64", "String", "String"];
                otherwise
                    error('Unkown DB type');
            end
            
            % convert to cell array:
            Type   = {Type{:}};
            Ind    = tools.string.mex.findAllInAll(Type,{TypeM{:}});
            Result = TypeDB(Ind);
        end
    
        function ID = generateID(C, Args)
            % generate ID for DB
            %   see also: imProc.db.generateImageID
            % Input  : - A cell array of values corresponding to the keys.
            %          * ...,key,val,...
            %            'FormatSt' - Default is:
            %                       struct("Key",{'IMTYPE','LEVEL','NODENUMB','MOUNTNUM','CAMNUM','CROPID','JD'},...
            %                                       "BitNum", {4, 5, 5, 5, 3, 6, 36},...
            %                                        "Fun", {@(x) find(strcmp(x, AstroFileName.ListType)),...
            %                                            @(x) find(strcmp(x, AstroFileName.ListLevel)),...
            %                                            @(x) x,...
            %                                            @(x) x,...
            %                                            @(x) x,...
            %                                            @(x) tools.array.replace(x,NaN,0),...
            %                                            @(jd) uint64((jd-2451545.5).*86400.*10)});
            % Output : - ID
            % Author : Eran Ofek (Mar 2025)
            % Example: ID = db.Db.generateID({'sci','merged',1,1,1,10,2460000.0})

            arguments
                C    % a cell array of ['IMTYPE','LEVEL','NODENUMB','MOUNTNUM','CAMNUM','CROPID','JD']
                Args.FormatSt          = struct("Key",{'IMTYPE','LEVEL','NODENUMB','MOUNTNUM','CAMNUM','CROPID','JD'},...
                                                "BitNum", {4, 5, 5, 5, 3, 6, 36},...
                                                "Fun", {@(x) find(strcmp(x, AstroFileName.ListType)),...
                                                        @(x) find(strcmp(x, AstroFileName.ListLevel)),...
                                                        @(x) x,...
                                                        @(x) x,...
                                                        @(x) x,...
                                                        @(x) tools.array.replace(x,NaN,0),...
                                                        @(jd) uint64((jd-2451545.5).*86400.*10)});
            end

            Nsub = numel(Args.FormatSt);
            BitNum = zeros(1,Nsub);
            BitVal = zeros(1,Nsub);
            for Isub=1:1:Nsub
                BitVal(Isub) = Args.FormatSt(Isub).Fun(C{Isub});
                BitNum(Isub) = Args.FormatSt(Isub).BitNum;
            end
            ID = tools.bit.bitEncode(BitNum, BitVal);
        end
    end

    methods % construct queries / dynamic
        function Result = genQueryGroupBy(Obj, TableName, GroupByCols, AddCols, Args)
            % Generate a select group by query.
            %   query of the form: INSERT INTO last.fastmoving_asteroids11 SELECT id, jd, any(col1), any(col2), ... FROM last.fastmoving_asteroids1 GROUP BY id, jd;
            % Input  : - self.
            %          - Table name.
            %          - GroupByCols cell array. Default is {'id','jd'}
            %          - AddCols. Columns to add. Default is '*'.
            %          * ...,key,val,...
            %            'Fun' - Function for selection 'min'|'max'|'any'.
            %                   Default is 'min'.
            % Output : - Query string.
            % Author : Eran Ofek (Apr 2025)
            % Example: Result = genQueryGroupBy(DB, 'fastmoving_asteroids', {'id','jd'}, '*')
            %          Result = genQueryGroupBy(DB, 'visit_asteroids', {'id_visit_im','desig'}, '*')
            %          Result = genQueryGroupBy(DB, 'mergedmat_var', {'id','srcnumber'}, '*')
            %          Result = genQueryGroupBy(DB, 'visit_images', {'id_visit'}, '*')


            arguments
                Obj
                TableName   = 'fastmoving_asteroids1';
                GroupByCols = {'id','jd'};
                AddCols     = '*';
                Args.Fun         = 'min';  % 'min'|'max'|'any'
                Args.Having      = '';
                Args.PreWhere    = '';
            end

            
            if strcmp(AddCols, '*')
                % get all columns       
                [ColNames, ColTypes, Error] = getColumns(Obj, TableName, Obj.DbName);

                % remove group columns
                AddCols = setdiff(ColNames, GroupByCols);
            end

            % build template like:
            %INSERT INTO last.fastmoving_asteroids11 SELECT id, jd, any(col1), any(col2), ... FROM last.fastmoving_asteroids1 GROUP BY id, jd;

            GroupCols = tools.cell.sprintf_concatCell(", ", GroupByCols);
            if isempty(AddCols)
                AnyStr = '';
                ExtraComa = '';
            else
                AnyCols = tools.cell.sprintf2cell('min(%s) AS %s',[AddCols(:), AddCols(:)]);
                AnyStr  = tools.cell.sprintf_concatCell(", ",AnyCols);
                ExtraComa = ',';
            end

            if ~isempty(Args.Having)
                Args.Having = sprintf('HAVING %s',Args.Having);
            end
            if ~isempty(Args.PreWhere)
                Args.PreWhere = sprintf('PREWHERE %s',Args.PreWhere);
            end


            Result = sprintf("SELECT %s %s %s FROM %s %s GROUP BY %s %s", GroupCols, ExtraComa, AnyStr, TableName, Args.PreWhere, GroupCols, Args.Having);
           
        end
    end
    
    methods % utilities
        function disconnect(Obj)
            % General DB disconnect
            % Input : - self.
            % Author : Eran Ofek (Dec 2024)
            % Example: D.disconnect

            Obj.disconnectCH_Java;

        end

        function Obj=disconnectCH_Java(Obj)
            % disconnect Clickhouse Java connection
            % Input  : - self.
            % Output : - self.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.dissconectCH_Java

            close(Obj.Conn);
            Obj.Conn = [];
        end

        function [Result, Host, Port]=getDbConfig(Obj, DbName)
            % Get configuration file for db
            %   The config file is stored in AstroPack/config/
            %   under file db.<DbType>.yml
            % Input  : - self.
            %          - Optional DbName. If provided, then will compare it
            %             to available DbName in config file and will
            %             return also te Host and Port for the specific
            %             DbName.
            % Output : - A structure with the db configuration.
            %          - Host name. This is extracted if DbName is provided
            %            as the second input and it is found in the config.
            %          - Like Host, but for the Port.
            % Author : Eran Ofek (Oct 2024)
            % Example: D=db.Db;
            %          S=D.getDbConfig();
            %          [~,Host,Port]=D.getDbConfig('test_db');
            
            arguments
                Obj
                DbName  = [];
            end
            
            if isfield(Obj.Config.Data,'db')
                Result = Obj.Config.Data.db.(Obj.DbType);
                if isempty(DbName)
                    Host = [];
                    Port = [];
                else
                    Ind  = find(strcmp(Result.DbName, DbName));
                    Host = Result.Host{Ind};
                    Port = Result.Port{Ind};
                end
            else
                Host = [];
                Port = [];
                Result = [];
            end

        end
    end
    
        
    methods % main commands
        function [Result, Error] = query(Obj, Query, Args)
            % Execute Query/Exec command on database/table and format the output.
            % Input  : - self.
            %          - A string with a query or exec command to execute.
            %            Alternatively a cell array with arguments to pass
            %            to db.Db.genQuery that will generay the query
            %            string.
            %          * ...,key,val,...
            %            'IsExec' - A logical indicating if to run the
            %                   Query string as a query (false) or exec
            %                   (true). If IsExec is true, then the output
            %                   Result will be empty.
            %                   Default is false.
            %            'Convert2String' - A logical indicating if to
            %                   convert the columns in cell arrays to
            %                   string array. Default is true.
            % Output : - Table with output result. If IsExec=true, then
            %            this is empty.
            %          - Error message. If ok, then this is empty.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.query("SELECT * FROM test_db.test_table;");
            %          D.query("SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'");
            %          D.query("SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db'");

            arguments
                Obj
                Query     = [];
                
                Args.IsExec logical           = false;
                Args.Convert2String logical   = true;
                Args.Opts                     = [];         
                Args.ExactDataTypes           = false; % if true, will override Args.Opts
            end
            
            if iscell(Query)
                Query = db.Db.genQuery(Query{:});
            end
            
            if Args.ExactDataTypes % get types from the server table
                TName = regexp(Query, 'from\s+([a-zA-Z0-9_]+)', 'tokens', 'once');
                if ~isempty(TName) % if there is no table name in the query, we do not require any data type options 
                    TName = TName{1};  % extract the actual table name
                    Idx = find(strcmp(TName, Obj.DataTypes(1, :)));
                    if isempty(Idx) % the table is queried first time
                        T = Obj.describeTable(TName);
                        Types = lower(T.type);
                        Types = regexprep(Types, 'nullable\((.*?)\)', '$1');
                        Types(Types == "datetime64(3, 'utc')") = "unknown";
                        Types(contains(Types, "datetime")) = "string"; 
                        Types(Types == "float64") = "double";
                        Types(Types == "float32") = "single";
                        Types(Types == "bool") = "logical";
                        Args.Opts = databaseImportOptions(Obj.Conn,TName);
                        Args.Opts.VariableTypes = cellstr(Types(:))';
                        Obj.DataTypes{1, end+1} = TName;
                        Obj.DataTypes{2, size(Obj.DataTypes,2)} = Args.Opts;
                    else            % table data types are already in the object
                        Args.Opts =  Obj.DataTypes{2,Idx};
                    end
                end
            end

            if strcmpi(Obj.DbType, 'clickhouse') && strcmpi(Obj.ConnType, 'java')    
                if isempty(Args.Opts)
                    [Result,Error] = queryCH_Java(Obj, Query, 'IsExec',Args.IsExec);
                else
                    Args.Opts = setSQLQuery(Args.Opts,Query); % otherwise the check in fetch.m:196 fails
                    [Result,Error] = queryCH_Java(Obj, Query, 'IsExec',Args.IsExec,'Opts',Args.Opts);
                end
            else
                error('DbType=%s and ConnType=%s query is not supported', Obj.DbType, Obj.ConnType);
            end

            if Args.Convert2String && ~isempty(Result)
                Result = tools.table.table_cell2string(Result);
            end

        end

        function Error=createTable(Obj, TableName, ColNames, ColTypes, ColDefaults, Args)
            % Create table
            % Input  : - self.
            %          - Table name.
            %          - Cell array or string array of column names.
            %            Alternatively, if this is a table and the next
            %            argument is empty, then will use the column names
            %            and types.
            %          - Cell array or string arry of column types (one
            %            type per column name).
            %            Default is empty.
            %          - Cell array of column defaults. Default is {}.
            %          * ...,key,val,...
            %            'Engine' - Search engine.
            %                   Ask ChatGPT for more options.
            %                   Default is 'MergeTree()'
            %            'OrderBy' - ORDER BY column name.
            %                   Default is 'id'.
            %            'LowerCase' - true will convert column names to
            %                   lower case. Default is true.
            %            'Index' - A ceel array of additional index lines - e.g.,
            %                   'INDEX name_index name TYPE set(100) GRANULARITY 1'
            %                   Default is {}
            % Output : - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.createTable('test_db',["id"; "name"; "age"], ["UInt32"; "String"; "UInt8"]);
            %          D.createTable('test_db',["id"; "name"; "age"],["UInt32"; "String"; "UInt8"], 'Index', {'INDEX id_index id TYPE minmax GRANULARITY 1'})
            %          DB.createTable('fastmoving_asteroids1',AstC.Table);
            %
            %          Error=DB.createTable('mergedmat_var',VarAC.Table, [], 'Index', {'INDEX ra_index ra TYPE minmax GRANULARITY 1', 'INDEX dec_index dec TYPE minmax GRANULARITY 1', 'INDEX pm_jd_index pm_jd TYPE minmax GRANULARITY 1', 'INDEX id_index id TYPE minmax GRANULARITY 1', 'INDEX upix_high_index upix_high TYPE minmax GRANULARITY 1', 'INDEX upix_low_index upix_low TYPE minmax GRANULARITY 1', 'INDEX upix_partition_index upix_partition TYPE minmax GRANULARITY 1'});
            %          DB.createTable('fastmoving_asteroids',AstAC.Table, [], 'Index', {'INDEX ra_index ra TYPE minmax GRANULARITY 1', 'INDEX dec_index dec TYPE minmax GRANULARITY 1', 'INDEX jd_index jd TYPE minmax GRANULARITY 1', 'INDEX id_index id TYPE minmax GRANULARITY 1'});
            % % Example with defaults:         
            % DB.createTable('fastmoving_asteroids',AstAC.Table, [], {[],'1'}, 'Index', {'INDEX ra_index ra TYPE minmax GRANULARITY 1', 'INDEX dec_index dec TYPE minmax GRANULARITY 1', 'INDEX jd_index jd TYPE minmax GRANULARITY 1', 'INDEX id_index id TYPE minmax GRANULARITY 1'});
            % [~,Error] = DB.query('DROP TABLE IF EXISTS mergedmat_var1', 'IsExec',true)
            % [~,Error] = DB.query('TRUNCATE TABLE mergedmat_var2', 'IsExec',true)

            arguments
                Obj
                TableName
                ColNames
                ColTypes       = [];
                ColDefaults    = [];
                Args.Engine    = 'MergeTree()';
                Args.OrderBy   = 'id';
                Args.LowerCase = true;
                Args.Index     = {};
            end
           
            if istable(ColNames) && isempty(ColTypes)
                % use column names from table
                ColTypes = tools.table.colClass(ColNames);
                ColTypes = db.Db.convertClass2DB_Class(ColTypes);
                ColNames = string(ColNames.Properties.VariableNames);
            end
            if Args.LowerCase
                ColNames = lower(ColNames);
            end

            Ncol = numel(ColNames);
            Command = sprintf('CREATE TABLE %s \n(\n',TableName);
            for Icol=1:1:Ncol
                if Icol>numel(ColDefaults)
                    % ColTypes{Icol} = ColTypes{Icol};
                else
                    if ~isempty(ColDefaults{Icol})
                        if isnumeric(ColDefaults{Icol})
                            ColTypes{Icol} = sprintf('%s DEFAULT %d', ColTypes{Icol}, ColDefaults{Icol});
                        else
                            ColTypes{Icol} = sprintf('%s DEFAULT %s', ColTypes{Icol}, ColDefaults{Icol});
                        end
                    end
                end

                if Icol==Ncol
                    Nindex = numel(Args.Index);
                    if Nindex==0
                        Command = sprintf('%s %s %s\n)', Command, ColNames{Icol}, ColTypes{Icol});
                    else
                        Command = sprintf('%s %s %s,\n', Command, ColNames{Icol}, ColTypes{Icol});
                        for Iindex=1:1:Nindex
                            if Iindex==Nindex
                                Command = sprintf('%s %s\n',Command, Args.Index{Iindex});
                            else
                                Command = sprintf('%s %s,\n',Command, Args.Index{Iindex});
                            end
                        end
                        Command = sprintf('%s)',Command);
                    end
                else                    
                    Command = sprintf('%s %s %s,\n', Command, ColNames{Icol}, ColTypes{Icol});
                end

                
                    
            end
            Command = sprintf('%s\n ENGINE = %s\n  ORDER BY %s;', Command, Args.Engine, Args.OrderBy);
            
            [~,Error] = Obj.query(Command, 'IsExec',true);
            
        end

        
        function [Error,StrEx,TestNew, ColAdded]=addNewColumnsToExistingTable(Obj, Table, NewColumns, NewClass, NewDefault, Nullable)
            % Add new columns to existing table
            % Input  : - self.
            %          - Table name.
            %          - A string array of new column names.
            %          - A string array of new column's class.
            %          - A string array of new column's default. Can be a
            %            scalar (one value for all columns).
            %          - A logical indicating if all the columns are nullable.
            %            Default is true.
            % Output : - Error string.
            %          - ALTER command string executed.
            %          - Updated table description.
            %          - A vector of logical indicating for each column, if
            %            it was added.
            % Author : Eran Ofek (Aug 2025)
            % Example: NewColumns = ["Ref_RA","Ref_Dec","Ref_X","Ref_Y","Ref_Xinit","Ref_Yinit","Ref_X2","Ref_Y2","Ref_XY","Ref_FlagIn","Ref_FLAGS","Ref_AnnulusBack","Ref_AnnulusStd","Ref_SN","Ref_FLUX_PSF","Ref_ZP","Ref_MAG_PSF","Ref_Chi2","Ref_Dof","RefJD","Ref_LIMMAG"];
            %          NewClass = ["Float64","Float64","Float32","Float32","Float32","Float32","Float32","Float32","Float32","UInt32","UInt32","Float32","Float32","Float32","Float32","Float32","Float32","Float32","Float32","Float64","Float32"];
            %          NewDefault = ["NULL"]
            % [Error,StrEx]=DB.addNewColumnsToExistingTable('last.forcedphotsub_output', NewColumns, NewClass, NewDefault)

            arguments
                Obj
                Table
                NewColumns
                NewClass 
                NewDefault
                Nullable     = true;
            end

            %ALTER TABLE db.tbl
            %ADD COLUMN IF NOT EXISTS new_str  Nullable(String)  DEFAULT NULL AFTER some_col,
            %ADD COLUMN IF NOT EXISTS new_num  Nullable(UInt32)  DEFAULT NULL;
            %ADD COLUMN IF NOT EXISTS new_num  UInt32  DEFAULT 0;


            %StrEx = sprintf("ALTER TABLE %s",Table);

            NewColumns = lower(NewColumns);
            Ncol  = numel(NewColumns);
            Ndef  = numel(NewDefault);
            for Icol=1:1:Ncol
                Idef = min(Ndef,Icol);
                StrEx = sprintf("ALTER TABLE %s",Table);
                if Nullable
                    StrEx = sprintf("%s\n ADD COLUMN IF NOT EXISTS %s Nullable(%s) DEFAULT %s;", StrEx, (NewColumns{Icol}), NewClass{Icol}, NewDefault{Idef});
                else
                    StrEx = sprintf("%s\n ADD COLUMN IF NOT EXISTS %s %s DEFAULT %s;", StrEx, (NewColumns{Icol}), NewClass{Icol}, NewDefault{Idef});
                end

                [~,Error] = Obj.query(StrEx, 'IsExec',true);

            end
            
            if nargout>2
                TestNew = Obj.describeTable(Table);

                % Check that all columns were added
                ColAdded = ismember(NewColumns, TestNew.name);
            end

        end
        
        % function removeDuplicates(Obj, Args)
        %     % Remove duplicate entries (same ID) from a table
        %     %
        %     % Example: removeDuplicates(DB, 'SrcTable','last.fastmoving_asteroids1', 'DestTable','last.fastmoving_asteroids11', 'ColID','id'); 
        % 
        %     arguments
        %         Obj
        %         Args.SrcTable  = 'last.fastmoving_asteroids1';
        %         Args.DestTable = 'last.fastmoving_asteroids11';
        %         Args.ColID     = {'id','jd'};
        %     end
        % 
        %     % show old table create statment
        %     Cmd = sprintf('SHOW CREATE TABLE %s',Args.SrcTable);
        %     CreateStatment = Obj.fetch(Cmd,'Parse',true);
        %     CreateStatment = strrep(CreateStatment, Args.SrcTable, Args.DestTable);
        % 
        %     % drop new table if exist
        %     DropCmd = sprintf('DROP TABLE IF EXISTS %s', Args.DestTable);
        %     [~,Error] = Obj.query(DropCmd, 'IsExec',true);
        % 
        %     % create new table using old table parameters
        %     [~,Error] = Obj.query(CreateStatment, 'IsExec',true);
        % 
        %     RemDupCmd = sprintf("INSERT INTO %s\n SELECT * \n FROM ( \n    SELECT *,\n           row_number() OVER (PARTITION BY %s ORDER BY jd ASC) AS rn\n    FROM %s\n)\n WHERE rn = 1;",Args.DestTable, Args.ColID, Args.SrcTable);
        %     RemDupCmd = sprintf("SELECT * \n FROM ( \n    SELECT *,\n           row_number() OVER (PARTITION BY %s ORDER BY jd ASC) AS rn\n    FROM %s\n)\n WHERE rn = 1;", Args.ColID, Args.SrcTable);
        %     [~,Error] = Obj.query(RemDupCmd, 'IsExec',true);
        % 
        %     % compare length of the two tables
        % 
        % end
        
        function Error=insertCharDump(Obj, TableName, InputTable, Args)
            % Insert entries in table object into ClickHouse table using char dump (direct insert)
            %   Good for insertion of small tables.
            %   See also db.Db/insertCsv and db.Db/insert
            % Input  : - self.
            %          - Table name to which to insert the data.
            %          - An object table containing the data to insert.
            %          * ...,key,val,...
            %            'LowerCase' - Make column names lower case.
            %                   Default is true.
            % Output : - Error message. If empty, then ok.      
            % Author : Eran Ofek (Oct 2024)
            % Dxample: D.insertCharDump('Images',T)
            
            arguments
                Obj
                TableName
                InputTable
                Args.LowerCase  = true;
            end

            ColNames    = InputTable.Properties.VariableNames;
            StrColNames = sprintf('%s, ',string(ColNames));
            StrColNames = StrColNames(1:end-2);
            if Args.LowerCase
                StrColNames = lower(StrColNames);
            end
            ValuesStr   = db.Db.table2charDump(InputTable);
            Command     = sprintf("INSERT INTO %s (%s) VALUES %s", TableName, StrColNames, ValuesStr);

            [~,Error]   = Obj.query(Command, 'IsExec',true);

        end

        function [Error,FileName]=insertCsv(Obj, TableName, Data, Args)
            % Insert table object or csv file into ClickHouse table using csv format insert (bulk)
            %   Good for insertion of big tables.
            %   See also db.Db/insertCharDump and db.Db/insert
            % Input  : - self.
            %          - Table name to which to insert the data.
            %          - An object table containing the data to insert,
            %            or a csv file name.
            %          * ...,key,val,...
            %            'FileName' - If the data input is table, then this
            %                   is the csv file name that will be created.
            %                   Default is tempname.
            %            'DeleteFile' - A logical indicating if to delete
            %                   the csv file after insertion.
            %                   Default is false.
            %            'table2csvArgs' - A cell array of additional
            %                   arguments to pass to db.Db.table2csv.
            %                   Default is {}.
            % Output : - Error message. If empty, then ok.     
            %          - CSV file name.
            % Author : Eran Ofek (Oct 2024)
            % Dxample: D.insertCsv('Images',T)

            arguments
                Obj
                TableName
                Data
                Args.FileName             = tempname;
                Args.DeleteFile logical   = false;
                Args.ColumnNames          = {};
                Args.table2csvArgs        = {};
            end

            if istable(Data)
                % convert table to csv file
                FileName = Args.FileName;
                db.Db.table2csv(Data, 'FileName',Args.FileName, Args.table2csvArgs{:});
            else
                % assume Data is a csv file name
                FileName = Data;
            end

            % Assume InputTable is a scv table
            %Command = sprintf('INSERT INTO %s FORMAT CSV FILE ''%s'';', TableName, InputTable);
            %[~,Error]   = Obj.query(Command, 'IsExec',true);
            
            if isempty(Args.ColumnNames) % versions of clickhouse-client ~> 22
                Command = sprintf('clickhouse-client --host=%s --user=%s --password=%s  --input_format_with_names_use_header=1 --query="INSERT INTO %s FORMAT CSVWithNames" < %s',...
                    Obj.Host, Obj.User, Obj.Password, TableName, FileName);
            else                         % versions of clickhouse-client ~< 20 
                Command = sprintf('clickhouse-client --host=%s --user=%s --password=%s  --query="INSERT INTO %s (%s) FORMAT CSVWithNames" < %s',...
                    Obj.Host, Obj.User, Obj.Password, TableName, strjoin(Args.ColumnNames, ', '), FileName);
            end
            [~,Error] = system(Command);

            if Args.DeleteFile
                delete(FileName);
            end

        end
    
    end
    
    methods % DB, Tables information
        function Result=fetch(Obj, Command, Args)
            % Fetch data from DB 
            % Input  : - self.
            %          - Command (e.g., 'SHOW CREATE TABLE fastmoving_asteroids')
            %          * ...,key,val,...
            %            'Parse' - If true will attempt to parse data from
            %                   table. For example, if the output table
            %                   contains a single column and a single line
            %                   then will return this clean line.
            %                   Default is false.
            % Output : - Table with output, or the content of the entry (if
            %            Parse=true).
            % Author : Eran Ofek (Apr 2025)
            % Example: DB.fetch('SHOW CREATE TABLE diff_src')
            %          DB.fetch('SHOW CREATE TABLE diff_src','Parse',true)

            arguments
                Obj
                Command,
                Args.Parse    = false;
            end

            Result = fetch(Obj.Conn, Command);

            if Args.Parse
                if numel(Result.Properties.VariableNames)==1
                    Result = Result.(Result.Properties.VariableNames{1}){1};
                else
                    error('Can not parse - table with multiple columns');
                end
            end

        end

        function [Result, Error] = showDB(Obj, Args)
            % Show all databases
            %   Using the 'SHOW DATABSES;' query.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Convert2String' - Default is true.
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - A table with column name 'name' containing a list
            %            of all databases.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showDB

            arguments
                Obj                
                Args.Convert2String logical   = true;
                Args.ReturnString logical     = true;
            end

            Query = 'SHOW DATABASES;';
%             [Result,Error] = query(Obj, Query, 'Convert2String',Args.Convert2String);
            Result = fetch(Obj.Conn, Query);
            
            if Args.Convert2String && ~isempty(Result)
                Result = tools.table.table_cell2string(Result);
            end

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end

        function [Result, Error] = showCurrentDB(Obj, Args)
            % Show current database (in use)
            %   Using the 'SELECT currentDatabase();' query.
            % Input  : - self.
            %          * ...,key,val,...
            %            'Convert2String' - Default is true.
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - Current DB (string).
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showCurrentDB

            arguments
                Obj                
                Args.Convert2String logical   = true;
                Args.ReturnString logical     = true;
            end

            Query = 'SELECT currentDatabase();';
            [Result,Error] = query(Obj, Query, 'Convert2String',Args.Convert2String);

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end

        function [Error]=useDB(Obj, DbName)
            % Change database (DbName property will change)
            %   Uisng the USE <DbName> query
            % Input  : - self.
            %          - Database name (use showDB to see list of DBs).
            % Output : - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.useDB('test_db');


            Query = sprintf('USE %s',DbName);
            [~,Error] = query(Obj, Query, 'Convert2String',false, 'IsExec',true);

            if isempty(Error)
                Obj.DbName = DbName;
            end

        end

        function [Result, Error] = showTables(Obj, Args)
            % Retuen all tables in current DB
            %       Use showCurrentDB to get the current DB.
            % Input  : - self.
            %          * ...,key,val,...
            %            'ReturnString' - Logical indicating if to return
            %                   string array (true), or table (false).
            %                   Default is true.
            % Output : - List of tables in DB.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.showTables

            arguments
                Obj
                Args.ReturnString logical   = true;
            end

            Query = 'SHOW TABLES;';
%             [Result, Error] = Obj.query(Query);
            Result = fetch(Obj.Conn, Query);

            if Args.ReturnString
                Result = Result.(Result.Properties.VariableNames{1});
            end
        end

        function [Result, Error] = describeTable(Obj, TableName, Args)
            % Get table description (schema) with all column names and their properties.
            % Input  : - self.
            %          - Table name, or <database>.<table_name>
            %          * ...mkey,val,...
            %            'Convert2String' - A logical indicating if to
            %                   convert the columns in cell arrays to
            %                   string array. Default is true.
            % Author : Eran Ofek (Oct 2024)
            % Example: D.describeTable('test_table')
            %          D.describeTable('test_db.test_table')

            arguments
                Obj
                TableName
                Args.Convert2String logical   = true;
            end

            Query = sprintf('DESCRIBE %s;',TableName);
%             [Result, Error] = Obj.query(Query, 'Convert2String',Args.Convert2String);
            Result = fetch(Obj.Conn, Query);
            if Args.Convert2String && ~isempty(Result)
                Result = tools.table.table_cell2string(Result);
            end

        end

        function [ColNames, ColTypes, Error] = getColumns(Obj, TableName, DbName)
            % Return all column names and their type in a table.
            % Input  : - self.
            %          - Table name.
            %          - DB name. If empty, then use current DB.
            %            Default is [].
            % Output : - A string array of column names.
            %          - A string array of types of columns.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Example: [ColNames, ColTypes]=D.getColumns('test_db')
            
            arguments
                Obj
                TableName
                DbName     = [];
            end

            if isempty(DbName)
                DbName = Obj.showCurrentDB;
            end

            Query = sprintf('SELECT name, type FROM system.columns WHERE table = ''%s'' AND database = ''%s''', TableName, DbName);
            [Tmp, Error] = Obj.query(Query, 'Convert2String',true);
            ColNames = Tmp.name;
            ColTypes = Tmp.type;
            
        end
     
    end
    
    methods % low level functions
        function [Result,Error]=queryCH_Java(Obj, Query, Args)
            % Query Clickhouse DB using Java interface
            % Input  : - self.
            %          - A string with a query or exec command to execute.
            %          * ...,key,val,...
            %            'IsExec' - A logical indicating if to run the
            %                   Query string as a query (false) or exec
            %                   (true). If IsExec is true, then the output
            %                   Result will be empty.
            %                   Default is false.
            % Output : - Output.
            %          - Error message.
            % Author : Eran Ofek (Oct 2024)
            % Notes  : Java is x2 faster compared to the http interface.
            % Example: D.queryCH_Java("SELECT * FROM test_db.test_table;");
            
            
            arguments
                Obj                
                Query     = "SELECT * FROM test_db.test_table;";                
                
                Args.IsExec logical           = false;
                Args.Opts                     = [];
                Args.UseExec logical          = false;
            end
            
            Error = Obj.Conn.Message;
            if isempty(Error)
                % Execute the query
                if Args.IsExec
                    exec(Obj.Conn, Query);
                    Result = [];
                else
                    if Args.UseExec
                        % use exec instead of select

JConn = Obj.Conn.Handle;    % should now be ClickHouseConnectionImpl without class conflicts
Stmt = JConn.createStatement();
Stmt.setQueryTimeout(600);   % seconds

% 3) Execute your SQL string (not 'Result') and get a java.sql.ResultSet
rs = stmt.executeQuery(Query);


                        Curs = exec(Obj.Conn, Query);              % send the query
                        Curs = set(Curs, 'QueryTimeout',600);
                        Curs.QueryTimeout = 600;

                        Curs = fetch(Curs, '-mode', 'cell');       % pull back results in cell mode
                        Result = Curs.Data;
                        close(Curs);
                    else
                        if isempty(Args.Opts)   
                            Result = select(Obj.Conn, Query); %, 'QueryTimeOut',600);
    
                            %Curs   = exec(Obj.Conn, Query);
                            %Curs.Statement.setQueryTimeout(600);
                            %Curs   = fetch(Curs);
                            %Result = Curs.Data;
                            %close(Curs);
                        else
                            Result = fetch(Obj.Conn, Query, Args.Opts);
                        end
                    end
                end
        
                % Convert result to a table if it's not already a table
                if ~istable(Result) && ~isempty(Result)
                    Result = struct2table(Result);
                end
                
            else
                Result = [];
            end
        
        end
        
        function Result=queryCH_Http(Obj, TableName, Query)
            % Query Clickhoues using the http interface [NOT FINALIZED]
            
            arguments
                Obj
                TableName = "test_db.users";
                Query     = "SELECT * FROM test_db.users;";
                
                %"SELECT name FROM system.columns WHERE table = 'users' AND database = 'test_db'";

                %SELECT name, type FROM system.columns WHERE table = 'users' AND database = 'test_db';

                %"SELECT * FROM test_db.users;";
            end
       
                       
            % Define ClickHouse server URL for HTTP interface
            %URL = 'http://localhost:8123/';
            URL = sprintf('http://%s:%s/',Obj.Host, Obj.Port);
    
            QueryURL = sprintf("%s?user=%s&password=%s&query=%s", URL, Obj.User, Obj.Password, urlencode(Query));
      
            % Send HTTP request and fetch result as text
            try
                QueryResult = webread(QueryURL);
            catch ME
                error('Error querying ClickHouse: %s', ME.message);
            end

            Ncol = 3;
            
            Format = tools.string.str_duplicate('%s ',Ncol, '\n');
            CellResult = textscan(QueryResult, Format,'Delimiter','\t');
                
            
            ResultLines = strsplit(QueryResult, '\n');
            
            % Remove any empty lines from the result
            %ResultLines = ResultLines(~tools.cell.isempty_cell(ResultLines));
            
            
            %Result = cellfun(@(x) strsplit(x, '\t'), ResultLines, 'UniformOutput', false);
            %Result = table((vertcat(ResultCell{:})));
           
            Result = QueryResult;
        end
            
    end
        
        
            
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
%         Result = unitTest()
        
        function DB = connectLAST_DB(Args)
            arguments
                Args.Pass
            end
            DB = db.Db;
            DB.User = 'last_user';
            DB.Password = Args.Pass;
            DB.useDB('last');
        end
    end
    
end
