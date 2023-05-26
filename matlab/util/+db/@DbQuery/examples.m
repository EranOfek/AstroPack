
function Result = examples()
    % DbQuery examples
    %
    % Install Postgres v14
    % Install pgAdmin4 (version 6.3) as administration tool    
    %
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
    %
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    Q = db.DbQuery('unittest');
    
    % Set table name so next calls will not need to specify it       
    Q.TableName = 'master_table';
    
    % Query Postgres version, result should be similar to
    % 'Version: PostgreSQL 14.1 (Ubuntu 14.1-1.pgdg18.04+1) on x86_64-pc-linux-gnu,
    % compiled by gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0, 64-bit'
    pgver = Q.getDbVersion();
    fprintf('Postgres version: %s\n', pgver);
       
    % Insert record from struct, update it below
    Uuid = Component.newUuid();
    R = db.DbRecord;    
    R.Data(1).recid = sprintf('%s_%02d', Uuid, 1);
    R.Data(1).fint = 0;
    R.Data(1).fdouble = 0;            
    R.Data(1).fstring = sprintf('MyStr_%03d', 1);
    disp(R.Data(1));
    Q.insert(R);
    
    % Update using the same recid(s) used above
    Where = sprintf('recid like ''%s_%%''', Uuid);
    MyStr = sprintf('NewValue_%04d', 1);
    Q.update(sprintf('fint=%d,fdouble=%f,fstring=''%s''', 1, 0.1*i, MyStr), 'Where', Where);

    % Select one record and delete it
    R = Q.select('recid', 'Limit', 1, 'Order', 'recid ASC');
    recid = R.Data(1).recid;
    Where = sprintf("recid = '%s'", recid);
    Q.deleteRecord('Where', Where);
      
    % Insert matrix with user-function to generate primary key
    % make_recid will be called from insert()
    Mat = rand(10, 2);
    Q.insert(Mat, 'ColNames', 'fdouble1,fdouble2', 'InsertRecFunc', @make_recid, 'BatchSize', 10000);
   
    % Select number of rows in table
    Count = Q.selectTableRowCount('TableName', 'master_table');
    fprintf('Rows: %d\n', Count);
    
    % Select all columns with limit, return output as table
    Where = 'fdouble1 > fdouble2';
    Tab = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'OutType', 'table', 'Where', Where);
    disp(Tab);
      
    % Select double columns into AstroTable
    AstTable = Q.select('fdouble1,fdouble2', 'TableName', 'master_table', 'OutType', 'AstroTable', 'Limit', 100);
    disp(AstTable.Catalog);
    
    % Insert AstroTable, callback is used to generate primary key
    Q.insert(AstTable, 'InsertRecFunc', @make_recid);
               
    
    % Insert AstroHeader, callback is called to generate primary key
    % 'ColumnsOnly', true - means that only columns that exist in the
    % database table will be added to the INSERT statement
    H = AstroHeader();
    H.insertKey({'FInt1',7777,'CommentA'; 'FInt2',2,'CommentB'; 'FIntXX','XX','CommentXX'}, 'end-1');
    Q.insert(H, 'TableName', 'master_table', 'InsertRecFunc', @make_recid, 'ColumnsOnly', true);
     
    % Select to CSV file
    % Note: Currently Shared Path must be used, as CSVWriter does not work
    % from unknown reason (24/03/2022)
    
    %MyCsvFileName = fullfile(tools.os.getTempDir(), 'my_select.csv');
    %Result = Q.select('*', 'TableName', 'master_table', 'CsvFileName', MyCsvFileName);
    %disp(Result);
    
    % Insert Csv file, it must include primary key column
    [MyPath,~,~] = fileparts(mfilename('fullpath'));
    CsvFileName = fullfile(MyPath, 'unitTest_insert1.csv');
    Where = "recid like 'pk_UnitTest_%'";
    Q.deleteRecord('TableName', 'master_table', 'Where', Where);
    Count = Q.selectTableRowCount('TableName', 'master_table', 'Where', Where);
    fprintf('Count after delete %d\n', Count);
    
    Q.insert([], 'TableName', 'master_table', 'CsvFileName', CsvFileName);
    
    Count = Q.selectTableRowCount('TableName', 'master_table', 'Where', Where);
    fprintf('Count after insert %d\n', Count);
    
    % Select into AstroHeader
    H = Q.select('*', 'Where', 'FInt1 = 7777', 'OutType', 'AstroHeader');
    fprintf('AstroHeaders: %d\n', numel(H));
    disp(H);
    
    % Get list of fields composing the primary key
    PkList = Q.getTablePrimaryKey('master_table');
    fprintf('Primary keys: %s\n', strjoin(PkList, ','));
    
    % Get list of index NAMES (not the fields)
    IndexList = Q.getTableIndexList('master_table');
    fprintf('Index names: %s\n', strjoin(IndexList, ','));
    
    % Get tables list
    TablesList = Q.getTablesList();
    fprintf('Tables: %s\n', strjoin(TablesList, ','));    
    
    % Get columns list
    ColumnList = Q.getTableColumnNames('master_table');
    fprintf('Columns: %s\n', strjoin(ColumnList, ','));

    
    % Select all records to CSV file
    % This requires that config/local/Database.DbConnections.UnitTest.yml
    % includes correct values for ServerSharePath and MountSharePath
    % See instructions how to set shared folder in:
    % matlab/util/+db/@DbAdmin/postgressql_installation.md
    CsvFileName = Q.select('*', 'TableName', 'master_table', 'Limit', 1000, 'UseCopy', true, 'Load', false);
    fprintf('CSV file: %s\n', CsvFileName);
    if isfile(CsvFileName)
        Tab = io.files.readtable1(CsvFileName);
        sz = size(Tab);
        fprintf('Rows: %d, Cols: %d\n', sz(1), sz(2));
    end       
    
    Result = true;
end

%--------------------------------------------------------------------------
% User callback function to generate primary key for records
% Called from insert() with argument 'InsertRecFunc', @make_recid
function Result = make_recid(Query, Rec, First, Last, Args)
    arguments
        Query   db.DbQuery
        Rec     db.DbRecord
        First
        Last
        Args.PK = 'recid'       % This is the column name for priamry key
    end
    UU = Rec.newKey();
    for i=First:Last
        
        % Generate here the actual primary key column
        Rec.Data(i).(Args.PK) = sprintf('PK_%s_%08d', UU, i);
    end
    Result = true;
end

%--------------------------------------------------------------------------

function Result = examples_admin()
    % DbAdmin examples (see also DbAdmin.unitTest)
    %
    % Requirements:
    %
    % 1. Install Postgres v14 - See detailed instructions in files:
    %
    %       postgres_installation_ubuntu18.md
    %       postgres_shared_folder.md
    %
    % 2. Install pgAdmin4 (version 6.3) as administration tool    
    %    On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    %    On Linux, use DataGrip by JetBrains (License is required)
    %
    % 3. You need to have configuration file with database user and password, as:
    %
    %       config/local/Database.DbConnections.UnitTest.yml
    %
    % From command line (term), connect to server:
    % 
    %       psql -h gauss -U admin -d unittest -W 
    %
    % ServerSharePath : '/var/samba/pgshare'
    % MountSharePath  : '/media/gauss_pgshare' 
    %
    %----------------------------------------------------------------------
    
    % Create DbAdmin instance from arguments:
    %
    % When you connect, you must specify a database name.
    % Postgres always have a database called 'postgres' that you can connect
    % to with full permissions.
    % Default port is 5432, we connect to server named 'gauss'
    Admin = db.DbAdmin('Host', 'gauss', 'Port', 5432, 'UserName', 'postgres', 'Password', 'PassRoot', 'DatabaseName', 'postgres');
    
    % Create local configuration file for database
    % Local configuration files are stored in AstroPack/config/local/ folder
    ConfigFileName = Admin.createConnectionConfig('DatabaseName', 'unittest6', 'Host', 'gauss', 'Port', 5432, 'UserName', 'admin', 'Password', 'Passw0rd');
    fprintf('Config file created: %s\n', ConfigFileName);
        
    % Create database from XLSX file (Google Sheets)
    % NOTE: If the system() call fails, copy and paste the command in new
    % 'term' window. 
    % NOTE: bash uses 'export var=...' and tcsh uses 'setenv var ...'
    % NOTE: You need Ultrasat repoistory and ULTRASAT_PATH environment set 
    %       to its location, because the Python script is located there.
    %
    % Assume that we downloaded Google Sheets file and stored it as 'unittest5.xlsx'.
    xlsx = fullfile(tools.os.getAstroPackPath(), 'database/xlsx/unittest5.xlsx');
    if isfile(xlsx)
        % Check if database already exists
        Exists = Admin.isDatabaseExist('unittest5');
        fprintf('Before create, Exists: %d\n', Exists);
        
        % Create database from file
        Result = Admin.createDatabase('XlsFileName', xlsx);

        % Check if it was created        
        Exists = Admin.isDatabaseExist('unittest5');
        fprintf('Afetr create, Exists: %d\n', Exists);        
    end
   
    % Get list of databases
    DbList = Admin.getDbList();
    disp(DbList);
    
    % Get list of users (roles)
    UserList = Admin.getUserList();
    disp(UserList);
    
    % Add user
    % To check that user was created, use pgAdmin, right-click on the
    % database name (i.e. 'unittest5'), select 'Properties' and open tab 'Secutity'
    %
    % Add user with full permissions on specified database
    Admin.addUser('test2', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'full');
    
    % Add user with read-only permissions on specified database
    Admin.addUser('test2r', 'pass', 'DatabaseName', 'unittest5', 'Permission', 'read');    

    % Remove user
    % Note that removing users may fail of there are databases or tables
    % that depends on this user.
    % In such case, it is required to first remove the dependecy using pgAdmin.
    % Admin.removeUser('Test1');
    
    Result = true;
end
