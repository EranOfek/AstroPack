
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
    Count = Q.selectCount('TableName', 'master_table');
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
    Q.select('*', 'TableName', 'master_table', 'CsvFileName', 'c:/temp/master_table.csv');
    
    % Insert Csv file, it must include primary key column @Todo
    %Q.insert(
    
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
    ColumnList = Q.getTableColumnList('master_table');
    fprintf('Columns: %s\n', strjoin(ColumnList, ','));

    
    % Select all records to CSV file
    % This requires that config/local/Database.DbConnections.UnitTest.yml
    % includes correct values for ServerSharePath and MountSharePath
    % See instructions how to set shared folder in:
    % matlab/util/+db/@DbAdmin/share_linux_folder.md
    Q.select('*', 'TableName', 'master_table', 'UseCopy', true, 'Load', false);
    CsvFileName = Q.ClientShareFileName;
    fprintf('CSV file: %s\n', CsvFileName);
    if isfile(CsvFileName)
        Tab = readtable(CsvFileName);
        sz = size(Tab);
        fprintf('Rows: %d, Cols: %d\n', sz(1), sz(2));
    end       
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
