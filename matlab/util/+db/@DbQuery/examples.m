
function Result = examples()
    % DbQuery examples
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 

    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    
    
    % Query Postgres version, result should be similar to
    % 'PostgreSQL 13.1, compiled by Visual C++ build 1914, 64-bit'    
    Q = db.DbQuery('unittest');    
    pgver = Q.getDbVersion();
    fprintf('Postgres version: %s\n', pgver);
    
    io.msgLog(LogLevel.Test, 'Version: %s', pgver);
    assert(contains(pgver, 'PostgreSQL'));

    % Set table name so next calls will not need to specify it
    Q.TableName = 'master_table';
    
    
    % Insert record and update some records
    Uuid = Component.newUuid();
    R = db.DbRecord;    
    R.Data(1).recid = sprintf('%s_%02d', Uuid, i);
    R.Data(1).fint = 0;
    R.Data(1).fdouble = 0;            
    R.Data(1).fstring = sprintf('MyStr_%03d', i);        
    Q.insert(R);
    
    Where = sprintf('recid like ''%s_%%''', Uuid);
    MyStr = sprintf('NewValue_%04d', i);
    Q.update(sprintf('fint=%d,fdouble=%f,fstring=''%s''', i, 0.1*i, MyStr), 'Where', Where);

    
    % Select number of rows in table
    Count = Q.selectCount('TableName', 'master_table');
    fprintf('Rows: %d\n', Count);
    
    % Select all columns with limit, return ourput as table
    Where = 'fdouble1 > fdouble2';
    Tab = Q.select('*', 'TableName', 'master_table', 'Limit', 10, 'OutType', 'table', 'Where', Where);

      
    % Select double columns into AstroTable
    Columns = 'fdouble1,fdouble2';
    AstTable = Q.select(Columns, 'TableName', 'master_table', 'OutType', 'AstroTable', 'Limit', 100);

   
    % Insert struct array
    R = db.DbRecord;    
    for i = 1:10
        R.Data(i).recid = R.newKey();
        R.Data(i).fint = 1000 + i;
        R.Data(i).fbool = true;
        R.Data(i).fstring = sprintf('MyStr_%03d', i);        
    end
    Q.insert(R);
          
    
    % Insert with user-function to generate primary key
    ColNames = 'fdouble1, fdouble2';
    Mat = rand(10, 2);
    Q.insert(Mat, 'ColNames', ColNames, 'InsertRecFunc', @make_recid, 'BatchSize', 10000);

    
    
    % Select one record and delete it
    R = Q.select('recid', 'Limit', 1);
    recid = R.Data(1).recid;
    Where = sprintf("recid = '%s'", recid);
    Q.deleteRecord('Where', Where);

            
    % Get list of fields composing the primary key
    PkList = Q.getTablePrimaryKey('master_table');
    disp(PkList);
    
    % Get list of index NAMES (not the fields)
    IndexList = Q.getTableIndexList('master_table');
    disp(IndexList);
    
    % Get tables list
    TablesList = Q.getTablesList();
    disp(TablesList);
    
    % Get columns list
    ColumnList = Q.getTableColumnList('master_table');
    disp(ColumnList);
    
    % Select all records to CSV file, 
    Q.select('*', 'TableName', 'master_table', 'UseCopy', true, 'Load', false);
    CsvFileName = Q.ClientShareFileName;
    fprintf('CSV file: %s\n', CsvFileName);
   
    
    % Convert AstroHeader to DbRecord
    H = AstroHeader();
    H.insertKey('Key1');
    H.insertKey({'KeyA','ValueA','CommentA';'KeyB','ValueB','CommentB'},'end-1');
    R = db.DbRecord(H);
    
    
    H = AstroHeader();
    H.insertKey({'FInt1',1,'CommentA';'FInt2',2,'CommentB';'FIntXX','XX','CommentXX'},'end-1');
    Q.insert(H, 'TableName', 'master_table', 'InsertRecFunc', @make_recid, 'ColumnsOnly', true);
    
    %
    Q = db.DbQuery('unittest:master_table');
    Q = db.DbQuery('unittest', 'TableName', 'master_table');
    io.msgLog(LogLevel.Test, 'Number of records in table: %d', Q.selectCount());
    

    ColNames = 'fdouble1,fdouble2,fdouble3';
    if isfield(Q.Config.Data.Database.DbConnections.UnitTest, 'DoubleFields')
        ColNames = Q.Config.Data.Database.DbConnections.UnitTest.DoubleFields;
    end
    
    Cols = numel(strsplit(ColNames, ','));
    Mat = rand(1000, Cols);
    R = db.DbRecord(Mat, 'ColNames', ColNames);
    %Q.insert(R, 'InsertRecFunc', @make_recid);
    Q.insert(Mat, 'ColNames', ColNames, 'InsertRecFunc', @make_recid, 'InsertRecArgs', {'PK', 'recid'});
    
    % Select fields
    Q = db.DbQuery('unittest:master_table');    
    Limit = 10000;    
    Columns = 'recid,fdouble1,fdouble2,fdouble3';
    
    % Still need to find a solution for this:
    % https://gpdb.docs.pivotal.io/6-9/admin_guide/load/topics/g-loading-data-with-copy.html
    % The COPY source file must be accessible to the postgres process on the master host. 
    % Specify the COPY source file name relative to the data directory on the master host, or specify an absolute path.
    UseCopy = false;  %%% !!!!!!!!    
    
    % CsvFileName
    Q.select(Columns, 'CsvFileName', 'C:/temp/test_select.csv');

    % Compare performance of SELECT vs COPY TO
    for Iter=1:5
        TempFile = 'C:/temp/__tmp1.csv';
        delete(TempFile);

        t = tic();
        RecSelect = Q.select(Columns,  'Limit', Limit);
        io.msgStyle(LogLevel.Test, 'magenta', 'SELECT: %0.5f', double(tic()-t)/1e7);

        t = tic();
        
        % @Todo
        RecCopy   = Q.select(Columns,  'Limit', Limit, 'UseCopy', UseCopy);   %, 'TempName', TempFile);
        io.msgStyle(LogLevel.Test, 'magenta', 'SELECT using COPY: %0.5f', double(tic()-t)/1e7);

        assert(numel(RecSelect.Data) == numel(RecCopy.Data));
    end
    
    %Mat1 = Q.select('fdouble1,fdouble2,fdouble3',       'Where', 'fdouble1 > fdouble2', 'OutType', 'mat', 'Limit', Limit);
    Rec1 = Q.select('recid,fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'Limit', Limit, 'UseCopy', UseCopy, 'TempName', 'C:/temp/__tmp1.csv');
    Rec2 = Q.select('recid,fdouble1,fdouble2,fdouble3', 'Where', 'fdouble1 > fdouble2', 'Limit', Limit, 'UseCopy', UseCopy, 'TempName', 'C:/temp/__tmp2.csv');
        
    
    % Insert Mat
    Q = db.DbQuery('unittest:master_table', 'InsertRecFunc', @make_recid);    
    Mat = rand(10, 3);
    Q.insert(Mat, 'ColNames', 'fdouble1,fdouble2,fdouble3');
    
    DoubleColumns = 'fdouble1,fdouble2,fdouble3';
    Cols = numel(strsplit(DoubleColumns, ','));
    Mat = rand(1000, Cols);
    Q.insert(Mat, 'ColNames', DoubleColumns);
 
end



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
        Rec.Data(i).(Args.PK) = sprintf('PK_%s_%08d', UU, i);
    end
    Result = true;
end
