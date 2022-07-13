
function Result = BigDbTest()
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
      
    % Query Postgres version, result should be similar to
    % 'Version: PostgreSQL 14.1 (Ubuntu 14.1-1.pgdg18.04+1) on x86_64-pc-linux-gnu,
    % compiled by gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0, 64-bit'
    pgver = Q.getDbVersion();
    fprintf('Postgres version: %s\n', pgver);
       
    % Set table name so next calls will not need to specify it       
    Q.TableName = 'big_table2';
    
    ColNames = Q.getColumnNamesOfType('Double');
    Cols = 50; %numel(strsplit(ColNames, ','));
    %assert(~isempty(ColNames));
    
    StartPK = datestr(now, 'yyyy_mm_dd_HH_MM_SS_FFF');
    BatchSize = 20000;
    Index = 1;
    X = 1;
    
    % Prepare data
    Data = struct;
    for i=1:BatchSize
        Data(i).recid = '_';  %sprintf('%s_%08d', StartPK, Index);            
        for Col=1:Cols
            Data(i).(sprintf('fdouble%02d', Col)) = X;
            X = X + 1;
        end
        Index = Index+1;
    end
        
    CsvFileName = fullfile(tools.os.getTempDir(), 'BigDbTest.csv');    
    
    Counter = 1;
    while true
        
        % Generate CSV       
        %fprintf('[%05d] preparing data: %d x %d...\n', Counter, BatchSize, Cols);
        
        % Update data with keys
        for i=1:BatchSize
            Data(i).recid = sprintf('%s_%08d', StartPK, Index);            
            Index = Index+1;
        end
        
        %fprintf('[%05d] writing csv file: %s\n', Counter, CsvFileName);
        Table = struct2table(Data);
        writetable(Table, CsvFileName);           
        
        Count = Q.selectTableRowCount();
        %fprintf('Count: %d\n', Count);
        t1 = tic;
        Q.insert([], 'CsvFileName', CsvFileName);        
        io.msgLog(LogLevel.Test, '[%05d] RowCount: %d, insert %d x %d: %0.5f sec\n', Counter, Count, BatchSize, Cols, toc(t1));
        delete(CsvFileName);
        Counter = Counter + 1;
    end
       
end

