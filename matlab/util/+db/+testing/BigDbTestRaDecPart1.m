
function Result = BigDbTestRaDecPart1()
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    % 
    DatabaseName = 'unittest';
    TableName = 'table_f_part2'; 
    Cols = 52;
    BatchSize = 200;
    Pid = feature('getpid');
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug);
    
    Q = db.DbQuery(DatabaseName);
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Postgres version: %s', pgver);
    Q.TableName = TableName;
             
    %
    FileName = sprintf('BigDbTestPart1_%d.csv', Pid);
    CsvFileName = fullfile(tools.os.getTempDir(), FileName);    
    
    [ServerFileName, ClientFileName] = Q.getSharedFileName(CsvFileName);
    CsvFileName = ClientFileName;
    
    % Prepare data
    Data = struct;
    x = 0.001;
    for i=1:BatchSize
        Data(i).f_ra = 180 * (i / BatchSize);
        Data(i).f_dec = 180 * (i / BatchSize);
        for Col=1:50
            ColName = sprintf('fdouble%03d', Col);
            Data(i).(ColName) = x;
            x = x + 1;
        end
    end       
   
    Table = struct2table(Data);
    writetable(Table, CsvFileName);                     
            
    BatchCounter = 1;
    RowCount = 0;
    ItersPerPartition = 10;
    
    SqlText = sprintf('SELECT MAX(pk1) FROM %s', TableName);
    Q.query(SqlText);
    pk_start = Q.getColumn('max');
               
    while true
       
        Q.createPartition(sprintf('table_f_part1_%d', pk_start), 'Type', 'range', ...
            'RangeStart', pk_start, 'RangeEnd', pk_start + ItersPerPartition*BatchSize);

        for Iter=1:ItersPerPartition
                    
            if mod(BatchCounter, 10) == 1
                RowCount = Q.selectCount('Fast', true);
            else
                RowCount = RowCount + BatchSize;
            end

            t1 = tic;
            Q.insert([], 'CsvFileName', CsvFileName);        
            io.msgLog(LogLevel.Test, '[%05d] RowCount=%d, insert %d x %d: %0.5f sec', BatchCounter, RowCount, BatchSize, Cols, toc(t1));

            BatchCounter = BatchCounter + 1;
        end
    end
       
end

