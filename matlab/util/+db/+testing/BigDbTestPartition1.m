
function Result = BigDbTestPartition1()
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    % 
    DatabaseName = 'perftest';
    TableName = 'table_f_part1';   %'big_table2';
    Cols = 50;
    BatchSize = 200000;
    Pid = feature('getpid');
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug);
    
    Q = db.DbQuery(DatabaseName);
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Postgres version: %s', pgver);
    Q.TableName = TableName;         
    
    %
    FileName = sprintf('BigDbTest_%d.csv', Pid);
    CsvFileName = fullfile(tools.os.getTempDir(), FileName);    
    
    [~, ClientFileName] = Q.getSharedFileName(CsvFileName);
    CsvFileName = ClientFileName;

    
    % Prepare data
    Data = struct;
    ColNames = {};
    for Col=1:Cols
        ColNames{end+1} = sprintf('fdouble%03d', Col);
    end    
    
    % Prepare data
    x = 0.1;
    time = rand;
    for i=1:BatchSize
        Data(i).f_time = time;
        for Col=1:Cols
            Data(i).(ColNames{Col}) = x; %Col;
            x = x + 1;
        end
    end
         
            
    BatchCounter = 1;
    RowCount = 0;
    
    while true

        % Create new partition
        %pause(1)        
        BaseDate = 738580;  % 27/02/2022
        f_time = int32((now()-BaseDate)*1000000);
    
        f_time = floor((now-738000)*100000);
        f_start = f_time * 10000;
        %SqlText = sprintf('CREATE TABLE table_f_part1_%d PARTITION OF table_f_part1 FOR VALUES FROM (%f) TO (%f)', f_time, f_start, f_start+9999);
        %Q.exec(SqlText);
        
        Q.createPartition(sprintf('table_f_part1_%d', f_time), 'Type', 'range', ...
            'RangeStart', f_start, 'RangeEnd', f_start+9999);
                       
        for Iter=1:10
            
            % Update data
            io.msgLog(LogLevel.Debug, 'preparing data...');
            time = f_start + Iter;
            for i=1:BatchSize
                Data(i).f_time = time;
            end
            
            writeStructCsv_mex(Data, CsvFileName);
            %Table = struct2table(Data);
            %writetable(Table, CsvFileName);

            if mod(BatchCounter, 10) == 1
                RowCount = Q.selectTableRowCount('Fast', true);
            else
                RowCount = RowCount + BatchSize;
            end

            io.msgLog(LogLevel.Debug, 'inserting...');
            t1 = tic;
            Q.insert([], 'CsvFileName', CsvFileName);        
            io.msgLog(LogLevel.Test, '[%05d] RowCount=%d, insert %d x %d: %0.5f sec', BatchCounter, RowCount, BatchSize, Cols, toc(t1));

            BatchCounter = BatchCounter + 1;
        end
    end
       
end

