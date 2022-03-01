
function Result = BigDbTest2()
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    % 
    DatabaseName = 'perftest';
    TableName = 'table_a';   %'big_table2';
    Cols = 50;
    BatchSize = 200000;
    PidPk = 0;
    UseIntPk = true;
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug);
    
    Q = db.DbQuery(DatabaseName);
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Postgres version: %s', pgver);
    Q.TableName = TableName;
          
    BaseDate = 738580;  % 27/02/2022
    StartIntPk = int32((now()-BaseDate)*10000) * 100000;
    StartStringPK = datestr(now, 'yyyy_mm_dd_HH_MM_SS');
       
    UnitTestDb = strcmp(DatabaseName, 'unittest');
    
    % Prepare data
    Data = struct;
    ColNames = {};
    %ColNames{end+1} = 'pk1';
    for Col=1:Cols
        if UnitTestDb
            ColNames{end+1} = sprintf('fdouble%02d', Col);
        else
            ColNames{end+1} = sprintf('fdouble%03d', Col);
        end
    end    
    
    for i=1:BatchSize
        %Data(i).recid = '_';
        Data(i).pk1 = 0;
        for Col=1:Cols
            Data(i).(ColNames{Col}) = Col;
        end
    end
        
    %
    CsvFileName = fullfile(tools.os.getTempDir(), 'BigDbTest.csv');    
    %[ServerFileName, ClientFileName] = Q.getSharedFileName(CsvFileName);
    %CsvFileName = ClientFileName;
    
            
    BatchCounter = 1;
    RowIndex = 1;
    while true
        
        % Generate CSV       
        % io.msgLog(LogLevel.Test, '[%05d] preparing data: %d x %d...', BatchCounter, BatchSize, Cols);
        
        % Update data with keys
        for i=1:BatchSize
            Data(i).pk1 = StartIntPk + RowIndex;
            RowIndex = RowIndex+1;
        end
        
        % Update data with keys
%         for i=1:BatchSize
%             if UseIntPk
%                 if PidPk > 0
%                     Data(i).pk1 = PidPk;
%                     Data(i).pk2 = StartIntPk + RowIndex;
%                 else
%                     Data(i).pk1 = StartIntPk + RowIndex;
%                 end                    
%             else
%                 Data(i).recid = sprintf('%s_%08d', StartStringPK, RowIndex);            
%             end
%             RowIndex = RowIndex+1;
%         end

        
        %io.msgLog(LogLevel.Test, '[%05d] writing csv file: %s', BatchCounter, CsvFileName);
        Table = struct2table(Data);
        writetable(Table, CsvFileName);           
        
        RowCount = Q.selectCount();
        t1 = tic;
        Q.insert([], 'CsvFileName', CsvFileName);        
        io.msgLog(LogLevel.Test, '[%05d] RowCount=%d, insert %d x %d: %0.5f sec', BatchCounter, RowCount, BatchSize, Cols, toc(t1));
        %delete(CsvFileName);
        
        BatchCounter = BatchCounter + 1;
    end
       
end

