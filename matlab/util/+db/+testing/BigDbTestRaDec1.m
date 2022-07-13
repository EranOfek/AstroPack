
function Result = BigDbTestRaDec1()
    % You need to have configuration file with database user and password:
    % config/local/Database.DbConnections.UnitTest.yml
    % 
    DatabaseName = 'unittest';
    TableName = 'table_f_radec1';   %'big_table2';
    Cols = 52;  %4;
    BatchSize = 200000;
    PidPk = 0;
    UseIntPk = true;
    Pid = feature('getpid');
    
    MsgLogger.getSingleton().setLogLevel(LogLevel.Debug);
    
    Q = db.DbQuery(DatabaseName);
    pgver = Q.getDbVersion();
    io.msgLog(LogLevel.Test, 'Postgres version: %s', pgver);
    Q.TableName = TableName;
          
    BaseDate = 738580;  % 27/02/2022
    Pk1 = int32((now()-BaseDate)*1000000);
    StartIntPk = int32((now()-BaseDate)*10000) * 100000000;
    StartStringPK = datestr(now, 'yyyy_mm_dd_HH_MM_SS');
       
    UnitTestDb = strcmp(DatabaseName, 'unittest');
    
    %
    FileName = sprintf('BigDbTest_%d.csv', Pid);
    CsvFileName = fullfile(tools.os.getTempDir(), FileName);    
    
    [ServerFileName, ClientFileName] = Q.getSharedFileName(CsvFileName);
    CsvFileName = ClientFileName;

    
    % Prepare data
    Data = struct;
    ColNames = {};
    ColNames{end+1} = 'f_ra';
    ColNames{end+1} = 'f_dec';
    for Col=1:50
        ColNames{end+1} = sprintf('fdouble%03d', Col);
    end    
    
    %ColNames = { 'f_ra', 'f_dec', 'fdouble001', 'fdouble001' };
    
    
    x = 0.001;
    for i=1:BatchSize
        %Data(i).recid = '_';
        %Data(i).pk1 = ''; %Pk1;
        %Data(i).pk2 = 0;
        Data(i).f_ra = 180 * (i / BatchSize);
        Data(i).f_dec = 180 * (i / BatchSize);
        for Col=1:50
            ColName = sprintf('fdouble%03d', Col);
            Data(i).(ColName) = x; %Col;
            x = x + 1;
        end
    end
        
   
    Table = struct2table(Data);
    writetable(Table, CsvFileName);                     
            
    BatchCounter = 1;
    RowIndex = 1;
    RowCount = 0;
    while true
        
        % Generate CSV       
        % io.msgLog(LogLevel.Test, '[%05d] preparing data: %d x %d...', BatchCounter, BatchSize, Cols);
        
        % Update data with keys
        %for i=1:BatchSize
            %Data(i).pk2 = int64(RowIndex);
            %Data(i).pk1 = StartIntPk + RowIndex;
            RowIndex = RowIndex+1;
        %end
        
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
        %Table = struct2table(Data);
        %writetable(Table, CsvFileName);           
        
        if mod(BatchCounter, 10) == 1
            RowCount = Q.selectTableRowCount('Fast', true);
        else
            RowCount = RowCount + BatchSize;
        end
        
        t1 = tic;
        Q.insert([], 'CsvFileName', CsvFileName);        
        io.msgLog(LogLevel.Test, '[%05d] RowCount=%d, insert %d x %d: %0.5f sec', BatchCounter, RowCount, BatchSize, Cols, toc(t1));
        %delete(CsvFileName);
        
        BatchCounter = BatchCounter + 1;
    end
       
end

