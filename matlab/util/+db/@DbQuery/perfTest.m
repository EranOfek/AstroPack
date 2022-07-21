% NOTE: Needs rework due to modifications in DbQuery, see unitTest.m as
% reference

function Result = perfTest()
    % DbQuery.perfTest
    
    % On Windows, use SQL Manager Lite for PostgreSQL by EMS Software
    % On Linux, use DataGrip by JetBrains 
    io.msgStyle(LogLevel.Test, '@start', 'DbQuery perfTest started')
    io.msgLog(LogLevel.Test, 'Postgres database "unittest" should exist');

    % ---------------------------------------------- Connect
    % NOTE: Database 'unittest' should exist

    Conn = db.Db.getUnitTest();
    Q = db.DbQuery(Conn);

    % NOTE: At this point, we assume that tables master_table and
    % details_table exist and are not empty

    % ---------------------------------------------- Select            
    % Select two fields from table, using LIMIT            
    ItersCount = 1; %000;
    io.msgLog(LogLevel.Info, 'Perf: select, Iters: %d ...', ItersCount);            
    T = tic();
    for i = 1:ItersCount                      
        Q.query(['SELECT * FROM master_table LIMIT 1']);
    end
    Time = toc(T) / ItersCount;
    io.msgLog(LogLevel.Info, 'Perf: select: %f', Time);            

    % ---------------------------------------------- insertRecord: struct

    ItersCount = 10000;
    io.msgLog(LogLevel.Info, 'Perf: insert, Iters: %d ...', ItersCount);
    Count1 = Q.selectTableRowCount('master_table');            
    T = tic();
    for i = 1:ItersCount                      
        s = struct;            
        s.recid = Component.newUuid();
        Q.insertRecord('master_table', s);            
    end
    Time = toc(T) / ItersCount;
    io.msgLog(LogLevel.Info, 'Perf: insert: %f', Time);
    Count2 = Q.selectTableRowCount('master_table');
    if Count2 ~= Count1 + ItersCount
        io.msgLog(LogLevel.Info, 'Wrong number of records: Count1: %d, Count2: %d', Count1, Count2);
    end

    % ---------------------------------------------- insertRecord: Batch

    BatchSize = 1000;
    io.msgLog(LogLevel.Info, 'Perf: insert Batch: %d, Iters: %d ...', BatchSize, ItersCount);
    Count1 = Q.selectTableRowCount('master_table');
    T = tic();
    s = [];
    for i = 1:ItersCount                      
        s(i).recid = Component.newUuid();
    end
    Q.insertRecord('master_table', s, 'BatchSize', BatchSize);
    Time = toc(T) / ItersCount;
    io.msgLog(LogLevel.Info, 'Perf: insert batch: %f', Time);

    Count2 = Q.selectTableRowCount('master_table');
    if Count2 ~= Count1 + ItersCount
        io.msgLog(LogLevel.Info, 'Wrong number of records: Count1: %d, Count2: %d', Count1, Count2);
    end 

    % ----------------------------------------------


    io.msgStyle(LogLevel.Test, '@passed', 'DbQuery perfTest passed')
    Result = true;
end

