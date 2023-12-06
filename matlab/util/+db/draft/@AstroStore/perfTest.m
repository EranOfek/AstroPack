function Result = perfTest()
    % AstroStore.perfTest
    
    io.msgStyle(LogLevel.Test, '@start', 'AstroStorge perfTest started')

    Store = db.AstroStore.get();
    assert(~isempty(Store.DataPath));

    [SrcFileName, FileSize] = UnitTester.getTestFits(1);
    assert(isfile(SrcFileName));

    FileSizeMB = FileSize / 1024 / 1024;
    FileSizeGB = FileSize / 1024 / 1024 / 1024;

    % ---------------------------------------------- Copy            
    % Select two fields from table, using LIMIT            
    ItersCount = 1000;
    io.msgLog(LogLevel.Info, 'Perf: copy, Iters: %d ...', ItersCount);            
    T = tic();
    for i = 1:ItersCount                      
        DstName = ['perfTest', filesep, 'perfTest_', char(string(i))];
        [Result, Dst] = Store.copyFileToStore(SrcFileName, DstName);  %, Args)
        assert(Result && isfile(Dst));
    end
    Time = toc(T) / ItersCount;
    io.msgLog(LogLevel.Info, 'Perf: copy: %f', Time);
    io.msgLog(LogLevel.Info, 'Perf: copy: %f per MB', Time / FileSizeMB);
    io.msgLog(LogLevel.Info, 'Perf: copy: %f per GB', Time / FileSizeGB);


    % function Result = insertFile(Obj, SrcFileName, DstFileName, Args)

    % function Result = getDataPath(Obj, ImPath)

    % function Result = getImagePath(Obj, ImPath)


    % function Result = getImageFileName(Obj, ImPath)


    io.msgStyle(LogLevel.Test, '@passed', 'AstroDbStore perfTest passed')
    Result = true;
end

