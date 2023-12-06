function Result = unitTest()
    % AstroStore.unitTest
    Result = true;
    return;
    
    io.msgStyle(LogLevel.Test, '@start', 'AstroStorge test started')

    Store = db.AstroStore.get();
    assert(~isempty(Store.DataPath));

    % Get image file name for testing
    % @Todo - Move to function
    SrcFileName = UnitTester.getTestFits(1);
    assert(isfile(SrcFileName));

    % Copy to Store's root
    [Result, Dst] = Store.copyFileToStore(SrcFileName, '');  %, Args)
    assert(Result && isfile(Dst));

    % Copy to Store's subfolder
    DstSub = ['TestFolder', filesep];
    [Result, Dst] = Store.copyFileToStore(SrcFileName, DstSub);  %, Args)            


    % function Result = insertFile(Obj, SrcFileName, DstFileName, Args)

    % function Result = getDataPath(Obj, ImPath)

    % function Result = getImagePath(Obj, ImPath)


    % function Result = getImageFileName(Obj, ImPath)

    io.msgStyle(LogLevel.Test, '@passed', 'AstroDbStore perfTest passed')
    Result = true;
end
