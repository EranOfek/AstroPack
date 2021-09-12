function Result = unitTest()
    % ImagePath.unitTest        
    io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');

    % genFile
    ip = ImagePath();
    ExpectedPath, ExpectedFileName = ip.setTestData();
    FileName = ip.genFile('FullPath', false);
    assert(strcmp(FileName, ExpectedResult));
    disp(FileName);
    
    % genPath
    ip = ImagePath();
    ExpectedResult = ip.setTestData();
    ip.Level = 'proc';
    ip.SubDir = 'subdir';
    Path = ip.getPath();
    disp(Path);
    assert(Path == ExpectedResult

            
%     % Default
%     IP = ImagePath();
%     FileName = IP.makeFileName();
%     assert(~isempty(FileName));
% 
%     IP.setTestData();
% 
%     s = IP.writeToStruct();
%     disp(s);


    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
    Result = true;
end


