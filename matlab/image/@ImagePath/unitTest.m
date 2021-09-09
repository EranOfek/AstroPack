function Result = unitTest()
    % ImagePath.unitTest        
    io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');

    % constructFile
    ip = ImagePath();
    ExpectedResult = ip.setTestData();
    FileName = ip.constructFile();
    assert(strcmp(FileName, ExpectedResult));
    disp(FileName);
    
    % constructPath
    ip = ImagePath();
    Path = ip.constructPath('ProjName', 'ULTRASAT');
    disp(Path);

            
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


