function Result = unitTest()
    % ImagePath.unitTest        
    io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');

    % genFile
    ip = ImagePath();
    fprintf('%s\n', ip.needUuid());
    [ExpectedPath, ExpectedFileName] = ip.setTestData();
    FileName = ip.genFile('Time', ip.Time, 'FullPath', false);
    assert(strcmp(FileName, ExpectedFileName));
    disp(FileName);
    
    % Copy
    %fprintf('ip.Uuid = %s\n', ip.needUuid());        
    ip1 = ip;
    ip2 = ip.copy();
    %fprintf('ip1.Uuid = %s\n', ip1.needUuid());    
    %fprintf('ip2.Uuid = %s\n', ip2.needUuid());    
    
    % genPath
    ip = ImagePath();
    [ExpectedPath, ExpectedFileName] = ip.setTestData();
    ip.Level = 'proc';
    ip.SubDir = 'subdir';
    Path = ip.genPath('Time', ip.Time);
    disp(Path);
    assert(strcmp(Path, ExpectedPath));

    Full = ip.genFile('Time', ip.Time, 'FullPath', true);
    ExpectedFull = [ExpectedPath, ExpectedFileName];
    assert(strcmp(Full, ExpectedFull));
    
            
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


