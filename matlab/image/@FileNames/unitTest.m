function Result = unitTest()
    % FileNames.unitTest        
    io.msgStyle(LogLevel.Test, '@start', 'FileNames test started\n');

    
    F = FileNames;
    F.setTestData;         
    F.genFull;  

    F.ProjName = 'LAST';
    F.Time     = 2451545;
    
    Ans = F.validate;
    if ~Ans
        Result = false;
    end
    
    F.jd2str;
    
    F = FileNames;
    F.ProjName = 'LAST';
    F.Time     = 2451545+(1:1:1000)';
    
    F.getProp('CropID');
    F.getProp('Time',1);
    
    F.genFile(1,true)
    tic;F.genFile;toc
    F.genPath
    
    F.genFull
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'FileNames test passed')
    %Result = true;
end

