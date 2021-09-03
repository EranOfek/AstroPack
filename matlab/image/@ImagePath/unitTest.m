function Result = unitTest()
    % ImagePath.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'ImagePath test started\n');

    % Default
    IP = ImagePath();
    FileName = IP.makeFileName();
    assert(~isempty(FileName));

    IP.setTestData();

    s = IP.writeToStruct();
    disp(s);


    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'ImagePath test passed')
    Result = true;
end


