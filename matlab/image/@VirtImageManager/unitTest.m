function Result = unitTest()
    % VirtImageManager.unitTest
    
    io.msgLog(LogLevel.Test, 'VirtImageManager test started');

    Manager = VirtImageManager.getSingleton();

    % See tests in VirtImage.m

    io.msgLog(LogLevel.Test, 'VirtImageManager test passed');
    Result = true;
end
