function Result = unitTest()
    % VirtImage.unitTest
    
    io.msgLog(LogLevel.Test, 'VirtImage test started');

    Manager = VirtImageManager.getSingleton();

    % Add images
    Count = Manager.getCount();
    Image1 = VirtImage;
    Image2 = VirtImage;                     
    assert(Manager.getCount() == Count+2);

    % Delete images
    delete(Image1);
    delete(Image2);
    assert(Manager.getCount() == Count);

    io.msgLog(LogLevel.Test, 'VirtImage test passed');
    Result = true;
end
