
function Result = unitTest()
    % DbDriver.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'DbDriver test started\n');

    % Test: Open (load), close (unload) driver
    Driver = db.DbDriver;
    Driver.open();
    assert(Driver.IsOpen); 
    Driver.close();
    assert(~Driver.IsOpen);

    % Get/register driver, open, close
    Drv = db.DbDriver.getDbDriver('postgres');
    assert(~isempty(Drv));
    Drv.open();
    assert(Drv.IsOpen);
    Drv.close();
    Drv2 = db.DbDriver.getDbDriver('postgres');
    assert(~isempty(Drv2));
    assert(~Drv2.IsOpen);

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbDriver test passed')
    Result = true;
end
