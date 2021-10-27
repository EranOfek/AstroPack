function Result = unitTest()
    % DbDriver.unitTest    
    io.msgStyle(LogLevel.Test, '@start', 'DbDriver test started\n');

    % Test: Open (load), close (unload) driver
    % Note that this can result with the following warning message which is
    % fine: [WRN] DbDriver: (already running?) cannot copy file...
    % When not specifying database type, the default Postgres is used
    Driver = db.DbDriver();
    Driver.loadDriver();
    assert(Driver.IsLoaded); 
    Driver.unloadDriver();
    assert(~Driver.IsLoaded);

    % Get/register the singleton driver, open, close
    Drv = db.DbDriver.getDbDriver('postgres');
    assert(~isempty(Drv));
    Drv.loadDriver();
    assert(Drv.IsLoaded);
    Drv.unloadDriver();
    Drv2 = db.DbDriver.getDbDriver('postgres');
    assert(~isempty(Drv2));
    assert(~Drv2.IsLoaded);

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbDriver test passed')
    Result = true;
end
