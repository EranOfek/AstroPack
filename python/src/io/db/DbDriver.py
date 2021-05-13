
class DbDriver(Component):

    def __init__(self):
        self.IsOpen = False
        self.needUuid();
        self.msgLog(LogLevel.Debug, 'DbDriver created: %s', self.Uuid)


    # Destructor
    function delete(Obj)
        Obj.msgLog(LogLevel.Debug, 'DbDriver deleted: %s', Obj.Uuid);


    def open(self):
        self.msgLog(LogLevel.Info, 'DbDriver: open');
        return True


    def close(self)
        return True;


    def getDbDriver(DatabaseType)
        return None
        persistent Map
        if isempty(Map)
            Map = ComponentMap('DbDriver');
        end

        % Set default database type
        if isempty(DatabaseType)
            DatabaseType = 'postgres';
        end

        % Search in map
        Comp = Map.find(DatabaseType);
        if isempty(Comp)
            % Not found, create
            Comp = io.db.DbDriver();
            Comp.DatabaseType = DatabaseType;
            Comp.MapKey = DatabaseType;
            Map.add(Comp);
        else
            % Already exist
        end
        Result = Comp;


    @staticmethod
    def unitTest():
        io.msgStyle(LogLevel.Test, '@start', 'DbDriver test started\n');

        % Test: Open/close driver
        Driver = io.db.DbDriver;
        Driver.open();
        assert(Driver.IsOpen);
        Driver.close();
        assert(~Driver.IsOpen);

        % Get/register driver, open, close
        Drv = io.db.DbDriver.getDbDriver('postgres');
        assert(~isempty(Drv));
        Drv.open();
        assert(Drv.IsOpen);
        Drv.close();
        Drv2 = io.db.DbDriver.getDbDriver('postgres');
        assert(~isempty(Drv2));
        assert(~Drv2.IsOpen);

        % Done
        io.msgStyle(LogLevel.Test, '@passed', 'DbDriver test passed')
        Result = true;
