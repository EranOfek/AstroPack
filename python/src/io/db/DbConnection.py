import psycopg2
import Component

class DbConnection(Component):
    
    def __init__(self):
        
        # Connection details
        self.DatabaseName = 'pipeline'   # Database name
        self.UserName = 'postgres'       # Login user
        self.Password = 'pass'           # Login password
        self.Host = 'localhost'          # Host name or IP address
        self.Port = 5432                 # Post number, 5432 is Postgres default
        self.DriverName = 'postgres'     #
        self.Conn = None
        self.IsOpen = False

        self.needUuid();
        self.msgLog(LogLevel.Debug, 'DbConnection created: %s', self.Uuid);


    # Destructor
    def __del__(self):
        self.msgLog(LogLevel.Debug, 'DbConnection deleted: %s', self.Uuid);



    def open(self):
        if self.DriverName == 'postgres':
        self.Conn = psycopg2.connect(
            host=self.Host,
            database=self.DatabaseName,
            user=self.UserName,
            password=self.Password);



% Connect to database specified by Host:Port:Database as UserName/Password
        f_ = io.FuncLog('DbConnection.open');
        Obj.msgLog(LogLevel.Info, 'DbConnection.open');

        % Already open
        if Obj.IsOpen
            Obj.msgLog(LogLevel.Info, 'DbConnection.open: already open');
            return
        end

        % Setup driver
        if isempty(Obj.Driver)
            Obj.Driver = io.db.DbDriver.getDbDriver(Obj.DriverName);
        end

        % Open driver
        if ~Obj.Driver.IsOpen
            Obj.Driver.open();
        end

        % Prepare username and password
        try
            Obj.msgLog(LogLevel.Debug, 'DbConnection.open: setProperty: %s/%s', Obj.UserName, Obj.Password);
            props = java.util.Properties;
            props.setProperty('user', Obj.UserName);
            props.setProperty('password', Obj.Password);
        catch
            Obj.msgLog(LogLevel.Error, 'DbConnection.open: setProperty failed');
        end

        % Connect
        try
            Obj.Url = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
            Obj.msgLog(LogLevel.Info, 'DbConnection.open: Url: %s', Obj.Url);
            Obj.Conn = Obj.Driver.Driver.connect(Obj.Url, props);
            Obj.IsOpen = true;
            Obj.msgLog(LogLevel.Info, 'DbConnection.open: connect OK: %s', Obj.Url);
        catch
        end

        % Get metadata
        try
            Obj.msgLog(LogLevel.Debug, 'DbConnection.open: calling getMetaData');
            Obj.Metadata = Obj.Conn.getMetaData();
        catch
            Obj.msgLog(LogLevel.Error, 'DbConnection.open: getMetaData failed');
        end

        Result = Obj.IsOpen;
        Obj.msgLog(LogLevel.Info, 'DbConnection.open finished');




    function Result = close(Obj)
        % Disconnect from database

        Obj.msgLog(LogLevel.Info, 'DbConnection.close');
        try
            Obj.IsOpen = false;
        catch
        end

        Result = ~Obj.IsOpen;
    end



    # Create new DbQuery instance
    function Result = newQuery(Obj)


        Result = io.db.DbQuery(Obj)
    end



methods(Static) % getDbConnection

    function Result = getConnectionKey()
        %Key = ['jdbc:postgresql://', Obj.Host, ':', string(Obj.Port).char, '/', Obj.DatabaseName];
        Result = '';
    end


    function Result = getDbConnection(ConnKey) %, Args)
        arguments
            ConnKey
        end

        persistent Map
        if isempty(Map)
            Map = ComponentMap('DbConnection');
        end

        if isempty(ConnKey)
            ConnKey = 'Default';
        end

        Key = ConnKey;
        Comp = Map.find(Key);
        if isempty(Comp)
            Comp = io.db.DbConnection();
            Comp.MapKey = ConnKey;
            Map.add(Comp);
        else
        end
        Result = Comp;
    end
end



    % Unit test
    methods(Static)
        function Result = setupDefault()
            
            Con = io.db.DbConnection.getDbConnection('default');
            assert(~isempty(Con));
            Result = true;


    # Unit test

    def unitTest()
        io.msgStyle(LogLevel.Test, '@start', 'DbConnection test started');

        # Open/close connection
        Conn = io.db.DbConnection(); %'Database', 'unittest');
        Conn.DatabaseName = 'unittest';
        Conn.open();
        assert(Conn.IsOpen);
        Conn.close();
        assert(~Conn.IsOpen);

        # Get/register connection, open, close
        Con = io.db.DbConnection.getDbConnection('test');
        Con.DatabaseName = 'unittest';
        assert(~isempty(Con));
        Con.open();
        assert(Con.IsOpen);
        Con.close();
        Con2 = io.db.DbConnection.getDbConnection('test');
        assert(~isempty(Con2));
        assert(~Con2.IsOpen);

        # Done
        io.msgStyle(LogLevel.Test, '@passed', 'DbConnection test passed');
        Result = true;
