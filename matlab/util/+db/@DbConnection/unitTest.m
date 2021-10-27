function Result = unitTest()
    % dbConnection.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbConnection test started');

    % Open/close connection
    % When not specifying database 
    Conn = db.DbConnection(); %'Database', 'unittest');
    Conn.DatabaseName = 'unittest';
    Conn.open();
    assert(Conn.IsOpen);        
    Conn.close();
    assert(~Conn.IsOpen);                        

    % Get/register connection, open, close
    Con = db.DbConnection.getDbConnection('test');
    Con.DatabaseName = 'unittest';
    assert(~isempty(Con));
    Con.open();
    assert(Con.IsOpen);
    Con.close();
    Con2 = db.DbConnection.getDbConnection('test');
    assert(~isempty(Con2));
    assert(~Con2.IsOpen);

    % Done            
    io.msgStyle(LogLevel.Test, '@passed', 'DbConnection test passed');
    Result = true;
end
