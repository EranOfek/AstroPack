function Result = unitTest()
    % dbConnection.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbConnection test started');

    % Open/close connection
    % When not specifying database 
    Conn = db.DbConnection.getDbConnection('unittest');
    Conn.open();
    assert(Conn.IsOpen);        
    Conn.close();
    assert(~Conn.IsOpen);                        
    Conn.open();

    % Try to create DbConnection which is already registered,
    % this SHOULD FAIL and the returned object should not be used
    % @Todo: Replace with exception
    Conn2 = db.DbConnection('Db', 'unitTeST');
    assert(Conn2 ~= Conn);
    assert(~Conn2.IsOpen);        
            
    % Get/register connection, open, close
    Conn3 = db.DbConnection.getDbConnection('unittest');
    assert(Conn3 == Conn);
    assert(Conn3.IsOpen);

    % Done            
    io.msgStyle(LogLevel.Test, '@passed', 'DbConnection test passed');
    Result = true;
end
