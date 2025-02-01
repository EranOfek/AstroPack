function Result = unitTest()
    % DbRecord.unitTest
    
    io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');

    % Load from struct
    S.MyX = 1;
    S.MyY = 2;
    S.MyZ = 3;
    R = db.DbRecord;
    R.loadStruct(S);
    assert(R.MyX == S.MyX);
    assert(R.MyY == S.MyY);
    assert(R.MyX ~= S.MyY);

    % Convert to struct
    Q = db.DbRecord;
    Q.loadStruct(S);
    assert(R.Equal(Q));

    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
    Result = true;
end
