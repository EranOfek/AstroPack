function Result = unitTest()
    % DbRecord.unitTest   
    io.msgStyle(LogLevel.Test, '@start', 'DbRecord test started');

    % Load from struct
    D = db.DbRecord;
    for i = 1:4
        D.Data(i).a = 10*i + 1;
        D.Data(i).b = 10*i + 2;
        D.Data(i).c = 10*i + 3;
    end
    
    % Done
    io.msgStyle(LogLevel.Test, '@passed', 'DbRecord test passed');
    Result = true;
end
