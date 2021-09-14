function Result = unitTest()
    % HandleBase.unitTest
    
    io.msgLog(LogLevel.Test, 'HandleBase test started');

    % Test copyObject()
    a = HandleBase();
    a.UserData = 123;            
    b = a.copyObject();
    assert(a.UserData == b.UserData);
    b.UserData = 0;
    assert(a.UserData ~= b.UserData);

    % Test copyProp()
    c = HandleBase();
    a.copyProp(c, {'UserData'});
    assert(a.UserData == c.UserData);

    % Test setProps
    a = HandleBase();
    s = struct;
    args.UserData = 7;
    a.setProps(args);
    assert(a.UserData == args.UserData);

    io.msgLog(LogLevel.Test, 'HandleBase test passed');
    Result = true;
end
