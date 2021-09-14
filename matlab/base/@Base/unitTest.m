function Result = unitTest()
    % Base.unitTest
    
    io.msgLog(LogLevel.Test, 'Base test started');

    % Test copyObject()
    a = Base();
    a.UserData = 123;            
    b = a.copyObject();
    assert(a.UserData == b.UserData);
    b.UserData = 0;
    assert(a.UserData ~= b.UserData);

    % Test copyProp()
    c = Base();
    
    % If Base is non-handle class, 'a.copyProp(c, {'UserData'})' does not
    % work, and we need 'c = a.copyProp(c, {'UserData'})'
    c = a.copyProp(c, {'UserData'});
    assert(a.UserData == c.UserData);

    % Test setProps
    a = Base();
    s = struct;
    args.UserData = 7;
    
    % Does not work if Base is not handle
    a.setProps(args);
    assert(a.UserData == args.UserData);

    io.msgLog(LogLevel.Test, 'Base test passed');
    Result = true;
end
