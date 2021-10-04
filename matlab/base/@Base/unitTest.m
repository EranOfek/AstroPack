function Result = unitTest()
    % Base.unitTest
    
    io.msgLog(LogLevel.Test, 'Base test started');

    % Test copyObject()
    % this functionality should be tested for every subsequent class as user data might change (O.S.)
    % the function state it should do deep copy, need to modify that if the
    % function is a subclass of Base, It should run copy on it.
    a = Base();
    userdata_hendle = Base();
    userdata_hendle.UserData = 123;
    a.UserData = userdata_hendle;            
    b = a.copyObject();
    assert(a.UserData == b.UserData);
    b.UserData.UserData = 0;
    assert(a.UserData ~= b.UserData);

    % Test copyProp()
    c = Base();
    
    % If Base is non-handle class, 'a.copyProp(c, {'UserData'})' does not
    % work, and we need 'c = a.copyProp(c, {'UserData'})'
    % will create data integrity issues. copy is only shallow if userdata
    % is a handle obj (O.S.)
    c = a.copyProp(c, {'UserData'});
    assert(a.UserData == c.UserData);

    % Test setProps
    % not related to setProps (O.S.)
    a = Base();
    s = struct;
    args.UserData = 7;
    
    % Does not work if Base is not handle
    a.setProps(args);
    assert(a.UserData == args.UserData);

    io.msgLog(LogLevel.Test, 'Base test passed');
    Result = true;
end
