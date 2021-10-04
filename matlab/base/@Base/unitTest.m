function Result = unitTest()
    % Base.unitTest
    
    io.msgLog(LogLevel.Test, 'Base test started');

    % Test copyObject()
    % O.S. : consider scraping some methods, there is no need for copyObj, just
    % work with copyElement, see details of Copyable
    a = Base();
    assert(a ~= a.copy())

    userdata_hendle = Base(); % test deep copy functionality
    a.UserData = userdata_hendle;            
    b = a.copyObject();
    assert(~any(getByteStreamFromArray(a) ~= getByteStreamFromArray(b)));
    assert(a.UserData ~= b.UserData);

    % Test copyProp()
    % o.s. should it be deep copy? if so, modify the implementation
    c = Base();
    
    % If Base is non-handle class, 'a.copyProp(c, {'UserData'})' does not
    % work, and we need 'c = a.copyProp(c, {'UserData'})'
    % will create data integrity issues.
    c = a.copyProp(c, {'UserData'});
    assert(a.UserData == c.UserData);
    c.UserData.UserData = 1;
    assert(a.UserData ~= c.UserData);

    % Test setProps
    % should it be deep copy?
    a = Base();
    args.UserData = Base();
    
    % Does not work if Base is not handle
    a.setProps(args);
    assert(a.UserData == args.UserData);

    io.msgLog(LogLevel.Test, 'Base test passed');
    Result = true;
end
