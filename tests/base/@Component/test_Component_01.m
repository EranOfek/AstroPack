function Result = unitTest()
    % Component.unitTest
    
    %io.msgLog(LogLevel.Test, 'Component test started');

    % Create instances
    a = Component;
    %a.msgLog(LogLevel.Test, 'a created');            
    b = Component;
    %b.msgLog(LogLevel.Test, 'b created');            
    c = Component;
    %c.msgLog(LogLevel.Test, 'c created');

    % Make sure that we get different Uuids
    %io.msgLog(LogLevel.Test, 'Testing Uuid');
    a.needUuid();            
    b.needUuid();
    assert(~all(a.Uuid ~= b.Uuid));

    % Make sure that we get different MapKeys
    %io.msgLog(LogLevel.Test, 'Testing MapKey');
    a.needMapKey();            
    b.needMapKey();
    assert(~all(a.MapKey ~= b.MapKey));            

    % Generate Uuid and MapKey for arrays
    c(1) = Component;
    c(2) = Component;
    c.msgLog(LogLevel.Test, 'Msg');
    u = c.needUuid();
    disp(u);
    k = c.needMapKey();
    disp(k);

    % Copy
    d = Component;
    d.needUuid();
    e = d;
    assert(strcmp(d.Uuid, e.Uuid));
    
    % When using copy(), Component.copyElement() is invoked and generates a
    % new Uuid for the new instance of the object
    f = d.copy();
    assert(~strcmp(d.Uuid, f.Uuid));
    
    %io.msgStyle(LogLevel.Test, '@passed', 'Component test passed');                          
    Result = true;
end
