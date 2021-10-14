function Result = unitTest()
    % ComponentMap.unitTest

    io.msgLog(LogLevel.Test, 'ComponentMap test started');

    Map = ComponentMap;
    assert(Map.getCount() == 0);

    Comp1 = Component;
    Map.add(Comp1);
    assert(Map.getCount() == 1);
    assert(~isempty(Map.find(Comp1.MapKey)));

    Map.remove(Comp1);
    assert(isempty(Map.find(Comp1.MapKey)));

    Comp1 = Component;
    Map.add(Comp1);    
    clear Map;
    
    io.msgLog(LogLevel.Test, 'ComponentMap test passed');
    Result = true;
end
