function Result = unitTest()
    % unitTest for celestial.earth
    % Example: celestial.earth.unitTest
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    [Result]=celestial.earth.observatoryCoo('Name','Wise')
    
    %func_unitTest();
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end
