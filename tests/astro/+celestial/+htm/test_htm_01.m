function Result = unitTest()
    % unitTest for celestial.htm
    % Example: celestial.htm.unitTest
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    
    [H,L]=celestial.htm.htm_build(5);
    if size(L(5).ptr)~=2048
        error('Error in celestial.htm.htm_build');
    end
    
    %io.msgStyle(LogLevel.Test, '@passed', 'test passed');
    
	Result = true;
end
