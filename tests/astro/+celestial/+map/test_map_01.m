function Result = unitTest()
    % unitTest for celestial.map
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    %func_unitTest();
    
    % plot_monthly_smap
    figure;
    celestial.map.plot_monthly_smap([1 1 2010 18./24]);
    close;
    
	%io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
   
    
    
	%io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

