% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Unit-Test for telescope.Optics
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    func_unitTest();
    
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

