% @QA - Write

% Package Unit-Test
%
% ### Requirements:
%
%
%


function unitTest
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    func_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

