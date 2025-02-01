function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'constant test started');
	
	io.msgStyle(LogLevel.Test, '@passed', 'constant test passed');
	Result = true;
end
