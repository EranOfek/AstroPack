function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'convert test started');
	
	io.msgStyle(LogLevel.Test, '@passed', 'convert test passed');
	Result = true;
end
