function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'Installer test started');
	
	io.msgStyle(LogLevel.Test, '@passed', 'Installer test passed');
	Result = true;
end
