function Result = unitTest()
	% unitTest for ds9
	io.msgStyle(LogLevel.Test, '@start', 'ds9 test started');
     
	if ~isunix && ~ismac
		io.msgStyle(LogLevel.Test, 'red', 'ds9 - Windows is not supported yet !!!');
        Result = false;
        return;
    end

	io.msgStyle(LogLevel.Warning, 'red', 'You may need to run ds9 manually before running unitTest');
        
	io.msgStyle(LogLevel.Test, '@passed', 'ds9 test passed');
	Result = true;
end
