function Result = unitTest()
	% unitTest for DS9
    
	io.msgStyle(LogLevel.Test, '@start', 'ds9 test started');
     
	if ~isunix && ~ismac
		io.msgStyle(LogLevel.Test, 'red', 'ds9 - Windows is not supported yet !!!');
        Result = false;
        return;
    end
    
    % create a DS9 object
    D = DS9;
    % open a window
    D.open
    % open another window
    D.open(true);
    D.exit
    % open again
    D.open(true);
    D.isOpen
    D.isWindowExist
    D.isOpen
    
    % switch ds9 window
    
    
	io.msgStyle(LogLevel.Test, '@passed', 'ds9 test passed');
	Result = true;
end
