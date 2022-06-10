function Result = unitTest()
	% unitTest for ds9
	io.msgStyle(LogLevel.Test, '@start', 'ds9 test started');
     
	if ~isunix && ~ismac
		io.msgStyle(LogLevel.Test, 'red', 'ds9 - Windows is not supported yet !!!');
        Result = false;
        return;
    end
    
    % Check that we have connection to DS9 service
    if system('xpaget ds9')
    	io.msgStyle(LogLevel.Warning, 'red', 'You may need to run ds9 manually at terminal before running unitTest');
    else
        %f = fullfile(Configuration.getSingleton.Data.System.Folders.TestImages, 'asu.fit');
        % o.s. this already has a bug, how do I get a reference to the data
        % folder path.
        %f = '/home/omrisee/matlab/data/LAST/TestImages/LAST.2.1.2_20200820.163931.354_clear_0_twflat.fits' ;
        f = fullfile(tools.os.getTestDataDir, 'asu.fit');
        ds9o(f)
        a = ds9o.read2AstroImage();
        assert(isa(a, 'AstroImage'))
    end
    
    % Test plot regions
    %function plot(varargin)    
    
	io.msgStyle(LogLevel.Test, '@passed', 'ds9 test passed');
	Result = true;
end
