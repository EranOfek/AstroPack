function Result = unitTest(Obj)
	% Dictionary unit test
	
	io.msgStyle(LogLevel.Test, '@start', 'Dictionary test passed')
	
	% Create test dictionary
	St.EXPTIME = {'AEXPTIME','EXPTIME','EXPOSURE'};
	St.IMTYPE  = {'IMTYPE','TYPE','IMGTYPE','IMAGETYP'};
	Conv.EXPTIME = {@(x) x, @(x) x, @(x) x};
	D = Dictionary;
	D.Dict = St;
	
	% Search
	[Alt, AltConv] = D.searchKey('EXPTIME')
	[Key, AltConv, AllAlt, FlagKey] = D.searchAlt('AEXPTIME')
	
	
	io.msgStyle(LogLevel.Test, '@passed', 'Dictionary test passed')
	Result = true;
end
