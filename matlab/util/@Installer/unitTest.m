function Result = unitTest(Obj)
	%
	io.msgStyle(LogLevel.Test, '@start', 'Installer test started');
	
    I = Installer;
    
    I.seeAvailableData()
    I.getDataDir('cats')
    I.getDataDir(I.Items.cats)
    I.getFilesInDataDir('cats')
    
    I.install('Atmosphere');
    %I.install(2);
    %I.install({'cats', 'EarthGravity'});
    
	io.msgStyle(LogLevel.Test, '@passed', 'Installer test passed');
	Result = true;
end
