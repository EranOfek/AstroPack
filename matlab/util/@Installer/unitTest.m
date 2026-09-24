function Result = unitTest(Obj)
	% unitTest for Installer
	
    In = Installer;
    
    In.seeAvailableData()
    In.getDataDir('cats')
    In.getDataDir(I.Items.cats)
    In.getFilesInDataDir('cats')
    
    In.install('Atmosphere');
    %I.install(2);
    %I.install({'cats', 'EarthGravity'});
    
	Result = true;
end
