function Result = unitTest()
	% unitTest for ImageIO class
	io.msgStyle(LogLevel.Test, '@start', 'ImageIO test started');                          
	
    
    DataSampleDir = tools.os.getTestDataDir;
    PWD = pwd;
    cd(DataSampleDir);
	
	% static class to read a single image/table/header
	
	% @FIX - @Eran
	[D,H]=ImageIO.read1('asu.fit','IsTable',1);
	[D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
	[D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10]);
	[D]=ImageIO.read1('WFPC2ASSNu5780205bx.fits');
	[D,H]=ImageIO.read1('WFPC2ASSNu5780205bx.fits','ReadData',false);

	% write1
	FileName = ImageIO.write1(rand(10,10),'tmp.fits');
	delete(FileName);
    
    % writeHDF5
    ImageIO.writeHDF5(rand(100,3),'temp.h5');
    delete('temp.h5');
    
	% constroctor
	I = ImageIO;
	I = ImageIO([2, 2]);
	I = ImageIO('asu.fit','IsTable',true);
	I = ImageIO('WFPC2ASSNu5780205bx.fits','ReadHeader',0);
	I = ImageIO('WFPC2ASSNu5780205bx.fits','CCDSEC',[1 10 1 10]);
	I = ImageIO({rand(2,2),rand(3,3)});
	I = ImageIO({'WFPC2ASSNu5780205bx.fits','WFPC2u5780205r_c0fx.fits'});
	I = ImageIO('*.fits');
	
    
    cd(PWD);
    io.msgStyle(LogLevel.Test, '@passed', 'ImageIO test passed');
    Result = true;
    
	
end
