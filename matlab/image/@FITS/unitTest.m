function Result = unitTest(Obj)
	% unitTest for the FITS class
    % Use FV to view FITS files:
    % https://heasarc.gsfc.nasa.gov/docs/software/ftools/fv/


	io.msgLog(LogLevel.Test, 'FITS.unitTest sarted');
	
% 	DataSampleDir = tools.os.getTestDataDir;
    I = Installer; DataSampleDir = I.getDataDir('TestImagesAstroPack');
	PWD = pwd;
	cd(DataSampleDir);
          
    %test_writeHeader();
    
    % test constructor
	io.msgLog(LogLevel.Test, 'testing FITS constructor');
	F = FITS('*.fits');
	
	% test static functions
	io.msgLog(LogLevel.Test, 'testing FITS readHeader1');
	Nh1=FITS.numHDU1(F(1).File);
	FITS.readHeader1(F(1).File);
    
	io.msgLog(LogLevel.Test, 'testing FITS read1');
	[Im,H,Nh2]=FITS.read1(F(1).File);
	if Nh1~=Nh2
		error('unitTest for FITS class failed - number of HDus not consistent');
	end
	
	% read CCDSEC
	io.msgLog(LogLevel.Test, 'testing FITS read1 CCDSEC');
	[Im,H,Nh]=FITS.read1(F(1).File,'CCDSEC',[1 10 1 10]);
	
	% read multiple images into a cube
	io.msgLog(LogLevel.Test, 'testing FITS read2cube');
	[Cube]=FITS.read2cube({F(1:2).File},1,'CCDSEC',[1 10 1 10]);
	
	% readTable1
	io.msgLog(LogLevel.Test, 'testing FITS readTable1');
	
	%
	[Out, Head, Col] = FITS.readTable1('asu.fit');
	
	% get_keys
	io.msgLog(LogLevel.Test, 'testing FITS get_keys');
	[KeysVal,KeysComment,Struct]=FITS.get_keys('WFPC2ASSNu5780205bx.fits',{'NAXIS1','NAXIS2'});
	
	% mget_keys
	io.msgLog(LogLevel.Test, 'testing FITS mget_keys');
	[KeysVal,KeysComment,Struct,List]=FITS.mget_keys('*.fits',{'NAXIS1','NAXIS2'});
	
	% non static 
	F.numHDU;  
	F.readHeader;  % the headers are stored in F.Header
	F.read;
	F = FITS('*.fits');
	F(1:2).read([],[],'CCDSEC',[1 10 1 10]);
	
	F = FITS('asu.fit',2);
	io.msgLog(LogLevel.Test, 'testing FITS readTable');
	F.readTable;
	
	% read/write
	A=rand(10,11);
	File = 'tmp/tmpfile.fits';
	io.msgLog(LogLevel.Test, 'testing FITS write');
	FITS.write(A,File,'Header',{'a',1,'';'b',2,''});
	[B,H]=FITS.read1(File);  
	delete(File);
	if max(abs(A(:)-B(:)))>1e-7
		error('write and read files are not identical');
	end
	
	% write_keys
	io.msgLog(LogLevel.Test, 'testing FITS write_keys');
	File = 'tmp/tmpfile.fits';
	FITS.write(A,File,'Header',{'a',1,'';'b',2',''});
	FITS.write_keys(File,{'try','A','comm';'try2',6,'what'});
	delete(File);
	
    % Test writeTable1()
    test_writeTable();    
    
	cd(PWD);	
	io.msgStyle(LogLevel.Test, '@passed', 'FITS test passed')
	Result = true;
end



function Result = test_writeTable()

    % unitTest for the FITS.writeTable()
    %WorkDir = tools.os.getTestWorkDir;
   
    FileName = 'tmp/wrtable1a.fits';
    if isfile(FileName)
        delete(FileName);
    end

    AC = AstroTable({rand(10, 2)}, 'ColNames', {'RA','Dec'});    
    AC2 = AstroTable({rand(7, 3)}, 'ColNames', {'RA','Dec','Dog'});    
    AC3 = AstroTable({rand(4, 2)}, 'ColNames', {'ColA','ColB'});    
    
    FITS.writeTable1(AC, FileName, 'ExtName', 'MyExtName');
    FITS.writeTable1(AC2, FileName, 'Append', true, 'HDUnum', 2, 'ExtName', 'MyExtDog');    
    
    % Write also extra header
    H = AstroHeader();
    for i=1:20
        H.insertKey({sprintf('Key%03d', i), sprintf('Value%03d', i), sprintf('Comment%03d', i)});
    end
    
    % Write table with additional Header
    FITS.writeTable1(AC3, FileName, 'Append', true, 'HDUnum', 3, 'ExtName', ...
        'MyHeader1', 'Header', H, 'HeaderHDUnum', 4);
    
    % When 'HeaderHDUnum' is not specified, the additional Header follows
    % the table.
    FITS.writeTable1(AC3, FileName, 'Append', true, 'HDUnum', 5, 'ExtName', ...
        'MyHeader2', 'Header', H);
    
    % Read all headers
    Count = FITS.numHDU1(FileName);
    io.msgLog(LogLevel.Test, 'HDUs: %d', Count);
    for i=1:Count
        io.msgLog(LogLevel.Test, 'Header of HDU # %d', i);
        Header = FITS.readHeader1(FileName, i);
        disp(Header);
    end
        
    
    % Bug: readTable1 returns Col not the same order as AC.ColNames
    %     Out.ColNames
    % 
    % ans =
    % 
    %   2×1 cell array
    % 
    %     {'RA' }
    %     {'Dec'}
    %
    % AC.ColNames - @Todo: Fix to this form
    % 
    % ans =
    % 
    %   1×2 cell array
    % 
    %     {'RA'}    {'Dec'}
    %
    
    % Read it    
	[Out, Head, Col] = FITS.readTable1(FileName, 'OutTable', 'AstroTable');
    
    assert(isequal(Out.Catalog,AC.Catalog));
    
	%A=rand(10,11);
	%File = 'tmp/tmptable.fits';
	%io.msgLog(LogLevel.Test, 'testing FITS write');
	%FITS.writeTable(A, File, 'Header' ,{'ColA', 1, '';'ColB', 2,''});
    
    % Todo: Test ASCII table
    %
    %
    
    % @Todo: Test also with 'ascii' table

	delete(FileName);    
    Result = true;
end



function Result = test_writeHeader()
    % Test performance of writeHeader and other fits issues (under work)
    
    FileName = 'tmp/writeheader.fits';
    
    NumKeys = 10;    
    for Iter=1:5
        if isfile(FileName)
            delete(FileName);
        end

        % Write also extra header
        Header = AstroHeader();
        for i=1:NumKeys
            Header.insertKey({sprintf('Key%03d', i), sprintf('Value%03d', i), sprintf('Comment%03d', i)});
        end

        A = rand(1,1);
        FITS.write(A, FileName, 'Header', Header);

        delete(FileName);    
        NumKeys = NumKeys + 100;
    end
    
    Result = true;
end

