% Package Unit-Test
%
% ### Requirements:
%
%
%


function Result = unitTest()
    % Package Unit-Test   
	io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    % testing usim simulation utility:
    
    SimA = ultrasat.usim('Cat',1000, 'Tile', 'A');
    SimB = ultrasat.usim('Cat',1000, 'Tile', 'B');
    SimC = ultrasat.usim('Cat',1000, 'Tile', 'C');
    SimD = ultrasat.usim('Cat',1000, 'Tile', 'D');
    
    MergedImage = ultrasat.umergeTileImages ();
    
    % testing ULTRASAT PSF image content:

    I = Installer;
    PSF_db = sprintf('%s%s',I.getDataDir('ULTRASAT_PSF'),'/ULTRASATlabPSF5.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2};
    
    ContRad = zeros(91,25);
    Lam = 200:10:1100;
    Rad = linspace(0,10,25);
    for iR = 1:25
        for iL = 1:91
            ContRad(iL,iR) = imUtil.psf.quantileRadius(PSFdata(:,:,iL,iR),'Level',0.9)./5;
        end
    end
    imagesc('XData',Rad,'YData',Lam,'CData',ContRad)
    
%     func_unitTest();
    
	io.msgStyle(LogLevel.Test, '@passed', 'test passed');
	Result = true;
    
    !rm SimImage* 
end

%--------------------------------------------------------------------------


function Result = func_unitTest()
	% Function Unit-Test
	io.msgStyle(LogLevel.Test, '@start', 'test started');
   
	io.msgStyle(LogLevel.Test, '@passed', 'passed');
	Result = true;
end


%--------------------------------------------------------------------------

