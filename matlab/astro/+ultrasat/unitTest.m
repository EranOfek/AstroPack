function Result = unitTest()
    % Package Unit-Test   
    
    % testing usim simulation utility:
    
    SimA = ultrasat.usim('Cat',1000, 'Tile', 'A');
    SimB = ultrasat.usim('Cat',1000, 'Tile', 'B');
    SimC = ultrasat.usim('Cat',1000, 'Tile', 'C');
    SimD = ultrasat.usim('Cat',1000, 'Tile', 'D');

    MergedImage = ultrasat.umergeTileImages ();

    % testing multiple extended-object simulation in a single call:

    SimExt = ultrasat.usim('ExtProfileType','sersic', ...
        'ExtProfilePar',[40 4 1; 12 2 1; 60 4 1], ...
        'ExtSizeRA',[200 60 400],'ExtSizeDec',[200 60 250],'ExtOversampling',1, ...
        'ExtRA0',[221.60 221.75 221.99],'ExtDec0',[56.30 56.40 56.45], ...
        'ExtMag',[12 15 13],'ExtSpecType','BB','ExtSpec',[8000;6000;12000], ...
        'Tile','B','Exposure',[1 300],'OutName','SimImageExt');
    
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
        
	Result = true;
    
    !rm SimImage* 
end
