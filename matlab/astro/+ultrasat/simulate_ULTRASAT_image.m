function simImage = simulate_ULTRASAT_image (Args)
    % Simulate with ultrasat.usim a realistic distribution of sky sources 
    % NB: the size of the actuall modelled region should not be smaller
    % than 0.5 x 0.5 deg, otherwise we will sample only the brightest part of
    % the source distribution!
    % Input: -
    %        * ...,key,val,... 
    %        'Size'     - [deg] size of the modelled FOV, 7.15 is the full FOV 
    %        'SkyCat'   - whether to use sky coordinates instead of the pixel coordinates
    %        'X0'       - [deg] the lower left corner of the modelled
    %                     square region or RA0 if SkyCat = true
    %        'Y0'       - [deg] the lower left corner of the modelled
    %                     square region or Dec0 if SkyCat = true
    %        'RA_inner' - RA of the the inner corner of the tile (needed to define Rad and RXX) 
    %        'Dec_inner'- Dec of the the inner corner of the tile (needed to define Rad and RXX) 
    %        'PlaneRotation' - [deg] rotation of the detector plane
    %        'Shift'    - catalog shift (in pix)
    %        'Rot'      - catalog rotation (in deg) 
    %        'ExpNum'   - number of the standard 300 s exposures to stack 
    %        'OutDir'   - the output diretory 
    %        'OutName'  - the output filename template
    % Output : - an AstroImage object containing the resulting source image and catalog                 
    %            
    % Author : A. Krassilchtchikov Aug 2023
    % Example: Image = simulate_ULTRASAT_image('ExpNum', 30, 'OutDir', '/home/sasha/', 'Size', 7.15)
    %          Im2 = ultrasat.simulate_ULTRASAT_image('SkyCat',1,'RA0',215,'Dec0',52,'X0',216,'Y0',53,'OutName','Pos1')
    %          Im3 = ultrasat.simulate_ULTRASAT_image('SkyCat',1,'RA0',219,'Dec0',51,'X0',216,'Y0',53,'OutName','Pos3','PlaneRotation', 45,'Shift',[0 0])
    
    arguments    
        Args.Size           = 0.5;        % [deg] size of the modelled FOV 
        Args.SkyCat logical = false;      % whether to use sky coordinates instead of the pixel coordinates
        Args.X0             = 1e-6;       % [deg] the lower left corner of the modelled square region 
                                          % if SkyCat = true this should be RA in sky degrees!
        Args.Y0             = 1e-6;       % [deg] the lower left corner of the modelled square region 
                                          % if SkyCat = true this should be Dec in sky degrees!
        Args.RA0            = 214.99;     % RA of the the inner corner of the tile 
        Args.Dec0           = 52.78;      % Dec of the the inner corner of the tile
        Args.PlaneRotation  = 0;          % [deg] rotation of the detector plane 
        Args.Shift          = [];         % catalog shift (in pix) 
        Args.Rot            = [];         % catalog rotation (in deg) 
        Args.ExpNum         = 1;          % number of the standard 300 s exposures to stack 
        Args.OutDir         =  '.';       % the output directory
        Args.OutName        = 'SimImage'; % the output filename template
    end
    
    %%%%% ULTRASAT parameters
    
    PixSize    = 5.44;         % pixel size (arcsec)
    PixSizeDeg = PixSize/3600; % pixel size (deg)
    Wave       = 2000:11000;   % the wavelength band in A
    
    if Args.SkyCat
    % make a local WCS:
        SimWCS = AstroWCS();
        SimWCS.ProjType  = 'TAN';
        SimWCS.ProjClass = 'ZENITHAL';
        SimWCS.CooName   = {'RA'  'DEC'};
        SimWCS.CTYPE     = {'RA---TAN','DEC---TAN'};
        SimWCS.CUNIT     = {'deg', 'deg'};
        SimWCS.CD(1,1)   = PixSizeDeg;
        SimWCS.CD(2,2)   = PixSizeDeg;  
        SimWCS.CRVAL(1)  = Args.RA0;
        SimWCS.CRVAL(2)  = Args.Dec0;
        SimWCS.CRPIX(1)  = 0; 
        SimWCS.CRPIX(2)  = 0; 
        SimWCS.populate_projMeta;
    end 
    
    %%%%% source distribution parameters
    
    MagL = 13; MagH = 26; Delta_m = 0.2; % the distribution grid in NUV Mag (from GALEX)
    MagBins = (MagH - MagL) / Delta_m;
    
    Mag       = zeros(MagBins,1);
    SrcDist   = zeros(MagBins,1);
    
    for iMag = 1:1:MagBins
        
        Mag(iMag)  = MagL + (iMag - 1) * Delta_m;
        
        SrcDeg      = 10.^( 0.35 * Mag(iMag) - 4.9 );  % per 1 deg^2 (fitted from the GALEX data)
        SrcDist(iMag) = ceil( SrcDeg * Args.Size^2 );  % rescaled for Args.Size^2
        
    end
    
    NumSrc = sum(SrcDist,'all');
    
    Cat    = zeros(NumSrc,2);
    MagUS  = zeros(NumSrc,1);
    
    if Args.SkyCat % make an additional array of pixel coordinates
        CatPix = zeros(NumSrc,2);
    end
    
    %         S(1,:) = AstroSpec.blackBody(Wave',3500).Flux; % appears too slow!
    %         S(2,:) = AstroSpec.blackBody(Wave',5800).Flux;
    %         S(3,:) = AstroSpec.blackBody(Wave',20000).Flux;
    
    S(1) = AstroSpec.blackBody(Wave',3500);
    S(2) = AstroSpec.blackBody(Wave',5800);
    S(3) = AstroSpec.blackBody(Wave',20000);
    
    % read in the relation of NUV magnitudes and ULTRASAT magnitudes
    % for a given set of source spectra (usually, just the 3 BB temperatures)
    % NB: if you wish to model another set of spectra, first rerun convertGALEXdistribution
    % in order to make a new GALEX_ULTRASAT_magn.mat object!
    MagDB = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/GALEX_ULTRASAT_magn.mat');
    io.files.load1(MagDB); % variables: MagU (3D), Temp, MagNUV, Rad 
    
    if isempty(Args.Shift) && isempty(Args.Rot) % shift and rotation not given, create and save a new catalog
        
        rng('shuffle');
        if Args.SkyCat % NB: this will not correspond to a Size x Size region on the sky!
            Cat(:,1) = Args.X0  + Args.Size * rand(NumSrc,1); % X0 is here RA in sky degrees
            Cat(:,2) = Args.Y0  + Args.Size * rand(NumSrc,1); % Y0 is here Dec in sky degrees
            [CatPix(:,1) CatPix(:,2)] = SimWCS.sky2xy(Cat(:,1),Cat(:,2)); % calculate pixel coordinates
        else           % use pixel coordinates
            X0 = ceil(Args.X0 * 3600 / PixSize); % left corner of the modelled square region
            Y0 = ceil(Args.Y0 * 3600 / PixSize); % left corner of the modelled square region
            Range = Args.Size * 3600 / PixSize;  % X and Y size of the modelled square region
            Cat(:,1) = X0 + Range * rand(NumSrc,1);
            Cat(:,2) = Y0 + Range * rand(NumSrc,1);
        end
        
        % temporary code %%%
        %             CatIn = readtable('SimImage_InCat.txt');
        %             Cat(:,1) = CatIn.Var1; Cat(:,2) = CatIn.Var2;
        % end temporary code %%%
        
        save('cat0.mat','Cat');
        
    else  % read, shift and/or rotate the existing catalog
        
        io.files.load1('cat0.mat');
        
        if ~isempty(Args.Shift)
            Cat(:,1) = Cat(:,1) + Args.Shift(1); % X shift
            Cat(:,2) = Cat(:,2) + Args.Shift(2); % Y shift
            fprintf('Source catalog shifted by %d x %d pixels\n',Args.Shift(1),Args.Shift(2));
        end
        
        if ~isempty(Args.Rot) 
            if Args.SkyCat
                error('Source catalog rotation in sky coordinates is not available, better rotate the telescope, exiting..');
            end
            Alpha = Args.Rot * (pi/180.);
            Cat(:,1) = Cat(:,1) * cos(Alpha) - Cat(:,2) * sin(Alpha);
            Cat(:,2) = Cat(:,1) * sin(Alpha) + Cat(:,2) * cos(Alpha);
            fprintf('Source catalog rotated by %d degrees\n',Alpha);
        end
        
    end
    
    Isrc = 0;
    for iMag = 1:1:MagBins
        
        for jSrc = 1:1:SrcDist(iMag)
            
            Isrc = Isrc + 1;
            
            if Args.SkyCat % sky coordinates in Cat, use CatPix
                RadSrc = sqrt( CatPix(Isrc,1)^2 + CatPix(Isrc,2)^2 ) * (PixSize/3600); % deg
            else           % pixel coordinates in Cat
                RadSrc = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) * (PixSize/3600); % deg
            end
            
            [~, IndR] = min( abs(RadSrc - Rad) ); % search for the nearest node
            
            % divide the population into 3 colours:
            IndT = rem(Isrc,3) + 1; Spec(Isrc,:) = S(IndT);
            
            % search for the nearest magnitude (from the GALEX_ULTRASAT_magn.mat grid)
            [~, IndM] = min( abs( Mag(iMag) - MagNUV ) ); 
            
            MagUS(Isrc) = MagU( IndT, IndM, IndR );
            
        end
        
    end
    
    %%%% run the simulation
    
    simImage = ultrasat.usim('Cat', Cat, 'Mag', MagUS, 'Spec', Spec,'Exposure',[Args.ExpNum 300],...
                    'OutDir', Args.OutDir,'SkyCat', Args.SkyCat, 'PlaneRotation', Args.PlaneRotation,...
                    'RA0',Args.RA0,'Dec0',Args.Dec0,'OutName',Args.OutName);                    
    
end
