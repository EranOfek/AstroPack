function simImage = simulate_ULTRASAT_image (Args)
    % Simulate an ULTRASAT image of a realistic distribution of sky sources 
    % NB: the size of the actual modelled region should not be smaller
    % than 0.5 x 0.5 deg, otherwise we will sample only the brightest part of
    % the input source distribution!
    % Input: -
    %        * ...,key,val,... 
    %        'Size'     - [deg] size of the modelled FOV, 7.15 is the full FOV 
    %        'SkyCat'   - whether to use sky coordinates instead of the pixel coordinates
    %        'X0'       - [deg] the pixel coordinate or the RA (if SkyCat = y)
    %                     of the lower left corner of the modelled region 
    %        'Y0'       - [deg] the pixel coordinate or the Dec (if SkyCat = y)
    %                     of the lower left corner of the modelled region 
    %        'RA0'      - RA of the camera's aimpoint 
    %        'Dec0'     - Dec of the camera's aimpoint 
    %        'PlaneRotation' - [deg] rotation of the detector plane
    %        'Shift'    - catalog shift (in pix)
    %        'Rot'      - catalog rotation (in deg) 
    %        'ExpNum'   - number of the standard 300 s exposures to stack 
    %        'OutDir'   - the output diretory 
    %        'OutName'  - the output filename template
    % Output : - an AstroImage object containing the resulting source image and catalog                             
    % Author : A. Krassilchtchikov (Aug 2023)
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
        Args.RA0            = 214.99;     % RA of the camera's aimpoint 
        Args.Dec0           = 52.78;      % Dec of the camera's aimpoint 
        Args.PlaneRotation  = 0;          % [deg] rotation of the detector plane 
        Args.Shift          = [];         % catalog shift (in pix) 
        Args.Rot            = [];         % catalog rotation (in deg) 
        Args.ExpNum         = 1;          % number of the standard 300 s exposures to stack 
        Args.Tile           = 'B';        % the detector tile to be simulated
        Args.OutDir         =  '.';       % the output directory
        Args.OutName        = 'SimImage'; % the output filename template
        Args.MagDistr       = 'GALEX';    % magnitide distribution 'GALEX' empirical distribution or 'art' (artificial)
        Args.Temp           = [3500 5800 20000]; % the temperature grid (the source are equally distributed on it)
        Args.NumSrc         = 1000;       % number of objects per square degree (if the distribution is not 'GALEX')
                                          % 10(3)/sq.deg. roughly correspond to the GALEX distribution 
                                          % integrated upto m_NUV = 22.3  
    end
    
    %%%%% ULTRASAT parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    
    RAD = 180/pi;
    
    FocalLength = 360;    % [mm] 
    PixelSizeMm = 9.5e-3; % [mm] pixel size 
    PixSizeDeg  = ( PixelSizeMm / FocalLength ) * RAD;  % [deg] pixel size
    
    GapMm       = 2.4;    % gap width in mm
    Ngap        = ceil( GapMm / PixelSizeMm); % number of pixels in the gap

    Wave       = 2000:11000;   % the wavelength band in A

    if strcmp(Args.Tile,'A')
        CRPIX1 = ImageSizeX+Ngap/2;
        CRPIX2 = -Ngap/2;
    elseif strcmp(Args.Tile,'B')
        CRPIX1 = -Ngap/2;
        CRPIX2 = -Ngap/2;
    elseif strcmp(Args.Tile,'C')
        CRPIX1 = -Ngap/2;
        CRPIX2 = ImageSizeY+Ngap/2;
    elseif strcmp(Args.Tile,'D')
        CRPIX1 = ImageSizeX+Ngap/2;
        CRPIX2 = ImageSizeY+Ngap/2;
    else
        error('Tile name is not correct');
    end
    
    % set a WCS    
    if Args.SkyCat % make a local WCS: 
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
        SimWCS.CRPIX(1)  = CRPIX1;
        SimWCS.CRPIX(2)  = CRPIX2;
        SimWCS.populate_projMeta;
    
        Alpha_rad = Args.PlaneRotation/RAD;
        RotMatrix = [cos(Alpha_rad), -sin(Alpha_rad);
                     sin(Alpha_rad),  cos(Alpha_rad)];
        SimWCS.CD = RotMatrix * SimWCS.CD;
    end 
    
    % initialize a set of artificial BB source spectra        
    NTemp = numel(Args.Temp);
    S = AstroSpec.blackBody(Wave',Args.Temp);    
    
    %%%%% source distribution parameters
    
    switch lower(Args.MagDistr)        
        case 'galex'
            MagL = 13; MagH = 26; Delta_m = 0.2; % the distribution grid in NUV Mag (from GALEX)
            MagBins = (MagH - MagL) / Delta_m;
            
            Mag       = zeros(MagBins,1);
            SrcDist   = zeros(MagBins,1);
            
            for iMag = 1:1:MagBins                
                Mag(iMag)  = MagL + (iMag - 1) * Delta_m;                
                SrcDeg      = 10.^( 0.35 * Mag(iMag) - 4.9 );            % per 1 deg^2 (fitted from the GALEX data)
                SrcDist(iMag) = ceil( SrcDeg * Delta_m * Args.Size^2 );  % rescaled for Delta_m and Args.Size^2                
            end                         
            NumSrc = sum(SrcDist,'all');            
            % read in the relation of NUV magnitudes and ULTRASAT magnitudes
            % for a given set of source spectra (usually, just the 3 BB temperatures)
            % NB: if you wish to model another set of spectra, first rerun convertGALEXdistribution
            % in order to make a new GALEX_ULTRASAT_magn.mat object!
            MagDB = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/GALEX_ULTRASAT_magn.mat');
            io.files.load1(MagDB); % variables: MagU (3D), Temp, MagNUV, Rad 
        case 'art'
            NumSrc = ceil(Args.NumSrc * Args.Size^2); 
            MagUS  = zeros(NumSrc,1);
            for Isrc = 1:NumSrc
                MagUS(Isrc) = 15 + 1.5 * log10(Isrc); % HERE THE DISTRIBUTION IS SET
                IndT        = rem(Isrc,NTemp) + 1; Spec(Isrc,:) = S(IndT);
            end
        otherwise
            error('Incorrect distribution type');
    end    
    
    %%%%% create a source catalog 
  
    Cat = zeros(NumSrc,2);    
    if Args.SkyCat % make an additional array of pixel coordinates
        CatPix = zeros(NumSrc,2);
    end
    
    if isempty(Args.Shift) && isempty(Args.Rot) % shift and rotation not given, create and save a new catalog
        
        rng('shuffle');
        if Args.SkyCat % NB: this will not correspond to a Size x Size region on the sky!
            Cat(:,1) = Args.X0  + Args.Size * rand(NumSrc,1); % X0 is here RA in sky degrees
            Cat(:,2) = Args.Y0  + Args.Size * rand(NumSrc,1); % Y0 is here Dec in sky degrees
            [CatPix(:,1), CatPix(:,2)] = SimWCS.sky2xy(Cat(:,1),Cat(:,2)); % calculate pixel coordinates
        else           % use pixel coordinates
            X0 = ceil(Args.X0 / PixSizeDeg); % left corner of the modelled square region
            Y0 = ceil(Args.Y0 / PixSizeDeg); % left corner of the modelled square region
            Range = Args.Size / PixSizeDeg;  % X and Y size of the modelled square region
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
                error(['Source catalog rotation in sky coordinates is not available, ...' ...
                    'better rotate the telescope with PlaneRotation, exiting..']);
            end
            Alpha = Args.Rot * (pi/180.);
            Cat(:,1) = Cat(:,1) * cos(Alpha) - Cat(:,2) * sin(Alpha);
            Cat(:,2) = Cat(:,1) * sin(Alpha) + Cat(:,2) * cos(Alpha);
            fprintf('Source catalog rotated by %d degrees\n',Alpha);
        end
        
    end

    %%%%% determine the ULTRASAT magnitudes from NUV magnitudes and distribute the spectra  
    if strcmpi(Args.MagDistr,'galex')        
        Isrc  = 0;
        MagUS = zeros(NumSrc,1);
        for iMag = 1:1:MagBins
            for jSrc = 1:1:SrcDist(iMag)
                
                Isrc = Isrc + 1;
                
                if Args.SkyCat % sky coordinates in Cat, use CatPix
                    RadSrc = sqrt( CatPix(Isrc,1)^2 + CatPix(Isrc,2)^2 ) * PixSizeDeg; % deg
                else           % pixel coordinates in Cat
                    RadSrc = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) * PixSizeDeg; % deg
                end
                [~, IndR] = min( abs(RadSrc - Rad) ); % search for the nearest node
                
                % divide the population into NTemp colours:
                IndT = rem(Isrc,NTemp) + 1; Spec(Isrc,:) = S(IndT);
                %             Spec(Isrc,:) = S(1); % ONE TIME CODE for 1 temperature simulation
                
                % search for the nearest magnitude (from the GALEX_ULTRASAT_magn.mat grid)
                [~, IndM] = min( abs( Mag(iMag) - MagNUV ) );
                
                MagUS(Isrc) = MagU( IndT, IndM, IndR );                
            end
        end        
    end
    
    %%%% run the simulation 
    simImage = ultrasat.usim('Cat', Cat, 'Mag', MagUS, 'Spec', Spec, 'Exposure', [Args.ExpNum 300],...
        'OutDir', Args.OutDir,'SkyCat', Args.SkyCat, 'PlaneRotation', Args.PlaneRotation,...
        'RA0', Args.RA0, 'Dec0', Args.Dec0, 'OutName', Args.OutName, 'Tile', Args.Tile);
    
end
