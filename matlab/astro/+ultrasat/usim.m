function usimImage =  usim ( Args ) 
    % Make a simulated ULTRASAT image from a source catalog
    % Package: ultrasat
    % Description: Make a simulated ULTRASAT image from a source catalog
    % Input:    
    %       -  Args.InCat (a catalog of simulated sources or the number of sources to generate randomly)
    %       -  Args.InMag (a vector of source magnitudes or 1 magnitude for all the sources)
    %       -  Args.InMagFilt (the filter[s] of source magnitudes)
    %       -  Args.InSpec (individual spectra or one spectral model for all the objects)
    %       -  Args.Exposure (image exposure)
    %       -  Args.Tile (name of the ULTRASAT tile)
    %       -  Args.ImRes (image resolution in 1/pix units)
    %       -  Args.RotAng (tile rotation angle[s] relative to the axis of the raw PSF database)
    %       -  Args.ArraySizeLimit (the maximal array size, machine-dependent, determines the method in specWeight)
    %       -  Args.NoiseDark (dark current noise)
    %       -  Args.NoiseSky  (sky background)
    %       -  Args.NoisePoisson (Poisson noise)
    %       -  Args.NoiseReadout (Read-out noise)
    %       -  Args.Inj (injection method, technical)
    %       -  Args.OutType (type of output image: FITS, AstroImage object, RAW object)
    %       -  Args.Dir (the output directory)
    % Output : - usimImage (simulated AstroImage object, FITS file output, RAW file output)           
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.   Mar 2023
    % Example: Sim = ultrasat.usim('InCat',10) 
    % put in 10 sources at random positions with the default spectrum and flux  
  
    arguments  
        
        Args.InCat           =  10;          % if a number N, generate N random fake sources
                                             % if a 2D table, use X, Y from this table
                                             % if an AstroCat object, use source coordinates from this object
                                             
        Args.InMag           =  20;          % apparent magnitude of the input sources: 
                                             % one magnitude for all the objects 
                                             % or a vector of magnitudes 
                                             
        Args.InMagFilt       = {'ULTRASAT','R1'}; % one filter for all the source magnitudes or an array of filters
        
        Args.InSpec          = {'BB', 5800}; % parameters of the source spectra: 
                                             % either an array of AstSpec or AstroSpec objects
                                             % or an array of model spectra parameters: 
                                             % '{'BB', 3500}', Temperature (K) -- blackbody
                                             % '{'PL', 2.}', Alpha -- power-law F ~ lambda^alpha
                                             % '{'Tab',table}, NumSrc spectra, each spectrum in a column
                                             % NB: the input spectral intensity should be
                                             % in [erg cm(-2) s(-1) A(-1)] as seen near Earth!
                                             
        Args.Exposure        = [3 300];      % number and duration of exosures [s]; 1 x 300 s is the standard ULTRASAT exposure
        
        Args.Tile            = 'B';          % the tile name: 'B' is the upper right tile  
        
        Args.ImRes           = 5;            % image resolution: 5 is 1/5 of the ULTRASAT pixel
                                             % possible values: 1, 2, 5, 10, 47.5

        Args.RotAng          = 0;            % tile rotation angle relative to the axis of the raw PSF database (deg)
                                             % may be a vector with individual angle for each of the sources
                                             
        Args.ArraySizeLimit  = 8;            % [Gb] the limit determines the method employed in inUtil.psf.specWeight
        
        Args.NoiseDark       = 1;            % Dark count noise
        Args.NoiseSky        = 1;            % Sky background 
        Args.NoisePoisson    = 1;            % Poisson noise
        Args.NoiseReadout    = 1;            % Read-out noise
                                             % (see details in imUtil.art.noise)
                                             
        Args.Inj             = 'direct';     % source injection method can be either 'FFTshift' or 'direct'
        
        Args.OutType         = 'all';        % output type: 'AstroImage', 'FITS', 'all' (default)
        
        Args.OutDir          = '.';          % the output directory
         
    end
    
                        % start of simulation
                        
                        Exposure = Args.Exposure(1) * Args.Exposure(2); 
    
                        cprintf('hyper','ULTRASAT simulation started\n');
                        fprintf('%s%d%s%s\n','The total requested exposure is ',Exposure,...
                                ' seconds for tile ',Args.Tile);
                        fprintf('%s%d%s\n','The requested PSF resolution is 1/',Args.ImRes,' of the pixel size');
                            
                        % performance speed test
                        tic; tstart = clock;
    
    %%%%%%%%%%%%%%%%%%%%% Simulation parameters and some physical constants
    
    Eps = 1e-12;            % precision (currently not employed?)
    
    C   = constant.c;       % the speed of light in vacuum, [cm/s]
    H   = constant.h;       % the Planck constant, [erg s]   
    
    RAD = 180./pi;          % the radian
    
    %%%%%%%%%%%%%%%%%%%% ULTRASAT PSF database parameters
               
    Nrad    = 25; 
    Rad     = linspace(0,10,Nrad);          % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    WavePSF = linspace(MinWave,MaxWave,91); % lab PSF grid points in wavelength
    
    PixRat  = 47.5; % the ratio of the ULTRASAT pixel size to that of the lab PSF image (currently not employed?)
        
    %%%%%%%%%%%%%%%%%%%% the spectral grid used for countrate calculations

    Nwave   = 9001;                                % number of spectral bins used for countrate calculations
    Wave    = linspace(MinWave,MaxWave,Nwave);     % the wavelength grid 
    
    DeltaLambda = (MaxWave-MinWave)/(Nwave-1);     % the wavelength bin size in Angstroms 
    
    %%%%%%%%%%%%%%%%%%%% basic ULTRASAT imaging parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    
    FocalLength = 360;    % [mm] 
    PixelSizeMm = 9.5e-3; % [mm] pixel size 
    PixSizeDeg  = ( PixelSizeMm / FocalLength ) * RAD;  % [deg] pixel size  
    PixSize     = PixSizeDeg * 3600; % [arcsec] 
    
    DAper       = 33.;                   % [cm]    aperture diameter
    SAper       = pi * DAper ^ 2 / 4;    % [cm(2)] aperture area
    
    % coordinates of the inner core of the tile and the rotation angle of the PSF: 
    % NB: the additional rotation by -90 deg for tile "B" is required to have the coma in the right position!
    
    switch Args.Tile
        
        case 'A'
            
            X0 = ImageSizeX + 0.5; Y0 = 0.5;
            RotAngle = Args.RotAng - 90 + 90;  
            
        case 'B'
            
            X0 = 0.5;              Y0 = 0.5;
            RotAngle = Args.RotAng - 90 + 0;  
            
        case 'C'
            
            X0 = 0.5;              Y0 = ImageSizeY + 0.5;
            RotAngle = Args.RotAng - 90 - 90;  
            
        case 'D'
            
            X0 = ImageSizeX + 0.5; Y0 = ImageSizeY + 0.5;
            RotAngle = Args.RotAng - 90 + 180;  
            
        otherwise
            
            cprintf('err','Invalid tile name, exiting..\n');
            return
        
    end
    
    % pixel saturation and per pixel background (estimated by YS),
    %  e-/ADU conversion coefficients
    
    FullWell0    = 1.6e5;                               % [e-] the pixel saturation limit for 1 exposure
    FullWell     = FullWell0 * Args.Exposure(1);        % the limit for a serie of exposures 
    
    GainThresh = 16000;
    E2ADUlow   = 1.185;  % below GainThresh e-/pix 
    E2ADUhigh  = 0.074;  % above GainThresh e-/pix
    
    % [e-/pix] background estimates for a 300 s exposure made by YS
    
    Back.Zody    = 27; Back.Cher  = 15; Back.Stray = 12; Back.Dark = 12;
    Back.Readout =  6; Back.Cross =  2; Back.Gain  =  1;
    
    Back.Tot = ( Back.Zody  + Back.Cher + Back.Stray + Back.Dark + ...
                 Back.Cross + Back.Gain ) * sqrt(Exposure/300.) + Back.Readout * Args.Exposure(1);
    
    %%%%%%%%%%%%%%%%%%%% load the matlab object with the ULTRASAT properties:
    
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    io.files.load1(UP_db,'UP'); 
    % is it possible to read just some of the UP structures: UP.TotT, UP.wavelength and, possibly, UP.Specs?
    % we can save them as separate .mat objects and read those  
        
    %%%%%%%%%%%%%%%%%%%% read source coordinates from an input catalog or make a fake catalog
    
    if isa(Args.InCat,'AstroCatalog') % read sources from an AstroCatalog object 
 
        % read in an appropriate ULTRASAT header and make a WCS
        % SimHeader = AstroHeader('/home/sasha/matlab/data/ULTRASAT/UC-3200-TN003-01_FITS_formatted_image_example.fits',0); 
        % SimWCS = AstroWCS.header2wcs(SimHeader);
        
        NumSrc = size(Args.InCat.Catalog,1); 

        RA            = Args.InCat.Catalog(:,find(strcmp(Args.InCat.ColNames, 'RAJ2000'))); 
        DEC           = Args.InCat.Catalog(:,find(strcmp(Args.InCat.ColNames, 'DEJ2000'))); 
        
        if isempty(find(strcmp(Args.InCat.ColNames, 'X'), 1)) % no pixel coordinates in the catalog
            [CatX, CatY]  = SimWCS.sky2xy(RA,DEC);         % needs an astro WCS object!
        else                                               % read pixel coordinates
            CatX      = Args.InCat.Catalog(:,find(strcmp(Args.InCat.ColNames, 'X'))); 
            CatY      = Args.InCat.Catalog(:,find(strcmp(Args.InCat.ColNames, 'Y'))); 
        end
                
        CatFlux       = zeros(NumSrc,1);   % will be determined below from spectra * transmission 
        InMag         = zeros(NumSrc,1);    
        
                            fprintf('%d%s\n',NumSrc,' sources read from the input AstroCatalog object');
        
    elseif size(Args.InCat,2) > 1 && size(Args.InCat,3) == 1 % read source pixel coordinates from a table
        
        NumSrc = size(Args.InCat,1);
        
        CatX   = Args.InCat(:,1);
        CatY   = Args.InCat(:,2);
        
        RA      = zeros(NumSrc,1);  % will be determined below if a WCS is set
        DEC     = zeros(NumSrc,1);  % -//-
        CatFlux = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
        InMag   = zeros(NumSrc,1);    
        
                            fprintf('%d%s\n',NumSrc,' sources read from the input table');
        
    elseif size(Args.InCat,2) == 1 % make a fake catalog with Args.InCat sources randomly distributed over the FOV
        
        NumSrc = Args.InCat;  % the number of fake sources

        CatX    = max(1, floor( ImageSizeX * rand(NumSrc,1) )); 
        CatY    = max(1, floor( ImageSizeY * rand(NumSrc,1) )); 
        
        RA      = zeros(NumSrc,1);  % will be determined below if a WCS is set
        DEC     = zeros(NumSrc,1);  % -//-
        CatFlux = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
        InMag   = zeros(NumSrc,1);    
        
                            fprintf('%d%s\n',NumSrc,' random sources generated');
                            
    else
        
                            cprintf('err','Incorrect catalog input, exiting ...\n'); 
                            return
        
    end

    %%%%%%%%%%%%%%%%%%%%% obtain radial distances of the sources from the INNER CORNER of the tile 
    %%%%%%%%%%%%%%%%%%%%% and regrid the throughput array at given source positions 
         
    RadSrc = zeros(NumSrc,1); 
    TotT   = zeros(NumSrc,Nwave);

    for Isrc = 1:1:NumSrc
        
        RadSrc(Isrc) = sqrt( ( CatX(Isrc) - X0 )^2 + ( CatY(Isrc) - Y0 )^2 ) *  PixSizeDeg; % the source radii [deg]
        
        TotT(Isrc,:) = interpn(UP.wavelength, Rad', UP.TotT, Wave', RadSrc(Isrc));          % regrid the throughput 
    end
    
    %%%%%%%%%%%%%%%%%%%%% read or generate source spectra
    %%%%%%%%%%%%%%%%%%%%%
    
                            fprintf('Reading source spectra.. ');
    
    SpecIn  = zeros(NumSrc, Nwave);  % the incoming spectra 
    SpecAbs = zeros(NumSrc, Nwave);  % the incoming spectra convolved with the throughput

    % read the input spectra or generate synthetic spectra 
    
        %%% TEST: use the Stellar Spectra from UP.Specs
        if false  
    
            fprintf('TEST RUN: using stellar spectra from UP.Specs ..');

            for Isrc = 1:1:NumSrc
                % stellar spectra from Pickles. NB: these are normalized to 1 at 5556 Ang !
                %Pick(Isrc) = UP.Specs( rem(Isrc,43)+1 ); 
                %Pick(Isrc) = UP.Specs(17); % a G0.0V star 
                %Pick(Isrc) = UP.Specs(27); % an M0.0V star 
                if rem(Isrc,2) == 0 % for Cat2 catalog
                    Pick(Isrc) = UP.Specs(17); % UP.Specs(17); 
                else
                    Pick(Isrc) = UP.Specs(27); % UP.Specs(27); 
                end
            end
            
            Args.InSpec = Pick(1:NumSrc);

        end
        %%% END TEST 
    
    switch isa(Args.InSpec,'AstroSpec') || isa(Args.InSpec,'AstSpec')
        
        case 0  % make a synthetic spectrum for a given model
            
            switch lower( Args.InSpec{1} ) 
                
                case 'bb'                   
                    
                if numel( Args.InSpec{2} ) ~= NumSrc && numel( Args.InSpec{2} ) ~= 1
                    
                    cprintf('err','\n The number of input temperatures is incorrect, exiting..\n')
                    return
                    
                end
                               
                Temp(1:NumSrc) = Args.InSpec{2};
 
                if numel( Args.InSpec{2} ) == 1
                    fprintf('%s%5.0f%s','generating BB spectra for T = ',Temp(1),' K .. ');
                else
                    fprintf('%s','generating BB spectra for individual source temperatures .. ');
                end
                
                for Isrc = 1:1:NumSrc
                    
                      SpecIn(Isrc,:) = AstroSpec.blackBody(Wave',Temp(Isrc)).Flux; % erg s(-1) cm(-2) A(-1)
                    
                end
            
                case 'pl'
                    
                if numel( Args.InSpec{2} ) ~= NumSrc && numel( Args.InSpec{2} ) ~= 1
                    
                    cprintf('err','\n The number of input spectral indexes is incorrect, exiting..\n')
                    return
                    
                end
                
                Alpha(1:NumSrc) = Args.InSpec{2};

%                 fprintf('%s%4.2f%s','generating PL spectra: Alpha(Source1) = ',Alpha(1),' ..');
                fprintf('%s','generating PL spectra .. ');
                
                for Isrc = 1:1:NumSrc
                                 
                    SpecIn(Isrc,:) = Wave .^ Alpha(Isrc);                          % erg s(-1) cm(-2) A(-1)                                                       
                    
                end
                                
                case 'tab'

                fprintf('%s','Reading source spectra from a table..');

                if size( Args.InSpec{2}, 2) == NumSrc && size( Args.InSpec{2}, 1) == Nwave
                    % NumSrc spectra at the standard grid 200:1:11000
                    for Isrc = 1:1:NumSrc

                        SpecIn(Isrc,:) = Args.InSpec{2}(:,Isrc); % read the spectra from table columns

                    end
                elseif size( Args.InSpec{2}, 2) == NumSrc + 1
                    % NumSrc spectra at a nonstandard grid, the wavelength
                    % grid is in the column number NumSrc + 1 
                    for Isrc = 1:1:NumSrc 

                        SpecIn(Isrc,:) = interp1(Args.InSpec{2}(:,NumSrc+1), Args.InSpec{2}(:,Isrc), Wave, 'linear', 0);

                    end
                else
                    cprintf('err','\n Number of columns or rows in the spectral input table is incorrect, exiting..\n')
                    return
                end
                
                otherwise
                
                cprintf('err','Spectral parameters not properly defined in uSim, exiting..\n');
                return
                
            end
            
            
        case 1  % read the table from an AstroSpec/AstSpec object and regrid it to Wave set of wavelengths 
            
            for Isrc = 1:1:NumSrc
                                                
                % the simplest way to regrid is to interpolate and set to 0 outside the range
                % deb: is not it safer to use griddedinterpolant? 
                
                if isa(Args.InSpec,'AstSpec') 
                    SpecIn(Isrc,:) = interp1( Args.InSpec(Isrc).Wave, Args.InSpec(Isrc).Int, Wave, 'linear', 0);
                elseif isa(Args.InSpec,'AstroSpec')
                    SpecIn(Isrc,:) = interp1( Args.InSpec(Isrc).Wave, Args.InSpec(Isrc).Flux, Wave, 'linear', 0);
                end
                
                                    
            end
            
            % try to make a 1-liner instead of a cycle? 
            % SpecIn = interp1( 1:NSrc, Args.InSpec.Wave, Args.InSpec.Int, 1:NSrc, Wave, 'linear', 0);
            
    end

                            fprintf('done\n'); 
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
                            tic
    
    
    %%%%%%%%%%%%%% TEST input: a flat spectrum the 240-280 band, null otherwise
    if false
        
        fprintf('TEST input: flat spectrum in the 240-280 band\n');
        
        for Iwave = 1:1:Nwave
            for Isrc = 1:1:NumSrc
                if (Wave(Iwave) < 2400) || (Wave(Iwave) > 2800) 
                    SpecIn(Isrc,Iwave) = 0.;
                else
                    SpecIn(Isrc,Iwave) = 1.;
                end
            end
        end
            
    end
    %%%%%%%%%%%%%%% END TEST
    
    %%%%%%%%%%%%%%%%%%%%%  rescale the spectra to the input magnitudes 
    
                            fprintf('%s%s%s%s%s','Rescaling spectra to fit the input ',...
                                     Args.InMagFilt{1},'/',Args.InMagFilt{2},' magnitudes...');
    
    for Isrc = 1:1:NumSrc
        
          AS = AstroSpec([Wave' SpecIn(Isrc,:)']);
          if numel(Args.InMag) > 1 
              InMag(Isrc) = Args.InMag(Isrc); 
          else
              InMag(Isrc) = Args.InMag(1); 
          end
          SpecScaled  = scaleSynphot(AS, InMag(Isrc), Args.InMagFilt{1}, Args.InMagFilt{2}); 
          SpecIn(Isrc,:) = SpecScaled.Flux; 
          
%           fprintf('%s%d%s%4.1f\n','Eff. magnitude of source ', Isrc,' = ',...
%               astro.spec.synthetic_phot([SpecScaled.Wave, SpecScaled.Flux],'ULTRASAT','R1','AB');
%               astro.spec.synthetic_phot([Wave', SpecIn(1,:)'],'ULTRASAT','R1','AB');
        
    end  
    
                            fprintf('done\n'); 
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
                            tic
                    
    %%%%%%%%%%%%%%%%%%%%%  convolve the spectrum with the ULTRASAT throughut and
    %%%%%%%%%%%%%%%%%%%%%  fill the CatFlux column with the spectrum-intergated countrate fluxes
    
    for Isrc = 1:1:NumSrc
        
        SpecAbs(Isrc,:) = SpecIn(Isrc,:) .* TotT(Isrc,:) .* DeltaLambda ... 
                          .* SAper ./ ( H * C ./ ( 1e-8 * Wave(:) ) )' ;
        % [ counts /s /bin ] = [ erg s(-1) cm(-2) A(-1) ] * [ counts / ph ] * [ A / bin ] * [ cm(2) ]/ [ erg / ph ]
        
        % the wavelength integrated source fluxes [ counts / s ]
        CatFlux(Isrc) = sum ( SpecAbs(Isrc,:), 'all' );   

    end
        
                            fprintf('Source spectra convolved with the throughput\n'); 
                
    %%%%%%%%%%%%%%%%%%%%%  read the chosen PSF database from a .mat file
    
                            fprintf('Reading PSF database.. '); 
    
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
%     if ( size(PSFdata,3) ~= Nwave ) || ( size(PSFdata,4) ~= Nrad )
    if size(PSFdata,4) ~= Nrad 
        cprintf('err','PSF array size mismatch, exiting..\n');
        return
    end
    
                            fprintf('done\n'); 
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); tic
    
    %%%%%%%%%%%%%%%%%%%%%  integrate the throughput-convolved source spectra 
    %%%%%%%%%%%%%%%%%%%%%  S_i(λ)*Th(λ,r_i) with their PSF_i(λ,r_i) over the frequency range 
    %%%%%%%%%%%%%%%%%%%%%  and obtain spectrum-weighted PSF_i for each source
    
    Nx = size(PSFdata,1); 
    Ny = size(PSFdata,2);
    WPSF = zeros( Nx, Ny, NumSrc );
    
                            fprintf('Weighting source PSFs with their spectra.. ');
                    
    WPSF = imUtil.psf.specWeight(SpecAbs, RadSrc, PSFdata, 'Rad', Rad, 'SizeLimit',Args.ArraySizeLimit, ...
                                 'Lambda',WavePSF,'SpecLam',Wave); 
        
                            fprintf('done\n'); 
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); tic

    %%%%%%%%%%%%%%%%%%%%% rotate the weighted source PSFs, blur them due to the S/C jitter 
    %%%%%%%%%%%%%%%%%%%%% and inject them into an empty tile image
    
                            fprintf('Rotating the PSFs and injecting them into an empty image.. ');
    
    [ImageSrc, PSF] = imUtil.art.injectArtSrc (CatX, CatY, CatFlux, ImageSizeX, ImageSizeY,...
                             WPSF, 'PSFScaling',Args.ImRes,'RotatePSF',RotAngle,...
                             'Jitter',1,'Method',Args.Inj,'MeasurePSF',0); 
                       
                            fprintf(' done\n');
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); tic
    
    %%%%%%%%%%%%%%%%%%%%% add and apply various types of noise to the tile image 
    %%%%%%%%%%%%%%%%%%%%% NB: while ImageSrc is in [counts/s], 
    %%%%%%%%%%%%%%%%%%%%%           ImageSrcNoise is already in [counts] !! 
    
                            fprintf('Adding noise .. ');
                    
                    
%     ImageSrcNoise = imUtil.art.noise(ImageSrc,'Exposure',Args.Exposure,...
%                                     'Dark',Args.NoiseDark,'Sky',Args.NoiseSky,...
%                                     'Poisson',Args.NoisePoisson,'ReadOut',Args.NoiseReadout);
%                                 
        
    NoiseLevel    = Back.Tot * ones(ImageSizeX,ImageSizeY);   % already in [counts], see above
    SrcAndNoise   = ImageSrc .* Exposure + NoiseLevel; 
    
%     ImageSrcNoise = poissrnd( SrcAndNoise, ImageSizeX, ImageSizeY);                             
%     ImageBkg      = poissrnd( NoiseLevel, ImageSizeX, ImageSizeY);
%     
%     As the noise level is already quite high for typical exposures,
%     we can use a faster normal distribution instead of the true Poisson distribution
%
    ImageSrcNoise =  normrnd( SrcAndNoise, sqrt(SrcAndNoise), ImageSizeX, ImageSizeY);              
    ImageBkg      =  normrnd( NoiseLevel,  sqrt(NoiseLevel),  ImageSizeX, ImageSizeY);
                                 
                            fprintf(' done\n');                   
                            elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); 
                    
    %%%%%%%%%%%%%%%%%%%%%%  cut the saturated pixels (to be refined later) 
    
    ImageSrcNoise = min(ImageSrcNoise, FullWell);
    
                            fprintf('Saturated pixels cutted\n');
    
    %%%%%%%%%%%%%%%%%%%%%%  make an ADU and mask ADU images
    
    ImageSrcNoiseGainMask = ImageSrcNoise > GainThresh * Args.Exposure(1);  % if the signal is above the threshold, use high gain
    ImageSrcNoiseADU = ImageSrcNoise .* ( ImageSrcNoiseGainMask .* E2ADUhigh + ...
                                          (ones(ImageSizeX,ImageSizeY)-ImageSrcNoiseGainMask) .* E2ADUlow );

    %%%%%%%%%%%%%%%%%%%%%%  output: a) an AstroImage object with filled image, header, and PSF attachments 
    %%%%%%%%%%%%%%%%%%%%%%  b) a FITS image c) a native (RAW) format image 
    
                            fprintf('Compiling output structures and writing files..\n'); drawnow('update'); tic
       
    % if we generated a fake catalog above, make a catalog table here
    if ~isa(Args.InCat,'AstroCatalog')
        Cat = [CatX CatY CatFlux InMag RA DEC];
        Args.InCat = AstroCatalog({Cat},'ColNames',{'X','Y','Counts','MAG','RAJ2000','DEJ2000'},'HDU',1);
    end
    
    % save the final source PSFs into an AstroPSF array 
    
    AP(1:NumSrc) = AstroPSF;
    
    for Isrc = 1:1:NumSrc
        
        AP(Isrc).DataPSF = PSF(:,:,Isrc);
    
    end
    
    % make sky background and variance images?
    Emptybox = zeros(ImageSizeX,ImageSizeY);
        
    % make an AstroImage (note, the image is to be transposed!)
    usimImage = AstroImage( {ImageSrcNoise'} ,'Back',{NoiseLevel}, 'Var',{ImageBkg},'Cat',{Args.InCat.Catalog});   

    % add some keywords and values to the image header % TBD
    funHeader(usimImage, @insertKey, {'DATEOBS','2026-01-01T00:00:00','';'EXPTIME', Exposure,''});  
       
    % save the object in a .mat file for a future usage:
    OutObjName = sprintf('%s%s%s','SimImage_tile',Args.Tile,'.mat');   
    save(OutObjName,'usimImage','-v7.3');
           
    % write the image to a FITS file    
    if strcmp( Args.OutType,'FITS') || strcmp( Args.OutType,'all')
        
        % NB: when writing to a fits image, we need to transpose the image
        OutFITSName = sprintf('%s%s%s%s%s','!',Args.OutDir,'/SimImage_tile',Args.Tile,'.fits'); 
%         imUtil.util.fits.fitswrite(ImageSrcNoise',OutFITSName,'Header',{'EXPTIME', Exposure,''});  %  DOES NOT WORK
        imUtil.util.fits.fitswrite(ImageSrcNoise',OutFITSName);   
        
        % an accompanying region file: 
        OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'.reg');
        DS9_new.regionWrite([CatX CatY],'FileName',OutRegName,'Color','blue','Marker','b','Size',1,'Width',4); 
        
        % more region files for various parts of the source distribution:
        idx = Cat(:,4) > 24.5 & Cat(:,4) < 25.5;      % faintest sources
        CatFaint = Cat(idx,:);  
        OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'faint.reg');
        DS9_new.regionWrite([CatFaint(:,1) CatFaint(:,2)],'FileName',OutRegName,'Color','blue','Marker','o','Size',1,'Width',4);     
      
        idx = Cat(:,4) > 23.5 & Cat(:,4) < 24.5;      % medium brightness sources 
        CatMed = Cat(idx,:);  
        OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'medium.reg');
        DS9_new.regionWrite([CatMed(:,1) CatMed(:,2)],'FileName',OutRegName,'Color','green','Marker','o','Size',1,'Width',4);
        
        idx = Cat(:,4) > 22.5 & Cat(:,4) < 23.5;      % bright sources 
        CatBright = Cat(idx,:);  
        OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'bright.reg');
        DS9_new.regionWrite([CatBright(:,1) CatBright(:,2)],'FileName',OutRegName,'Color','cyan','Marker','b','Size',1,'Width',4);     
        
    end

    %%%%%%%%%%%%%%%%%%%%    
    
                    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); 
                    cprintf('hyper','%s%4.0f%s\n','Simulation completed in ',etime(clock,tstart),...
                                         ' sec, see the generated images')

    %%%%%%%%%%%%%%%%%%%% post modeling checks
%     
%     MeasuredCat = imProc.sources.findMeasureSources(usimImage,'ForcedList',[CatX CatY],...
%                          'OnlyForced',1,'CreateNewObj',1,'ReCalcBack',0,'ZP',UP.ZP(1,1)).CatData;
    
    MeasuredCat = imProc.sources.findMeasureSources(usimImage,'RemoveBadSources',1,'CreateNewObj',1,...
                                                    'ReCalcBack',0,'ZP',UP.ZP(1,1)).CatData;
                     
    Coords = MeasuredCat.Catalog(:,1:2);
    SNRs = MeasuredCat.Catalog(:,8:12);
    Mag_Aper = MeasuredCat.Catalog(:,23:25);
    Mag_Aper_err = MeasuredCat.Catalog(:,26:28);
    Summary = [Coords SNRs(:,4:5) Mag_Aper(:,2:3)]; 
    
    idx5 = Summary(:,3) > 5; % take only sources over 5 sigma
    Summ5 = Summary(idx5,:); 
    
    idx3 = Summary(:,3) > 3; % take only sources over 3 sigma
    Summ3 = Summary(idx3,:); 
    
    OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'detected.reg');
    DS9_new.regionWrite([Summ5(:,1) Summ5(:,2)],'FileName',OutRegName,'Color','red','Marker','o','Size',1,'Width',4);     
  

end
