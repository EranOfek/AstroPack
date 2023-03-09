function usimImage =  usim ( Args ) 
    % Make a simulated ULTRASAT image from a source catalog
    % Package: ultrasat
    % Description: Make a simulated ULTRASAT image from a source catalog
    % Input:    
    %       -  Args.InCat (a catalog of simulated sources)
    %       -  Args.InMag (a vector of source magnitudes or 1 magnitude for all the sources)
    %       -  Args.InMagFilt (the filter of source magnitudes)
    %       -  Args.InSpec (individual spectra or one spectral model)
    %       -  Args.Exposure (image exposure)
    %       -  Args.Tile (name of the ULTRASAT tile)
    %       -  Args.ImRes (image resolution in 1/pix units)
    %       -  Args.RotAng (tile rotation angle[s] relative to the axis of the raw PSF database)
    %       -  Args.NoiseDark (dark current noise)
    %       -  Args.NoiseSky  (sky background)
    %       -  Args.NoisePoisson (Poisson noise)
    %       -  Args.NoiseReadout (Read-out noise)
    %       -  Args.Inj (injection method, technical)
    %       -  Args.OutType (type of output image: FITS, AstroImage object, RAW object)
    %       -  Args.Dir (the output directory)
    % Output : - usimImage (simulated AstroImage object, FITS file output, RAW file output)           
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.   Feb 2023
    % Example: Sim = ultrasat.usim('InCat',10) 
    % put in 10 sources at random positions with the default spectrum and flux  
  
    arguments  
        
        Args.InCat           =  10;          % if = N, generate N random fake sources
                                             % if an AstroCat object, use sources from this object
                                             
        Args.InMag           =  20;          % apparent magnitude of the input sources: 
                                             % one magnitude for all the objects 
                                             % or a vector of magnitudes 
                                             
        Args.InMagFilt       = {'ULTRASAT','R1'}; % the filter of the source magnitudes 
        
        Args.InSpec          = {'BB', 5800}; % parameters of the source spectra: 
                                             % either an array of AstSpec or AstroSpec objects
                                             % or an array of model spectra parameters: 
                                             % '{'BB', 3500}', Temperature (K) -- blackbody
                                             % '{'PL', 2.}', Alpha -- power-law F ~ lambda^alpha
                                             % NB: the input spectral intensity should be
                                             % in [erg cm(-2) s(-1) A(-1)] as seen near Earth!
                                             
        Args.Exposure        = 300;          % image exposure, [s]; 300 s in the standard ULTRASAT exposure
        
        Args.Tile            = 'B';          % the tile name: 'B' is the upper right tile  
        
        Args.ImRes           = 5;            % image resolution: 5 is 1/5 of the ULTRASAT pixel
                                             % possible values: 1, 2, 5, 10, 47.5

        Args.RotAng          = 0;            % tile rotation angle relative to the axis of the raw PSF database (deg)
                                             % may be a vector with individual angle for each of the sources
        
        Args.NoiseDark       = 1;            % Dark count noise
        Args.NoiseSky        = 1;            % Sky background 
        Args.NoisePoisson    = 1;            % Poisson noise
        Args.NoiseReadout    = 1;            % Read-out noise
                                             % (see details in imUtil.art.noise)
                                             
        Args.Inj             = 'direct';     % source injection method can be either 'FFTshift' or 'direct'
        
        Args.OutType         = 'all';        % output type: 'AstroImage', 'FITS', 'all' (default)
        
        Args.OutDir          = '.';          % the output directory
         
    end
    
                        % performance speed test
    
                        fprintf('ULTRASAT simulation started\n');
                        tic; tstart = clock;
    
    %%%%%%%%%%%%%%%%%%%%% Simulation parameters and some physical constants
    
    Eps = 1e-12;  % precision 
    
    C   = constant.c;       % the speed of light in vacuum, [cm/s]
    H   = constant.h;       % the Planck constant, [erg s]   
    
%     Parsec = constant.pc;   % the parsec, [cm]
% 
%     Rsun  = constant.SunR;  % [cm] Solar radius
%     Lsun  = constant.SunL;  % [erg/s] Solar luminosity
%     
%     Rstar = 1. * Rsun;   % stellar radius in Rsun % par 
%     Dstar = 10;          % [pc] stellar distance  % par  
    
    %%%%%%%%%%%%%%%%%%%% ULTRASAT PSF database parameters
               
    Nwave   = 91; % lab PSF grid points in wavelength
    Nrad    = 25; % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    Wave    = linspace(MinWave,MaxWave,Nwave);
    Rad     = linspace(0,10,Nrad);
    
    DeltaLambda = round( (MaxWave-MinWave)/(Nwave-1) );  % the wavelength bin size in Angstrom [should be 100 A]
    
    PixRat  = 47.5; % the ratio of the ULTRASAT pixel size to that of the lab PSF image
    
    %%%%%%%%%%%%%%%%%%%% basic ULTRASAT imaging parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    PixSize     =  5.4; % pixel size (arcsec)
    PixSizeDeg  = PixSize/3600; % pixel size (deg)
    
    DAper       = 33.;                   % [cm]    aperture diameter
    SAper       = pi * DAper ^ 2 / 4;    % [cm(2)] aperture area
        
    %%%%%%%%%%%%%%%%%%%% load the matlab object with the ULTRASAT properties:
    
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    load(UP_db,'UP'); 
    % is it possible to read just some of the UP structures: UP.TotT and, possibly, UP.Specs?
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
 
        
    elseif size(Args.InCat,2) > 1 % read source pixel coordinates from a table
        
        NumSrc = size(Args.InCat,1);
        
        CatX   = Args.InCat(:,1);
        CatY   = Args.InCat(:,2);
        
        RA      = zeros(NumSrc,1);  % will be determined below if a WCS is set
        DEC     = zeros(NumSrc,1);  % -//-
        CatFlux = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
        InMag   = zeros(NumSrc,1);    
        
                            fprintf('%d%s\n',NumSrc,' sources read from the input table');
        
        
    else % make a fake catalog with Args.InCat sources randomly distributed over the FOV
        
        NumSrc = Args.InCat;  % the number of fake sources

        CatX    = round( ImageSizeX * rand(NumSrc,1) ); 
        CatY    = round( ImageSizeY * rand(NumSrc,1) ); 
        
        RA      = zeros(NumSrc,1);  % will be determined below if a WCS is set
        DEC     = zeros(NumSrc,1);  % -//-
        CatFlux = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
        InMag   = zeros(NumSrc,1);    
        
                            fprintf('%d%s\n',NumSrc,' random sources generated');
        
    end

    %%%%%%%%%%%%%%%%%%%%% obtain radial distances of the sources from the INNER CORNER of the tile 
    %%%%%%%%%%%%%%%%%%%%% and rescale the throuput array at given source positions 
         
    RadSrc = zeros(NumSrc,1); 
    TotT   = zeros(NumSrc,Nwave);

    for Isrc = 1:1:NumSrc
        
        RadSrc(Isrc) = sqrt( CatX(Isrc)^2 + CatY(Isrc)^2 ) *  PixSizeDeg; 
        
        Ir = find (Rad  <= RadSrc(Isrc),  1, 'last');   % find the nearest grid point in the Rad array
                                                        % [later replace it by a linear interpolation (TBD) ]
        TotT(Isrc,:) = UP.TotT(1:DeltaLambda:9001, Ir); % take the throughput array values at a 100 A grid
       
    end
    
    %%%%%%%%%%%%%%%%%%%%% read or generate source spectra
    
    fprintf('Reading source spectra.. ');
    
    SpecIn  = zeros(NumSrc, Nwave);  % the incoming spectra 
    SpecAbs = zeros(NumSrc, Nwave);  % the incoming spectra convolved with the throughput

    % read the input spectra or generate synthetic spectra
    
        % TEST: use the Stellar Spectra from UP.Specs
        if true  
    
            fprintf('TEST RUN: using spectra from UP.Specs ..');

            for Isrc = 1:1:NumSrc
                % star spectra from Pickles. NB: these are normalized to 1 at 5556 Ang !
                %Pick(Isrc) = UP.Specs( rem(Isrc,43)+1 ); 
                %Pick(Isrc) = UP.Specs(17); % a G0.0V star 
                %Pick(Isrc) = UP.Specs(27); % an M0.0V star 
                if rem(Isrc,2) == 0 % for Cat2 catalog
                    Pick(Isrc) = UP.Specs(1); % UP.Specs(17); 
                else
                    Pick(Isrc) = UP.Specs(2); % UP.Specs(27); 
                end
            end
                Args.InSpec = Pick(1:NumSrc);

%             for Isrc = 1:1:NumSrc
%                   Args.InSpec(Isrc).Int = Args.InSpec(Isrc).Int / sum ( Args.InSpec(Isrc).Int * 5.0 ); % 5.0 A is the bin width
%                   Args.InSpec(Isrc).Int = Args.InSpec(Isrc).Int .* Lsun / (4 * pi * ( Dstar * 3.1e18 )^2 ); % Solar luminosity 
%             end
%                   
            % do not need to normalize the spectrum here, it will be done below based on the magnitude
        end
        % END TEST 
    
    switch isa(Args.InSpec,'AstroSpec') || isa(Args.InSpec,'AstSpec')
        
        case 0  % make a synthetic spectrum for a given model
            
            if Args.InSpec{1} == 'BB' 

                fprintf('%s%5.0f%s','generating BB spectra for T = ',Args.InSpec{2},' K .. ');
                
                for Isrc = 1:1:NumSrc
                    
                    % SpecIn(Isrc,:) = AstSpec.blackbody(Args.InSpec{2},Wave).Int; % AstSpec is deprecated
%                     SpecIn(Isrc,:) = AstroSpec.blackBody(Wave',Args.InSpec{2},...
%                                      'Radius',Rstar,'Dist',Dstar).Flux; % erg s(-1) cm(-2) A(-1)
                    % do not need to normalize the spectrum here, it will be done below based on the magnitude
                      SpecIn(Isrc,:) = AstroSpec.blackBody(Wave',Args.InSpec{2}).Flux; % erg s(-1) cm(-2) A(-1)
                    
                end
                        
            
            elseif Args.InSpec{1} == 'PL'


                fprintf('%s%4.2f%s','generating PL spectra for alpha = ',Args.InSpec{2},' ..');
                
                for Isrc = 1:1:NumSrc
                    
                    PLalpha = Args.InSpec{2};
%                     PLalpha1 = PLalpha + 1.;
%                     PLnorm = 1 / ( (1 / PLalpha1 ) * ( Wave(Nwave)^PLalpha1-Wave(1)^PLalpha1 ) );
%                     SpecIn(Isrc,:) = PLnorm * Wave .^ PLalpha; % erg s(-1) cm(-2) A(-1) 
%                                                                % Flux = sum(F_l * Delta_l) normalized to 1
%                     SpecIn(Isrc,:) = SpecIn(Isrc,:) .* Lsun / (4 * pi * ( Dstar * Parsec )^2 ); % 
                    % do not need to normalize the spectrum here, it will be done below based on the magnitude
                    SpecIn(Isrc,:) = Wave .^ PLalpha; % erg s(-1) cm(-2) A(-1)                                                       
                    
                end
                                
            else
                
                fprintf('Spectra not defined in USim, exiting..\n');
                return
                
            end
            
            
        case 1  % read the table from an AstroSpec/AstSpec object and regrid it to Wave set of wavelengths 
            
            for Isrc = 1:1:NumSrc
                
                % Spec(Isrc,:) = specRegrid( Args.InSpec{Isrc}, Wave ); % to be written? 
                
                % the simplest way to regrid is to interpolate and set to 0 outside the range
                % deb: is it safer to use griddedinterpolant? 
                
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
    
    for Isrc = 1:1:NumSrc
        
          AS = AstroSpec([Wave' SpecIn(Isrc,:)']);
          if size(Args.InMag,1) > 1 
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
    ReadDB = struct2cell ( load(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
    if ( size(PSFdata,3) ~= Nwave ) || ( size(PSFdata,4) ~= Nrad )
        fprintf('PSF array size mismatch, exiting..\n');
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
                    
    WPSF = imUtil.psf.specWeight(SpecAbs, RadSrc, PSFdata, 'Rad', Rad); 
    
                    fprintf('done\n'); 
                    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update'); tic

    %%%%%%%%%%%%%%%%%%%%% rotate the weighted source PSFs, blur them due to the S/C jitter 
    %%%%%%%%%%%%%%%%%%%%% and inject them into an empty tile image
    
    RotAngle = Args.RotAng - 90;  
    % the additional rotation by -90 deg is required 
    % to have the coma in the right place!
    
                    fprintf('Rotating the PSFs and injecting them into an empty image.. ');
    
    [ImageSrc, PSF] = imUtil.art.injectArtSrc (CatX, CatY, CatFlux, ImageSizeX, ImageSizeY,...
                             WPSF, 'PSFScaling',Args.ImRes,'RotatePSF',RotAngle,...
                             'Jitter',1,'Method',Args.Inj); 
                       
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

    Back_YS = 75.0 ; % [e-/pix] total background estimate for a 300 s exposure made by YS
    NoiseLevel = Back_YS * sqrt(Args.Exposure/300.) * ones(ImageSizeX,ImageSizeY);
    SrcAndNoise   = ImageSrc .* Args.Exposure + NoiseLevel; 
    
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

    %%%%%%%%%%%%%%%%%%%%%%  output: a) an AstroImage object with filled image, header, and PSF attachments 
    %%%%%%%%%%%%%%%%%%%%%%  b) a FITS image c) a native (RAW) format image 
       
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
        
    % make an AstroImage 
    usimImage = AstroImage( {ImageSrcNoise} ,'Back',{NoiseLevel}, 'Var',{ImageBkg},'Cat',{Args.InCat.Catalog});
    
    % add some keywords and values to the image header % TBD
    funHeader(usimImage, @insertKey, {'DATEOBS','2026-01-01T00:00:00','';'EXPTIME',Args.Exposure,''});  
       
    % save the object in a .mat file for a future usage:
    OutObjName = sprintf('%s%s%s','SimImage_tile',Args.Tile,'.mat');   
    save(OutObjName,'usimImage','-v7.3');
           
    % write the image to a FITS file    
    if strcmp( Args.OutType,'FITS') || strcmp( Args.OutType,'all')
        
        % NB: when writing to a fits image, we need to transpose the image
        OutFITSName = sprintf('%s%s%s%s%s','!',Args.OutDir,'/SimImage_tile',Args.Tile,'.fits'); 
        imUtil.util.fits.fitswrite(ImageSrcNoise',OutFITSName);   
        
        % an accompanying region file: 
        OutRegName  = sprintf('%s%s%s%s',Args.OutDir,'/SimImage_tile',Args.Tile,'.reg');
        DS9_new.regionWrite([CatX CatY],'FileName',OutRegName,'Color','blue'); 
        
    end

    %%%%%%%%%%%%%%%%%%%%    
    
                    fprintf('%s%4.0f%s\n','Simulation completed in ',etime(clock,tstart),...
                                         ' sec, see the generated images')

    %%%%%%%%%%%%%%%%%%%% post modeling checks
    
    MeasuredCat = imProc.sources.findMeasureSources(usimImage,'ForcedList',[CatX CatY],...
                         'OnlyForced',1,'CreateNewObj',1,'ReCalcBack',0,'ZP',UP.ZP(1,1)).CatData;
    
    SNRs = MeasuredCat.Catalog(:,8:12);
    Mag_Aper = MeasuredCat.Catalog(:,23:25);
    

end
