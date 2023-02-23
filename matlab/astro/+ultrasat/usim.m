function usimImage =  usim ( Args ) 

% Make simulated ULTRASAT images from source catalogs
% Package: ultrasat
% Description: Make simulated ULTRASAT images from source catalogs
% Input:    
%       -  Args.InCat (a catalog of simulated sources)
%       -  Args.InSpec (individual spectra or one spectral model)
%       -  Args.ImRes (image resolution in 1/pix units)
%       -  Args.RotAng (SC rotation angle relative to the axis of the raw PSF database)
%       -  Args.Noise (dark noise on the blank image, Poisson noise on the image with sources)
%       -  Args.Inj (injection method, technical)
%       -  Args.OutType (type of output image: FITS, AstroImage object, RAW object)
% Output : - usimImage (simulated AstroImage object, FITS file output, RAW file output)           
% Tested : Matlab R2020b
%     By : A. Krassilchtchikov et al.   Feb 2023
% Example: Sim = ultrasat.usim('InCat',10) 
% put in 10 sources at random positions with the default spectrum and flux  
  
    arguments  
        
        Args.InCat           =  100;         % if = N, generate N random fake sources
                                             % if an AstroCat object, use sources from this object
        
        Args.InSpec          = {'BB', 3500}; % parameters of the source spectra: 
                                             % either an array of AstSpec or AstroSpec objects
                                             % or an array of model spectra parameters: 
                                             % '{'BB', 3500}', Temperature (K) -- blackbody
                                             % '{'PL', 2.}', Alpha -- power-law F ~ lambda^alpha
                                             % NB: the input spectral intensity should be
                                             % in [erg cm(-2) s(-1) A(-1)] as seen near Earth!
        
        Args.ImRes           = 5;            % image resolution: 5 is 1/5 of the ULTRASAT pixel
                                             % possible values: 1, 2, 5, 10, 47.5

        Args.RotAng          = 0;            % tile rotation angle relative to the axis of the raw PSF database (deg)
        
        Args.Noise           = {false,'N'};      % the first cell denotes intial detector noise (dark counts): 
                                             % '1' -- add white (Gaussian) noise
                                             % the second cell denotes the noise applied after 
                                             % the source and sky counts are in: 'P' = Poisson noise
                                             
        Args.Inj             = 'direct';     % source injection method can be either 'FFTshift' or 'direct'
        
        Args.OutType         = 'all';        % output type: 'AstroImage', 'FITS', 'all' (default)
        
        Args.OutDir          = '.';          % the output directory
         
    end
    
    % performance speed test
    
    fprintf('ULTRASAT simulation started\n');
    tic
    
    % Simulation parameters and physical constants
    
    Eps = 1e-12;  % precision 
    
    C   = 2.99792458e10; % the speed of light in vacuum, [cm/s]
    H   = 6.6260755e-27; % the Planck constant, [erg s]
    
    Parsec = 3.1e18;     % [cm]

    Rsun  = 6.957e10;    % [cm] Solar radius
    Lsun  = 3.846e33;    % [erg/s] Solar luminosity
    
    Rstar = 1. * Rsun;   % stellar radius in Rsun % par -- put into Args? 
    
    Dstar = 10;          % [pc] stellar distance  % par -- put into Args? 
    
    % PSF database parameters
               
    Nwave   = 91; % lab PSF grid points in wavelength
    Nrad    = 25; % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    Wave    = linspace(MinWave,MaxWave,Nwave);
    Rad     = linspace(0,10,Nrad);
    
    DeltaLambda = round( (MaxWave-MinWave)/(Nwave-1) );  % the wavelength bin size in Angstrom [should be 100 A]
    
    PixRat  = 47.5; % the ratio of the ULTRASAT pixel size to that of the lab image
    
    % basic ULTRASAT parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    PixSize     =  5.4; % pixels size (arcsec)
    PixSizeDeg  = PixSize/3600; % pixel size (deg)
    
    DAper       = 33.;                   % [cm]    aperture diameter
    SAper       = pi * DAper ^ 2 / 4;    % [cm(2)] aperture area
    STileAper   = SAper/4;               % [cm(2)] 1 tile area
    
    % load the matlab object with the ULTRASAT properties:
    
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    load(UP_db,'UP'); 
    % is it possible to read just some of the UP structures: UP.TotT and, possibly, UP.Specs?
    % we can save them as separate .mat objects and read those 
    
    % make a blank image
    
    Emptybox = zeros( ImageSizeX, ImageSizeY );
    
    % add some dark counts
    
    if Args.Noise{1}
        Image0   = imnoise(Emptybox,'gaussian', 1, 2); % what should be the actual noise scale? 
    else
        Image0   = Emptybox;
    end
    
    % write the empty image with dark counts to a FITS file
    
    OutFITS0     = sprintf('%s%s%s','!',Args.OutDir,'/SimImage0.fits'); 
    OutFITS      = sprintf('%s%s%s','!',Args.OutDir,'/SimImage.fits'); 
    OutFITSdiff  = sprintf('%s%s%s','!',Args.OutDir,'/SimImageDiff.fits'); 
    imUtil.util.fits.fitswrite(Image0',OutFITS0);     
        
    % read sources from a catalog or make a fake catalog
    
    if ~isa(Args.InCat,'AstroCatalog') % make a fake catalog
      
        NumSrc = Args.InCat;  % number of fake sources

        CatX    = round( ImageSizeX * rand(NumSrc,1) ); 
        CatY    = round( ImageSizeY * rand(NumSrc,1) ); 
        RA      = zeros(NumSrc,1);  % will be determined below if a WCS is set
        DEC     = zeros(NumSrc,1);  % -//-
        CatFlux = zeros(NumSrc,1);  % will be determined below from spectra * transmission 
 
        Cat = [CatX CatY CatFlux RA DEC];
        
        fprintf('%d%s\n',NumSrc,' random sources generated');
        
    else % read sources from an AstroCatalog object
        
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
        
        Cat = [CatX CatY CatFlux RA DEC];

        fprintf('%d%s\n',NumSrc,' sources read from the input catalog');
        
    end

    % obtain radial distances of the sources from the INNER CORNER of the tile 
    % and rescale the throuput array at given source positions 
         
    RadSrc = zeros(NumSrc,1); 
    TotT   = zeros(NumSrc,Nwave);

    for Isrc = 1:1:NumSrc
        
        RadSrc(Isrc) = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) *  PixSizeDeg; 
        
        Ir = find (Rad  <= RadSrc(Isrc),  1, 'last'); % find the nearest grid point in the Rad array
                                                      % [later replace it by a linear interpolation (TBD) ]
        TotT(Isrc,:) = UP.TotT(1:DeltaLambda:9001, Ir); % take the throughput array values at a 100 A grid
       
    end
    
    % initialize an array of source spectra
    
    fprintf('Reading source spectra.. ');
    
    SpecIn  = zeros(NumSrc, Nwave);  % the incoming spectra 
    SpecAbs = zeros(NumSrc, Nwave);  % the incoming spectra convolved with the throughput

    % read the input spectra or generate synthetic spectra
    
    % test: use the Spectra from UP.Specs
    for Isrc = 1:1:NumSrc
        Pick(Isrc) = UP.Specs( rem(Isrc,43)+1 ); % star spectra from Pickles. NB: these are normalized to 1 at 5556 Ang !
    end
        Args.InSpec = Pick(1:NumSrc);
        
    for Isrc = 1:1:NumSrc
          Args.InSpec(Isrc).Int = Args.InSpec(Isrc).Int / sum ( Args.InSpec(Isrc).Int * 5.0 ); % 5.0 A is the bin width
          Args.InSpec(Isrc).Int = Args.InSpec(Isrc).Int .* Lsun / (4 * pi * ( Dstar * 3.1e18 )^2 ); % Solar luminosity 
    end
    % end test
    
    switch isa(Args.InSpec,'AstroSpec') || isa(Args.InSpec,'AstSpec')
        
        case 0  % make a synthetic spectrum for a given model
            
            if Args.InSpec{1} == 'BB' 
                
                for Isrc = 1:1:NumSrc
                    
                    % SpecIn(Isrc,:) = AstSpec.blackbody(Args.InSpec{2},Wave).Int; % AstSpec is deprecated
                    SpecIn(Isrc,:) = AstroSpec.blackBody(Wave',Args.InSpec{2},...
                                     'Radius',Rstar,'Dist',Dstar).Flux; % erg s(-1) cm(-2) A(-1)
                    
                end
                        
            
            elseif Args.InSpec{1} == 'PL'
                
                for Isrc = 1:1:NumSrc
                    
                    PLalpha = Args.InSpec{2};
                    PLalpha1 = PLalpha + 1.;
                    PLnorm = 1 / ( (1 / PLalpha1 ) * ( Wave(Nwave)^PLalpha1-Wave(1)^PLalpha1 ) );
                    SpecIn(Isrc,:) = PLnorm * Wave .^ PLalpha; % erg s(-1) cm(-2) A(-1) 
                                                               % Flux = sum(F_l * Delta_l) normalized to 1
                    SpecIn(Isrc,:) = SpecIn(Isrc,:) .* Lsun / (4 * pi * ( Dstar * Parsec )^2 ); % 
                    
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
            % SpecIn = interp1( Args.InSpec.Wave, Args.InSpec.Int, Wave, 'linear', 0);
            
    end

    fprintf('done\n'); 
    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
    tic
    
    % convolve the spectrum with the ULTRASAT throughut and
    % fill the Cat(Isrc,3) column with the spectrum-intergated countrate fluxes
    
    for Isrc = 1:1:NumSrc
        
        SpecAbs(Isrc,:) = SpecIn(Isrc,:) .* TotT(Isrc,:) .* DeltaLambda ... 
                          .* STileAper ./ ( H * C ./ ( 1e-8 * Wave(:) ) )' ;
        % [ counts /s /bin ] = [ erg s(-1) cm(-2) A(-1) ] * [ counts / ph ] * [ A / bin ] * [ cm(2) ]/ [ erg / ph ]
        
        Cat(Isrc,3) = sum ( SpecAbs(Isrc,:), 'all' );  % the source fluxes in [ counts / s ] 

    end
        
    fprintf('Source spectra convolved with the throughput\n'); 
    fprintf('Reading PSF database.. '); 
                
    % read the chosen PSF database from a .mat file
    
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( load(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
    if ( size(PSFdata,3) ~= Nwave ) || ( size(PSFdata,4) ~= Nrad )
        fprintf('PSF array size mismatch, exiting..\n');
        return
    end
    
    fprintf('done\n'); 
    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
    tic
    
    % initialize source PSFs
    
    Nx = size(PSFdata,1); 
    Ny = size(PSFdata,2);
    PSF = zeros( Nx, Ny, NumSrc );
    
    fprintf('Weighting source PSFs with their spectra.. ');
     
    % integrate the throughput-convolved source spectra Si(λ)*Th(λ,r) with their PSFs(λ,r) over the
    % frequency range and obtain a single PSF for each source: imUtil.psf.specWeight
      
    PSF = imUtil.psf.specWeight( PSFdata, RadSrc, Rad, SpecAbs );     
    
    fprintf('done\n');
        
    % rotate the integrated PSFs according to the tile rotation angle:
    % make some advanced imUtil.psf.rotate routine or just use the built-in imrotate? 
    
    fprintf('Rotating the weighted PSFs.. ');
    
    Ang = Args.RotAng;  
    
    if abs( Ang ) < 1 || abs( Ang - 360) < 1 % does not need to spend time on this 
        
        RotPSF = PSF;
        
    else

        for Isrc = 1:1:NumSrc

            RotPSF(:,:,Isrc) = imrotate(PSF(:,:,Isrc), Ang, 'bilinear', 'loose'); 

            % the rotated PSF does not conserve the energy, so need to rescale
            Cons = sum ( RotPSF(:,:,Isrc), 'all' );
            RotPSF(:,:,Isrc) = RotPSF(:,:,Isrc) / Cons;

        end
    
    end
    
    fprintf('done\n');
    
    % NB: the actual size of rotated PSF stamp depends on the particular rotation angle,
    % varying between Nx x Ny and sqrt(2) * Nx x sqrt(2) * Ny
    
    StampSize0 = size( PSF, 1);
    StampSize  = size( RotPSF, 1 );   
    
    fprintf('%s%4.1f%s\n','Final PSF stamp size ',StampSize * Args.ImRes / PixRat, ' pixels');
    
    
    % test containment 
    
    R50 = zeros(NumSrc,2); 
    
    for Isrc = 1:1:NumSrc
        
        R50(Isrc,1) = RadSrc(Isrc);
        R50(Isrc,2) = imUtil.psf.containment('PSF',RotPSF(:,:,Isrc),'Level',0.5) * PixSize / Args.ImRes;
        
    end
    
    R50 = sortrows(R50);
        
    % a visual test
    
%     figure(1)
%     subplot(2,2,1); imagesc(PSF(:,:,1)); title('R= ',RadSrc(1))
%     subplot(2,2,2); imagesc(RotPSF(:,:,1));  title('R= ',RadSrc(1))
%     subplot(2,2,3); imagesc(PSF(:,:,3)); title('R= ',RadSrc(3))
%     subplot(2,2,4); imagesc(RotPSF(:,:,3)); title('R= ',RadSrc(3))            
    

    % performance control
    
    fprintf('Simulated PSFs ready..\n');
    
    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
    fprintf('PSF injection.. ');
      
    tic
    
    if strcmp( Args.Inj, 'FFTshift' ) 
        
    % inject into the blank tile image all the rotated source PSFs: imUtil.art.injectSources
    % (NB: injectSources currently works only with odd stamp sizes!)
    
        if rem( size(RotPSF,1) , 2) == 0 
            fprintf('The size of RotPSF is even, while imUtil.art.injectSources accepts odd size only! Exiting..');
            return
        end

        Image1 = imUtil.art.injectSources(Image0,Cat,RotPSF); 

    elseif strcmp( Args.Inj, 'direct')

    % direct injection
           
        Image1 = imUtil.art.directInjectSources(Image0,Cat,Args.ImRes,RotPSF);
    
    else
        
        fprintf('Injection method not defined! Exiting..\n');
        return
        
    end

    % performance control
    
    fprintf(' done\n');
    
    elapsed = toc; fprintf('%4.1f%s\n',elapsed,' sec'); drawnow('update');
    
    % add some sky noise to the tile image 
    
    SkyNoise = zeros(ImageSizeX, ImageSizeY);

    % SkyNoise = ??

    Image1 = Image1 + SkyNoise; 
    
    % apply Poisson noise
      
    if Args.Noise{2} == 'P'
        Image1 = poissrnd(Image1); % apply Poisson noise to the count rate 
    end
    
    % add read-out noise to the tile image
    
    ReadOutNoise = zeros(ImageSizeX, ImageSizeY); 

    % ReadOutNoise = ??

    Image1 = Image1 + ReadOutNoise; 
    
    % output: a) a native RAW format image 
    % b) a FITS image of an ULTRASAT tile with all the sources PSF + sky noise + read-out noise
    % c) an AstroImage object with filled image, header, and PSF attachments 
   
    % make a source-less AstroImage object (do we really need it?)
        
    usimImage(1) = AstroImage( {Image0} ,'Back',{Emptybox}, 'Var',{Emptybox}); 
    
    % add some keywords and values to the image header % TBD
    funHeader(usimImage(1), @insertKey, {'DATEOBS','2003-07-24T18:28:58','';'EXPTIME',60,''}); 
    
    % save the rotated PSFs into an AstroPSF array and attach it to the image
    
    AP(1:NumSrc) = AstroPSF;
    
    for Isrc = 1:1:NumSrc
        
        AP(Isrc).DataPSF = RotPSF(:,:,Isrc);
    
    end
        
    if strcmp( Args.OutType,'AstroImage') 
        
        % NOTE: what is denoted 'MAG' here will actually be a count rate below!
        Args.InCat = AstroCatalog({Cat},'ColNames',{'X','Y','MAG','RAJ2000','DEJ2000'},'HDU',1);
        % make a new AstroImage with sources injected and write it to FITS file
        usimImage(2) = AstroImage( {Image1} ,'Back',{Emptybox}, 'Var',{Emptybox},'Cat',{Args.InCat.Catalog});
        
        
    elseif strcmp( Args.OutType,'FITS')
        
        imUtil.util.fits.fitswrite(Image1',OutFITS);     
        % imUtil.util.fits.fitswrite(Image1-Image0,OutFITSdiff); % make a difference image
        
    else % all the possible outputs
        
        % NOTE: what is denoted 'MAG' here will actually be a count rate below!
        Args.InCat = AstroCatalog({Cat},'ColNames',{'X','Y','MAG','RAJ2000','DEJ2000'},'HDU',1);
        % make a new AstroImage with sources injected and write it to FITS file
        usimImage(2) = AstroImage( {Image1} ,'Back',{Emptybox}, 'Var',{Emptybox},'Cat',{Args.InCat.Catalog});
                
        imUtil.util.fits.fitswrite(Image1',OutFITS);     
        % imUtil.util.fits.fitswrite(Image1-Image0,OutFITSdiff); % make a difference image
        
        % NB: when writing to a fits image, need to transpose the image?
    
    end

    fprintf('Simulation completed. See the generated images!\n')
    
end
