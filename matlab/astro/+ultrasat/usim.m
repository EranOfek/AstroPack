function usimImage =  usim ( Args ) 

% Make simulated ULTRASAT images from source catalogs
% Package: ultrasat
% Description: Make simulated ULTRASAT images from source catalogs
% Input:    
%       -  Args.InCat (a catalog of simulated sources)
%       -  Args.InSpec (individual spectra or one spectral model)
%       -  Args.ImRes (image resolution in 1/pix units)
%       -  Args.RotAng (SC rotation angle relative to the axis of the raw PSF database)
% Output : - usimImage (simulated AstroImage object, FITS file output, RAW file output)           
% Tested : Matlab R2020b
%     By : A. Krassilchtchikov et al.   Feb 2023
% Example: Sim = usim (Cat, Spec, Resolution, RotAng, OutputType); 
  
    arguments  
        
        Args.InCat           = AstroCatalog({'~/matlab/AstroPack/data/test_tables/asu.fit'},'HDU',2);
        
        Args.InSpec          = {'BB', 3500}; % parameters of the source spectra: either an array of AstroSpec objects
                                             % or an array of model spectra parameters: 
                                             % 'BB', Temperature (K) -- blackbody
                                             % 'PL', Alpha -- power-law F ~ lambda^alpha
        
        Args.ImRes           = 5;          % image resolution: 2 is 1/2 of the ULTRASAT pixel
                                           % possible values: 1, 2, 5, 10, 47.5

        Args.RotAng          = 0;          % tile rotation angle relative to the axis of the raw PSF database (deg)
        
        Args.OutType         = 'AstroImage';
        
    end
    
    % Simulation parameters
    
    Eps = 1e-12;  % precision
    
    % PSF database parameters
               
    Nwave   = 91; % lab PSF grid points in wavelength
    Nrad    = 25; % lab PSF grid points in radius    
            
    Wave    = linspace(2000,11000,Nwave);
    Rad     = linspace(0,10,Nrad);
    
    PixRat  = 47.5; % the ratio of ULTRASAT pixel size to that of the lab image
    
    % ULTRASAT parameters

    ImageSizeX  = 4738; % tile size (pix)
    ImageSizeY  = 4738; % tile size (pix)
    PixSize     = 5.4/3600; % pixel size in degrees 
 
    % load the matlab object with the ULTRASAT properties:
%     
%   [Wave, Rad, Trans] = load();
%    UPP = load('~/matlab/data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat', ...
%         'UP.wavelength', 'UP.Rdeg', 'UP.TotT');  
%     load('~/matlab/data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat', 'wavelength', 'Rdeg', 'TotT');

    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    load(UP_db); % need to read just some of the UP data? 
    
    % make a blank image

    Emptybox = zeros(ImageSizeX,ImageSizeY);
    
    % add some dark counts
    
    Image0   = imnoise(Emptybox,'gaussian', 1, 2);
    
    % read in an appropriate ULTRASAT header and make a WCS 
    
    % SimHeader = AstroHeader('~/matlab/data/ULTRASAT/UC-3200-TN003-01_FITS_formatted_image_example.fits',0); % TBD 
    
    % SimWCS = AstroWCS.header2wcs(SimHeader); %TBD 
            
    % make a source-less AstroImage object 
        
    SimImage0 = AstroImage( {Image0} ,'Back',{Emptybox}, 'Var',{Emptybox}, 'Cat',{Args.InCat.Catalog}); 
    
    % add some keywords and values to the image header 
    
    funHeader(SimImage0,@insertKey,{'DATEOBS','2003-07-24T18:28:58','';'EXPTIME',60,''}); % TBD
    
    % write the empty image to FITS file
    
    % imUtil.util.fits.fitswrite(SimImage0.Image,'!~/SimImage0.fits');
  
    % use a fake catalog (parameters? defined number of sources at random positions?)
    
    Cat = [2003 2022  125; 
           2543 2518  134;
            100  200  880;
           1543 1518  334;
           4000 4500  220;
           2612 2886  238]; 
    
    % or make an array of pixel coordinates and fluxes from InCat AstroCatalog objects
    
    % [CatX, CatY] = SimWCS.sky2xy(Alpha,Delta);
    
    % determine number of sources in the catalog
    
    NumSrc = size(Cat,1);
    
    % obtain radial distances of the sources from the INNER CORNER of the tile
    
    RadSrc = zeros(NumSrc,1);
    
    for Isrc = 1:1:NumSrc
        
        RadSrc(Isrc) = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) *  PixSize;
        
    end
  
    % initialize an array of source spectra
    
    Spec = zeros(NumSrc, Nwave);  % the spectra arrays are defined on the 1..Nwave grid
    
    % read the input spectra or generate synthetic spectra
    
    % test
    Args.InSpec = UP.Specs(1:NumSrc); % star spectra from Pickles % test
    
    switch isa(Args.InSpec,'AstroSpec') || isa(Args.InSpec,'AstSpec')
        
        case 0  % make a synthetic spectrum for a given model
            
            if Args.InSpec{1} == 'BB' 
                
                for ISrc = 1:1:NumSrc
                    
                    % Spec(ISrc,:) = AstSpec.blackbody(Args.InSpec{2},Wave).Int; % AstSpec is deprecated
                    Spec(ISrc,:) = AstroSpec.blackBody(Wave',Args.InSpec{2}).Flux; 
                    
                end
                        
            
            elseif Args.InSpec{1} == 'PL'
                
                for ISrc = 1:1:NumSrc
                    
                    PLalpha = Args.InSpec{2};
                    PLalpha1 = PLalpha + 1.;
                    PLnorm = (1 / PLalpha1 ) * ( Wave(Nwave)^PLalpha1-Wave(1)^PLalpha1 );
                    Spec(ISrc,:) = PLnorm * Wave .^ PLalpha; 
                    
                end
                                
            else
                
                fprintf('Spectra not defined in USim, exiting..\n');
                return
                
            end
            
            
        case 1  % read the table from an AstroSpec/AstSpec object and regrid it to Wave set of wavelengths 
            
            for ISrc = 1:1:NumSrc
                
                % Spec(ISrc,:) = specRegrid( Args.InSpec{ISrc}, Wave ); % to be written? 
                
                % the simplest way to regrid is to interpolate and set to 0 outside the range
                % deb: is it safer to use griddedinterpolant? 
                
                if isa(Args.InSpec,'AstSpec') 
                    Spec(ISrc,:) = interp1( Args.InSpec(ISrc).Wave, Args.InSpec(ISrc).Int, Wave, 'linear', 0);
                elseif isa(Args.InSpec,'AstroSpec')
                    Spec(ISrc,:) = interp1( Args.InSpec(ISrc).Wave, Args.InSpec(ISrc).Flux, Wave, 'linear', 0);
                end
                
                                    
            end
            
            % try to make a 1-liner instead of a cycle? 
            % Spec = interp1( Args.InSpec.Wave, Args.InSpec.Int, Wave, 'linear', 0);
            
    end
                
    % read the chosen PSF database from a .mat file
    
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( load(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
    if ( size(PSFdata,3) ~= Nwave ) || ( size(PSFdata,4) ~= Nrad )
        fprintf('PSF array size mismatch, exiting..\n');
        return
    end
    
    % initialize sources PSFs
    
    Nx = size(PSFdata,1); 
    Ny = size(PSFdata,2);
    PSF = zeros( NumSrc, Nx, Ny );
    
    % integrate the source spectra Si(λ) with their PSFs(λ,r) over the
    % frequency range and obtain a single PSF for each source: imUtil.psf.specWeight
      
    PSF = imUtil.psf.specWeight( PSFdata, RadSrc, Rad, Spec );     
        
    % rotate the integrated PSFs according to the tile rotation angle:
    % imUtil.psf.rotate or just imrotate? 
    
    Ang = Args.RotAng;
    
    Ang = -45; % test
    
    for ISrc = 1:1:NumSrc
        RotPSF(:,:,ISrc) = imrotate(PSF(:,:,ISrc), Ang, 'bilinear', 'loose'); 
        
        % the rotated PSF does not conserve the energy, so need to rescale
        Cons = sum ( RotPSF(:,:,ISrc), 'all' );
        RotPSF(:,:,ISrc) = RotPSF(:,:,ISrc) / Cons;
    end
    
    % NB: the actual size of rotated PSF stamp depends on the particular rotation angle,
    % varying between Nx x Ny and sqrt(2) * Nx x sqrt(2) * Ny
    
    size( PSF, 1);
    size( RotPSF, 1 );   
        
    % visual test
    
    subplot(2,2,1)
    imagesc(PSF(:,:,1))
    subplot(2,2,2)
    imagesc(RotPSF(:,:,1))
    subplot(2,2,3)
    imagesc(PSF(:,:,3))
    subplot(2,2,4)
    imagesc(RotPSF(:,:,3))
        
    
    % save the final PSFs into an AstroPSF array and attach it to the image: AstroPSF
    
    AP(1:NumSrc) = AstroPSF;
    
    for ISrc = 1:1:NumSrc
        
        AP(ISrc).DataPSF = RotPSF(:,:,ISrc);
    
    end
    
    % inject into the blank tile image all the rotated PSFs at fluxes set to 1: imUtil.art.injectSources
    % (note that injectSources currently works only with odd stamp sizes)
    % [Do we really need an injection by FFT shift? Why can’t we inject directly?] 


    % add sky noise to the tile image + Poisson noise
    
    % add read-out noise to the tile image
    
    % add other noise factors

    % output: a) a native RAW format image 
    % b) a FITS image of an ULTRASAT tile with all the sources PSF + sky noise + read-out noise
    % c) an AstroImage object with filled image, header, and PSF attachments 
    
    
    
    
    
    return
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    % make a grid of BB spectra
    
    NTemp = 5;
    
    TemperGrid = logspace(3.3,4.3,NTemp); % NTemp points from 2000 to 20000 K
    
    SpecBB = AstSpec.blackbody(TemperGrid,UP.wavelength); % erg s(-1) cm(-2) A(-1)
                 
    %
    
    Nwave = size(UP.wavelength,1);
    Nrad  = size(UP.Rdeg,2);
    
    Spec0 = zeros(Nwave,1);
    Spec  = zeros((Nwave-1)/100+1, Nrad, NTemp);
    
    for It = 1:1:NTemp
        
        for Ir = 1:1:Nrad
        
        % convolve each spectrum with transmission for a defined radius
   
        Spec0(:) = SpecBB(It).Int .* UP.TotT(:,Ir);
        
            for j = 1:1:size(Spec,1)
                
               Sum = 0;
               for k = 1:1:10
                   Ind = min( 100*(j-1) + k , size(Spec0,1) );
                   Sum = Sum + Spec0(Ind); 
               end
               Spec(j,Ir,It) = Sum/10; % erg s(-1) cm(-2) A(-1)
               
            end
            
        end
        
    end
      
    save('BBcounts.mat', 'TemperGrid', 'Spec', '-v7.3');
    
   
    StampSize = [5 5];    % what is the real stamp size?
    
    % for each of the sources make its own position-dependent PSF 
    % weighted with source spectra and with the effective area
       
    for Isrc = 1:1:NumSrc 
        
       SimPSF(Isrc) = AstroPSF;
       
       Rad_pix = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ); % in pix
       Rad_deg = Rad_pix * PixSize; % in deg
       
       SigmaPSF = [2 2 0];
       
       SimPSF(Isrc).DataPSF = imUtil.kernel2.gauss(SigmaPSF, StampSize);
       
       % make a 3D array for imUtil.art.injectSources
       VecPSF(:,:,Isrc) = SimPSF(Isrc).DataPSF; 
       
       % check flux conservation
       if abs( sum(SimPSF(Isrc).DataPSF,'all') - 1 ) > Eps 
           fprintf('Warning: a PSF is not normalized to 1\n');
       end
       
    end
        
    % inject sources from the catalog into the image
    
    Image1 = imUtil.art.injectSources(Image0,Cat,VecPSF); 
    
    % add some Poisson noise
    
    if (Noise.P)
        Image = Image1 + imnoise(Image1,'poisson'); %add Poisson noise 
    else
        Image = Image1; 
    end
        
    % make a new AstroImage with sources injected and write it to FITS file
    
    usimImage = AstroImage( {Image} ,'Back',{Emptybox}, 'Var',{Emptybox});
    imUtil.util.fits.fitswrite(usimImage.Image,'!/home/sasha/SimImage.fits');
    
    
    % some test outputs
    
    % show the PSF and the injected source images:
    
    % SimPSF.DataPSF
    % Image1(2019:2025,2000:2006)
    % Image1(2515:2521,2540:2546)
    
    % make a difference image and output it to a FITS file
    
    DiffImage = Image1 - Image0;
    DiffAI = AstroImage( {DiffImage} ,'Back',{Emptybox}, 'Var',{Emptybox});
    imUtil.util.fits.fitswrite(DiffAI.Image,'!/home/sasha/SimDiffImage.fits');
    
    % check "flux conservation":
    
    AllSrcFlux_Image = sum(DiffImage,'all');
    AllSrcFlux_Cat = sum(Cat);
    if abs( AllSrcFlux_Image - AllSrcFlux_Cat(3) ) > Eps 
        fprintf('Warning: source flux not conserved\n');
    end
               
 end
