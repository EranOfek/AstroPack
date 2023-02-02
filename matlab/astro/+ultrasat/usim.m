function usimImage =  usim ( Cat, Noise )

    % Make simulated ULTRASAT images from source catalogs
    %
    % Input:    
    %       -  
    %       -  Cat (a catalog of simulated sources)
    %       -  Noise (noise parameters)
    %
    % Output:
    %       -  usimImage (simulated AstroImage object and FITS file output)

    arguments
        
        Cat        = [1 1 0]; % no sources, nothing to do

        Noise.P    = 1;       % add Poisson noise on the image with sources ON/OFF
        
    end
    
    Eps = 1e-12;        % precision

    ImageSizeX  = 4738; % actual U image size (1 tile)
    ImageSizeY  = 4738; % actual U image size (1 tile)
    PixSize     = 5.4/3600; % pixel size in degrees

    Emptybox = zeros(ImageSizeX,ImageSizeY);
    Image0   = imnoise(Emptybox,'gaussian', 1, 2);
        
    % make a source-less AstroImage and write it to FITS file
    SimImage0 = AstroImage( {Image0} ,'Back',{Emptybox}, 'Var',{Emptybox});
    imUtil.util.fits.fitswrite(SimImage0.Image,'!/home/sasha/SimImage0.fits');

    % test catalog (later use an input AstroCatalog)
    Cat = [2003 2022  125;
           2543 2518  134;
           1543 1518  334;
           2612 2886  238];

    % determine number of sources in the catalog
    NumSrc = size(Cat,1);
    
    % load the matlab object with the ULTRASAT data:
    %
    % load /home/sasha/Downloads/P90_UP_test_60_ZP_Var_Cern_21.mat
    
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
