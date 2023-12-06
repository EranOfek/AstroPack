function JPSF = jitter (PSF, Cat, Args)
    % Blur an array of ULTRASAT PSFs due to the S/C jitter
    % Package: ultrasat
    % Description: Blur an array of ULTRASAT PSFs due to the S/C jitter
    % Input:   - PSF: an initial array of PSF images to be blurred
    %          - Cat: an array of source X, Y coordinates on a detector tile
    %          * ...,key,val,...
    %          'Exposure' - exposure in [s]
    %          'SigmaX0'  - S/C jittering in the X direction
    %          'SigmaY0'  - S/C jittering in the Y direction
    %          'Rotation' - S/C rotation jittering angle
    %          'Scaling'  - ULTRASAT pixel size / PSF pixel size
    % Output : - JPSF: a PSF image (2D array) blurred due to S/C jitter
    %            
    % Tested : Matlab R2020b
    % Author : A. Krassilchtchikov et al. (Feb 2023)
    % Example: JPSF = ultrasat.jitter(PSF, Cat, ...
    %                 'Exposure',300,'SigmaX0',2.,'SigmaY0',2.,'Rotation',10);
    
    arguments
        
        PSF
        
        Cat
        
        Args.Exposure   =   300;  % [s]
        
        Args.SigmaX0    =     2;  % arcsecond
        
        Args.SigmaY0    =     2;  % arcsecond
        
        Args.Rotation   =    10;  % arcsecond
        
        Args.Scaling    =     1;  % PSF pixel = ULTRASAT pixels
        
        
    end
    
    % ULTRASAT parameters
    
    PixSize   = 5.44;                   % [arcsec]
    Exposure0 =  300;                   % [s] a nominal ULTRASAT exposure
    
    Q = Args.Exposure / Exposure0;      % is it indeed linear ??
    
    X0Jitt    = Args.Scaling * Args.SigmaX0 / PixSize;  % convert arcsec to PSF pixels
    Y0Jitt    = Args.Scaling * Args.SigmaY0 / PixSize;  % convert arcsec to PSF pixels
    AlphaJitt = Args.Scaling * Args.Rotation * pi / (3600 * 180);  % 
    
    % PSF blurring
    
    NSrc = size(Cat,1);                 % determine the number of sources
    
    if size(PSF,3) ~= NSrc
        frprintf('The numbers of sources and PSFs in ultrasat.jitter do not match, exiting..');
        return
    end
    
    for Isrc = 1:1:NSrc
        
        SigmaX = sqrt( X0Jitt^2 + ( Cat(Isrc,2) * AlphaJitt)^2 ) * Q;
        SigmaY = sqrt( Y0Jitt^2 + ( Cat(Isrc,1) * AlphaJitt)^2 ) * Q;
        JPSF(:,:,Isrc) = imgaussfilt( PSF(:,:,Isrc), [SigmaX SigmaY]' );
        
        % do not need to renormalize, imgaussfilt conserves the flux
        % JPSF(:,:,Isrc) = JPSF(:,:,Isrc) / sum( JPSF(:,:,Isrc), 'all');
        
    end
    
end