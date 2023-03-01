function [Image, JPSF] = injectArtSrc (X, Y, CPS, SizeX, SizeY, PSF, Args)
    % Make an artificial image with rotated and jitter-blurred source PSFs injected to the catalog postions     
    % Package: imUtil.art
    % Description: Make an artificial image with rotated and jitter-blurred source PSFs injected to the catalog postions
    %          - X, Y, CPS      : pixel coordinates and countrates of the sources
    %          - SizeX, SizeY   : pixel sizes of the image containing the source PSFs
    %          - PSF            : either a single 2D PSF for all the object or 
    %                             a 3D array of individual PSFs
    %          - Args.PSFScaling: Image pixel size / PSF pixel size ratio
    %          - Args.RotatePSF : PSF rotation angle, either a single value
    %                             for all the sources or a vector of angles
    %          - Args.Jitter    : apply PSF blurring due to the S/C jitter  
    %          - Args.Method    : source injection method, either 'direct' 
    %                             or 'PSFshift'
    %          
    % Output : - Image: a 2D array containing the resulting source image 
    %                   and a 2+1 D array of rotated and jittered source PSFs
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: [Image, JPSF] = injectArtSrc (X, Y, CPS, SizeX, SizeY, PSF,...
    %                                        'PSFScaling',5,'RotatePSF',-90,'Jitter',1);

    arguments
        
        X
        Y
        CPS
        SizeX
        SizeY
        PSF 
        Args.PSFScaling     =    1;       % Image/PSF pixel size ratio
        Args.RotatePSF      =    0;       % PSF stamp rotation angle
        Args.Jitter         =    0;       % PSF blurring due to the S/C jitter
        Args.Method         =   'direct'; % injection method
                                          % 'direct' or 'FFTShift'
%         Args.OutPut         =   'all';    % 'image' - image only; 
%                                           'all'   - image + rotated and rescaled PSFs        
    end

    % create an impty image of the given size

    Image0 = zeros(SizeX, SizeY);

    % get the number of sources and produce a source "catalog" array

    NumSrc = size(CPS,1);
    
    Cat = [X Y CPS];

    % consistency checks

    if size(X,1) ~= size(Y,1) || size(X,1) ~= NumSrc
        fprintf('Input sizes inconsistent in injectArtSrc, exiting..');
        return
    end

    % rotate the PSFs (if needed)  
    %
    % the rotation does not conserve the flux, thus also need to renormalize
    % NB: the actual size of rotated PSF stamp depends on the particular rotation angle,
    % varying between Nx x Ny and sqrt(2) * Nx x sqrt(2) * Ny

    if size(Args.RotatePSF,1) == NumSrc % an individual angle for each source
        for Isrc = 1:1:NumSrc
            RotPSF(:,:,Isrc) = imrotate(PSF(:,:,Isrc), Args.RotatePSF(Isrc), 'bilinear', 'loose'); 
            RotPSF(:,:,Isrc) = RotPSF(:,:,Isrc) / sum ( RotPSF(:,:,Isrc), 'all' ); % rescale
        end
    elseif abs( Args.RotatePSF ) < 1 || abs( Args.RotatePSF - 360) < 1 % do nothing for small angles
        RotPSF = PSF;
    else                                % rotate all the PSFs by the same angle
        for Isrc = 1:1:NumSrc
            RotPSF(:,:,Isrc) = imrotate(PSF(:,:,Isrc), Args.RotatePSF(1), 'bilinear', 'loose'); 
            RotPSF(:,:,Isrc) = RotPSF(:,:,Isrc) / sum ( RotPSF(:,:,Isrc), 'all' ); % rescale
        end
    end
                
    % apply PSF blurring due to the S/C jitter
    
    if Args.Jitter
        JPSF = ultrasat.jitter(RotPSF, Cat, 'Exposure',300,'SigmaX0',2.,'SigmaY0',2.,...
                               'Rotation',10,'Scaling',Args.PSFScaling);    
    else
        JPSF = RotPSF;
    end
    
    % StampSize  = size(JPSF);   
    % fprintf('%s%4.1f%s\n','Final PSF stamp size ', StampSize / Args.PSFScaling , ' image pixels');
    
    % test PSF containment width and pseudoFWHM width
    
    ContWidth  = zeros(NumSrc,1);  % radius of the encircled flux PSF region
    PseudoFWHM = zeros(NumSrc,1);  % pseudo FWHM of the PSFs (see imUtil.psf.pseudoFWHM for the particular algorithm)
    
    for Isrc = 1:1:NumSrc
        
        ContWidth(Isrc) = imUtil.psf.containment('PSF',JPSF(:,:,Isrc),'Level',0.5);
        
        [ widthX, widthY ] = ... 
                    imUtil.psf.pseudoFWHM('PSF',JPSF(:,:,Isrc),'Level',0.5);

        PseudoFWHM(Isrc) = sqrt ( widthX^2 + widthY^2 );
        
    end
    
    ContWidth   = ContWidth  / Args.PSFScaling ;  % convert to image pixel size    
    PseudoFWHM  = PseudoFWHM / Args.PSFScaling ;  % convert to image pixel size
   
    % some visual tests
    
%       figure(2); plot(sqrt(X.^2+Y.^2).*5.4./3600, ContWidth * 5.4,'*'); % 5.4 arcsec pixel size for ULTRASAT
%       xlabel('Radius, deg'); ylabel('50% encirclement radius, arcsec')
%     
%       figure(3); plot(sqrt(X.^2+Y.^2).*5.4./3600, PseudoFWHM * 5.4,'*'); 
%       xlabel('Radius, deg'); ylabel('pseudoFWHM, arcsec')
   
    % PSF injection: inject all the rotated source PSFs into the blank image 
  
    switch lower(Args.Method)
        
        case 'fftshift'
                   
            if rem( size(JPSF,1) , 2) == 0 
                fprintf('The size of RotPSF is even, while imUtil.art.injectSources accepts odd size only! Exiting..');
                return
            end
    
            Image = imUtil.art.injectSources(Image0,Cat,JPSF); 

        case 'direct'
                  
            Image = imUtil.art.directInjectSources(Image0,Cat,Args.PSFScaling,JPSF);
    
        otherwise
        
        fprintf('Injection method not defined! Exiting..\n');
        return
        
    end

   
end