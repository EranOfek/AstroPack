function BlurredImage = jitter (Image, Args)
    % Blur an artificial ULTRASAT image due to the S/C jitter
    % Package: ultrasat
    % Description: Blur an artificial ULTRASAT image due to the S/C jitter
    %          - Image: initial image to be blurred
    %          - Args.Exposure: exposure in [s]
    %          - Args.SigmaX0: S/C jittering in the X direction
    %          - Args.SigmaY0: S/C jittering in the Y direction
    %          - Args.Rotation: S/C rotation jittering angle
    % Output : - BlurredImage 
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: BlurredImage = ultrasat.jitter(Image, ...
    % 'Args.Exposure',300,'Args.SigmaX0',2.,'Args.SigmaY0',2.,'Args.Rotation',10);
    
    arguments
        
        Image
        
        Args.Exposure   =   300;  % [s]
        
        Args.SigmaX0    =     2;  % arcsecond
        
        Args.SigmaY0    =     2;  % arcsecond
        
        Args.Rotation   =    10;  % arcsecond
        
    end
    
    PixSize   = 5.4;                    % [arcsec]
    Exposure0 = 300;                    % [s] a nominal ULTRASAT exposure
    
    Q = Args.Exposure / Exposure0;      % is it indeed linear ??
    
    Nx = size(Image,1);
    Ny = size(Image,2);
    
    JitterSigma = zeros(Nx,Ny,2);
    
    X0Jitt   = Args.SigmaX0 / PixSize;  % convert arcsec to pixels
    Y0Jitt   = Args.SigmaY0 / PixSize;  % convert arcsec to pixels
    AlphaJitt = Args.Rotation * pi / (3600 * 180);  % 
    
    for iX = 1:1:Nx
        for iY = 1:1:Ny
            
            JitterSigma(iX,iY,1) = sqrt( X0Jitt^2 + (iY * AlphaJitt)^2 ) * Q; 
            JitterSigma(iX,iY,2) = sqrt( Y0Jitt^2 + (iX * AlphaJitt)^2 ) * Q;        
            
        end
    end  
    
    BlurredImage = Image; % do nothing by now
    
    % which method to use: 
    % imUtil.filter.conv2d_fast ?
    % imfilter ? 
    % imgaussfilt (only 1 value of sigma_x, sigma_y for the whole image)?
    
end