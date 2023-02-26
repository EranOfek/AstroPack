function Image = noise (ImageSrc, Args)
    % Add various types of noise to an artificial image made from a source catalog
    % Package: imUtil.art
    % Description: Add various types of noise to an artificial image made from a source catalog  
    %          - ImageSrc: inital noiseless image containing only source PSFs [counts/s]
    %          - Args.Exposure: exposure in [s]
    %          - Args.Dark: dark counts (position-dependent?)
    %          - Args.Sky:  sky background (position-dependent?)
    %          - Args.Jitter: blurring due to S/C jitter
    %          - Args.Possion: Poisson noise
    %          - Args.ReadOut: read-out noise (position-dependent?)
    % Output : - Image: image with all the said noise components
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: ImageSrcNoise = imUtil.art.noise(ImageSrc,'Exposure',300,'Dark',1,'Sky',0,...
    %                                           'Jitter',0,'Poisson',1,'ReadOut',0);
    
    arguments
        
        ImageSrc
        
        Args.Exposure  =    1;   % exposure time [s]
        
        Args.Dark      =    0;   % dark counts (position-dependent?)           
        
        Args.Sky       =    0;   % sky background (position-dependent?)
        
        Args.Jitter    =    0;   % blurring due to S/C jitter
        
        Args.Poisson   =    1;   % Poisson noise
        
        Args.ReadOut   =    0;   % Read-out noise (position-dependent?)
        
    end
    
    % get the image size
    
    Nx = size(ImageSrc,1);
    Ny = size(ImageSrc,2);
    
    DarkCounts   = zeros(Nx,Ny);
    SkyBckg      = zeros(Nx,Ny);
    ReadOutNoise = zeros(Nx,Ny);
    
    % make the exposure-integrated image:
    
    Image = ImageSrc .* Args.Exposure;  % [counts/s] * [s] = [counts]
    
    % add dark counts and sky background:
    
    if Args.Dark
        
%         DarkCounts = imnoise(DarkCounts,'gaussian', 1, 2); % multiply by Args.Exposure ?
        
    end
    
    if Args.Sky
        
%         SkyBckg = ; % multiply by Args.Exposure ?

    end
    
    Image = Image + DarkCounts + SkyBckg;

    % apply the S/C jitter  
           
    Image = ultrasat.jitter(Image,'Exposure',300,'SigmaX0',2.,'SigmaY0',2.,'Rotation',10);
    
    %  WARNING! ultrasat.jitter is NOT fully CODED YET!
    
    % apply the Poisson noise
      
    if Args.Poisson 
        
        Image = poissrnd(Image); 
        
    end

    % add read-out noise:
    
    if Args.ReadOut
        
%         ReadOutNoise = ; % multiply by Args.Exposure ?
        
    end
    
    Image = Image + ReadOutNoise;
                                 
end