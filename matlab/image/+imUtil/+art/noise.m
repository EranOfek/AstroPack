function Image = noise (ImageSrc, Args)
    % Add various types of noise to an artificial image made from a source catalog
    % Package: imUtil.art
    % Description: Add various types of noise to an artificial image made from a source catalog  
    %          - ImageSrc: inital noiseless image containing only source PSFs [counts/s]
    %          - Args.Exposure: exposure in [s]
    %          - Args.ExposureNum: number of exposures in a sequence
    %          - Args.Dark: dark counts 
    %          - Args.Sky:  sky background (position-dependent?)
    %          - Args.Possion: Poisson noise
    %          - Args.ReadOut: read-out noise 
    % Output : - Image: image with all the said noise components
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: ImageSrcNoise = imUtil.art.noise(ImageSrc,'Exposure',300,'Dark',1,'Sky',0,...
    %                                           'Jitter',0,'Poisson',1,'ReadOut',0);
    
    arguments
        
        ImageSrc
        
        Args.Exposure    =  300; % exposure time [s]
        
        Args.ExposureNum =    1;   % number of exposures in a sequence
        
        Args.Dark        =    1;   % dark counts           
        
        Args.Sky         =    1;   % sky background (position-dependent?)
               
        Args.Poisson     =    1;   % Poisson noise
        
        Args.ReadOut     =    1;   % Read-out noise 
        
    end
    
    % get the image size
    
    Nx = size(ImageSrc,1);
    Ny = size(ImageSrc,2);
    
    DarkCounts   = zeros(Nx,Ny);
    SkyBckg      = zeros(Nx,Ny);
    ReadOutNoise = zeros(Nx,Ny);
    
    % calculate the total exposure
    
    Exposure = Args.Exposure * Args.ExposureNum;
    
    % make the exposure-integrated image:
    
    Image = ImageSrc .* Exposure;  % [counts/s] * [s] = [counts]
    
% In Yossi's presentation the variance of all the background
% components: Zodiac + Cherenkov + Stray ligh + Dark current + ReadOut is estimated as 75 e-/pix (for a 300 s exposure?)
% so in principle we could model it all as max( 0, normrnd(0,75,Nx,Ny) ) ??

    % add dark counts and sky background:
    
    if Args.Dark
        
        DarkCounts = poissrnd(1.5,Nx,Ny); 
        % simulate dark noise < 7.8 e-/pix (300*0.026 e-/pix/s) for a 300 ks exposure 
        % visual test: rr = DarkCounts > 8; imagesc(rr)
        
    end
    
    if Args.Sky 
            
    % search for the spectrum in the literature, convolve with the
    % throughput, multiply by the exposure, then make a poisson distribution of it
        
%         SkyBckg = Sky + Zodiac + Cherenkov ; % correct for Args.Exposure ?
          
        SkyBckg = poissrnd(SkyBckg);
        
    end
    
    Image = Image + DarkCounts + SkyBckg;

    % apply the Poisson noise
      
    if Args.Poisson 
        
        Image = poissrnd(Image); 
        
    end

    % add read-out noise:
    
    if Args.ReadOut
        
        RdNsigma = 3.5/3; % is the < 3.5 e-/pix specification for the high gain is a 3 sigma limit
        
        ReadOutNoise = max( 0, normrnd(0,RdNsigma,Nx,Ny) ) .* Args.ExposureNum; 
        
    end
    
    Image = Image + ReadOutNoise; 
                                 
end 