function simImage = simulateGALEXdistr ()
    % Simulate with ultrasat.usim a distribution of sources taken from Bianchi et al. 2007, ApJS, 173, 659     
    % Package: ultrasat
    % Description: Simulate with ultrasat.usim a distribution of sources taken from Bianchi et al. 2007, ApJS, 173, 659
    %   
    %          
    % Output : - Image: a 2D array containing the resulting source image 
    %                   
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Feb 2023
    % Example: Image = umergeTileImages (Args)
    %               
    
    % data from Bianchi et al.
    
    SrcDens = zeros(22,1); % GALEX NUV source density per square deg. 
    
    SrcDens(12) = 0.07;
    SrcDens(13) = 0.56;
    SrcDens(14) = 1.7;
    SrcDens(15) = 3.6;
    SrcDens(16) = 7.1;
    SrcDens(17) = 15;
    SrcDens(18) = 29;
    SrcDens(19) = 59;
    SrcDens(20) = 130;
    SrcDens(21) = 320;
    SrcDens(22) = 1000;
    
    % ULTRASAT parameters
    
    ImageSizeX = 4738;
    ImageSizeY = 4738;
    PixSize    =  5.4; % pixel size (arcsec)
    
    STile = PixSize^2 * ImageSizeX * ImageSizeY / (3600^2); % tile size in [deg]
    
    Nwave   = 91; % lab PSF grid points in wavelength
         
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    Wave    = linspace(MinWave,MaxWave,Nwave);
    
    %
    
    SrcNum = ceil( SrcDens * STile );
    
    NumSrc = sum(SrcNum,'all');
    
    Cat = zeros(NumSrc,NumSrc);
    Mag = zeros(NumSrc,1);
    
    Isrc = 0;
    
    for Imag = 12:1:22
        
        for Inum = 1:1:SrcNum(Imag)
            
            Isrc = Isrc + 1;
            
            Cat(Isrc,1) = max(1, floor( ImageSizeX * rand )); 
            Cat(Isrc,2) = max(1, floor( ImageSizeY * rand ));
            
            Mag(Isrc)   = Imag;
            
            if      rem(Isrc,3) == 1
                Spec(Isrc,:) = AstroSpec.blackBody(Wave',3500);
            elseif  rem(Isrc,3) == 2
                Spec(Isrc,:) = AstroSpec.blackBody(Wave',5800);
            else
                Spec(Isrc,:) = AstroSpec.blackBody(Wave',20000);
            end
            
        end
        
    end
    
    
    simImage = ultrasat.usim('InCat',Cat,'InMag',Mag,'InSpec',Spec,'Exposure',900);
    
end
