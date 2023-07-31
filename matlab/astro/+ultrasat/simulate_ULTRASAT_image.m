function simImage = simulate_ULTRASAT_image (Args)
    % Simulate with ultrasat.usim a realistic distribution of sky sources 
    % NB: the size of the actuall modelled region should not be smaller
    % than 0.5 x 0.5 deg, otherwise we will take only the brightest part of
    % the source distribution
    % Input: -
    %           
    % Output : - Image: a 2D array containing the resulting source image                 
    %            
    % Author : A. Krassilchtchikov et al. Mar 2023
    % Example: Image = simulate_ULTRASAT_image('ExpNum', 30, 'OutDir', '/home/sasha/', 'Same', true)
    
    arguments    
        Args.Size       = 0.5;            % [deg] size of the modelled FOV 
        Args.X0         = 1e-6;           % [deg] the lower left corner of the modelled square region
        Args.Y0         = 1e-6;           % [deg] the lower left corner of the modelled square region
        Args.Shift      = [];             % catalog shift (in pix)
        Args.Rot        = [];             % catalog rotation (in deg)
        Args.ExpNum     = 3;              % number of standard 300 s exposures
        Args.Same       = 0;              % read in a source distribution or generate a random new one
        Args.Distfile   = 'fitted_distr_cat.mat'; % if Same=1, read the input distribution from this file
        Args.OutDir     =  '.';           % output directory
    end
    
    %%%%% ULTRASAT parameters
    
    PixSize    = 5.44;       % pixel size (arcsec)
    Wave       = 2000:11000; % the wavelength band in A
                     
    if ~Args.Same % model a new distribution
        
        MagL = 13; MagH = 26; Delta_m = 0.2; % the distribution grid in NUV Mag (from GALEX)       
        MagBins = (MagH - MagL) / Delta_m;
        
        Mag       = zeros(MagBins,1);
        SrcDist   = zeros(MagBins,1);
        
        for iMag = 1:1:MagBins
            
            Mag(iMag)  = MagL + (iMag - 1) * Delta_m;
            
            SrcDeg     = 10.^( 0.35 * Mag(iMag) - 4.9 );   % per 1 deg^2 fitted from the GALEX data
            SrcDist(iMag) = ceil( SrcDeg * Args.Size^2 );  % rescaled for Args.Size^2 
            
        end
        
        NumSrc = sum(SrcDist,'all');
        
        Cat    = zeros(NumSrc,2);
        MagUS  = zeros(NumSrc,1);
        FiltUS = repmat({ },1,NumSrc);
%         
%         S(1,:) = AstroSpec.blackBody(Wave',3500).Flux; % appears to slow!
%         S(2,:) = AstroSpec.blackBody(Wave',5800).Flux;
%         S(3,:) = AstroSpec.blackBody(Wave',20000).Flux;
%         
        S(1) = AstroSpec.blackBody(Wave',3500);
        S(2) = AstroSpec.blackBody(Wave',5800);
        S(3) = AstroSpec.blackBody(Wave',20000);
        
        % read the relation of NUV magnitudes and ULTRASAT magnitudes 
        % for a given set of source spectra (usually, just the 3 BB temperatures)  
        io.files.load1('/home/sasha/magu.mat'); % variables: MagU (3D), Temp, MagNUV, Rad  
             
        X0 = ceil(Args.X0 * 3600 / PixSize); % left corner of the modelled square region
        Y0 = ceil(Args.Y0 * 3600 / PixSize); % left corner of the modelled square region        
        Range = Args.Size * 3600 / PixSize;  % X and Y size of the modelled square region
        
        if isempty(Args.Shift) && isempty(Args.Rot) % shift and rotation not given, create and save a new catalog
        
            rng('shuffle');                      % reseed the random number generator
            Cat(:,1) = X0 + Range * rand(NumSrc,1);
            rng('shuffle');                      % reseed the random number generator
            Cat(:,2) = Y0 + Range * rand(NumSrc,1); 

            save('cat0.mat','Cat');

        else  % read, shift and/or rotate the existing catalog
            
            io.files.load1('cat0.mat');
            
            if ~isempty(Args.Shift)
                Cat(:,1) = Cat(:,1) + Shift(1); % X shift
                Cat(:,2) = Cat(:,2) + Shift(2); % Y shift
                fprintf('Source catalog shifted by %d x %d pixels\n',Shift(1),Shift(2));
            end
            
            if ~isempty(Args.Rot)     
                Alpha = Args.Rot * (pi/180.);
                Cat(:,1) = Cat(:,1) * cos(Alpha) - Cat(:,2) * sin(Alpha);
                Cat(:,2) = Cat(:,1) * sin(Alpha) + Cat(:,2) * cos(Alpha);
                fprintf('Source catalog rotated by %d degrees\n',Alpha);
            end

        end
        
        Isrc = 0; 
        for iMag = 1:1:MagBins 
            
            for jSrc = 1:1:SrcDist(iMag)
                
                Isrc = Isrc + 1;
                
                RadSrc = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) * (PixSize/3600); %deg
                [~, IndR] = min( abs(RadSrc - Rad) ); % search for the nearest node
                RXX = sprintf('R%d',IndR); FiltUS{Isrc} = RXX; % fill in the appropriate filter
                
                % divide the population into 3 colours: 
                IndT = rem(Isrc,3) + 1; Spec(Isrc,:) = S(IndT);
                
                [~, IndM] = min( abs( Mag(iMag) - MagNUV ) ); % search for the nearest magnitude (from the magu.mat grid)
                
                MagUS(Isrc) = MagU( IndT, IndM, IndR );
                
            end
            
        end
        
        % save the catalog for the case you wish to repeat the same setup
        % (for large simulations this can be way too voluminous)
        
%         save('fitted_distr_cat.mat', 'Cat','MagUS', 'FiltUS', 'Spec', '-v7.3');
        
    else
        % read in the catalog, magnitudes, and spectra to be modelled       
        DataFile = sprintf('%s%s',tools.os.getAstroPackPath,Args.Distfile);
        io.files.load1(DataFile);  % load Cat, MagUS, FiltUS, Spec,
        
    end
    
    simImage = ultrasat.usim_dev('Cat',Cat,'MaxNumSrc',10000,'Mag',MagUS,'Filt',FiltUS, ...
               'Spec',Spec,'Exposure',[Args.ExpNum 300],'FiltFam',{'ULTRASAT'},'OutDir',Args.OutDir);
    
end
