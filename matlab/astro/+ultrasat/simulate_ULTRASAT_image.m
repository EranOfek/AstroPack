function simImage = simulate_ULTRASAT_image (Args)
    % Simulate with ultrasat.usim a realistic distribution of sky sources 
    % Input: -
    %           
    % Output : - Image: a 2D array containing the resulting source image                 
    %            
    % By : A. Krassilchtchikov et al. Mar 2023
    % Example: Image = simulate_ULTRASAT_image('ExpNum', 30, 'OutDir', '/home/sasha/', 'Same', true)
    
    arguments    
        Args.Size       = 0.5;            % [deg] size of the modelled FOV 
        Args.ExpNum     = 3;              % number of standard 300 s exposures
        Args.Same       = 0;              % read in a source distribution or generate a random new one
        Args.Distfile   = '/../data/ULTRASAT/fitted_distr8.44deg_cat.mat'; % if Same=1, read the input distr. from this file
        Args.OutDir     =  '.'  ;         % output directory
    end
    
    %%%%% ULTRASAT parameters
    
    ImageSizeX = 4738;
    ImageSizeY = 4738;
    PixSize    = 5.44; % pixel size (arcsec)
    
    STile = PixSize^2 * ImageSizeX * ImageSizeY / (3600^2); % tile size in [deg]
    
    Nwave   =  9001; %          
    MinWave =  2000; % [A] the band boundaries
    MaxWave = 11000; % [A]
    Wave    = linspace(MinWave,MaxWave,Nwave);
            
            
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
        
        Cat   = zeros(NumSrc,2);
        MagUS = zeros(NumSrc,1);
        IndR  = zeros(NumSrc,1);
        Filt  = repmat({ },1,NumSrc);
        
        S(1) = AstroSpec.blackBody(Wave',3500);
        S(2) = AstroSpec.blackBody(Wave',5800);
        S(3) = AstroSpec.blackBody(Wave',20000);
        
        io.files.load1('/home/sasha/magu.mat');
        
        Isrc = 0;
        
        for iMag = 1:1:MagBins
            
            rng('shuffle');   % reseed the random number generator
            
            for jSrc = 1:1:SrcDist(iMag)
                
                Isrc = Isrc +1;
                
                % Cat(Isrc,1) = 1800 + 333 * rand ; % about 4.22 deg from the corner
                % Cat(Isrc,2) = 1800 + 333 * rand ; % hence using ULTRASAT R11 filter
                
                % Cat(Isrc,1) =   30 + 333 * rand ; % about 0.42 deg from the corner
                % Cat(Isrc,2) =   30 + 333 * rand ; % hence using ULTRASAT R2 filter
                %
                % Cat(Isrc,1) = 3783 + 333 * rand; % about 8.44 deg from the corner
                % Cat(Isrc,2) = 3783 + 333 * rand; % hence using ULTRASAT R21 filter
                
                % NB: the range of pixel positions should comply to the Args.Size:
                Range = Args.Size * 3600 / PixSize;
                Cat(Isrc,1) = 1 + Range * rand; 
                Cat(Isrc,2) = 1 + Range * rand; 
                
                RadSrc = sqrt( Cat(Isrc,1)^2 + Cat(Isrc,2)^2 ) * (PixSize/3600); %deg
                [~, IndR(Isrc)] = min( abs(RadSrc - Rad) ); % search for the nearest node
                
                RXX        = sprintf('R%d',IndR(Isrc));
                Filt{Isrc} = RXX;
                
                % divide the population into 3 colours
                
                IndT = rem(Isrc,3) + 1;
                
                Spec(Isrc,:) = S( IndT );
                
                % recalculate the magnitudes into the ULTRASAT R system
                
                %                         Sp         = scaleSynphot(Spec(Isrc,:), Mag(iMag), 'GALEX', 'NUV');
                %                         MagU(Isrc) = astro.spec.synthetic_phot([Wave', Sp.Flux],'ULTRASAT','R21','AB');
                
                [~, IndM] = min( abs( Mag(iMag) - MagNUV ) ); % search for the nearest
                
                MagUS(Isrc) = MagU( IndT, IndM, IndR(Isrc) );
                
            end
            
        end
        
        % save the catalog for the case you wish to repeat the same setup
        
        save('fitted_distr_cat.mat', 'Cat','MagUS', 'Spec', 'Filt','-v7.3');
        
    else
        % read in the catalog and spectra to be modelled
        
        DataFile = sprintf('%s%s',tools.os.getAstroPackPath,Args.Distfile);
        io.files.load1(DataFile);  % load Cat, MagUS, Spec, Filt
        
    end
    
    simImage = ultrasat.usim_dev('InCat',Cat,'MaxNumSrc',10000,'InMag',MagUS,'InSpec',Spec,...
               'Exposure',[Args.ExpNum 300],'FiltFam',{'ULTRASAT'},'Filt',Filt,'OutDir',Args.OutDir);
    
end
