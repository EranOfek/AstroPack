function simImage = simulate_ULTRASAT_image (Args)
    % Simulate with ultrasat.usim a distribution of sources from certain catalogues       
    % Package: ultrasat
    % Description: Simulate with ultrasat.usim a distribution of sources from certain catalogues  
    %           
    % Output : - Image: a 2D array containing the resulting source image                 
    %            
    % Tested : Matlab R2020b
    %     By : A. Krassilchtchikov et al.    Mar 2023
    % Example: Image = simulateGALEXdistr ('OutDir','/home/sasha/','Cat','GALEX')
    
    arguments
        
        Args.Cat        =  'GALEX_CESAM'; % 'HSC': Subaru HSC HDF data from a 10'x10' region (7215 obj.)
                                          % https://hsc-release.mtk.nao.ac.jp/datasearch/
                                          % 'GALEX': Source distribution measured 
                                          %          by GALEX (Bianchi et al. 2007, ApJS, 173, 659)
                                          % 'GALEX_CESAM': GALEX deep field
                                          % distribution from CESAM http://cesam.lam.fr/galex-emphot/search/criteria
                                          % 
        Args.ExpNum     = 3;              % number of standard 300 s exposures
        Args.Same       = 0;              % read in a source distribution or generate a random new one
        Args.OutDir     =  '.'  ;         % output directory
        
    end
    
    %%%%% ULTRASAT parameters
    
    ImageSizeX = 4738;
    ImageSizeY = 4738;
    PixSize    =  5.4; % pixel size (arcsec)
    
    STile = PixSize^2 * ImageSizeX * ImageSizeY / (3600^2); % tile size in [deg]
    
    Nwave   = 9001;  % 
         
    MinWave =  2000; % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    Wave    = linspace(MinWave,MaxWave,Nwave);
    
    
    switch Args.Cat
        
        case 'HSC'
            
            %%%%%%%%% modeling Subaru HSC HDF data from a 30' x 30' region (~ 130 000 obj.)
            %%%%%%%%% with g < 27.5 (actually, g is in the 16 -- 27.5 range)

            
            DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/subaru_hsc_udf_30x30min_g27.5.mat'); 
            load (DataFile);  % load Mag_G, ColorT, Spec, NumSrc
            
            % make 3 times more objects to fit the GALEX distribution:
            
            NumSrc3 = 3 * NumSrc;
            
            MagG = zeros(NumSrc3,1);
                        
            for Isrc = 1:1:NumSrc3
                
                r = rem(Isrc,NumSrc)+1;
                MagG(Isrc) = Mag_G(r);
                SpecT(Isrc) = Spec(r);
                
            end
            
            %
            
            Cat = zeros(NumSrc3,2);
            
            for Isrc = 1:1:NumSrc3
                
                Cat(Isrc,1)    = floor( 1000 + 333 * rand ); % put the sources into a 333 x 333 pix ~ 30' x 30' box
                Cat(Isrc,2)    = floor( 1000 + 333 * rand );
                
            end
            
            % make a distribution
            
            Mag_R1 = zeros(NumSrc3,1);
            
            for Isrc = 1:1:NumSrc3
                
                SpecScaled  = scaleSynphot(SpecT(Isrc), MagG(Isrc), 'SDSS', 'g');
                Mag_R1(Isrc) = astro.spec.synthetic_phot([Wave', SpecScaled.Flux],'ULTRASAT','R1','AB'); 
                
            end

            % run the simulation 

            simImage = ultrasat.usim('InCat',Cat,'InMag',MagG,'InMagFilt',{'SDSS','g'},...
                         'InSpec',SpecT,'Exposure',[Args.ExpNum 300],'OutDir',Args.OutDir);
        
        case 'HSCslow'
            
            %%%%%%%%% modeling Subaru HSC HDF data from a 30'x30' region (~ 50000 obj.)
            %%%%%%%%% with g = 16 -- 26.0 


            % DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/subaru_hsc_udf_30x30min.csv');  
            DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/subaru_hsc_udf_30x30min_g27.5.csv');
            
            FileID = fopen(DataFile,'r');
                    % skip the first 17 lines in a datafile
                    for Skip = 1:17
                        Empty = fgets(FileID);
                    end
            ObjList = fscanf(FileID,'%f,%f,%f',[3,inf]); 
            fclose(FileID);

            NumSrc = size(ObjList,2);

            Cat = zeros(NumSrc,2);
            Mag = zeros(NumSrc,1);

            fprintf('%s%d\n','NumSrc = ',NumSrc);
            fprintf('%s%3.1f%s%3.1f\n','g = ',min(ObjList(2,:)),'-',max(ObjList(2,:)));

            for Isrc = 1:1:NumSrc

                Cat(Isrc,1)    = floor( 1000 + 333 * rand );  % put the sources into a 333 x 333 pix ~ 30' x 30' box
                Cat(Isrc,2)    = floor( 1000 + 333 * rand );

                Mag_G(Isrc)    = ObjList(2,Isrc);

                ColorT(Isrc)   = min(1e5, astro.spec.color2temp(ObjList(3,Isrc), 'g', 'r', 'SDSS', 'AB') ); 

                Spec(Isrc,:)   = AstroSpec.blackBody(Wave', ColorT(Isrc) );

            end
            
            save('subaru_hsc_udf_30x30min_g27.5.mat','NumSrc','Mag_G','ColorT','Spec','-v7.3'); % save for faster runs

            % run the simulation 

            simImage = ultrasat.usim('InCat',Cat,'InMag',Mag_G,'InMagFilt',{'SDSS','g'},...
                         'InSpec',Spec,'Exposure',[Args.ExpNum 300],'OutDir',Args.OutDir);
                 
        case 'GALEX'
    
            %%%%%%%%%%% modeling GALEX data from Bianchi et al.

            SrcDens = zeros(22,1); % GALEX NUV source density per 1 square deg. 

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

            %

            SrcNum = ceil( SrcDens * STile );

            NumSrc = sum(SrcNum,'all');

            Cat = zeros(NumSrc,2);
            Mag = zeros(NumSrc,1);

            Isrc = 0;

            for Imag = 12:1:22

                for Inum = 1:1:SrcNum(Imag)

                    Isrc = Isrc + 1;

                    Cat(Isrc,1) = max(1, floor( ImageSizeX * rand )); 
                    Cat(Isrc,2) = max(1, floor( ImageSizeY * rand ));

                    Mag(Isrc)   = Imag;

                    % divide the population into 3 colours 

                    if      rem(Isrc,3) == 1
                        Spec(Isrc,:) = AstroSpec.blackBody(Wave',3500);
                    elseif  rem(Isrc,3) == 2
                        Spec(Isrc,:) = AstroSpec.blackBody(Wave',5800);
                    else
                        Spec(Isrc,:) = AstroSpec.blackBody(Wave',20000);
                    end

                end

                
                
                
                
                
                
                
                
                
            end 

            % run the simulation 

            simImage = ultrasat.usim('InCat',Cat,'InMag',Mag,'InSpec',Spec,'Exposure',[Args.ExpNum 300],'OutDir',Args.OutDir);
            
        case 'GALEX_CESAM'
            
            %%%%%%%%%%% modeling GALEX data from the CESAM archive
            
            if ~Args.Same % model a new distribution 
             
                DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/cesam_xmmlss_00_deep_catalog_galex.csv');

                FileID = fopen(DataFile,'r');
                        % skip the first 2 lines in a datafile
                        for Skip = 1:2
                            Empty = fgets(FileID);
                        end
                ObjList = fscanf(FileID,'%f,%f',[2,inf]); 
                fclose(FileID);

                NumSrc = size(ObjList,2); 

                Cat  = zeros(NumSrc,2);
                MagU = zeros(NumSrc,1);
                
                MagNUV = ObjList(2,:);  % the GALEX NUV magnitudes from the catalog

                for Isrc = 1:1:NumSrc

                     Cat(Isrc,1) = floor( 1800 + 333 * rand ); % about 4.2 deg from the corner
                     Cat(Isrc,2) = floor( 1800 + 333 * rand ); % hence using ULTRASAT R11 filter

                    % divide the population into 3 colours 

                        if      rem(Isrc,3) == 1
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',3500);
                        elseif  rem(Isrc,3) == 2
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',5800);
                        else
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',20000);
                        end
                        
                    % recalculate the magnitudes into the ULTRASAT R system
                    
                    S          = scaleSynphot(Spec(Isrc,:), MagNUV(Isrc), 'GALEX', 'NUV');
                    MagU(Isrc) = astro.spec.synthetic_phot([Wave', S.Flux],'ULTRASAT','R11','AB');

                end

                % save the catalog for the case you wish to repeat the same setup

                % save('GALEX_CESAM_cat.mat', 'Cat','MagNUV','MagU', 'Spec','-v7.3');

            else % read in the catalog and spectra to be modelled
                                
                DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/GALEX_CESAM_cat.mat'); 
                load (DataFile);  % load Cat, MagU, MagNUV, Spec 
                
            end
        
            % run the simulation 

            simImage = ultrasat.usim('InCat',Cat,'InMag',MagU,'InSpec',Spec,'Exposure',[Args.ExpNum 300],...
                                     'InMagFilt',{'ULTRASAT','R11'},'OutDir',Args.OutDir);
            
        case 'distribution'
            
            MagL = 13; MagH = 26; Delta_m = 0.2; % the distribution grid in Mag (GALEX NUV)
            
            MagBins = (MagH - MagL) / Delta_m; 
            
            Mag       = zeros(MagBins,1);
            Src30min  = zeros(MagBins,1);
            
            for iMag = 1:1:MagBins
                
                Mag(iMag)  = MagL + (iMag - 1) * Delta_m;
                
                SrcDeg     = 10.^( 0.35 * Mag(iMag) - 4.9 );  % fitted from the GALEX data
                Src30min(iMag) = ceil( SrcDeg / 4 ); 
                
            end
            
            NumSrc = sum(Src30min,'all');
            
            Cat  = zeros(NumSrc,2);
            MagU = zeros(NumSrc,1);
            
            Isrc = 0;

            for iMag = 1:1:MagBins
                
                for jSrc = 1:1:Src30min(iMag)
                    
                     Isrc = Isrc +1;
                    
                     Cat(Isrc,1) = floor( 1800 + 333 * rand ); % about 4.2 deg from the corner
                     Cat(Isrc,2) = floor( 1800 + 333 * rand ); % hence using ULTRASAT R11 filter

                    % divide the population into 3 colours 

                        if      rem(Isrc,3) == 1
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',3500);
                        elseif  rem(Isrc,3) == 2
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',5800);
                        else
                            Spec(Isrc,:) = AstroSpec.blackBody(Wave',20000);
                        end
                        
                    % recalculate the magnitudes into the ULTRASAT R system
                    
                    S          = scaleSynphot(Spec(Isrc,:), Mag(iMag), 'GALEX', 'NUV');
                    MagU(Isrc) = astro.spec.synthetic_phot([Wave', S.Flux],'ULTRASAT','R11','AB');                    
                    
                end
                    
            end

            simImage = ultrasat.usim('InCat',Cat,'InMag',MagU,'InSpec',Spec,'Exposure',[Args.ExpNum 300],...
                                     'InMagFilt',{'ULTRASAT','R11'},'OutDir',Args.OutDir);

            
        otherwise
            
            cprintf('err','Incorrect input catalog in simulate_ULTRASAT_image\n');
            
    end
    
end
