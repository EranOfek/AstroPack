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
        
        Args.Cat        =  'GALEX'; % 'HSC': Subaru HSC HDF data from a 10'x10' region (7215 obj.)
                                    % 'GALEX': Source distribution measured 
                                    %          by GALEX (Bianchi et al. 2007, ApJS, 173, 659)
        Args.OutDir     =  '.'  ;   % output directory
        
    end
    
    %%%%% ULTRASAT parameters
    
    ImageSizeX = 4738;
    ImageSizeY = 4738;
    PixSize    =  5.4; % pixel size (arcsec)
    
    STile = PixSize^2 * ImageSizeX * ImageSizeY / (3600^2); % tile size in [deg]
    
    Nwave   = 91; % lab PSF grid points in wavelength
         
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    Wave    = linspace(MinWave,MaxWave,Nwave);
    
    
    switch Args.Cat
        
        case 'HSC'
            
            %%%%%%%%% modeling Subaru HSC HDF data from a 10'x10' region (7215 obj.)
            %%%%%%%%% with g = 17.8 -- 26.0 


            DataFile = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/subaru_hsc_udf.csv');  
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

                Cat(Isrc,1)    = floor( 1000 + 111 * rand );  % put the sources into a 111 x 111 pix ~ 10' x 10' box
                Cat(Isrc,2)    = floor( 1000 + 111 * rand );

                Mag_G(Isrc)    = ObjList(2,Isrc);

                ColorT(Isrc)   = min(1e5, astro.spec.color2temp(ObjList(3,Isrc), 'g', 'r', 'SDSS', 'AB') ); 

                Spec(Isrc,:)   = AstroSpec.blackBody(Wave', ColorT(Isrc) );

            end

            % run the simulation 

            simImage = ultrasat.usim('InCat',Cat,'InMag',Mag_G,'InMagFilt',{'SDSS','g'},...
                         'InSpec',Spec,'Exposure',[3 300],'OutDir',Args.OutDir);
                 
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

            simImage = ultrasat.usim('InCat',Cat,'InMag',Mag,'InSpec',Spec,'Exposure',[3 300],'OutDir',Args.OutDir);
        
        otherwise
            
            cprintf('err','Incorrect input catalog in simulate_ULTRASAT_image\n');
            
    end
    
end
