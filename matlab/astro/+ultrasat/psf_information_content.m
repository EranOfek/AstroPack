function Result = psf_information_content(ImRes)
    % characterize information content loss when using various ULTRASAT PSFs
    
    % ULTRASAT PSF database parameters
    arguments
        
        ImRes = 5  % PSF resolution 
        
    end
               
    Nrad    = 25; 
    Nwave   = 91;
    Rad     = linspace(0,10,Nrad);   % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    WavePSF = linspace(MinWave,MaxWave,Nwave); % lab PSF grid points in wavelength
    
    PixRat  = 47.5; % the ratio of the ULTRASAT pixel size to that of the lab PSF image 

                            fprintf('Reading PSF database.. '); 
    
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
    if size(PSFdata,4) ~= Nrad 
        error('PSF array size mismatch, exiting..');
    end
    
                            fprintf('done\n'); 
                            
    % PSF dependence on the radius for different wavelengths
                            
    IL = zeros(Nrad,Nrad);
    ILbin = zeros(Nrad,Nrad, 4);
    
    Lam = [4 7 16 51];
    
    for ILam = 1:4
    
    for Ir = 1:1:Nrad
    
        M0 = PSFdata(:,:,Lam(ILam),Ir);

        for Rd = 1:1:Nrad

            M1 = PSFdata(:,:,Lam(ILam),Rd);
            IL(Ir, Rd) = imUtil.psf.information_loss(M0, M1) ;
            
            if IL(Ir, Rd) < 0.1
                ILbin(Ir, Rd, ILam) = 0;
            else
                ILbin(Ir, Rd, ILam) = 1;
            end

        end
        
    end

    end
    
    figure(1)
    subplot(2,2,1); imagesc(ILbin(:,:,1)); title('230 nm')
    subplot(2,2,2); imagesc(ILbin(:,:,2)); title('260 nm')
    subplot(2,2,3); imagesc(ILbin(:,:,3)); title('350 nm')
    subplot(2,2,4); imagesc(ILbin(:,:,4)); title('700 nm')
    
    % PSF dependence on the radius for different BB temperatures
    
    S   = 108;
    PSF = zeros(S,S,Nrad,3);
    ILbin = zeros(Nrad,Nrad,3);
    
    io.files.load1('/home/sasha/wpsf_3500.mat'); PSF(:,:,:,1) = WPSF;
    io.files.load1('/home/sasha/wpsf_5800.mat'); PSF(:,:,:,2) = WPSF;
    io.files.load1('/home/sasha/wpsf_20000.mat'); PSF(:,:,:,3) = WPSF;
    
    for T = 1:3
        for i = 1:Nrad
            M0 = PSF(:,:,i,T);
            for j = 1:Nrad 
                M1 = PSF(:,:,j,T);
                IL = imUtil.psf.information_loss(M0, M1);
                if IL > 0.1
                    ILbin(i,j,T) = 1;
                end
            end
        end
    end
    
    figure(2)
%     lab = sprintf('%5.1f',Rad(5:5:end));
%     xticks(1:size(Rad, 2));
%     xticklabels(lab);
    subplot(1,3,1); imagesc(ILbin(:,:,1)); title('3500 K'); 
    subplot(1,3,2); imagesc(ILbin(:,:,2)); title('5800 K')
    subplot(1,3,3); imagesc(ILbin(:,:,3)); title('20000 K')
    
    % PSF dependence on the spectrum for different radii
    
    IL = zeros(Nwave,Nwave);
    ILbin = zeros(Nwave,Nwave, 4);
    
    RadList = [2 11 16 22];
    
    for IRad = 1:4
    
        for Ilam = 1:1:Nwave

            M0 = PSFdata(:,:,Ilam,RadList(IRad));

            for Jlam = 1:1:Nwave

                M1 = PSFdata(:,:,Jlam,RadList(IRad));
                
                IL(Ilam,Jlam) = imUtil.psf.information_loss(M0, M1) ;

                if IL(Ilam,Jlam) < 0.1
                    ILbin(Ilam, Jlam, IRad) = 0;
                else
                    ILbin(Ilam, Jlam, IRad) = 1;
                end

            end

        end

    end
    
    figure(3)
    subplot(2,2,1); imagesc(ILbin(:,:,1)); title('0.42 deg')
    subplot(2,2,2); imagesc(ILbin(:,:,2)); title('4.17 deg')
    subplot(2,2,3); imagesc(ILbin(:,:,3)); title('6.25 deg')
    subplot(2,2,4); imagesc(ILbin(:,:,4)); title('8.75 deg')
    
    % PSF dependence on the spectrum temperature
    S   = 108;
    NTemp = 4;
    NRad  = 4;
    PSF = zeros(S,S,NTemp,NRad);
    IL = zeros(NTemp,NTemp,NRad);
    ILbin = zeros(NTemp,NTemp,NRad);
    
    io.files.load1('/home/sasha/wpsf4_3500.mat'); PSF(:,:,1,:) = WPSF;
    io.files.load1('/home/sasha/wpsf4_5800.mat'); PSF(:,:,2,:) = WPSF;
    io.files.load1('/home/sasha/wpsf4_10000.mat'); PSF(:,:,3,:) = WPSF;
    io.files.load1('/home/sasha/wpsf4_20000.mat'); PSF(:,:,4,:) = WPSF;
    
    for IRad = 1:NRad
    
        for IT = 1:1:NTemp

            M0 = PSF(:,:,IT,IRad);

            for JT = 1:1:NTemp

                M1 = PSF(:,:,JT,IRad);
                
                IL(IT,JT,IRad) = imUtil.psf.information_loss(M0, M1) ;

                if IL(IT,JT,IRad) < 0.1
                    ILbin(IT, JT, IRad) = 0;
                else
                    ILbin(IT, JT, IRad) = 1;
                end

            end

        end

    end
    
    
    figure(4)
    subplot(2,2,1); imagesc(ILbin(:,:,1)); title('0.42 deg')
    subplot(2,2,2); imagesc(ILbin(:,:,2)); title('4.17 deg')
    subplot(2,2,3); imagesc(ILbin(:,:,3)); title('6.25 deg')
    subplot(2,2,4); imagesc(ILbin(:,:,4)); title('8.75 deg')
    
    Result = 0;
                            
end