function Result = psf_information_content(ImRes)
    % characterize information content loss when using various ULTRASAT PSFs
    
    % ULTRASAT PSF database parameters
    arguments
        
        ImRes = 5  % PSF resolution 
        
    end
               
    Nrad    = 25; 
    Rad     = linspace(0,10,Nrad);   % lab PSF grid points in radius    
            
    MinWave = 2000;  % [A] the band boundaries
    MaxWave = 11000; % [A]
    
    WavePSF = linspace(MinWave,MaxWave,91); % lab PSF grid points in wavelength
    
    PixRat  = 47.5; % the ratio of the ULTRASAT pixel size to that of the lab PSF image 

                            fprintf('Reading PSF database.. '); 
    
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at chosen resolution
    PSFdata = ReadDB{2}; 
    
    if size(PSFdata,4) ~= Nrad 
        error('PSF array size mismatch, exiting..');
    end
    
                            fprintf('done\n'); 
                            
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
                            
    Result = 0;
                            
end