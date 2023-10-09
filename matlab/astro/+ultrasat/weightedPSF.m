function weightedPSF(Args)
    % make a bank of spectrum-weighted ULTRASAT PSFs for a set of spectra and a range of radial distances
    % Input: -
    %        * ...,key,val,... 
    %       'ImRes' - image oversampling in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    % Output: a matlab object with the weighted PSFs
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Exmaple: 
    arguments
        Args.ImRes = 5; 
    end
    
    % lab PSF grid points in radius and wavelength
    Nrad    = 25; Rad = linspace(0,10,Nrad);
    Nwave   = 91; WavePSF = linspace(2000,11000,Nwave);                
    
    % load the matlab object with the ULTRASAT properties: 
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    io.files.load1(UP_db,'UP');
    
    % read the chosen PSF database from a .mat file
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2}; 
    
    % make a grid in Teff and log(g), use tefflogg2Pickles to get the spectral class 
    NTemp = 30; Ng = 4; 
    logT = linspace(3.4,4.8,NTemp); 
    logg = [3.01 4.49 4.51 6]; 
    PiCl = astro.stars.tlogg2picklesClass(10.^logT,logg); 
    
    WPSF = zeros(size(PSFdata,1),size(PSFdata,2),NTemp,Ng,Nrad);
    Rad90 = zeros(NTemp,Ng,Nrad);
    
    for ITemp = 1:NTemp
        for Ig = 1:Ng
            Sp  = AstroSpec.specStarsPickles(PiCl(ITemp,Ig).class,PiCl(ITemp,Ig).lumclass);
            Sp2 = interp1(Sp.Wave, Sp.Flux, WavePSF);
            Sp3 = reshape(Sp2,[1 1 Nwave]);
            for Irad = 1:Nrad
                Wcube = PSFdata(:,:,:,Irad) .* Sp3;
                SumL  = squeeze( sum(Wcube,3) );
                WPSF(:,:,ITemp,Ig,Irad) = SumL ./ sum( SumL, [1,2] );
                Rad90(ITemp,Ig,Irad) = imUtil.psf.containment(WPSF(:,:,ITemp,Ig,Irad),'Level',0.9)./Args.ImRes;
            end
        end
    end
    
    % weight the PSF stamps with the spectra
    save('~/weightedPSF.mat','WPSF','logT','logg','Rad');
    save('~/PSFContain90Rad.mat','Rad90','logT','logg','Rad');
    
end