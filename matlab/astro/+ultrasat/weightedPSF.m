function weightedPSF(Args)
    % make a bank of spectrum-weighted ULTRASAT PSFs for a set of spectra and a range of radial distances
    % Input: -
    %        * ...,key,val,... 
    %       'ImRes' - image oversampling in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    %       'Type'  - spectrum type ('Pickles','BB','galaxy')
    %       'ContainmentLevel' - calculate the radius where a certain percent of the total PSF flux is contained
    % Output: matlab objects with spectrum-weighted PSFs and their flux containment radii
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: ultrasat.weightedPSF('ContainmentLevel',0.9);
    arguments
        Args.ImRes = 5; 
        Args.Type  = 'Pickles';
        Args.ContainmentLevel = 0.5;
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
    
    switch lower(Args.Type)
        case 'pickles'
            % make a grid in Teff and log(g), use tefflogg2Pickles to get the spectral class
            NTemp = 30; Ng = 4;
            logT = linspace(3.4,4.8,NTemp);
            logg = [3.01 4.49 4.51 6];
            PiCl = astro.stars.tlogg2picklesClass(10.^logT,logg);
            
            WPSF = zeros(size(PSFdata,1),size(PSFdata,2),NTemp,Ng,Nrad);
            ContRad  = zeros(NTemp,Ng,Nrad);
            % weight the PSF stamps with the spectra
            for ITemp = 1:NTemp
                for Ig = 1:Ng
                    Sp  = AstroSpec.specStarsPickles(PiCl(ITemp,Ig).class,PiCl(ITemp,Ig).lumclass);
                    Sp2 = interp1(Sp.Wave, Sp.Flux, WavePSF);
                    Sp3 = reshape(Sp2,[1 1 Nwave]);
                    for Irad = 1:Nrad
                        Wcube = PSFdata(:,:,:,Irad) .* Sp3;
                        SumL  = squeeze( sum(Wcube,3) );
                        WPSF(:,:,ITemp,Ig,Irad) = SumL ./ sum( SumL, [1,2] );
                        ContRad(ITemp,Ig,Irad) = imUtil.psf.containment(WPSF(:,:,ITemp,Ig,Irad),'Level',Args.ContainmentLevel)./Args.ImRes;
                    end
                end
            end
            % save the weighted PSF cubes
            FName = sprintf('%s%.0f%s','spec_weightedPSF_Pickles_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'WPSF','logT','logg','Rad');
            FName = sprintf('%s%0.2f%s%.0f%s','spec_weightedPSF_Pickles_Contain',Args.ContainmentLevel,'Rad_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'ContRad','logT','logg','Rad');
            
        case 'bb'
            
        case 'galaxy'
            
    end
    
end