function [WPSF, ContRad] = weightedPSF(Args)
    % make a .mat library of spectrum-weighted ULTRASAT PSFs for a set of spectra and a range of radial distances
    % Input: -
    %        * ...,key,val,... 
    %       'ImRes' - image oversampling in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    %       'Type'  - spectrum type ('Pickles','BB','stellarclass','galaxy','all')
    %       'ContainmentLevel' - calculate the radius where a certain percent of the total PSF flux is contained
    %       'Class' - stellar class as a cell array of 2 cells, e.g. {'m2','v'} 
    %       'RDist' - radial distance from the detector tile's inner corner [deg]
    % Output: 2 matlab objects: 1. spectrum-weighted PSFs and 2. their flux containment radii
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: ultrasat.weightedPSF('ContainmentLevel',0.9);
    %          ultrasat.weightedPSF('Type','BB','ContainmentLevel',0.8);
    %          ultrasat.weightedPSF('Type','all');
    %          P = ultrasat.weightedPSF('Type','stellarclass','Class',{'o9','v'}, 'RDist', 5.5);
    arguments
        Args.ImRes = 5; 
        Args.Type  = 'Pickles';
        Args.ContainmentLevel = 0.5;
        Args.Class = {'g0', 'v'};
        Args.RDist = 0;
    end
    I = Installer; 
    % lab PSF grid points in radius and wavelength
    Nrad    = 25; Rad = linspace(0,10,Nrad);
    Nwave   = 91; WavePSF = linspace(2000,11000,Nwave);                
    
    % load the matlab object with the ULTRASAT properties: 
    UP_db = sprintf('%s%s',I.getDataDir('ULTRASAT_UP'),'/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    io.files.load1(UP_db,'UP');
    
    % read the chosen PSF database from a .mat file
    PSF_db = sprintf('%s%s%g%s',I.getDataDir('ULTRASAT_PSF'),'/ULTRASATlabPSF',Args.ImRes,'.mat');
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
                        ContRad(ITemp,Ig,Irad) = imUtil.psf.quantileRadius(WPSF(:,:,ITemp,Ig,Irad),'Level',Args.ContainmentLevel)./Args.ImRes;
                    end
                end
            end
            % save the weighted PSF cubes
            FName = sprintf('%s%.0f%s','spec_weightedPSF_Pickles_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'WPSF','logT','logg','Rad');
            FName = sprintf('%s%0.2f%s%.0f%s','spec_weightedPSF_Pickles_Contain',Args.ContainmentLevel,'Rad_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'ContRad','logT','logg','Rad');
            
        case 'bb'
            % make a grid in T
            NTemp = 30;
            logT = linspace(3.3,5,NTemp);
            
            WPSF = zeros(size(PSFdata,1),size(PSFdata,2),NTemp,Nrad);
            ContRad  = zeros(NTemp,Nrad);
            S = AstroSpec.blackBody(WavePSF,10.^logT);
            for ITemp = 1:NTemp
                Sp3 = reshape(S(ITemp).Flux,[1 1 Nwave]);
                for Irad = 1:Nrad
                    Wcube = PSFdata(:,:,:,Irad) .* Sp3;
                    SumL  = squeeze( sum(Wcube,3) );
                    WPSF(:,:,ITemp,Irad) = SumL ./ sum( SumL, [1,2] );
                    ContRad(ITemp,Irad) = imUtil.psf.quantileRadius(WPSF(:,:,ITemp,Irad),'Level',Args.ContainmentLevel)./Args.ImRes;
                end
            end
            % save the weighted PSF cubes
            FName = sprintf('%s%.0f%s','spec_weightedPSF_BB_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'WPSF','logT','Rad');
            FName = sprintf('%s%0.2f%s%.0f%s','spec_weightedPSF_BB_Contain',Args.ContainmentLevel,'Rad_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'ContRad','logT','Rad');
            
        case 'galaxy'
            error('Galactic spectra are not available as of yet');
            
        case 'stellarclass'
            Sp = AstroSpec.specStarsPickles(Args.Class{1},Args.Class{2});
            Sp2 = interp1(Sp.Wave, Sp.Flux, WavePSF);
            Sp3 = reshape(Sp2,[1 1 Nwave]);            
            [~, Irad] = min( abs(Args.RDist - Rad), [], 2); 
            Wcube = PSFdata(:,:,:,Irad) .* Sp3;
            SumL  = squeeze( sum(Wcube,3) );
            WPSF = SumL ./ sum( SumL, [1,2] );
            ContRad = imUtil.psf.quantileRadius(WPSF,'Level',Args.ContainmentLevel)./Args.ImRes;
            
        case 'all'
            % put all types of spectra into 1 array according to a special
            % indexing function which translates spectral types into index            
            Spec = ultrasat.weightedPSFindex('BuildSpec',true);
            
            NSp  = numel(Spec);
            WPSF = zeros(size(PSFdata,1),size(PSFdata,2),NSp,Nrad);
            ContRad  = zeros(NSp,Nrad);
            for ISp = 1:NSp
                Sp2 = interp1(Spec(ISp).Wave, Spec(ISp).Flux, WavePSF);
                Sp3 = reshape(Sp2,[1 1 Nwave]);
                for Irad = 1:Nrad
                    Wcube = PSFdata(:,:,:,Irad) .* Sp3;
                    SumL  = squeeze( sum(Wcube,3) );
                    WPSF(:,:,ISp,Irad) = SumL ./ sum( SumL, [1,2] );
                    ContRad(ISp,Irad) = imUtil.psf.quantileRadius(WPSF(:,:,ISp,Irad),'Level',Args.ContainmentLevel)./Args.ImRes;
                end
            end
            % save the weighted PSF cube
            FName = sprintf('%s%.0f%s','spec_weightedPSF_all_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'WPSF','Rad');
            FName = sprintf('%s%0.2f%s%.0f%s','spec_weightedPSF_all_Contain',Args.ContainmentLevel,'Rad_ovrsmpl',Args.ImRes,'.mat');
            save(FName,'ContRad','Rad');
    end
    
end