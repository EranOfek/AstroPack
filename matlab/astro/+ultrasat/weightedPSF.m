function Result = weightedPSF(Args)
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
    Nwave   = 91; WavePSF = linspace(2000,11000,91);                
    
    % load the matlab object with the ULTRASAT properties: 
    UP_db = sprintf('%s%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/P90_UP_test_60_ZP_Var_Cern_21.mat');   
    io.files.load1(UP_db,'UP');
    
    % read the chosen PSF database from a .mat file
    PSF_db = sprintf('%s%s%g%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/ULTRASATlabPSF',Args.ImRes,'.mat');
    ReadDB = struct2cell ( io.files.load1(PSF_db) ); % PSF data at the chosen spatial resolution
    PSFdata = ReadDB{2}; 
    
    % prepare the spectra
    Spec = UP.Specs; 
    % more options?
    
    Nspec = numel(Spec);
    
    % weight the PSF stamps with the spectra
    Result = zeros(size(PSFdata,1),size(PSFdata,2),Nspec,Nrad);
    for Ispec = 1:Nspec
        for Irad = 1:Nrad
            Sp = interp1(Spec(Ispec).Wave, Spec(Ispec).Int, WavePSF);
            Spec2 = reshape(Sp,[1 1 Nwave]);
            Wcube = PSFdata(:,:,:,Irad) .* Spec2;
            SumL  = squeeze( sum(Wcube,3) ); 
            Result(:,:,Ispec,Irad) = SumL ./ sum( SumL, [1,2] );
        end
    end
    
end