function PSF = getULTRASAT_PSF(Args)
    % get a weighted ULTRASAT PSF stamp
    % Input: -
    %       * ...,key,val,... 
    %       'Oversampling' - image oversampling in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    %       'Type' - spectrum type: 'Pickles' or 'star', 'BB', 'galaxy'
    %       'Teff' - Teff of the star or BB temperature (if a BB spectrum is requested)
    %       'logg' - log(g) of the star
    %       'Rad'  - angular distance of the source from the tile's inner corner
    %       'Class'- precise stellar class as a cell array
    % Output: 
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: P = ultrasat.getULTRASAT_PSF('Type','Pickles','Teff', 4300, 'logg', 4.2, 'Rad', 2.1);
    %          P = ultrasat.getULTRASAT_PSF('Type','BB','Teff', 4300, 'Rad', 1.2);
    %          P = ultrasat.getULTRASAT_PSF('Type','stellarclass','Class',{'o9','v'}, 'Rad', 4.6);
    arguments
        Args.Oversampling = 5;
        Args.Type         = 'Pickles';
        Args.Teff         = 5500;
        Args.logg         = 4.6;
        Args.Class        = {'g0', 'v'};
        Args.Rad          = 0;
    end
    I = Installer;
    switch lower(Args.Type)
        case 'pickles'
            Collection = sprintf('%s%s%.0f%s',I.getDataDir('ULTRASAT_PSF'),'/spec_weightedPSF_Pickles_ovrsmpl',Args.Oversampling,'.mat');
            if isfile(Collection)
                io.files.load1(Collection);
            else
                error('The requested oversampling data is not yet available, exiting..');
            end
            if Args.Teff < 10^logT(1) || Args.Teff > 10^logT(numel(logT)) || Args.logg < logg(1) || Args.logg > logg(numel(logg)) 
                cprintf('red','Warning! The input parameters are outside the modelled range!\n');
            end
            X = 1:size(WPSF,1); Y = 1:size(WPSF,2);            
            PSF = interpn(X,Y, logT, logg, Rad, WPSF, X, Y, log10(Args.Teff), Args.logg, Args.Rad);
            
        case 'bb'
            Collection = sprintf('%s%s%.0f%s',I.getDataDir('ULTRASAT_PSF'),'/spec_weightedPSF_BB_ovrsmpl',Args.Oversampling,'.mat');
            if isfile(Collection)
                io.files.load1(Collection);
            else
                error('The requested oversampling data is not yet available, exiting..');
            end
            if Args.Teff < 10^logT(1) || Args.Teff > 10^logT(numel(logT)) 
                cprintf('red','Warning! The input parameters are outside the modelled range!\n');
            end
            X = 1:size(WPSF,1); Y = 1:size(WPSF,2);            
            PSF = interpn(X,Y, logT, Rad, WPSF, X, Y, log10(Args.Teff), Args.Rad);
            
        case 'galaxy'
            error('Galactic spectra are not available as of yet');
            
        case 'stellarclass'
            PSF = ultrasat.weightedPSF('Type','stellarclass','Class',Args.Class,'RDist',Args.Rad,'ImRes',Args.Oversampling);
            
    end
end