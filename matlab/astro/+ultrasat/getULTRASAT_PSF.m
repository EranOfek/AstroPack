function PSF = getULTRASAT_PSF(Args)
    % get a weighted ULTRASAT PSF stamp
    % Input: -
    %       * ...,key,val,... 
    %       'Oversampling' - image oversampling in 1/pix units (allowed values: 1, 2, 5, 10, 47.5)
    %       'Type' - spectrum type: 'Pickles' or 'star', 'BB', 'galaxy'
    %       'Teff' - Teff of the star or BB temperature (if a BB spectrum is requested)
    %       'logg' - log(g) of the star
    %       'Rad'  - angular distance of the source from the tile's inner corner
    % Output: 
    % Author: A.M. Krassilchtchikov (Oct 2023)
    % Example: 
    arguments
        Args.Oversampling = 5;
        Args.Type         = 'Pickles';
        Args.Teff         = 5500;
        Args.logg         = 4.6;
        Args.Rad          = 0;
    end
    
    switch lower(Args.Type)
        case 'pickles'
            Collection = sprintf('%s%s%.0f%s',tools.os.getAstroPackPath,'/../data/ULTRASAT/PSF/spec_weightedPSF_Pickles_ovrsmpl',Args.Oversampling,'.mat');
            io.files.load1(Collection);
            if Args.Teff < 10^logT(1) || Args.Teff > 10^logT(numel(logT)) || Args.logg < logg(1) || Args.logg > logg(numel(logg)) 
                cprintf('red','Warning! The input parameters are outside the modelled range!\n');
            end
            X = 1:size(WPSF,1); Y = 1:size(WPSF,2);            
            PSF = interpn(X,Y, logT, logg, Rad, WPSF, X, Y, log10(Args.Teff), Args.logg, Args.Rad);
        case 'bb'
            
        case 'galaxy'
            
    end
end