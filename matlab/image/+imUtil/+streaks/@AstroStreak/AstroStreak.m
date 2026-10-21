classdef AstroStreak < handle
    properties
        X % 2xN % abscissae of the extremal points of the streak(s), pixel coordinates
        Y % 2xN % ordinatae of the extremal points of the streak, pixel coordinates
        RA % 2xN % same, in RA,Dec coordinates
        Dec % 2xN
        JD % 2xN % time bounds for the streak duration (may include rolling shutter effects)
        IsEdge % 2xN - flags if the extremes of the streak are at the image edge
        Flux % 1xN % photometric flux estimation, (sigma units)
        AngSpeed % in arcsec/sec. To be populated once direction and extent are found, perhaps by streaks metaanalysis
        FitPar % 3xN % a,b,c coefficients of the fitted sagittal deviation
        % coordinates and fluxes of streak slices, if populated by
        %  streak_photometry:
        Curve = struct('X',[],'Y',[], ... % coordinates of the slice maxima fits (center slice, sagitta of the peak)
                       'Flux',[], ... % slice integrated flux
                       'Mag',[], ... % converted to mag once speed and FWHM are determined
                       'TransverseSigma',[],... % sigma of the slice gaussian fit
                       'Hmean',[],... % sagitta of the maximum, per slice
                       'Acceptable',false(0,0),... % true if the slice fit is deemed reliable
                       'ExtentIndices', [], ... % first and last slice considered part of the streak
                       'TransversePSF',[]); % binned histogram, over all slices, in pixel steps
        ID % Telescope, Epoch, Crop ID.
    end

    methods
    end

end
