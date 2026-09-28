classdef AstroStreak < handle
    properties
        X % 2xN % abscissae of the extremal points of the streak(s), pixel coordinates
        Y % 2xN % ordinatae of the extremal points of the streak, pixel coordinates
        RA % 2xN % same, in RA,Dec coordinates
        Dec % 2xN
        JD % 2xN % time bounds for the streak duration (may include rolling shutter effects)
        IsEdge % 2xN - flags if the extremes of the streak are at the image edge
        Flux % 1xN % photometric flux estimation, (sigma units)
        FitPar % 3xN % a,b,c coefficients of the fitted sagittal deviation
        Curve = struct('X',[],'Y',[],'Flux',[],'TransverseSigma',[],...
            'Hmean',[],'Acceptable',false(0,0),'TransversePSF',[]);...
                                % coordinates and fluxes of streak slices
        ID % Telescope, Epoch, Crop ID.
    end
end