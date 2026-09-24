classdef AstroStreak < handle
    properties
        X % 2xN % abscissae of the extremal points of the streak(s), pixel coordinates
        Y % 2xN % ordinatae of the extremal points of the streak, pixel coordinates
        RA % 2xN
        Dec % 2xN
        JD % 2xN
        IsEdge % 2xN
        Flux % 1xN % photometric
        FitPar % 3xN % a,b,c coefficients of the fitted sagittal deviation
        Curve = struct('X',[],'Y',[],'Flux',[],'TransverseSigma',[],...
            'Hmean',[],'Acceptable',false(0,0),'TransversePSF',[]);...
                                % coordinates and fluxes of streak slices
        ID % Telescope, Epoch, Crop ID.
    end
end