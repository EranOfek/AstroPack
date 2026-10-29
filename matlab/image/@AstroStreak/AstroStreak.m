classdef AstroStreak < Base
% New AstroStreak class. Rationalization of the imUtil.streaks.AstroStreak
%  class which was originally introduced. Here, one element, one streaklet;
%  there, one element, results in one image, which could include several
%  streaklets
    properties
        ParentImageId % id of the image in which this streak was detected
        Xdetect % abscissae of the extremal points detected by LSD, pixel coordinates
        Ydetect % ordinatae. Both are relative to the frame size
        LightSec  % [Xmin,Ymin,Xmax,Ymax] pixel coordinaes of the original image which was cropped 
        RAextremes % RA(ExtentIndices) (two elements)
        DecExtremes % Dec(ExtentIndices) (two elements)
        JD % 2x2 % min and max JD of the two extremes
        IsEdge % flags if the extremes of the streak are at the image edge
        Flux % photometric total flux estimation, (sigma units)
        AngSpeed % in arcsec/sec
        FitPar % a,b,c coefficients of the fitted sagittal deviation
        ExtentIndices  % first and last slice considered part of the streak
        TransversePSF  % binned histogram, over all slices, in pixel steps
    end

    % per-slice quantities (all arrays of the same size):
    properties
        X % coordinates of the slice maxima fits (center slice, sagitta of the peak)
        Y %  also relative to the cropped frame size
        RA % RA of the slice peak
        Dec  % Dec, populated given astrometry, from X,Y, by AstroImage.addSkyCOO
        SliceFlux  % slice integrated flux
        Mag % converted to mag once speed and FWHM are determined
        TransverseSigma % sigma of the slice gaussian fit
        Hmean  % sagitta of the maximum, per slice
        Acceptable=false(0,0)  % true if the slice fit is deemed reliable
    end


    methods
        % creator, with optional input arguments
        function A=AstroStreak(varargin)
            if nargin>0 && isa(varargin{1},'imUtil.streaks.AstroStreak')
                A.importImUtilAstroStreak(varargin{1})
            end
        end

   end
end