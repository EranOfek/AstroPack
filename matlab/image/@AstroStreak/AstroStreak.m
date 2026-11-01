classdef AstroStreak < Base
% New AstroStreak class. Rationalization of the imUtil.streaks.AstroStreak
%  class which was originally introduced. Here, one element, one streaklet;
%  there, one element, results in one image, which could include several
%  streaklets
    properties
        ParentImageId % id of the image in which this streak was detected
        Xdetect % abscissae of the extremal points detected by LSD, pixel coordinates
        Ydetect % ordinatae. Both are relative to the frame size
        Xoffset=0; % original first column in the full image, which was cropped
        Yoffset=0; % original first row in the full image, which was cropped
        RAExtremes % RA(ExtentIndices) (two elements)
        DecExtremes % Dec(ExtentIndices) (two elements)
        JD % 1x2 JD span of the full frame exposure [JD(1)<JD(2)] 
        JDextremes  % timing of the two extremes
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
            % array of imUtil.streaks.AstroStreak
            if nargin>0 && isa(varargin{1},'imUtil.streaks.AstroStreak')
                A=A.importImUtilAstroStreak(varargin{1});
            end
            % AllStreaks array produced by pipelineI
            if nargin>0 && isfield(varargin{1},'Crop') && ...
              isfield(varargin{1},'Epoch') && isfield(varargin{1},'Streaks') && ...
              isa([varargin{1}.Streaks],'imUtil.streaks.AstroStreak')
                A=A.importAllStreaksStruct(varargin{1});
            end
        end

    end

end