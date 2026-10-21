classdef AstroStreak < handle
    properties
        X % 2xN % abscissae of the extremal points of the streak(s), pixel coordinates
        Y % 2xN % ordinatae of the extremal points of the streak, pixel coordinates
        RA % 2xN % same, in RA,Dec coordinates
        Dec % 2xN
        JD % 2xN % time bounds for the streak duration (may include rolling shutter effects)
        IsEdge % 2xN - flags if the extremes of the streak are at the image edge
        Flux % 1xN % photometric flux estimation, (sigma units)
        AngSpeed % 1xN % in arcsec/sec. To be populated once direction and extent are found, perhaps by streaks metaanalysis
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

        function populateMagnitude(S,ExpTime,FWHM)
            if ~exist('FWHM','var')
                FWHM=3;
                % TODO: compute it from S.Curve(k).TransversePSF instead,
                %  if present
            end
            for k=1:numel(S.Curve)
                if ~isempty(S.Curve(k).Flux) && numel(S.AngSpeed)>=k
                    % coarse, for the time being, and most likely wrong
                    %  (should also be normalized by L/slice_width, right?)
                    S.Curve(k).Mag = S.Curve(k).Flux - 2.5.*log10(ExpTime.*S.AngSpeed(k)/FWHM);
                end
            end
        end

        function setExtent(S)
            % determine the probable extent of the streak, based on the
            %  photometric data, attempting to filter out isolated sources
            %  which are accidentally in line with the extension of the streak
            for k=1:numel(S.Curve)
                acc=S.Curve(k).Acceptable;
                if numel(acc)>1
                    % coarse criterion: the extremal slices must have at least
                    %  one acceptable next neighbor
                    i1=find(acc(1:end-1) & acc(2:end), 1,'first');
                    i2=find(acc(1:end-1) & acc(2:end), 1,'last')+1;
                    if ~isempty(i1) && ~isempty(i2)
                        S.Curve(k).ExtentIndices=[i1,i2];
                    else
                        S.Curve(k).ExtentIndices=[];
                    end
                end
            end
        end
    end

end
