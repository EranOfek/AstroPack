function [OutlierMask, Info] = sigmaClip(Residuals, SigmaThresh, Args)
    % Sigma clipping of residuals with selectable method
    % Package: tools.math.stat
    % Description: Identifies outliers in a residual vector using sigma
    %              clipping. Two methods are supported:
    %              'weighted' - Threshold on error-normalized residuals:
    %                           |r_i / sigma_i| > SigmaThresh
    %              'median'   - Astropy-style iterative clipping on absolute
    %                           residuals: |abs(r_i) - median| > N * std
    %                           Replicates astropy.stats.sigma_clip with
    %                           cenfunc='median', stdfunc='std', maxiters=5.
    % Input  :  Residuals  - Residual vector [N x 1] (signed, in magnitudes)
    %           SigmaThresh - Sigma threshold for clipping (e.g., 3.0)
    %          * ...,key,val,...
    %            'Method'  - Clipping method:
    %                        'weighted' - |r_i / Errors_i| > SigmaThresh
    %                        'median'   - Iterative median+std on abs(residuals)
    %                                     (default)
    %            'Errors'  - Per-observation errors [N x 1] for 'weighted'
    %                        method. Ignored for 'median'. Default is [].
    %            'MaxIter' - Max internal iterations for 'median' method.
    %                        Default is 5 (matches astropy default).
    % Output :  OutlierMask - Logical [N x 1], true = outlier
    %           Info - Struct with diagnostics:
    %                   .NumOutliers - Number of flagged outliers
    %                   .Center - Center value used (0 for 'weighted',
    %                             median(abs(r)) for 'median')
    %                   .Scale  - Scale value used (NaN for 'weighted',
    %                             std(abs(r)) for 'median')
    %                   .NumIter - Number of iterations performed
    % Author : D. Kovaleva (Feb 2026)
    % Example: Residuals = randn(100,1);
    %          Residuals(1) = 10;  % outlier
    %          [Mask, Info] = tools.math.stat.sigmaClip(Residuals, 3.0);
    %          fprintf('Outliers: %d\n', Info.NumOutliers);
    %          % Weighted mode:
    %          Errors = 0.05 * ones(100,1);
    %          [Mask, Info] = tools.math.stat.sigmaClip(Residuals, 3.0, ...
    %              'Method', 'weighted', 'Errors', Errors);

    arguments
        Residuals
        SigmaThresh
        Args.Method  = 'median'
        Args.Errors  = []
        Args.MaxIter = 5
    end

    Info = struct('NumOutliers', 0, 'Center', NaN, 'Scale', NaN, 'NumIter', 0);

    switch Args.Method
        case 'weighted'
            % Threshold on error-normalized residuals: |r_i / sigma_i| > N
            if isempty(Args.Errors) || ~all(Args.Errors > 0)
                error('sigmaClip:MissingErrors', ...
                    'Positive per-observation Errors required for weighted method');
            end
            OutlierMask = abs(Residuals ./ Args.Errors) > SigmaThresh;
            Info.Center = 0;
            Info.Scale = NaN;
            Info.NumIter = 1;

        case 'median'
            % iterative sigma_clip on abs(residuals)
            % cenfunc = median, stdfunc = std, applied to abs(residuals)
            AbsRes = abs(Residuals);-
            ClipMask = false(size(AbsRes));
            Converged = false;
            Center = NaN;
            Scale = NaN;
            IterDone = 0;

            for Iter = 1:Args.MaxIter
                if ~Converged
                    ValidData = AbsRes(~ClipMask);
                    Center = median(ValidData);
                    Scale = std(ValidData);
                    NewClipMask = abs(AbsRes - Center) > SigmaThresh * Scale;
                    IterDone = Iter;
                    if isequal(NewClipMask, ClipMask)
                        Converged = true;
                    end
                    ClipMask = NewClipMask;
                end
            end
            OutlierMask = ClipMask;
            Info.Center = Center;
            Info.Scale = Scale;
            Info.NumIter = IterDone;

        otherwise
            error('sigmaClip:UnknownMethod', ...
                'Unknown method ''%s''. Use ''weighted'' or ''median''.', Args.Method);
    end

    Info.NumOutliers = sum(OutlierMask);
end
