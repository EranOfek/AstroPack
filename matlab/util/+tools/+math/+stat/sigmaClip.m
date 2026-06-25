function [OutlierMask, Info] = sigmaClip(Residuals, SigmaThresh, Args)
    % Sigma clipping of residuals: 'median' / 'median_signed' / 'weighted'.
    % Package: tools.math.stat
    %
    % Description: Identifies outliers in a residual vector using one of
    %              three sigma-clipping recipes. The 'median' and
    %              'median_signed' modes both follow the astropy iteration
    %              recipe (center = median, scale = std or mad_std,
    %              maxiters = MaxIter, clip when |x - center| > N * scale)
    %              and differ only in what they treat as x:
    %                'median'        - x = |Residuals|  (abs-folded vector)
    %                'median_signed' - x = Residuals    (signed vector)
    %              The 'weighted' mode is a single-shot test on
    %              error-normalised residuals, no iteration.
    %
    %              -- 'weighted' --
    %                Test: |r_i / sigma_i| > SigmaThresh
    %                Single pass, no median, no iteration. Requires per-
    %                observation errors. Center = 0 implicitly assumed.
    %
    %              -- 'median' --
    %                x = |Residuals|; iterate over the currently-unmasked
    %                points:
    %                  Center = median(x);  Scale = std(x) or mad_std(x);
    %                  flag i where |x_i - Center| > N * Scale.
    %                Statistical note: applied to symmetric, zero-mean
    %                signed residuals, |r| follows a half-normal with
    %                median(|r|) ≈ 0.6745 sigma and std(|r|) ≈ 0.6028 sigma.
    %                Consequence: at the same nominal SigmaThresh, this
    %                method is MORE AGGRESSIVE than 'median_signed'. The
    %                effective single-sided clip on the signed scale is
    %                ~0.6745 + N * 0.6028 sigma (i.e. N = 3 ≈ 2.48 sigma,
    %                N = 2 ≈ 1.88 sigma). Use when matching pipelines that
    %                feed abs(residuals) into astropy.stats.sigma_clip;
    %                this is the LAST/Python production behaviour.
    %
    %              -- 'median_signed' --
    %                x = Residuals (preserves sign); iterate:
    %                  Center = median(x);  Scale = std(x) or mad_std(x);
    %                  flag i where |x_i - Center| > N * Scale.
    %                Statistical note: this is the canonical interpretation
    %                of "N-sigma clip" - at SigmaThresh = N, the clip
    %                threshold IS N * sigma on the residual distribution.
    %                Equivalent to calling astropy.stats.sigma_clip(r,
    %                cenfunc='median', stdfunc='std', maxiters=MaxIter)
    %                directly on the SIGNED residual vector (no abs() pre-
    %                processing). Use when you want "N sigma" to mean
    %                literally N sigma.
    %
    %              On any error, returns no-outlier mask and a warning
    %              (pipeline-safe: never throws).
    %
    % Input  :  - Residual vector [N x 1] (signed, in magnitudes)
    %           - Sigma threshold for clipping (e.g., 3.0). See the
    %             effective-threshold note for 'median' above.
    %          * ...,key,val,...
    %            'Method'  - Clipping method (default 'median'):
    %                        'median'        - astropy iteration on |r|
    %                                          (LAST/Python parity; more
    %                                          aggressive than nominal N)
    %                        'median_signed' - astropy iteration on signed r
    %                                          (canonical N-sigma clip)
    %                        'weighted'      - single-shot |r_i / Errors_i|
    %                                          > SigmaThresh test
    %            'StdFunc' - Scale function for the two median modes:
    %                        'mad_std' - 1.4826 * MAD (robust scale; default)
    %                        'std'     - sample std (astropy default;
    %                                    needed for byte-identical Python
    %                                    parity)
    %                        Ignored for 'weighted'.
    %            'Errors'  - Per-observation errors [N x 1] for 'weighted'
    %                        method. Ignored otherwise. Default is [].
    %            'MaxIter' - Max iterations for the two median modes.
    %                        Default is 5 (matches astropy default).
    %                        Ignored for 'weighted'.
    % Output :  - OutlierMask, logical [N x 1], true = outlier
    %           - Info, struct with diagnostics:
    %                   .NumOutliers - Number of flagged outliers
    %                   .Center      - Final Center value used:
    %                                  median(|r|)  for 'median'
    %                                  median(r)    for 'median_signed'
    %                                  0            for 'weighted'
    %                   .Scale       - Final Scale value used:
    %                                  std/mad_std on |r| or r for the
    %                                  two median modes; NaN for 'weighted'
    %                   .NumIter     - Iterations performed (1 for 'weighted')
    %                   .Success     - true if clipping completed
    %                   .ErrorMsg    - Error message (empty on success)
    % Author : D. Kovaleva (Feb 2026)
    % Example: % Standard 3-sigma clip on signed residuals (canonical):
    %          Residuals = randn(100,1);
    %          Residuals(1) = 10;  % outlier
    %          [Mask, Info] = tools.math.stat.sigmaClip(Residuals, 3.0, ...
    %              'Method', 'median_signed', 'StdFunc', 'std');
    %          fprintf('Outliers: %d, Center=%.3f, Scale=%.3f\n', ...
    %              Info.NumOutliers, Info.Center, Info.Scale);
    %
    %          % LAST/Simone-parity clip (abs-folded astropy call):
    %          [Mask, Info] = tools.math.stat.sigmaClip(Residuals, 3.0, ...
    %              'Method', 'median', 'StdFunc', 'std');
    %          % Effective single-sided cut here is ~2.48 sigma (because
    %          % the median is taken over |r|, see Statistical note above).
    %
    %          % Weighted single-shot clip on r/sigma_i:
    %          Errors = 0.05 * ones(100,1);
    %          [Mask, Info] = tools.math.stat.sigmaClip(Residuals, 3.0, ...
    %              'Method', 'weighted', 'Errors', Errors);

    arguments
        Residuals
        SigmaThresh
        Args.Method  = 'median'
        Args.StdFunc = 'mad_std'   % 'mad_std' (default, robust) | 'std'
        Args.Errors  = []
        Args.MaxIter = 5
    end

    % Default safe output: no outliers
    N = numel(Residuals);
    OutlierMask = false(N, 1);
    Info = struct('NumOutliers', 0, 'Center', NaN, 'Scale', NaN, ...
                  'NumIter', 0, 'Success', true, 'ErrorMsg', '');

    try
        Performed = false;

        switch Args.Method
            case 'weighted'
                if isempty(Args.Errors) || ~all(Args.Errors > 0)
                    warning('sigmaClip:MissingErrors', ...
                        'Positive per-observation Errors required for weighted method; skipping clipping');
                    Info.Success = false;
                    Info.ErrorMsg = 'Missing or invalid Errors for weighted method';
                else
                    OutlierMask = abs(Residuals ./ Args.Errors) > SigmaThresh;
                    Info.Center = 0;
                    Info.Scale = NaN;
                    Info.NumIter = 1;
                    Performed = true;
                end

            case 'median'
                AbsRes = abs(Residuals);
                ClipMask = false(size(AbsRes));
                Converged = false;
                Center = NaN;
                Scale = NaN;
                IterDone = 0;

                for Iter = 1:Args.MaxIter
                    if ~Converged
                        ValidData = AbsRes(~ClipMask);
                        Center = median(ValidData);
                        switch Args.StdFunc
                            case 'std'
                                Scale = std(ValidData);
                            case 'mad_std'
                                Scale = 1.4826 * median(abs(ValidData - median(ValidData)));
                            otherwise
                                warning('sigmaClip:UnknownStdFunc', ...
                                    'Unknown StdFunc ''%s''; falling back to std', Args.StdFunc);
                                Scale = std(ValidData);
                        end
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
                Performed = true;

            case 'median_signed'
                % astropy.stats.sigma_clip parity:
                % operate on SIGNED residuals, not |residuals|.
                % Iterates center=median, scale=std (or mad_std) over the
                % currently-unmasked points; clip mask grows until either
                % nothing new is flagged or MaxIter reached.
                SigRes = Residuals;
                ClipMask = false(size(SigRes));
                Converged = false;
                Center = NaN;
                Scale = NaN;
                IterDone = 0;

                for Iter = 1:Args.MaxIter
                    if ~Converged
                        ValidData = SigRes(~ClipMask);
                        Center = median(ValidData);
                        switch Args.StdFunc
                            case 'std'
                                Scale = std(ValidData);
                            case 'mad_std'
                                Scale = 1.4826 * median(abs(ValidData - median(ValidData)));
                            otherwise
                                warning('sigmaClip:UnknownStdFunc', ...
                                    'Unknown StdFunc ''%s''; falling back to std', Args.StdFunc);
                                Scale = std(ValidData);
                        end
                        NewClipMask = abs(SigRes - Center) > SigmaThresh * Scale;
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
                Performed = true;

            otherwise
                warning('sigmaClip:UnknownMethod', ...
                    'Unknown method ''%s''; skipping clipping', Args.Method);
                Info.Success = false;
                Info.ErrorMsg = sprintf('Unknown method: %s', Args.Method);
        end

        if Performed
            Info.NumOutliers = sum(OutlierMask);
        end

    catch ME
        warning('sigmaClip:RuntimeError', ...
            'Sigma clipping failed: %s; skipping clipping', ME.message);
        OutlierMask = false(N, 1);
        Info.Success = false;
        Info.ErrorMsg = ME.message;
    end
end
