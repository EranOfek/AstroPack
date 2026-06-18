function [Back, Var, Info] = modeVar_LeftHist(Image, Args)
    % Estimate the global image background mode and variance via a left-flank fit.
    %   Estimate the background level B of an image whose pixel histogram is a
    %   Gaussian sky contaminated on its positive (right) side by sources. The
    %   estimator locates the histogram peak, then fits the peak plus the
    %   (uncontaminated) left flank only, deliberately excluding the right side.
    %   To avoid a covariance between the estimated peak and width (which an
    %   asymmetric window induces in a joint Gaussian fit, opening a pathway for
    %   width errors to bias the location), the width is NOT a fit parameter:
    %   under background-limited noise it is fixed by Sigma0^2 = B/VarianceRatio.
    %   The known curvature is subtracted from log-counts and the location is a
    %   weighted LINEAR regression on the left flank (z = logN + xx^2/(2 Sigma0^2)
    %   = c + s*xx, peak at M0 + s*Sigma0^2), so the peak is orthogonal to the
    %   width by construction. The variance is then measured in a decoupled
    %   second stage with the peak FIXED (regress logN on (Xc-B)^2), so the width
    %   estimate cannot feed back into the location. The estimator targets the
    %   true background B (not the histogram mode): the left flank is built from
    %   the cleanest, smallest-deflection pixels, so in the no-source limit it
    %   returns B exactly (see histogramModel for the residual mode/B offset).
    %   If no usable left flank exists (near-empty/clean field, too few bins, or
    %   a runaway slope) it falls back to the SExtractor mode 2.5*median-1.5*mean,
    %   accurate precisely in that low-contamination limit.
    % Input  : - Image : A 2-D image (or any numeric array). Non-finite
    %                     pixels are ignored.
    %          * ...,key,val,...
    %            'VarianceRatio' - Ratio B/Var(noise) (i.e. the gain), so the
    %                     noise variance is Sigma0^2 = B/VarianceRatio. For a
    %                     pure background-limited Poisson image in photons this
    %                     is 1. Default is 1.
    %            'BinFactor' - Histogram bin width in units of Sigma0.
    %                     Default is 0.2 (~5 bins per sigma).
    %            'RangeLo' - Lower histogram extent in units of Sigma0 below
    %                     the provisional center. Default is 5.
    %            'RangeHi' - Upper histogram extent in units of Sigma0 above
    %                     the provisional center. Default is 5.
    %            'WinLo'  - Fit-window extent below the peak, in Sigma0. Wide,
    %                     to anchor the clean rising flank. Default is 3.
    %            'WinHi'  - Fit-window extent above the peak, in Sigma0. The
    %                     sensitive knob: keep small so faint-source fill-in
    %                     does not bias the level up. Default is 0.5.
    %            'SmoothBins' - Width (bins) of the moving-average smoothing of
    %                     the histogram before peak finding. 0 disables.
    %                     Default is 3.
    %            'Niter'  - Number of refinement passes (recenter window and
    %                     Sigma0 on the fitted level, refit). Default is 1.
    %            'MinBins' - Minimum number of populated bins in the fit window
    %                     required to attempt the fit, else fallback.
    %                     Default is 5.
    % Output : - Back : Estimated background level B (targets B). From the
    %                   fixed-sigma linear location fit (orthogonal to width).
    %          - Var  : Background noise variance, measured in the decoupled
    %                   second stage (peak fixed). Compare to Info.VarPred;
    %                   Var >> VarPred flags residual contamination / a
    %                   non-background-limited field.
    %          - Info : Structure with diagnostics:
    %                   .Method ('fit' or 'fallback'), .Mode (raw histogram
    %                   peak), .Sigma0 (final working scale), .VarPred
    %                   (=Back/VarianceRatio), .Npix, .Nbins (used in fit),
    %                   .Niter, .Median, .Mean.
    % Author : Claude + Eran Ofek (Jun 2026)
    % Example: Image = 1000 + sqrt(1000)*randn(1024,1024);
    %          [Back,Var,Info] = imUtil.background.modeVar_LeftHist(Image);
    %          % with sources:
    %          Image(randperm(numel(Image),5e4)) = Image(randperm(numel(Image),5e4)) + 300;
    %          [Back,Var] = imUtil.background.modeVar_LeftHist(Image, 'VarianceRatio',1);

    arguments
        Image
        Args.VarianceRatio      = 1
        Args.BinFactor          = 0.2
        Args.RangeLo            = 5
        Args.RangeHi            = 5
        Args.WinLo              = 3
        Args.WinHi              = 0.5
        Args.SmoothBins         = 3
        Args.Niter              = 1
        Args.MinBins            = 5
    end

    % --- prepare the sample -------------------------------------------------
    X = Image(:);
    X = X(isfinite(X));
    Npix = numel(X);
    assert(Npix >= 10, 'Too few finite pixels for a background estimate.');

    MedX  = median(X);
    MeanX = mean(X);

    % Working scale from the physics (background-limited). If the provisional
    % level is non-positive (e.g. background-subtracted data), fall back to a
    % left-side MAD which only uses the clean half of the distribution.
    Sigma0 = local_scale(X, MedX, Args.VarianceRatio);

    Center   = MedX;
    Method   = 'fallback';
    BackFit  = NaN;
    VarFit   = NaN;
    ModeRaw  = NaN;
    NbinsUse = 0;
    MaskKeep = [];                                          % window of last good fit
    XcKeep   = [];
    CountKeep= [];

    % --- iterate: histogram -> peak -> fixed-sigma linear location fit -------
    % Sigma is NOT a fit parameter (it is set by Sigma0^2 = B/VarianceRatio),
    % so the location (slope) is orthogonal to the width by construction; there
    % is no peak/sigma covariance. The known curvature is subtracted and the
    % left flank is regressed linearly: z = logN + xx^2/(2 Sigma0^2) = c + s*xx,
    % with the peak at M0 + s*Sigma0^2.
    for Iter = 0:Args.Niter
        H = Args.BinFactor .* Sigma0;                       % bin width
        Edges = (Center - Args.RangeLo.*Sigma0) : H : (Center + Args.RangeHi.*Sigma0);
        if numel(Edges) < 3
            break;                                          % degenerate scale
        end
        Counts = histcounts(X, Edges);
        Xc     = Edges(1:end-1) + H./2;                     % bin centers (row)

        % Smooth for a stable peak location.
        if Args.SmoothBins >= 1
            Cs = movmean(Counts, round(Args.SmoothBins));
        else
            Cs = Counts;
        end
        [~, Ipk] = max(Cs);
        M0       = Xc(Ipk);
        ModeRaw  = M0;

        % Asymmetric window: wide clean left, tiny crossing to the right.
        Lo   = M0 - Args.WinLo .* Sigma0;
        Hi   = M0 + Args.WinHi .* Sigma0;
        Mask = (Xc >= Lo) & (Xc <= Hi) & (Counts > 0);
        NbinsUse = nnz(Mask);
        if NbinsUse < Args.MinBins
            break;                                          % -> fallback
        end

        % Fixed-sigma location: weighted linear regression of z on xx.
        % Poisson: Var(log N) ~ 1/N, so weight = N.
        xx = (Xc(Mask) - M0).';
        yy = log(Counts(Mask)).';
        w  = Counts(Mask).';
        z  = yy + xx.^2 ./ (2.*Sigma0.^2);                  % remove known curvature

        Sw   = sum(w);
        Xbar = sum(w.*xx) ./ Sw;
        Zbar = sum(w.*z)  ./ Sw;
        Sxx  = sum(w .* (xx - Xbar).^2);
        Sxz  = sum(w .* (xx - Xbar) .* (z - Zbar));
        if ~(Sxx > 0) || ~isfinite(Sxz)
            break;                                          % -> fallback
        end
        Slope    = Sxz ./ Sxx;
        CandBack = M0 + Slope .* Sigma0.^2;                 % peak location

        if ~(isfinite(CandBack) && CandBack >= Lo && CandBack <= Hi)
            break;                                          % runaway -> fallback
        end

        BackFit  = CandBack;
        Method   = 'fit';
        MaskKeep = Mask;  XcKeep = Xc;  CountKeep = Counts;  % for the Var stage

        % Refine: recenter and rescale Sigma0 from the new level (decoupled).
        Center = BackFit;
        Sigma0 = local_scale(X, BackFit, Args.VarianceRatio);
    end

    % --- decoupled variance measurement (peak fixed) ------------------------
    % With the location fixed, regress logN on (Xc-Back)^2 over the same clean
    % window: logN = c - q*uu, uu=(Xc-Back)^2, sigma^2 = 1/(2q). Because Back is
    % already pinned, this width estimate cannot feed back into the location.
    if strcmp(Method, 'fit')
        uu = ((XcKeep(MaskKeep) - BackFit).^2).';
        yv = log(CountKeep(MaskKeep)).';
        wv = CountKeep(MaskKeep).';
        Ubar = sum(wv.*uu) ./ sum(wv);
        Ybar = sum(wv.*yv) ./ sum(wv);
        Suu  = sum(wv .* (uu - Ubar).^2);
        Suy  = sum(wv .* (uu - Ubar) .* (yv - Ybar));
        if Suu > 0
            Q = -Suy ./ Suu;                                % slope is -q
            if Q > 0 && isfinite(Q)
                VarFit = 1 ./ (2.*Q);
            end
        end
    end

    % --- fallback for ill-conditioned / clean fields ------------------------
    if ~strcmp(Method, 'fit') || ~isfinite(BackFit)
        Method  = 'fallback';
        BackFit = 2.5.*MedX - 1.5.*MeanX;                    % SExtractor mode
        VarFit  = max(BackFit, 0) ./ Args.VarianceRatio;     % predicted variance
    elseif ~isfinite(VarFit)
        VarFit  = max(BackFit, 0) ./ Args.VarianceRatio;     % Var stage failed
    end

    % --- outputs ------------------------------------------------------------
    Back = BackFit;
    Var  = VarFit;                                           % decoupled measurement

    Info          = struct();
    Info.Method   = Method;
    Info.Mode     = ModeRaw;
    Info.Sigma0   = Sigma0;
    Info.VarPred  = max(Back, 0) ./ Args.VarianceRatio;      % from level + VR
    Info.Npix     = Npix;
    Info.Nbins    = NbinsUse;
    Info.Niter    = Args.Niter;
    Info.Median   = MedX;
    Info.Mean     = MeanX;
end

% ------------------------------------------------------------------------
function S0 = local_scale(X, Level, VarianceRatio)
    % Working scale: from the background-limited relation Sigma0^2=B/VR when
    % the level is positive, else a left-side (clean half) MAD.
    if Level > 0
        S0 = sqrt(Level ./ VarianceRatio);
    else
        Xl = X(X < Level);
        if numel(Xl) >= 5
            S0 = 1.4826 .* median(Level - Xl);
        else
            S0 = 1.4826 .* median(abs(X - Level));           % last resort
        end
    end
    if ~isfinite(S0) || S0 <= 0
        S0 = max(std(X), eps);                               % final guard
    end
end