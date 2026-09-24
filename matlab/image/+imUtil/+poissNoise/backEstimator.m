function [bhat, Result] = backEstimator(img, Args)
% Robust background estimation for low-count (Poisson) images, e.g. X-ray.
% Description: Estimate the background rate b [counts/pixel] of a low-count
%              image in which most pixels are background ~ Poisson(b) and a
%              minority contain sources (which only ADD counts, i.e. one-sided
%              positive contamination). In this regime the sample mean is not
%              robust (sources bias it up) and the median/MAD degenerate to 0
%              (for b < ln2 more than half the pixels are zero). Instead this
%              routine uses the lowest count bins, which are dominated by
%              background, via one of several estimators:
%                'ratio'    : b = N1/N0.  P(1)/P(0)=b for Poisson. This is the
%                             maximum-likelihood estimator conditional on a
%                             pixel being in {0,1}, and it is self-correcting
%                             against bright sources (which populate neither the
%                             0 nor the 1 bin). Near-optimal variance ~ b/N when
%                             b is small. Most robust choice.
%                'zerofrac' : b = -log(N0/Npix).  Uses only the zero bin; very
%                             low variance but more biased by sources than
%                             'ratio' (sources deplete N0 with no compensation).
%                'censored' : MLE of b using only pixels with count <= c, with
%                             the truncated-Poisson normalisation. c=1 reduces
%                             to 'ratio'. Larger c is more efficient but lets
%                             fainter sources leak in -> robustness/efficiency
%                             knob. Solved by bisection on the truncated mean.
%                'mean'     : plain sample mean (Poisson MLE). Efficient, NOT
%                             robust. Provided for comparison.
%              The generalised ratios r_k=(k+1)*N_{k+1}/N_k all estimate b for a
%              clean Poisson field; their disagreement is a diagnostic of source
%              contamination or a non-Poisson background, and is returned.
%              The estimate can be computed globally or locally on a grid of
%              cells (to follow a spatially varying background) via 'CellSize'.
%              For an extended PSF, faint source flux is spread thinly over many
%              pixels and leaks into the 1-count bin, biasing the estimator up.
%              Setting 'BinSize'>1 first sums counts in BinSize x BinSize blocks
%              (a sum of m=BinSize^2 independent Poisson(b) pixels is
%              Poisson(m*b), still Poisson), runs the estimator on the binned
%              image, and divides the result by the bin area m. Binning on a
%              scale comparable to the PSF gathers each source's diffuse flux
%              into a few high-count blocks that drop out of the low bins,
%              reducing source bias; the cost is a rising variance once m*b
%              leaves the deep low-count regime, so the useful bin size is of
%              order the PSF size.
% Input  : - img : 2-D image of non-negative integer counts.
%          * Arbitrary number of pairs of arguments: ...,keyword,value,...
%            where keyword are one of the followings:
%            'Method'    - 'ratio'|'zerofrac'|'censored'|'mean'. Default 'ratio'.
%            'CMax'      - c for the 'censored' method (>=1). Default 1.
%            'CellSize'  - [] for a single global estimate, or a scalar / [ny nx]
%                          giving the side of square / rectangular cells over
%                          which b is estimated locally. When 'BinSize'>1 this
%                          is interpreted in units of binned pixels. Default [].
%            'BinSize'   - Linear block size for pre-binning (positive integer).
%                          1 (default) means no binning. m=BinSize^2 pixels are
%                          summed per block; the estimator runs on the binned
%                          image and the rate is divided by m. Image edges that
%                          do not fill a whole block are trimmed. Useful for
%                          removing extended/large-PSF sources (see Description).
%            'MinPix'    - Minimum number of pixels in a cell to attempt an
%                          estimate; otherwise NaN. Default 100.
%            'BiasCorr'  - For 'ratio', apply the first-order finite-sample
%                          ratio bias correction. Default true.
%            'NRatios'   - Number of diagnostic ratios r_0..r_(NRatios-1) to
%                          return. Default 4.
% Output : - bhat : Background estimate per ORIGINAL pixel [counts/pixel].
%                   Scalar if global, else a matrix of per-cell estimates
%                   (size = number of cells in y,x). When BinSize>1 this is the
%                   binned-image estimate divided by the bin area.
%          - Result : struct with fields:
%                .bhat        - same as bhat (per original pixel)
%                .method      - method used
%                .BinSize, .BinArea - binning used (BinArea=BinSize^2)
%                .N0,N1,N2    - bin counts of the (possibly binned) image used:
%                               number of blocks with 0,1,2 counts (global) /
%                               per-cell (local, cell array)
%                .Npix        - number of blocks/pixels used (global or per cell)
%                .err         - approximate 1-sigma uncertainty on bhat
%                               (per original pixel)
%                .ratios      - r_k=(k+1) N_{k+1}/N_k, k=0..NRatios-1, expressed
%                               per original pixel (global)
%                .ratio_consistency - max|r_k-r_0| over returned ratios (global);
%                               large value flags contamination / non-Poisson.
%                .cell_centers_y, .cell_centers_x - for local mode (binned-pixel
%                               coordinates when BinSize>1).
% License: GNU general public license version 3
% Tested : Matlab R2021b
%     By : (statistical method & MATLAB implementation)             Jun 2026
%    URL :
% Example: % global, bright sources present -> 'ratio' stays unbiased
%          img = poissrnd(0.05, 200, 200);
%          k = randperm(numel(img), 200); img(k) = img(k) + poissrnd(5,200,1);
%          [b, R] = imUtil.poissNoise.backEstimator(img, 'Method','ratio');
%          % local 32x32 cells:
%          bmap = imUtil.poissNoise.backEstimator(img, 'Method','censored','CMax',2, ...
%                                     'CellSize',32);
%          % pre-bin 4x4 to suppress an extended PSF's diffuse source flux:
%          b = imUtil.poissNoise.backEstimator(img, 'Method','ratio', 'BinSize',4);
% Reliable: 2
%--------------------------------------------------------------------------

    arguments
        img                 {mustBeNumeric}
        Args.Method  char   = 'ratio'
        Args.CMax           = 1
        Args.CellSize       = []
        Args.MinPix         = 100
        Args.BiasCorr       = true
        Args.NRatios        = 4
        Args.BinSize        = 1
    end

    binSide = round(Args.BinSize);
    if binSide < 1
        error('BinSize must be a positive integer (>=1).');
    end
    binArea = binSide^2;

    if any(img(:) < 0 | (mod(img(:),1) ~= 0 & ~isnan(img(:))))
        warning('lowcount_background:noninteger', ...
                'Image contains non-integer or negative values; counts expected.');
    end

    % ---- optional pre-binning: sum counts in binSide x binSide blocks ----
    % A sum of m=binSide^2 independent Poisson(b) pixels is Poisson(m*b); we
    % estimate m*b on the binned image and divide by m at the end.
    if binSide > 1
        img = bin_image(img, binSide);
    end

    if isempty(Args.CellSize)
        % -------- global estimate --------
        [bhat, S] = local_estimate(img, Args);
        S.method = Args.Method;
        Result = S;
        % convert rate-valued quantities from per-block to per original pixel
        bhat              = bhat / binArea;
        Result.bhat       = bhat;
        Result.err        = Result.err / binArea;
        Result.ratios     = Result.ratios / binArea;
        Result.ratio_consistency = Result.ratio_consistency / binArea;
    else
        % -------- local estimate on a grid of cells --------
        cs = Args.CellSize;
        if isscalar(cs), cs = [cs, cs]; end
        [ny, nx] = size(img);
        ey = 1:cs(1):ny+1;  if ey(end) ~= ny+1, ey = [ey, ny+1]; end
        ex = 1:cs(2):nx+1;  if ex(end) ~= nx+1, ex = [ex, nx+1]; end
        ncy = numel(ey) - 1;  ncx = numel(ex) - 1;
        bhat = nan(ncy, ncx);
        N0 = nan(ncy,ncx); N1 = nan(ncy,ncx); N2 = nan(ncy,ncx); Npix = nan(ncy,ncx);
        err = nan(ncy,ncx);
        ccy = nan(ncy,1); ccx = nan(ncx,1);
        for iy = 1:ncy
            ccy(iy) = 0.5*(ey(iy)+ey(iy+1)-1);
            for ix = 1:ncx
                ccx(ix) = 0.5*(ex(ix)+ex(ix+1)-1);
                cell = img(ey(iy):ey(iy+1)-1, ex(ix):ex(ix+1)-1);
                [bc, Sc] = local_estimate(cell, Args);
                bhat(iy,ix) = bc / binArea;     % per original pixel
                N0(iy,ix)=Sc.N0; N1(iy,ix)=Sc.N1; N2(iy,ix)=Sc.N2;
                Npix(iy,ix)=Sc.Npix; err(iy,ix)=Sc.err / binArea;
            end
        end
        Result = struct('bhat',bhat,'method',Args.Method,'N0',N0,'N1',N1, ...
                        'N2',N2,'Npix',Npix,'err',err, ...
                        'cell_centers_y',ccy,'cell_centers_x',ccx);
    end
    Result.BinSize = binSide;
    Result.BinArea = binArea;
end

% =====================================================================
% Core single-region estimator
% =====================================================================
function [bhat, S] = local_estimate(region, Args)
    v = region(:);
    v = v(~isnan(v));
    Npix = numel(v);

    % bin counts of the low orders only. Clip any higher counts (e.g. source
    % pixels) into a single overflow bin so they don't enlarge the array; the
    % low-count estimators never use them.
    maxk = max(2, Args.NRatios);     % ensure we have enough for diagnostics
    vclip = min(v, maxk + 1);        % counts > maxk+1 fold into the last bin
    bc = accumarray(vclip + 1, 1, [maxk + 2, 1], @sum, 0);  % bc(k+1)=N_k
    N0 = bc(1); N1 = bc(2); N2 = bc(3);

    switch lower(Args.Method)
        case 'mean'
            bhat = mean(v);

        case 'zerofrac'
            if N0 > 0 && Npix > 0
                bhat = -log(N0 / Npix);
            else
                bhat = NaN;   % no zero pixels -> not in the low-count regime
            end

        case 'ratio'
            if N0 > 0
                bhat = N1 / N0;
                if Args.BiasCorr && N0 > 0
                    % first-order finite-sample correction for E[N1/N0]:
                    % E[R] ~ b * (1 + (1-p0)/(N p0)) with p0=e^{-b}.
                    % Use the plug-in p0hat=N0/Npix to deflate.
                    p0 = N0 / Npix;
                    if p0 > 0
                        bhat = bhat / (1 + (1 - p0) / (Npix * p0));
                    end
                end
            else
                bhat = NaN;
            end

        case 'censored'
            c = max(1, round(Args.CMax));
            bhat = censored_mle(v, c);

        otherwise
            error('Unknown Method "%s".', Args.Method);
    end

    if Npix < Args.MinPix
        bhat = NaN;
    end

    % approximate 1-sigma error. For 'ratio'/low b, Var ~ b/Npix; inflate
    % slightly for the ratio (factor ~ sqrt(1+b)).
    if isnan(bhat) || Npix == 0
        err = NaN;
    else
        err = sqrt(max(bhat,0) / Npix);
        if strcmpi(Args.Method,'ratio')
            err = err * sqrt(1 + max(bhat,0));
        end
    end

    % diagnostic ratios r_k=(k+1) N_{k+1}/N_k
    nr = max(1, Args.NRatios);
    ratios = nan(nr,1);
    for k = 0:nr-1
        nk = bc(k+1); nk1 = bc(k+2);
        if nk > 0
            ratios(k+1) = (k+1) * nk1 / nk;
        end
    end
    if any(~isnan(ratios)) && ~isnan(ratios(1))
        dr = abs(ratios - ratios(1));
        ratio_consistency = max(dr(~isnan(dr)));
    else
        ratio_consistency = NaN;
    end

    S = struct('bhat',bhat,'method',Args.Method,'N0',N0,'N1',N1,'N2',N2, ...
               'Npix',Npix,'err',err,'ratios',ratios, ...
               'ratio_consistency',ratio_consistency);
end

% =====================================================================
% Censored MLE on bins {0..c}: find b s.t. truncated-Poisson mean matches
% the observed mean of counts <= c. Bisection (no toolbox needed).
% =====================================================================
function b = censored_mle(v, c)
    kept = v(v <= c);
    Nt = numel(kept);
    if Nt == 0
        b = NaN; return;
    end
    kbar = mean(kept);
    ks = (0:c).';
    % truncated-Poisson mean m(b)=sum k p_k / sum p_k, p_k=e^{-b}b^k/k!
    gap = @(b) trunc_mean(b, ks) - kbar;

    if kbar <= 0
        b = 0; return;       % all kept pixels are zero
    end
    % bracket the root
    lo = 1e-9; hi = 1.0;
    flo = gap(lo);
    fhi = gap(hi);
    it = 0;
    while flo * fhi > 0 && hi < 1e6 && it < 100
        hi = hi * 2; fhi = gap(hi); it = it + 1;
    end
    if flo * fhi > 0
        b = NaN; return;
    end
    % bisection
    for it = 1:200
        mid = 0.5*(lo+hi);
        fm = gap(mid);
        if abs(fm) < 1e-12 || (hi-lo) < 1e-12
            break;
        end
        if flo * fm <= 0
            hi = mid; fhi = fm; %#ok<NASGU>
        else
            lo = mid; flo = fm;
        end
    end
    b = 0.5*(lo+hi);
end

function m = trunc_mean(b, ks)
    % numerically stable truncated-Poisson mean over k in ks=0..c
    c = ks(end);
    logpk = -b + ks*log(max(b,realmin)) - gammaln(ks+1);
    pk = exp(logpk - max(logpk));      % unnormalised, stabilised
    m = sum(ks .* pk) / sum(pk);
end

% =====================================================================
% Sum counts in s x s blocks. Image edges that do not fill a whole block
% are trimmed (so every block sums exactly s^2 pixels). Returns a smaller
% integer-count image that is still Poisson (rate s^2 * b for background).
% =====================================================================
function out = bin_image(img, s)
    [ny, nx] = size(img);
    ny2 = floor(ny / s) * s;
    nx2 = floor(nx / s) * s;
    if ny2 < ny || nx2 < nx
        warning('lowcount_background:trim', ...
            ['Image size [%d %d] not divisible by BinSize=%d; ' ...
             'trimming to [%d %d].'], ny, nx, s, ny2, nx2);
    end
    if ny2 == 0 || nx2 == 0
        error('BinSize=%d is larger than the image/cell dimension.', s);
    end
    A = img(1:ny2, 1:nx2);
    out = squeeze(sum(sum(reshape(A, s, ny2/s, s, nx2/s), 1), 3));
end