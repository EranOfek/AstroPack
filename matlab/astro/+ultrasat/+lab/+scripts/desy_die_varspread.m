% Stage 7 of the single-die chain: how much do the per-pixel VARIANCES differ?
%   For every step of both ladders (and for the bias frames as the zero-signal
%   point) this takes the distribution of the per-pixel temporal variance over
%   the whole die and compares it with the distribution the same measurement
%   would give if every pixel were identical.
%   The comparison is necessary because the estimator is very wide on its own.
%   A variance from Dof+1 frames is V = T*chi2(Dof)/Dof, so
%     E[V]   = E[T]
%     Var[V] = Var[T]*(1 + 2/Dof) + (2/Dof)*E[T]^2
%   and with 3 frames (Dof = 2) the second term alone gives sd/mean = 1: a
%   100 % spread with no pixel-to-pixel variation whatsoever. Any real spread
%   of the true variance T is a small excess on top of that, which is what
%   varSpread deconvolves.
%   The null is SIMULATED rather than analytic, because the frames are
%   integers: a variance built from 3 integers can only take multiples of
%   1/18, so at the low steps, where sigma is a couple of ADU, the measured
%   distribution is a comb. Drawing Gaussian samples about a mean with a
%   realistic fractional part and ROUNDING them as the detector does
%   reproduces that comb; a continuous chi2 null cannot, and the difference
%   would be read as pixel-to-pixel structure.
%   The spread must be measured ROBUSTLY. A cosmic ray in one of three frames
%   puts that pixel's variance at 10^7 ADU^2, and on the long dark steps the
%   mean-based sd of V is carried almost entirely by such pixels: dropping the
%   top 10^-5 of them takes it from 2714 times the chi2 expectation to 3.0.
%   Those are transients, not a property of the pixel, so the comparison is
%   made on a TRIMMED width: the standard deviation of V after dropping the
%   top 0.1 %, divided by the median, with exactly the same rule applied to
%   the null so the comparison stays like for like. The tail that was trimmed
%   is then reported separately as what it is.
%   A median-based width will not do either, and for a different reason: a
%   variance built from three integers can only take multiples of 1/18, so the
%   MAD of V is itself quantised and ties exactly with the null on several
%   steps. The trimmed standard deviation averages over millions of those
%   quantised values and is smooth.
%   The intrinsic spread is then deconvolved NUMERICALLY rather than by the
%   analytic formula of varSpread, which is mean-based and so inherits the
%   same contamination: nulls are simulated with a lognormal spread s injected
%   into the true variance, and s is interpolated from the one whose MAD/median
%   matches the measurement.
%   Reported per step, with and without the stage 4 column mask:
%     MAD/median measured and null  - how much wider than identical pixels
%     RelIntr                       - deconvolved spread of the true variance
%     Sigma                         - its significance
%     Tail                          - fraction above 2, 10 and 100 x the median
%   Output in DieOut: varspread.json (statistics and histograms; no maps).
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 7 (variance distributions): streaming both ladders\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol');
P.read;
P.subtractZero;
Siz  = size(P.Zero);
Mask = [];
if isfile(fullfile(DieOut, 'mask.bin'))
    Fid  = fopen(fullfile(DieOut, 'mask.bin'), 'r');
    Mask = logical(fread(Fid, Siz, 'uint8'));
    fclose(Fid);
end
fprintf('  %d x %d pixels, %d ZE frames, %.0f s\n', Siz(1), Siz(2), P.NZero, toc(T0));

Nsim  = 1e6;                       % synthetic pixels, shared by the null and the grid
Sgrid = [0 0.02 0.05 0.08 0.12 0.16 0.2 0.3 0.4 0.6 0.8 1.2];   % injected relative spread of T
Trim  = 1e-3;                      % top fraction dropped from data and null alike
Edges = linspace(0, 6, 601);       % histogram grid, in units of the measured median
rng(17);

Jobs = [{'ZE', 0}];
for Ty = {'D','B'}
    Flag = strcmp(P.Frames.FrameType, Ty{1});
    for St = unique(P.Frames.Step(Flag)).'
        Jobs(end+1,:) = {Ty{1}, St};                                   %#ok<SAGROW>
    end
end

R = {};      % one entry per step; a cell keeps the field order free
for Ij = 1:1:size(Jobs,1)
    Ty = Jobs{Ij,1};
    St = Jobs{Ij,2};
    A  = ultrasat.lab.readPTC(DieDev, 'Test',P.Test, 'FrameType',Ty, 'Gain',DieGain, ...
                              'Step',local_step(St));
    C = zeros([size(A(1).Image), numel(A)], 'single');
    for Ii = 1:1:numel(A)
        C(:,:,Ii) = single(A(Ii).Image);
    end
    Nf  = size(C, 3);
    Dof = max(Nf-1, 1);
    V   = var(double(C), 0, 3);                 % variance is unchanged by the bias subtraction
    Sg  = median(double(mean(C,3)) - double(P.Zero), 'all', 'omitnan');
    clear C
    Med = median(V(:), 'omitnan');
    if ~isfinite(Med) || Med<=0
        fprintf('  %s step %2d: median variance %.3g, skipped\n', Ty, St, Med);
        continue
    end
    % the common true variance of the null: the chi2-median-corrected median,
    % which the noisy-pixel tail cannot move
    Sig2 = Med.*Dof./(2.*gammaincinv(0.5, Dof./2));

    E = struct();
    E.Type = Ty;  E.Step = St;  E.Signal = Sg;  E.Nframes = Nf;  E.Dof = Dof;
    E.Saturated = Sg > 0.9.*P.SatLevel;
    E.SigmaNull = sqrt(Sig2);
    % Null and calibration grid from ONE set of random draws (common random
    % numbers): identical pixels at s = 0, and the same measurement when the
    % true variance itself spreads by a known relative amount. Sharing the
    % draws makes W(s) - W(0) a smooth function of s instead of burying it in
    % simulation noise -- at high signal the excess being measured is a few
    % parts in a thousand of W. The frames are rounded to integers exactly as
    % the detector does them.
    Zg = randn(Nsim, Nf);
    Ug = rand(Nsim, 1);
    Tg = randn(Nsim, 1);
    Wg = zeros(size(Sgrid));
    Vn = [];
    for Ig = 1:1:numel(Sgrid)
        Tv = Sig2;
        if Sgrid(Ig)>0
            Sl = sqrt(log(1 + Sgrid(Ig).^2));
            Tv = Sig2.*exp(Sl.*Tg - 0.5.*Sl.^2);
        end
        Vg     = var(round(Ug + sqrt(Tv).*Zg), 0, 2);
        Wg(Ig) = local_shape(Vg, Trim);
        if Sgrid(Ig)==0
            Vn = Vg;
        end
    end
    E.Grid = struct('Spread',Sgrid, 'Shape',Wg);
    E.Null = struct('Nsim',Nsim, 'Mean',mean(Vn), 'Median',median(Vn), 'Std',std(Vn), ...
                    'MAD',1.4826.*median(abs(Vn-median(Vn))), 'StdAnalytic',sqrt(2./Dof).*Sig2, ...
                    'Shape',Wg(1), 'Tail10',mean(Vn>10.*median(Vn)), ...
                    'Tail100',mean(Vn>100.*median(Vn)));
    % Sampling error of the width. The null's own simulation error dominates:
    % the data has 22.5 M pixels against the null's Nsim, so comparing the two
    % is limited by how well the null itself is known, and counting only the
    % data's error would turn simulation noise into a detection.
    Nb  = 20;
    Wb  = zeros(1, Nb);
    Chunk = floor(Nsim./Nb);
    for Ib = 1:1:Nb
        Wb(Ib) = local_shape(Vn((Ib-1).*Chunk + (1:Chunk)), Trim);
    end
    SEb = std(Wb)./sqrt(Nb);
    E.Null.ShapeSENull = SEb;
    E.Null.ShapeSEData = SEb.*sqrt(Nsim./numel(V));
    E.Null.ShapeSE     = sqrt(SEb.^2 + E.Null.ShapeSEData.^2);
    E.Unmasked = local_stats(V, [], Dof, E);
    if ~isempty(Mask)
        E.Masked = local_stats(V, Mask, Dof, E);
    else
        E.Masked = [];
    end
    % histograms on a common grid, in units of the measured median
    E.Hist = struct('Edges',Edges, 'Median',Med, ...
                    'Unmasked',histcounts(double(V(:))./Med, Edges), ...
                    'Null',histcounts(Vn./Med, Edges));
    if ~isempty(Mask)
        E.Hist.Masked = histcounts(double(V(Mask))./Med, Edges);
    end
    R{end+1} = E;                                                      %#ok<SAGROW>
    Sat = '';
    if E.Saturated
        Sat = '  SATURATED';
    end
    fprintf('  %-2s step %2d: signal %9.1f ADU, median V %9.2f, width %6.4f vs null %6.4f, intrinsic %5.1f %% (UL %5.1f %%, %.0f sigma), >10x %6.3f %% vs %5.3f%s\n', ...
        Ty, St, Sg, Med, E.Unmasked.Shape, E.Null.Shape, 100.*E.Unmasked.RelIntr, ...
        100.*E.Unmasked.RelIntrUL95, E.Unmasked.Sigma, 100.*E.Unmasked.Tail10, ...
        100.*E.Null.Tail10, Sat);
end

S = struct('Stage',7, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, 'Size',Siz, ...
           'Nsim',Nsim, 'Steps',{R});   % {R}: a cell value in struct() would make an array
Fid = fopen(fullfile(DieOut, 'varspread.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf(['\nMAD/median of the per-pixel variance against the identical-pixel null.\n', ...
    'The intrinsic spread is the lognormal width of the true variance that reproduces the\n', ...
    'measured width; the tail columns are the pixels above 10 times the median, measured\n', ...
    'which on the dark steps are mostly cosmic rays in one of three frames.\n\n']);
fprintf('%-3s %5s %10s %9s %9s %9s %9s %8s %8s %9s %9s\n', ...
    'set', 'step', 'signal', 'median V', 'width', 'null', 'intr [%]', 'UL95', 'masked', '>10x [%]', 'null >10x');
for Ie = 1:1:numel(R)
    E = R{Ie};
    Im = NaN;
    if ~isempty(E.Masked)
        Im = 100.*E.Masked.RelIntr;
    end
    fprintf('%-3s %5d %10.1f %9.2f %9.4f %9.4f %9.1f %8.1f %8.1f %9.3f %9.3f%s\n', ...
        E.Type, E.Step, E.Signal, E.Hist.Median, E.Unmasked.Shape, E.Null.Shape, ...
        100.*E.Unmasked.RelIntr, 100.*E.Unmasked.RelIntrUL95, Im, ...
        100.*E.Unmasked.Tail10, 100.*E.Null.Tail10, local_sat(E.Saturated));
end
% What the measured spread should be: the variance inherits the
% non-uniformity of whatever dominates it, so on the dark ladder it should
% approach the spread of the dark current itself.
if isfile(fullfile(DieOut, 'dark.json'))
    Dk = jsondecode(fileread(fullfile(DieOut, 'dark.json')));
    Id = find(cellfun(@(E) strcmp(E.Type,'D'), R), 1, 'last');
    if ~isempty(Id)
        fprintf(['\nAt the top of the dark ladder the variance is almost all dark signal, so its\n', ...
            'pixel-to-pixel spread should be that of the dark current: measured %.1f %% here\n', ...
            'against %.1f %% for the dark current over the die (%.1f %% pixel to pixel).\n', ...
            'At zero signal the variance is read noise instead, and spreads %.0f %%.\n'], ...
            100.*R{Id}.Unmasked.RelIntr, 100.*Dk.Fit.All.SlopeSpread.RelIntr, ...
            100.*Dk.Local.DC.RelIntr, 100.*R{1}.Unmasked.RelIntr);
    end
end
fprintf(['\nAbove a few hundred ADU the measurement is limited by how well the null itself is\n', ...
    'known: its width carries a simulation error of about %.4f from %g draws, against %.1e\n', ...
    'for the %0.1f M measured pixels, so the upper limits there say more than the point values.\n'], ...
    R{end}.Null.ShapeSENull, Nsim, R{end}.Null.ShapeSEData, numel(P.Zero)./1e6);
fprintf('[%4.0f s] VARSPREAD DONE -> %s\n', toc(T0), DieOut);

function T = local_sat(F)
    T = '';
    if F
        T = '  sat';
    end
end

function St = local_step(S)
    St = S;
    if S==0
        St = [];
    end
end

function S = local_invert(W, Sp, Wq)
    % the injected spread whose width matches Wq (0 below the grid, the last
    % grid point above it)
    S = 0;
    if Wq <= W(1)
        return
    end
    if Wq >= W(end)
        S = Sp(end);
        return
    end
    S = interp1(W, Sp, Wq, 'pchip');
end

function W = local_shape(V, Trim)
    % trimmed width of a variance distribution, in units of its median: the
    % standard deviation after the top Trim fraction is dropped. The same rule
    % is applied to the data and to the null, so the comparison is like for
    % like, and the cosmic rays that carry almost all of the untrimmed
    % variance are excluded from both.
    V  = sort(double(V(:)));
    N  = numel(V);
    Nk = max(floor(N.*(1-Trim)), 2);
    W  = std(V(1:Nk))./median(V);
end

function V = local_sim(Sig2, Nf, N, Spread)
    % N synthetic pixels whose TRUE variance is Sig2 (times a lognormal of
    % relative width Spread), measured from Nf integer frames about a mean
    % with a uniform fractional part
    T = Sig2;
    if Spread>0
        Sl = sqrt(log(1 + Spread.^2));
        T  = Sig2.*exp(Sl.*randn(N,1) - 0.5.*Sl.^2);
    end
    V = var(round(rand(N,1) + sqrt(T).*randn(N, Nf)), 0, 2);
end

function Q = local_stats(V, Mask, Dof, E)
    if isempty(Mask)
        Vv = double(V(:));
    else
        Vv = double(V(Mask));
    end
    Vv = Vv(isfinite(Vv));
    Q  = ultrasat.lab.PTCAnalysis.varSpread(Vv, Dof);   % mean-based, kept but outlier-driven
    Q.MeanBasedRelIntr = Q.RelIntr;
    Q.Median  = median(Vv);
    Q.MAD     = 1.4826.*median(abs(Vv - Q.Median));
    Q.Shape   = local_shape(Vv, 1e-3);
    Q.Quantiles = quantile(Vv, [0.05 0.25 0.5 0.75 0.95 0.999])./Q.Median;
    Q.Tail2   = mean(Vv > 2.*Q.Median);
    Q.Tail10  = mean(Vv > 10.*Q.Median);
    Q.Tail100 = mean(Vv > 100.*Q.Median);
    % interpolate the injected spread that reproduces the measured shape
    [Wu, Iu] = unique(E.Grid.Shape);
    Su       = E.Grid.Spread(Iu);
    Q.RelIntr = local_invert(Wu, Su, Q.Shape);
    % 95 % upper limit: the spread that the width plus 1.645 sampling errors
    % would imply. Where the width curve is flat -- high signal, where the
    % variance is nearly all shot noise -- this is the number that means
    % something and the point value does not.
    Q.RelIntrUL95 = local_invert(Wu, Su, Q.Shape + 1.645.*E.Null.ShapeSE);
    Q.AboveGrid   = Q.Shape >= Wu(end);
    Q.Sigma       = (Q.Shape - E.Null.Shape)./max(E.Null.ShapeSE, eps);
end
