% Stage 5 of the single-die chain: per-pixel PTC GAIN.
%   For each pixel, over the bright steps inside DieGainRange,
%     Var(S) = g*S + (RN^2 + g*T)
%   because the shot noise follows the COLLECTED charge Q while the measured
%   signal is S = g*(Q-T): Var(S) = g^2*Q = g*S + g*T_ADU. So the slope is the
%   conversion gain in ADU/e- and the intercept is NOT the read noise -- it is
%   the read noise plus the threshold's shot noise.
%   That makes this stage a MEASUREMENT of the threshold, not a check of one.
%   Stages 2 and 3 get their thresholds by extrapolating a response curve to
%   zero signal, which is only as good as the curve is straight there, and on
%   this device it is not: the dark route moves over 16 ADU with the fit
%   window. The shot noise instead reports the charge actually collected,
%   Q = (Var-RN^2)/g^2, against the signal recorded, S = g*(Q-T), so T follows
%   step by step with no extrapolation at all. A real threshold must then come
%   out the same at every step, which is a stronger test than any of the three
%   routes passing on its own. All three values are stored in ptc.json.
%
%   The statistics here are NOT those of the earlier stages and the difference
%   decides everything. A ladder point is a per-pixel VARIANCE from 3 frames,
%   so it carries 2 degrees of freedom and is chi2 distributed with a 100 %
%   error and a long tail -- not a Gaussian mean. Consequences, all of them
%   verified against a null simulation in which every pixel has exactly the
%   same gain:
%     * the fitted slope is unbiased in the MEAN but its MEDIAN is 10 % low,
%       because the distribution is skewed (skew 0.83, 4 % of pixels fit a
%       negative slope). The headline gain is therefore the mean. The same
%       skew puts the median intercept near -10 ADU^2 when the truth is +12.
%     * the Gaussian deconvolution of paramSpread does not apply: it pairs a
%       robust observed spread with an analytic rms, and for a heavy-tailed
%       distribution the first is much the smaller, which returns an
%       "intrinsic" spread of zero at a meaningless significance. The spread
%       is compared with the null simulation instead.
%     * one pixel's gain is good to ~70 %, so a per-pixel gain MAP is not a
%       measurement. Averaging the variances first is: one readout column
%       (4742 pixels) gives ~1 %, a 32x32 block ~2 %, and those maps are the
%       real products of this stage.
%   Weights are the ensemble model, w = dof/(2*(g_ens*S + c_ens)^2), never the
%   pixel's own measured variance: with 2 dof that would weight a point by its
%   own fluctuation. Steps are chosen by the ENSEMBLE median signal, the same
%   steps for every pixel (see desy_die_config).
%   Every ensemble number is reported with and without the stage 4 mask.
%   Output in DieOut: gain.bin, gain_var.bin, ptc_offset.bin, gchi2.bin
%   (single, [Ny Nx]), gain_col.bin (per readout column), gain_block.bin
%   (DieBlock x DieBlock blocks), ptc_cloud.bin (a subsample of the per-pixel
%   (mean, variance) pairs) and ptc.json.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 5 (PTC gain): streaming the bright ladder\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol');
P.read;
P.subtractZero;
G    = P.rawColGeom;
Siz  = [G.Ny G.Nx];
RN   = readBin(fullfile(DieStage1, 'rn.bin'), Siz, 'stage 1');
Mask = [];
if isfile(fullfile(DieOut, 'mask.bin'))
    Fid  = fopen(fullfile(DieOut, 'mask.bin'), 'r');
    Mask = logical(fread(Fid, Siz, 'uint8'));
    fclose(Fid);
end
fprintf('  %d ZE frames, %d x %d pixels, %.0f s\n', P.NZero, Siz(1), Siz(2), toc(T0));

% --- load the candidate steps once (the maps are reused by the window scan)
Flag  = strcmp(P.Frames.FrameType, 'B');
Steps = unique(P.Frames.Step(Flag)).';
Load  = max(cellfun(@(W) W(2), DieGainScan));
Mm = {};  Vv = {};  Sid = [];  Med = [];  VarEns = [];  Nrep = [];
for Is = 1:1:numel(Steps)
    A = ultrasat.lab.readPTC(DieDev, 'FrameType','B', 'Step',Steps(Is), 'Gain',DieGain);
    C = zeros([size(A(1).Image), numel(A)], 'single');
    for Ii = 1:1:numel(A)
        C(:,:,Ii) = single(A(Ii).Image);
    end
    C  = C - P.Zero;
    M  = mean(C, 3);
    Mk = median(double(M(:)), 'omitnan');
    if Mk > 1.15.*Load
        clear C M
        break
    end
    V   = var(double(C), 0, 3);
    Dof = max(size(C,3)-1, 1);
    Mm{end+1}     = M;                                                            %#ok<SAGROW>
    Vv{end+1}     = single(V);                                                    %#ok<SAGROW>
    Sid(end+1)    = Steps(Is);                                                    %#ok<SAGROW>
    Med(end+1)    = Mk;                                                           %#ok<SAGROW>
    VarEns(end+1) = median(V(:), 'omitnan').*Dof./(2.*gammaincinv(0.5, Dof./2));   %#ok<SAGROW>
    Nrep(end+1)   = size(C, 3);                                                   %#ok<SAGROW>
    fprintf('  step %2d: median %8.1f ADU, ensemble variance %8.1f ADU^2 (%d frames)\n', ...
        Steps(Is), Mk, VarEns(end), Nrep(end));
    clear C V M
end
fprintf('  %d steps held, %.0f s\n', numel(Sid), toc(T0));

% --- ensemble PTC of the step medians, over every candidate window
Scan = struct('Range',{}, 'Nsteps',{}, 'Gain',{}, 'Offset',{}, 'GainPixelMean',{}, 'OffsetPixelMean',{});
for Iw = 1:1:numel(DieGainScan)
    W   = DieGainScan{Iw};
    Sel = find(Med>=W(1) & Med<=W(2));
    if numel(Sel)<3
        continue
    end
    [Ge, Ce] = local_ens(Med(Sel), VarEns(Sel), Nrep(Sel));
    Scan(end+1) = struct('Range',W, 'Nsteps',numel(Sel), 'Gain',Ge, 'Offset',Ce, ...
                         'GainPixelMean',NaN, 'OffsetPixelMean',NaN);              %#ok<SAGROW>
end
Sel = find(Med>=DieGainRange(1) & Med<=DieGainRange(2));
if numel(Sel)<3
    error('ultrasat:lab:scripts:ptc', 'Only %d steps inside [%g %g] ADU', numel(Sel), DieGainRange);
end
[Gens, Cens] = local_ens(Med(Sel), VarEns(Sel), Nrep(Sel));
Dofs = max(Nrep(Sel)-1, 1);
fprintf('  ensemble PTC in [%g %g]: gain %.4f ADU/e-, offset %.2f ADU^2 over %d steps\n', ...
    DieGainRange, Gens, Cens, numel(Sel));

% --- per-pixel fit, and the same fit on column- and block-averaged variances
Wk = arrayfun(@(I) Dofs(I)./(2.*(Gens.*Med(Sel(I)) + Cens).^2), 1:numel(Sel));   % scalar part, per step
Sums = [];  Scol = [];  Sblk = [];
Npc  = Siz(3-G.Dim);          % pixels in one readout column
Ncol = Siz(G.Dim);            % number of readout columns
Nby  = floor(Siz(1)./DieBlock);  Nbx = floor(Siz(2)./DieBlock);
for Ii = 1:1:numel(Sel)
    Is = Sel(Ii);
    Xi = double(Mm{Is});
    Yi = double(Vv{Is});
    Wi = Dofs(Ii)./(2.*max(Gens.*Xi + Cens, 1).^2);
    Wi(~isfinite(Xi) | ~isfinite(Yi)) = 0;
    Sums = ultrasat.lab.PTCAnalysis.accumulateFit(Sums, Yi, Xi, Wi, [-Inf Inf]);
    % averaged over each raw readout column, and over blocks
    Scol = ultrasat.lab.PTCAnalysis.accumulateFit(Scol, ...
               mean(Yi, 3-G.Dim, 'omitnan'), mean(Xi, 3-G.Dim, 'omitnan'), Wk(Ii), [-Inf Inf]);
    Sblk = ultrasat.lab.PTCAnalysis.accumulateFit(Sblk, ...
               local_block(Yi, DieBlock, Nby, Nbx), local_block(Xi, DieBlock, Nby, Nbx), Wk(Ii), [-Inf Inf]);
end
F    = ultrasat.lab.PTCAnalysis.solveFit(Sums);
Fcol = ultrasat.lab.PTCAnalysis.solveFit(Scol);
Fblk = ultrasat.lab.PTCAnalysis.solveFit(Sblk);
clear Sums Scol Sblk
fprintf('  fits done (per pixel, %d columns of %d pixels, %dx%d blocks), %.0f s\n', ...
    Ncol, Npc, Nby, Nbx, toc(T0));

% --- null: what the estimator returns when every pixel has the SAME gain
Null = local_null(Med(Sel), Gens.*Med(Sel)+Cens, Dofs, 2e5, 7);
Null.ColSigma = Null.Analytic./sqrt(Npc);           % averaging makes it Gaussian
Null.BlkSigma = Null.Analytic./sqrt(DieBlock.^2);
fprintf('  null simulation done, %.0f s\n', toc(T0));

for Iw = 1:1:numel(Scan)
    Sw = find(Med>=Scan(Iw).Range(1) & Med<=Scan(Iw).Range(2));
    S2 = [];
    for Is = Sw
        Xi = double(Mm{Is});
        Yi = double(Vv{Is});
        Wi = max(Nrep(Is)-1,1)./(2.*max(Scan(Iw).Gain.*Xi + Scan(Iw).Offset, 1).^2);
        Wi(~isfinite(Xi) | ~isfinite(Yi)) = 0;
        S2 = ultrasat.lab.PTCAnalysis.accumulateFit(S2, Yi, Xi, Wi, [-Inf Inf]);
    end
    F2 = ultrasat.lab.PTCAnalysis.solveFit(S2);
    Scan(Iw).GainPixelMean   = mean(F2.Slope(:), 'omitnan');
    Scan(Iw).OffsetPixelMean = mean(F2.Intercept(:), 'omitnan');
    clear S2 F2
end

writeBin(fullfile(DieOut, 'gain.bin'),       F.Slope);
writeBin(fullfile(DieOut, 'gain_var.bin'),   F.VarSlope);
writeBin(fullfile(DieOut, 'ptc_offset.bin'), F.Intercept);
writeBin(fullfile(DieOut, 'gchi2.bin'),      F.Chi2Dof);
writeBin(fullfile(DieOut, 'gain_col.bin'),   Fcol.Slope);
writeBin(fullfile(DieOut, 'gain_block.bin'), Fblk.Slope);

rng(7);
Nsub  = 60000;
Idx   = randperm(numel(F.Slope), Nsub);
Cloud = zeros(Nsub, 2.*numel(Sid), 'single');
for Is = 1:1:numel(Sid)
    Cloud(:, 2*Is-1) = Mm{Is}(Idx);
    Cloud(:, 2*Is)   = Vv{Is}(Idx);
end
writeBin(fullfile(DieOut, 'ptc_cloud.bin'), Cloud);
clear Mm Vv Cloud

% --- summaries
S = struct('Stage',5, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, ...
           'Size',Siz, 'ReadoutDim',G.Dim, 'GainRange',DieGainRange, 'Block',DieBlock, ...
           'Steps',Sid(Sel), 'StepMedian',Med(Sel), 'StepVariance',VarEns(Sel), 'Nframes',Nrep(Sel), ...
           'CloudSteps',Sid, 'CloudMedian',Med, 'CloudN',Nsub, ...
           'GainEnsemble',Gens, 'OffsetEnsemble',Cens, 'Scan',Scan, 'Null',Null, ...
           'Ncol',Ncol, 'NpixPerCol',Npc, 'Nblock',[Nby Nbx]);
Sets = {'All', true(Siz)};
if ~isempty(P.ParityMap)
    Sets = [Sets; {'Even', ~P.ParityMap}; {'Odd', P.ParityMap}];
end
for Im = 1:1:size(Sets,1)
    for Mk = {'Unmasked','Masked'}
        Use = Sets{Im,2} & isfinite(F.Slope) & isfinite(F.Intercept);
        if strcmp(Mk{1}, 'Masked')
            if isempty(Mask), continue; end
            Use = Use & Mask;
        end
        Q = struct('Npix',nnz(Use), ...
                   'GainMean',mean(F.Slope(Use)), 'GainMedian',median(F.Slope(Use)), ...
                   'GainSE',std(F.Slope(Use))./sqrt(nnz(Use)), ...
                   'GainMAD',1.4826.*median(abs(F.Slope(Use)-median(F.Slope(Use)))), ...
                   'GainRMS',std(F.Slope(Use)), ...
                   'OffsetMean',mean(F.Intercept(Use)), 'OffsetMedian',median(F.Intercept(Use)), ...
                   'OffsetSE',std(F.Intercept(Use))./sqrt(nnz(Use)), ...
                   'MedianChi2Dof',median(F.Chi2Dof(Use), 'omitnan'));
        Q.MADoverNull = Q.GainMAD./Null.MAD;
        Q.RMSoverNull = Q.GainRMS./Null.RMS;
        Q.IntrFromMAD = sqrt(max(Q.GainMAD.^2 - Null.MAD.^2, 0));
        S.(Mk{1}).(Sets{Im,1}) = Q;
    end
end
% column and block maps: here the deconvolution is legitimate
Okc = isfinite(Fcol.Slope);
Okb = isfinite(Fblk.Slope);
S.Column = local_group(Fcol.Slope(Okc), Null.ColSigma);
S.BlockGain = local_group(Fblk.Slope(Okb), Null.BlkSigma);
if ~isempty(P.ParityMap)
    Par = G.RawCol(:);
    S.Column.Even = local_group(Fcol.Slope(Okc & mod(Par,2)==0), Null.ColSigma);
    S.Column.Odd  = local_group(Fcol.Slope(Okc & mod(Par,2)==1), Null.ColSigma);
end

Gm   = S.Unmasked.All.GainMean;
RNm  = median(double(RN(:)), 'omitnan');
Tlight = NaN;  Tdark = NaN;
if isfile(fullfile(DieOut, 'light.json'))
    L      = jsondecode(fileread(fullfile(DieOut, 'light.json')));
    Tlight = L.Threshold.Median;
end
if isfile(fullfile(DieOut, 'dark.json'))
    D     = jsondecode(fileread(fullfile(DieOut, 'dark.json')));
    Tdark = -D.Fit.All.InterceptSpread.Median;
end
Tadu = Tlight;
if strcmpi(DieThreshold, 'dark')
    Tadu = Tdark;
end
S.Closure = struct('Method',DieThreshold, 'RN',RNm, 'ThresholdADU',Tadu, 'GainMean',Gm, ...
                   'Predicted',RNm.^2 + Gm.*Tadu, 'Measured',S.Unmasked.All.OffsetMean, ...
                   'MeasuredSE',S.Unmasked.All.OffsetSE, 'MeasuredEnsemble',Cens);
S.Closure.Ratio = S.Closure.Measured./S.Closure.Predicted;
% The intercept turned round: what threshold does the PTC itself imply? This
% needs no extrapolation of a response curve -- the shot noise measures the
% charge actually collected, Q = (Var-RN^2)/g^2, against the signal recorded,
% S = g*(Q-T), so T follows step by step. A real threshold must come out the
% same at every step; a drift means the PTC is curved there and the intercept
% of a straight line through it is not a threshold at all.
S.Closure.ThresholdPTC = (S.Closure.Measured - RNm.^2)./Gm;
% All three routes are stored, whatever DieThreshold says, so that stage 6 and
% any report can carry the systematic instead of inheriting one choice.
S.Thresholds = struct('PTC_ADU',S.Closure.ThresholdPTC, 'Light_ADU',Tlight, 'Dark_ADU',Tdark, ...
                      'PTC_e',S.Closure.ThresholdPTC./Gm, 'Light_e',Tlight./Gm, 'Dark_e',Tdark./Gm, ...
                      'Selected',DieThreshold);
% The gain itself carries a window systematic: see Scan. It propagates into
% every electron-unit number, so it is recorded next to the gain.
Gs = [Scan.GainPixelMean];
S.GainSystematic = struct('Min',min(Gs), 'Max',max(Gs), 'Rel',(max(Gs)-min(Gs))./Gm);
S.PerStepThreshold = struct('Step',Sid, 'Median',Med, 'Variance',VarEns, ...
                            'ThresholdADU',(VarEns - RNm.^2)./Gm - Med);
S.GainElectrons = struct('ADUperE',Gm, 'RN_e',RNm./Gm, 'Threshold_e',Tadu./Gm);

Fid = fopen(fullfile(DieOut, 'ptc.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\nnull check (every pixel the same gain, %d draws with the real steps and weights):\n', Null.Nsim);
fprintf('  truth %.4f -> estimator mean %.4f, median %.4f (%+.1f %%), MAD %.4f, rms %.4f, analytic %.4f\n', ...
    Null.Truth, Null.Mean, Null.Median, 100.*(Null.Median./Null.Truth-1), Null.MAD, Null.RMS, Null.Analytic);
fprintf('  so the MEAN is the gain and the median is %.1f %% low by construction\n', ...
    100.*(1-Null.Median./Null.Truth));

fprintf('\n%-6s %-4s %10s %10s %10s %9s %9s %8s\n', ...
    'set', 'mask', 'g mean', 'g median', 'g MAD', 'MAD/null', 'chi2/null', 'Npix/1e6');
for Mk = {'Unmasked','Masked'}
    if ~isfield(S, Mk{1}), continue; end
    for Pn = {'All','Even','Odd'}
        if ~isfield(S.(Mk{1}), Pn{1}), continue; end
        Q = S.(Mk{1}).(Pn{1});
        fprintf('%-6s %-4s %10.4f %10.4f %10.4f %9.4f %8.3f %8.1f\n', Pn{1}, lower(Mk{1}(1:3)), ...
            Q.GainMean, Q.GainMedian, Q.GainMAD, Q.MADoverNull, ...
            Q.MedianChi2Dof./Null.Chi2DofMedian, Q.Npix./1e6);
    end
end
fprintf('\nis the gain different from pixel to pixel? observed MAD %.4f vs null %.4f (ratio %.4f)\n', ...
    S.Unmasked.All.GainMAD, Null.MAD, S.Unmasked.All.MADoverNull);
fprintf('  -> intrinsic at most %.4f ADU/e- (%.1f %% of the gain) from a single pixel\n', ...
    S.Unmasked.All.IntrFromMAD, 100.*S.Unmasked.All.IntrFromMAD./Gm);
fprintf('averaging first, where the gain IS measurable:\n');
fprintf('  per readout column (%d pixels each): median %.4f, spread %.4f, null %.4f -> intrinsic %.4f (%.2f %%)\n', ...
    Npc, S.Column.Median, S.Column.StdObs, S.Column.StdNull, S.Column.StdIntr, 100.*S.Column.RelIntr);
if isfield(S.Column, 'Even')
    fprintf('  even columns %.4f, odd %.4f, difference %+.4f ADU/e- (%+.2f %%)\n', ...
        S.Column.Even.Median, S.Column.Odd.Median, S.Column.Odd.Median-S.Column.Even.Median, ...
        100.*(S.Column.Odd.Median./S.Column.Even.Median-1));
end
fprintf('  per %dx%d block (%d blocks): median %.4f, spread %.4f, null %.4f -> intrinsic %.4f (%.2f %%)\n', ...
    DieBlock, DieBlock, numel(Fblk.Slope), S.BlockGain.Median, S.BlockGain.StdObs, ...
    S.BlockGain.StdNull, S.BlockGain.StdIntr, 100.*S.BlockGain.RelIntr);

fprintf('\nthreshold: the PTC intercept is RN^2 + g*T, so the shot noise measures T directly\n');
fprintf('  RN %.4f ADU (stage 1), T %.2f ADU (%s method), g %.4f ADU/e-\n', RNm, Tadu, DieThreshold, Gm);
fprintf('  predicted %.2f ADU^2 (RN^2 alone is only %.2f), measured %.2f +- %.2f -> ratio %.3f\n', ...
    S.Closure.Predicted, RNm.^2, S.Closure.Measured, S.Closure.MeasuredSE, S.Closure.Ratio);
fprintf('  the PTC gives T = %.2f ADU = %.1f e-, against %.2f (light) and %.2f (dark)\n', ...
    S.Closure.ThresholdPTC, S.Closure.ThresholdPTC./Gm, Tadu, ...
    Tdark);
fprintf('  per step, T = (Var-RN^2)/g - S [ADU] (constant only where the PTC is straight):\n    ');
for J = 1:1:numel(Sid)
    In = '';
    if any(Sel==J), In = '*'; end
    fprintf('%.0f:%+.1f%s  ', Med(J), S.PerStepThreshold.ThresholdADU(J), In);
end
fprintf('\n    (* = inside the fit window)\n');
fprintf('\nwindow scan (ensemble fit of the step medians; per-pixel means):\n');
fprintf('%-16s %7s %12s %12s %12s %12s\n', 'window [ADU]', 'steps', 'g ensemble', 'offset', 'g per pixel', 'offset');
for Iw = 1:1:numel(Scan)
    fprintf('%-16s %7d %12.4f %12.2f %12.4f %12.2f\n', mat2str(Scan(Iw).Range), Scan(Iw).Nsteps, ...
        Scan(Iw).Gain, Scan(Iw).Offset, Scan(Iw).GainPixelMean, Scan(Iw).OffsetPixelMean);
end
fprintf('\nin electrons: gain %.4f ADU/e- (%.1f %% window systematic), read noise %.3f e-\n', ...
    Gm, 100.*S.GainSystematic.Rel, RNm./Gm);
fprintf('  threshold %.1f e- (PTC), %.1f e- (light), %.1f e- (dark) -- stage 6 must carry all three\n', ...
    S.Thresholds.PTC_e, S.Thresholds.Light_e, S.Thresholds.Dark_e);
fprintf('[%4.0f s] PTC DONE -> %s\n', toc(T0), DieOut);

function [Ge, Ce] = local_ens(X, Y, Nrep)
    % ensemble PTC of the per-step medians, weighted by the sampling error of
    % a variance measured with Nrep-1 degrees of freedom
    Dof = max(Nrep-1, 1);
    Wv  = (Dof./(2.*Y.^2)).';
    A   = [ones(numel(X),1), X(:)];
    Cf  = (A.'*(Wv.*A))\(A.'*(Wv.*Y(:)));
    Ce  = Cf(1);
    Ge  = Cf(2);
end

function N = local_null(X, Sig2, Dof, Nsim, Seed)
    % Distribution of the fitted slope when every pixel has the same gain and
    % every point is a chi2_Dof sample variance. The analytic parameter error
    % assumes Gaussian points and so describes the rms but not the median or
    % the MAD of this distribution.
    rng(Seed);
    W  = Dof./(2.*Sig2.^2);
    Sw = sum(W);  Swx = sum(W.*X);  Swxx = sum(W.*X.^2);
    D  = Sw.*Swxx - Swx.^2;
    Swy = zeros(Nsim,1);  Swxy = zeros(Nsim,1);
    for I = 1:1:numel(X)
        Yi   = Sig2(I).*sum(randn(Nsim, Dof(I)).^2, 2)./Dof(I);
        Swy  = Swy  + W(I).*Yi;
        Swxy = Swxy + W(I).*Yi.*X(I);
    end
    Sl = (Sw.*Swxy - Swx.*Swy)./D;
    In = (Swy.*Swxx - Swx.*Swxy)./D;
    Tr = (Sig2(end)-Sig2(1))./(X(end)-X(1));
    % chi2 of the same draws: with chi2_2 points this is NOT a chi2_(n-2)
    % variable, so the measured goodness of fit has to be read against this
    rng(Seed);
    Ch = zeros(Nsim,1);
    for I = 1:1:numel(X)
        Yi = Sig2(I).*sum(randn(Nsim, Dof(I)).^2, 2)./Dof(I);
        Ch = Ch + W(I).*(Yi - In - Sl.*X(I)).^2;
    end
    Ch = Ch./max(numel(X)-2, 1);
    N  = struct('Nsim',Nsim, 'Truth',Tr, 'Mean',mean(Sl), 'Median',median(Sl), ...
                'MAD',1.4826.*median(abs(Sl-median(Sl))), 'RMS',std(Sl), ...
                'Analytic',sqrt(Sw./D), 'MedianBias',median(Sl)./Tr, ...
                'OffsetMean',mean(In), 'OffsetMedian',median(In), 'FracNegative',mean(Sl<0), ...
                'Chi2DofMedian',median(Ch));
end

function Q = local_group(V, StdNull)
    % spread of a group-averaged gain, with the null (sampling) width removed
    V = double(V(isfinite(V)));
    Md = median(V);
    Q  = struct('N',numel(V), 'Median',Md, 'Mean',mean(V), ...
                'StdObs',1.4826.*median(abs(V-Md)), 'StdNull',StdNull);
    Q.StdIntr = sqrt(max(Q.StdObs.^2 - StdNull.^2, 0));
    Q.RelIntr = Q.StdIntr./abs(Md);
end

function Bm = local_block(A, B, Nby, Nbx)
    % mean over BxB blocks, returned as [Nby Nbx]
    C  = A(1:Nby.*B, 1:Nbx.*B);
    Bm = squeeze(mean(mean(reshape(C, B, Nby, B, Nbx), 1, 'omitnan'), 3, 'omitnan'));
end

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 5 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
