% Read noise of a single die, measured over the WHOLE die from the ZE frames.
%   One configuration at a time; the defaults are run 32, W04_D07, high-gain
%   half, DESY orientation. Nothing but the zero-exposure frames is read, so
%   this is quick compared with a full ladder reduction.
%   Reports, split by raw readout-column parity (odd = detector columns
%   1,3,5...): the bias level and its fixed pattern, the per-pixel read noise
%   and its distribution, the intrinsic pixel-to-pixel spread of that noise
%   with the chi2 sampling scatter deconvolved, the tail of noisy pixels, the
%   per-frame common mode, and the row / column / residual decomposition of
%   the bias map with its lag autocorrelations.
%   With only 5 ZE frames each pixel's sigma carries 4 degrees of freedom and
%   so about 50 % sampling scatter: the medians are solid but the WIDTH of the
%   observed distribution is mostly sampling, which is why varSpread is used
%   and why the plotting script overlays the all-pixels-identical expectation.
%   Output in desy_rn/<tag>/: rn.bin, rn_raw.bin and bias.bin (single,
%   [Ny Nx], column-major), rawcol.bin (int32, the raw readout-column index of
%   every image row) and stats.json.
% A driver can select the dataset by defining DieSelect (Run, Folder, Die, Gain)
% before running this file -- the same struct the desy_die_* stages take through
% desy_die_config -- so that one batch driver sets the dataset once for the whole
% chain. Without it the defaults below apply.
if exist('DieSelect', 'var') && isstruct(DieSelect)
    Run    = DieSelect.Run;
    Folder = DieSelect.Folder;
    Die    = DieSelect.Die;
    Gain   = DieSelect.Gain;
else
    Run    = '32';
    Folder = 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27';
    Die    = 'W04_D07';
    Gain   = 'high';
end
Root   = '/Data1/DESY';
if ~isfolder(Root)
    Root = '/bigdata3/projects/ultrasat/DESY';
end
Tag    = sprintf('run%s_%s_%s', Run, Die, Gain);
OutDir = fullfile('/home/sasha/claude/desy_rn', Tag);
if ~isfolder(OutDir), mkdir(OutDir); end

T0 = tic;
fprintf('%s: reading the ZE frames of the whole die\n', Tag);
P = ultrasat.lab.PTCAnalysis(fullfile(Root, Folder, ['LOT_TH02954_', Die]), ...
                             'CCDSEC',[], 'Gain',Gain, 'Parity','rawcol');
P.read;
P.subtractZero;
fprintf('  %d ZE frames, %d x %d pixels, %.0f s\n', P.NZero, size(P.Zero,1), size(P.Zero,2), toc(T0));

Z = P.zeroNoiseStats('Maps',true);
fprintf('  statistics done, %.0f s\n', toc(T0));

G = P.rawColGeom;
writeBin(fullfile(OutDir, 'rn.bin'),      Z.Maps.Sigma);
writeBin(fullfile(OutDir, 'rn_raw.bin'),  Z.Maps.SigmaRaw);
writeBin(fullfile(OutDir, 'bias.bin'),    Z.Maps.Bias);
writeBin(fullfile(OutDir, 'rawcol.bin'),  int32(G.RawCol), 'int32');

% Distribution of the bias map, and the four quite different things that get
% called "the error on the bias level":
%   one pixel's estimate   RN/sqrt(Nframes)
%   spread over pixels     the fixed pattern, already in FixedPatternRMS
%   the die-level value    limited by the frame-to-frame common mode, not by the
%                          pixel count -- with 22.5 M pixels the spatial average
%                          is free and only the number of FRAMES matters
%   the quantisation       a mean of Nframes integers lands on multiples of
%                          1/Nframes, so the median is granular at that step and
%                          the mean is not
Bm = double(Z.Maps.Bias(:));
Bm = Bm(isfinite(Bm));
Bmed = median(Bm);
Z.BiasDist = struct('Mean',mean(Bm), 'Median',Bmed, 'Std',std(Bm), ...
                    'MAD',1.4826.*median(abs(Bm - Bmed)), ...
                    'QuantileP',[0.01 0.25 0.5 0.75 0.99], ...
                    'Quantiles',quantile(Bm, [0.01 0.25 0.5 0.75 0.99]), ...
                    'TailFrac5MAD',mean(abs(Bm-Bmed) > 5.*1.4826.*median(abs(Bm-Bmed))), ...
                    'Quantisation',1./Z.Nframes, ...
                    'ErrOnePixel',Z.All.ReadNoiseMedian./sqrt(Z.Nframes), ...
                    'ErrDieFromPixels',std(Bm)./sqrt(numel(Bm)), ...
                    'ErrDieFromCommonMode',Z.CommonMode.Std./sqrt(Z.Nframes));
Z.BiasDist.ErrDieTotal = hypot(Z.BiasDist.ErrDieFromPixels, Z.BiasDist.ErrDieFromCommonMode);

S = rmfield(Z, 'Maps');
S.Tag = Tag;  S.Run = Run;  S.Die = Die;  S.GainHalf = Gain;
S.Size = size(P.Zero);  S.ReadoutDim = G.Dim;
S.Lot = P.Info.Lot;  S.Wafer = P.Info.Wafer;  S.Device = P.Info.Device;
S.BiasLevelAll = P.ZeroStats.BiasLevel;
S.Pass = P.Sidecar.Result.Trailer.Pass;
Fid = fopen(fullfile(OutDir, 'stats.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

for Pn = {'All','Even','Odd'}
    Q = Z.(Pn{1});
    fprintf('%-5s N %9d  bias %7.2f  FPN %6.3f  RN med %6.3f rms %6.3f robust %6.3f  tail(>2x) %5.2f %%  intrinsic spread %5.1f %% (UL %5.1f %%, %.0f sigma)\n', ...
        Pn{1}, Q.Npix, Q.BiasLevel, Q.FixedPatternRMS, Q.ReadNoiseMedian, Q.ReadNoiseRMS, ...
        Q.ReadNoiseRobust, 100*Q.TailFrac, 100*Q.SpreadSigmaRel, 100*Q.SpreadSigmaUL95, Q.Spread.Sigma);
end
fprintf('common mode: mean %.2f  std %.3f  ptp %.3f ADU over %d frames\n', ...
    Z.CommonMode.Mean, Z.CommonMode.Std, Z.CommonMode.PtP, Z.Nframes);
fprintf('bias structure: rows %.3f  cols %.3f  residual %.3f ADU; lag1 along dim1 %.3f, dim2 %.3f (readout dim %d)\n', ...
    Z.Structure.RowMeanStd, Z.Structure.ColMeanStd, Z.Structure.ResidStd, ...
    Z.Structure.LagDim1(1), Z.Structure.LagDim2(1), Z.Structure.ReadoutDim);
fprintf('[%4.0f s] RN DONE -> %s\n', toc(T0), OutDir);

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end
