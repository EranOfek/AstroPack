% Stage 3 of the single-die chain: per-pixel RESPONSE, PRNU and LIGHT THRESHOLD.
%   Whole die, streamed (see desy_die_dark for the mechanics).
%   Signal(int) = R*int + I_B per pixel over the steps of DieFitStepsB, all at
%   the same sensor exposure, so
%     R       = Slope                  [ADU/int]  photo-response per pixel
%     T_light = DC*ExpSen - I_B        [ADU]      charge threshold, light method
%   with Var(T_light) = ExpSen^2*Var(DC) + Var(I_B): the dark current and its
%   fit variance come from stage 2 (dc.bin, dc_var.bin), the two fits being
%   independent. The light method is the one that does not need the dark
%   ladder to be linear at the bottom.
%   The PRNU is NOT taken from the spread of R. The published bright window
%   holds three closely spaced steps, which fixes a pixel's slope to a few per
%   cent -- far coarser than the ~0.5 % pattern being measured -- so that
%   spread is almost all fit noise. It is measured instead from the spatial
%   spread of each step's mean map with the temporal noise removed
%   (stepFixedPattern over all 34 bright steps), where one step already
%   determines the pattern from 22.5 M pixels.
%   As in stage 2, the spread over the whole die is a total non-uniformity
%   dominated by large-scale structure, and the local (block-detrended) spread
%   is the pixel-to-pixel term a noise budget needs.
%   Settings: ultrasat.lab.scripts.desy_die_config. Output in DieOut:
%   resp.bin, tlight.bin, resp_var.bin, bchi2.bin (single, [Ny Nx],
%   column-major), bnused.bin (uint8) and light.json.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 3 (light): streaming the whole die\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', ...
                             'FitSteps',struct('D',DieFitStepsD, 'B',DieFitStepsB), ...
                             'GainADU',DieGainADU, 'Verbosity',1);
P.read;
P.subtractZero;
fprintf('  %d ZE frames, %d x %d pixels, bias %.3f ADU, exposure %.4g s, %.0f s\n', ...
    P.NZero, size(P.Zero,1), size(P.Zero,2), median(P.Zero(:)), P.ExpSen, toc(T0));

Ref = local_stage1(DieStage1, DieRun, Die, DieGain, size(P.Zero));
if ~isempty(Ref)
    dBias = median(P.Zero(:)) - Ref.BiasLevelAll;
    dRN   = median(P.ZeroNoise(:))./Ref.All.ReadNoiseMedianRaw - 1;
    fprintf('  stage 1 agrees: d(bias) %+.3f ADU, d(RN median) %+.3f %%\n', dBias, 100.*dRN);
    if abs(dBias)>0.05 || abs(dRN)>1e-3
        error('ultrasat:lab:scripts:stage1', 'stage 1 dump disagrees with the frames just read');
    end
end
[DC, VarDC, Dark] = local_stage2(DieOut, DieRun, Die, DieGain, size(P.Zero));
fprintf('  stage 2: dark current median %.4f ADU/s over %d steps\n', ...
    median(DC(isfinite(DC))), numel(Dark.FitSteps));

B = P.perPixelFits('B');
fprintf('  per-pixel response fit done (%d steps), %.0f s\n', numel(B.Steps), toc(T0));
F = P.stepFixedPattern('B');
fprintf('  per-step fixed pattern done (%d steps), %.0f s\n', numel(F.Step), toc(T0));

% light-method threshold, with both fit variances propagated
Tlight    = DC.*P.ExpSen - B.Intercept;
VarTlight = (P.ExpSen.^2).*VarDC + B.VarIntercept;
Ok  = isfinite(Tlight) & isfinite(VarTlight);
Spt = ultrasat.lab.PTCAnalysis.paramSpread(Tlight(Ok), VarTlight(Ok), 'Robust',true);

Lr = ultrasat.lab.PTCAnalysis.localSpread(B.Slope, 'Block',DieBlock, 'StdFit',B.All.SlopeSpread.StdFitRobust);
Lt = ultrasat.lab.PTCAnalysis.localSpread(Tlight,  'Block',DieBlock, 'StdFit',Spt.StdFitRobust);
Chi2Exp = 2.*gammaincinv(0.5, max(numel(B.Steps)-2,1)./2)./max(numel(B.Steps)-2,1);

G = P.rawColGeom;
writeBin(fullfile(DieOut, 'resp.bin'),     B.Slope);
writeBin(fullfile(DieOut, 'resp_var.bin'), B.VarSlope);
writeBin(fullfile(DieOut, 'tlight.bin'),   Tlight);
writeBin(fullfile(DieOut, 'bchi2.bin'),    B.Chi2Dof);
writeBin(fullfile(DieOut, 'bnused.bin'),   uint8(min(B.Nused, 255)), 'uint8');

S = struct('Stage',3, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, ...
           'Size',size(P.Zero), 'ReadoutDim',G.Dim, 'Mode',P.Mode, ...
           'ExpSen',P.ExpSen, 'IntensityScale',P.IntensityScale, ...
           'GainADU',DieGainADU, 'FitSteps',B.Steps, 'Intensity',B.X, ...
           'StepMedian',B.StepMedian, 'VarStep',B.VarStep, 'Weights',B.Weights, ...
           'Chi2DofExpected',Chi2Exp, 'SatLevel',P.SatLevel, 'PatternFitMax',0.75.*P.SatLevel);
for Pn = {'All','Even','Odd'}
    if isfield(B, Pn{1})
        S.Fit.(Pn{1})     = B.(Pn{1});
        S.Pattern.(Pn{1}) = F.(Pn{1});
    end
end
S.PatternStep = F.Step;  S.PatternX = F.X;  S.PatternNframes = F.Nframes;
S.Threshold = Spt;
S.Local = struct('Resp',Lr, 'T',Lt);
S.PRNU  = struct('Multiplicative',F.All.Multiplicative, 'Err',F.All.MultiplicativeErr, ...
                 'Additive',F.All.Additive, 'AdditiveErr',F.All.AdditiveErr, ...
                 'Nsteps',F.All.PatternNsteps, 'TopStep',F.Step(end), ...
                 'SlopeRel',B.All.SlopeSpread.RelIntr);
Fid = fopen(fullfile(DieOut, 'light.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\n%-5s %9s %12s %11s %10s %11s %8s %8s\n', ...
    'set', 'Npix', 'R [ADU/int]', 'spread(R)', 'of the die', 'PRNU', 'chi2', 'resid');
for Pn = {'All','Even','Odd'}
    if ~isfield(B, Pn{1}), continue; end
    Q = B.(Pn{1});  Sl = Q.SlopeSpread;
    fprintf('%-5s %9d %12.1f %11.1f %8.2f %%  %9.3f %% %8.2f %8.3f\n', Pn{1}, Q.Npix, ...
        Sl.Median, Sl.StdIntr, 100.*Sl.RelIntr, 100.*F.(Pn{1}).Multiplicative, ...
        Q.MedianChi2Dof, Q.MedianResidRMS);
end
fprintf('\nPRNU %.4f +- %.4f %% (a + b*S fit over %d steps below %.0f ADU; additive %.2f ADU)\n', ...
    100.*F.All.Multiplicative, 100.*F.All.MultiplicativeErr, F.All.PatternNsteps, ...
    0.75.*P.SatLevel, F.All.Additive);
fprintf('  the response slope alone would read %.2f %% -- %d closely spaced steps, almost all fit noise\n', ...
    100.*B.All.SlopeSpread.RelIntr, numel(B.Steps));
fprintf('light threshold: median %.2f ADU, observed spread %.2f, fit noise %.2f, intrinsic %.2f ADU\n', ...
    Spt.Median, Spt.StdRobust, Spt.StdFitRobust, Spt.StdIntr);
fprintf('  pixel-to-pixel (%dx%d blocks detrended): T %.2f ADU, response %.2f %%\n', ...
    Lt.Block, Lt.Block, Lt.StdIntr, 100.*Lr.RelIntr);
fprintf('  dark method gave %.2f ADU (stage 2); the two methods differ by %.2f ADU\n', ...
    -Dark.Fit.All.InterceptSpread.Median, Spt.Median + Dark.Fit.All.InterceptSpread.Median);
fprintf('chi2/dof median %.3f against %.3f expected for exact weights (%+.1f %%)\n', ...
    B.All.MedianChi2Dof, Chi2Exp, 100.*(B.All.MedianChi2Dof./Chi2Exp - 1));
if ~isempty(DieGainADU) && isfinite(DieGainADU)
    fprintf('electrons (gain %.4f ADU/e-): T_light %.2f e-\n', DieGainADU, Spt.Median./DieGainADU);
end
fprintf('[%4.0f s] LIGHT DONE -> %s\n', toc(T0), DieOut);

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end

function R = local_stage1(Dir, Run, Die, Gain, Siz)
    R = [];
    Path = fullfile(Dir, 'stats.json');
    if ~isfile(Path)
        return
    end
    R = jsondecode(fileread(Path));
    if ~strcmp(R.Run, Run) || ~strcmp(R.Die, Die) || ~strcmp(R.GainHalf, Gain) || ~isequal(R.Size(:).', Siz)
        error('ultrasat:lab:scripts:stage1', 'stage 1 dump %s is not run %s %s %s %dx%d', ...
              Path, Run, Die, Gain, Siz(1), Siz(2));
    end
end

function [DC, VarDC, R] = local_stage2(Dir, Run, Die, Gain, Siz)
    % dark current and its fit variance, validated against this dataset
    Path = fullfile(Dir, 'dark.json');
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage2', ...
              'stage 3 needs the dark current: run ultrasat.lab.scripts.desy_die_dark first (%s)', Path);
    end
    R = jsondecode(fileread(Path));
    if ~strcmp(R.Run, Run) || ~strcmp(R.Die, Die) || ~strcmp(R.GainHalf, Gain) || ~isequal(R.Size(:).', Siz)
        error('ultrasat:lab:scripts:stage2', 'stage 2 dump %s is not run %s %s %s %dx%d', ...
              Path, Run, Die, Gain, Siz(1), Siz(2));
    end
    DC    = readBin(fullfile(Dir, 'dc.bin'),     Siz);
    VarDC = readBin(fullfile(Dir, 'dc_var.bin'), Siz);
end

function A = readBin(Path, Siz)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage2', 'missing %s: rerun stage 2', Path);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
