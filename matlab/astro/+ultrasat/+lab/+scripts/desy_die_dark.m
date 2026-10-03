% Stage 2 of the single-die chain: per-pixel DARK CURRENT and DARK THRESHOLD.
%   Whole die, streamed: the dark ladder is read one step at a time and only
%   the running sums of the weighted per-pixel fit are kept (see
%   ultrasat.lab.PTCAnalysis.perPixelFits in full mode).
%   Signal(t) = DC*t + I_D per pixel over the steps of DieFitStepsD, with
%   each point weighted by its MEASURED variance (the shot noise of a ladder
%   point follows the collected charge, which no model of the measured signal
%   reproduces when a threshold eats the first electrons). From the fit:
%     DC      = Slope                  [ADU/s]  dark current per pixel
%     T_dark  = -Intercept             [ADU]    charge threshold, dark method
%   and the spread of both with the analytic fit noise removed (paramSpread),
%   which is the only way to compare setups whose ladders have different lever
%   arms. DSNU per step comes from stepFixedPattern.
%   Two spreads are reported and they mean different things. The spread over
%   the WHOLE die is dominated by large-scale structure (on this device the
%   dark current ramps 0.53 -> 0.25 ADU/s across the readout columns), so it
%   is a total non-uniformity, not the pixel-to-pixel DSNU that a noise budget
%   needs. The LOCAL spread removes everything above the block scale (a block
%   median, 32x32 by default) and then takes out the fit noise, which leaves
%   the genuine pixel-to-pixel term.
%   Settings: ultrasat.lab.scripts.desy_die_config. Output in DieOut:
%   dc.bin, tdark.bin, dc_var.bin (the fit variance of the dark current, which
%   stage 3 propagates into the light threshold), chi2.bin (single, [Ny Nx],
%   column-major), nused.bin (uint8), rawcol.bin (int32, raw readout column of
%   every image row) and dark.json.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s stage 2 (dark): streaming the whole die\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol', ...
                             'FitSteps',struct('D',DieFitStepsD, 'B',DieFitStepsB), ...
                             'GainADU',DieGainADU, 'Verbosity',1);
P.read;
P.subtractZero;
fprintf('  %d ZE frames, %d x %d pixels, bias %.3f ADU, RN median %.4f ADU, %.0f s\n', ...
    P.NZero, size(P.Zero,1), size(P.Zero,2), median(P.Zero(:)), median(P.ZeroNoise(:)), toc(T0));

% --- stage 1 cross-check (the read noise of these very pixels sets the fit weights)
Ref = local_stage1(DieStage1, DieRun, Die, DieGain, size(P.Zero));
if ~isempty(Ref)
    dBias = median(P.Zero(:)) - Ref.BiasLevelAll;
    dRN   = median(P.ZeroNoise(:))./Ref.All.ReadNoiseMedianRaw - 1;
    fprintf('  stage 1 agrees: d(bias) %+.3f ADU, d(RN median) %+.3f %%\n', dBias, 100.*dRN);
    if abs(dBias)>0.05 || abs(dRN)>1e-3
        error('ultrasat:lab:scripts:stage1', 'stage 1 dump disagrees with the frames just read');
    end
else
    fprintf('  no stage 1 dump in %s: nothing to cross-check\n', DieStage1);
end

D = P.perPixelFits('D');
fprintf('  per-pixel dark fit done, %.0f s\n', toc(T0));
F = P.stepFixedPattern('D');
fprintf('  per-step fixed pattern done, %.0f s\n', toc(T0));

Ldc = localSpread(D.Slope,     32, D.All.SlopeSpread.StdFitRobust);
Lt  = localSpread(-D.Intercept, 32, D.All.InterceptSpread.StdFitRobust);
Chi2Exp = 2.*gammaincinv(0.5, max(numel(D.Steps)-2,1)./2)./max(numel(D.Steps)-2,1);

G = P.rawColGeom;
writeBin(fullfile(DieOut, 'dc.bin'),     D.Slope);
writeBin(fullfile(DieOut, 'tdark.bin'), -D.Intercept);
writeBin(fullfile(DieOut, 'dc_var.bin'), D.VarSlope);      % fit variance, for the stage 3 threshold
writeBin(fullfile(DieOut, 'chi2.bin'),   D.Chi2Dof);
writeBin(fullfile(DieOut, 'nused.bin'),  uint8(min(D.Nused, 255)), 'uint8');
writeBin(fullfile(DieOut, 'rawcol.bin'), int32(G.RawCol), 'int32');

% --- summary (no maps in the json)
S = struct('Stage',2, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, ...
           'Size',size(P.Zero), 'ReadoutDim',G.Dim, 'Mode',P.Mode, ...
           'NZero',P.NZero, 'BiasLevel',median(P.Zero(:)), 'RNMedian',median(P.ZeroNoise(:)), ...
           'GainADU',DieGainADU, 'FitSteps',D.Steps, 'ExpTime',D.X, ...
           'StepMedian',D.StepMedian, 'VarStep',D.VarStep, 'Weights',D.Weights);
for Pn = {'All','Even','Odd'}
    if isfield(D, Pn{1})
        S.Fit.(Pn{1})     = D.(Pn{1});
        S.Pattern.(Pn{1}) = F.(Pn{1});
    end
end
S.PatternStep = F.Step;  S.PatternX = F.X;  S.PatternNframes = F.Nframes;
S.Local = struct('DC',Ldc, 'T',Lt);
S.Chi2DofExpected = Chi2Exp;
Fid = fopen(fullfile(DieOut, 'dark.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf('\n%-5s %9s %10s %11s %10s %9s %9s %8s %8s\n', ...
    'set', 'Npix', 'DC [ADU/s]', 'spread(DC)', 'of the die', 'T [ADU]', 'spread(T)', 'chi2', 'resid');
for Pn = {'All','Even','Odd'}
    if ~isfield(D, Pn{1}), continue; end
    Q  = D.(Pn{1});
    Sl = Q.SlopeSpread;  In = Q.InterceptSpread;
    fprintf('%-5s %9d %10.4f %11.4f %8.2f %%  %9.2f %9.2f %8.2f %8.3f\n', Pn{1}, Q.Npix, ...
        Sl.Median, Sl.StdIntr, 100.*Sl.RelIntr, -In.Median, In.StdIntr, ...
        Q.MedianChi2Dof, Q.MedianResidRMS);
end
fprintf('\nfit noise removed: DC %.4f -> %.4f ADU/s, T %.2f -> %.2f ADU (observed -> intrinsic)\n', ...
    D.All.SlopeSpread.StdRobust, D.All.SlopeSpread.StdIntr, ...
    D.All.InterceptSpread.StdRobust, D.All.InterceptSpread.StdIntr);
fprintf('spread over the whole die %.2f %% of the median, but that is large-scale structure;\n', ...
    100.*D.All.SlopeSpread.RelIntr);
fprintf('  pixel-to-pixel (%dx%d blocks detrended, fit noise removed): DC %.2f %%, T %.2f ADU\n', ...
    Ldc.Block, Ldc.Block, 100.*Ldc.Rel, Lt.Intr);
fprintf('chi2/dof median %.3f against %.3f expected for exact weights (%+.1f %%)\n', ...
    D.All.MedianChi2Dof, Chi2Exp, 100.*(D.All.MedianChi2Dof./Chi2Exp - 1));
fprintf('per-step dark fixed pattern (median signal: fixed / signal):\n  ');
for J = 1:1:numel(F.Step)
    fprintf('%.0f:%.2f%% ', F.All.Median(J), 100.*F.All.RelFixed(J));
end
fprintf('\n  additive %.3f ADU, multiplicative %.3f %% (%d steps)\n', ...
    F.All.Additive, 100.*F.All.Multiplicative, F.All.PatternNsteps);
if ~isempty(DieGainADU) && isfinite(DieGainADU)
    % the conversion gain is measured in stage 5; until then it is a setting
    fprintf('electrons (gain %.4f ADU/e-): DC %.4f e-/s, T %.2f e-\n', DieGainADU, ...
        D.All.SlopeSpread.Median./DieGainADU, -D.All.InterceptSpread.Median./DieGainADU);
end
fprintf('[%4.0f s] DARK DONE -> %s\n', toc(T0), DieOut);

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end

function R = localSpread(M, B, StdFit)
    % Pixel-to-pixel spread of a map: the robust spread of the residual to a
    % BxB block median, with the fit noise of the individual pixels removed in
    % quadrature. Everything varying on scales above B pixels -- gradients,
    % banding, the bright patches of a dark-current map -- is absorbed by the
    % block median and so does not enter, which is what distinguishes this
    % from the spread over the whole die.
    [Ny, Nx] = size(M);
    ny = floor(Ny./B).*B;
    nx = floor(Nx./B).*B;
    C  = double(M(1:ny, 1:nx));
    C  = reshape(permute(reshape(C, B, ny./B, B, nx./B), [1 3 2 4]), B.*B, []);
    Med = median(C, 1, 'omitnan');
    Res = C - Med;
    Res = Res(isfinite(Res));
    Obs = 1.4826.*median(abs(Res - median(Res)));
    R = struct('Block',B, 'Level',median(Med,'omitnan'), 'StdObs',Obs, 'StdFit',StdFit, ...
               'Intr',sqrt(max(Obs.^2 - StdFit.^2, 0)));
    R.Rel = R.Intr./abs(R.Level);
end

function R = local_stage1(Dir, Run, Die, Gain, Siz)
    % stage 1 summary, validated against the dataset actually being processed
    R = [];
    Path = fullfile(Dir, 'stats.json');
    if ~isfile(Path)
        return
    end
    R = jsondecode(fileread(Path));
    if ~strcmp(R.Run, Run) || ~strcmp(R.Die, Die) || ~strcmp(R.GainHalf, Gain) || ~isequal(R.Size(:).', Siz)
        error('ultrasat:lab:scripts:stage1', ...
              'stage 1 dump %s is run %s %s %s %dx%d, not run %s %s %s %dx%d', ...
              Path, R.Run, R.Die, R.GainHalf, R.Size(1), R.Size(2), Run, Die, Gain, Siz(1), Siz(2));
    end
end
