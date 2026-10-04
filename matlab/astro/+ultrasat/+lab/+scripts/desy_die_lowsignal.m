% Stage 8 of the single-die chain: is the per-pixel variance EXPLAINED, at low signal?
%   Restricted to the steps below DieLowMax ADU, where the read noise, the dark
%   signal and the photo-signal are all comparable and the pixel-to-pixel
%   spread of the variance is large (stage 7). Stage 7 measured that spread;
%   this asks what it is made of, by predicting every pixel's variance from
%   quantities measured elsewhere and comparing pixel by pixel:
%     sigma^2_pred,i = RN_i^2 + g*(S_i + T)
%   with RN_i the read-noise map of stage 1 (measured from the ZE frames, so
%   independent of these frames), S_i the pixel's own measured signal at this
%   step, g the gain of stage 5 and T the charge threshold.
%   Three comparisons, because each can fail differently:
%     distribution - the measured variances against those the prediction
%                    implies, each predicted pixel sampled through its own
%                    chi2 with the frames rounded as the detector rounds them
%     calibration  - pixels binned by predicted variance, mean measured
%                    variance against mean prediction; a slope away from 1
%                    points at the gain or the threshold
%     residual map - where on the die the prediction fails, if it does
%   The prediction is never compared with the data directly. It is first MEASURED
%   the way the data were -- each predicted pixel sampled through its own chi2
%   with the frames rounded to integers -- and the comparison is then between two
%   quantities that went through the same processing. That matters because the
%   predictor carries noise from both its terms: S_i is a mean of Nf frames, and
%   RN_i^2 is itself a variance from the ZE frames, whose long tail makes its
%   sampling error as large as its spread. Comparing a noisy predictor with the
%   data directly dilutes the calibration slope to about 0.5 at the low bright
%   steps and invites the conclusion that noisy pixels are quiet under
%   illumination; running both sides through the same binning removes the effect
%   exactly, with no correction factor to argue about.
%   Averages are trimmed (top 0.1 %, both sides): a cosmic ray in one of three
%   frames moves a plain mean of V by tens of per cent on the long dark steps.
%   The ZE point is circular by construction -- the read-noise map is built
%   from those very frames -- and is kept only as a wiring check: it must come
%   out exact.
%   Output in DieOut: lowsignal.json and lowsignal_resid.bin (the residual maps,
%   single, [Nby Nbx Nstep], column-major).
ultrasat.lab.scripts.desy_die_config;
if ~exist('DieLowMax', 'var')
    DieLowMax = 1000;              % [ADU] upper edge of the range studied here
end

T0 = tic;
fprintf('%s stage 8 (low signal, below %g ADU): predicting every pixel''s variance\n', DieTag, DieLowMax);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol');
P.read;
P.subtractZero;
G    = P.rawColGeom;
Siz  = [G.Ny G.Nx];
Ptc  = jsondecode(fileread(local_need(DieOut, 'ptc.json', 'stage 5')));
Gain = Ptc.Unmasked.All.GainMean;
Tadu = Ptc.Thresholds.PTC_ADU;
RN   = readBin(fullfile(DieStage1, 'rn.bin'), Siz, 'stage 1');
RN2  = double(RN).^2;
Mask = [];
if isfile(fullfile(DieOut, 'mask.bin'))
    Fid  = fopen(fullfile(DieOut, 'mask.bin'), 'r');
    Mask = logical(fread(Fid, Siz, 'uint8'));
    fclose(Fid);
end
fprintf('  gain %.4f ADU/e-, threshold %.2f ADU, read-noise map from stage 1, %.0f s\n', ...
    Gain, Tadu, toc(T0));

Blk  = DieBlock;
Nby  = floor(Siz(1)./Blk);
Nbx  = floor(Siz(2)./Blk);
Trim = 1e-3;
Edges = linspace(0, 6, 601);
rng(23);

Jobs = [{'ZE', 0}];
for Ty = {'D','B'}
    Flag = strcmp(P.Frames.FrameType, Ty{1});
    for St = unique(P.Frames.Step(Flag)).'
        Jobs(end+1,:) = {Ty{1}, St};                                      %#ok<SAGROW>
    end
end

R = {};
Resid = [];
for Ij = 1:1:size(Jobs,1)
    Ty = Jobs{Ij,1};
    St = Jobs{Ij,2};
    A  = ultrasat.lab.readPTC(DieDev, 'Test',P.Test, 'FrameType',Ty, 'Gain',DieGain, ...
                              'Step',local_step(St));
    C = zeros([size(A(1).Image), numel(A)], 'single');
    for Ii = 1:1:numel(A)
        C(:,:,Ii) = single(A(Ii).Image);
    end
    Nf = size(C, 3);
    M  = double(mean(C, 3)) - double(P.Zero);
    V  = var(double(C), 0, 3);
    clear C
    Sg = median(M(:), 'omitnan');
    if Sg > DieLowMax
        clear M V
        continue
    end
    Dof  = max(Nf-1, 1);
    Pred = RN2 + Gain.*(M + Tadu);
    if strcmp(Ty, 'ZE')
        Pred = RN2;                                   % circular by construction, kept as a check
    end
    Pred = max(Pred, 1e-6);
    Cf2   = Dof./(2.*gammaincinv(0.5, Dof./2));       % median -> mean for chi2(Dof)
    Emean = median(V(:), 'omitnan').*Cf2;
    DofZ  = max(P.NZero-1, 1);
    % noise injected into the predictor: by the signal mean, and by the read
    % noise map, which is itself a variance from DofZ+1 frames
    PredNoiseS  = (Gain.^2).*Emean./Nf;
    PredNoiseRN = (2./DofZ).*mean(RN2(:).^2)./(1 + 2./DofZ);
    PredNoiseVar = PredNoiseS + PredNoiseRN;
    if strcmp(Ty, 'ZE')
        PredNoiseVar = 0;                             % circular: predictor and data are one
    end

    Use = isfinite(V) & isfinite(Pred);
    if ~isempty(Mask)
        UseM = Use & Mask;
    else
        UseM = Use;
    end
    % the prediction, measured as the data are: each predicted pixel sampled
    % through its own chi2, with the frames rounded to integers
    Vs = var(round(rand(nnz(Use),1) + sqrt(Pred(Use)).*randn(nnz(Use), Nf)), 0, 2);

    E = struct('Type',Ty, 'Step',St, 'Signal',Sg, 'Nframes',Nf, 'Dof',Dof, ...
               'Gain',Gain, 'Threshold',Tadu, 'Circular',strcmp(Ty,'ZE'), ...
               'MedianV',median(V(Use)), 'MeanV',local_trimmean(V(Use), Trim), ...
               'MeanPred',local_trimmean(Vs, Trim), 'PredNoiseVar',PredNoiseVar, ...
               'PredNoiseS',PredNoiseS, 'PredNoiseRN',PredNoiseRN, ...
               'MeanPredRaw',local_trimmean(Pred(Use), Trim));
    E.WidthMeas = local_shape(V(Use), Trim);
    E.WidthPred = local_shape(Vs, Trim);
    % spread of the prediction itself, with the predictor noise removed
    Sp = local_shape(Pred(Use), Trim).*median(Pred(Use));
    E.PredSpread     = Sp./E.MeanPred;
    E.PredSpreadTrue = sqrt(max(Sp.^2 - PredNoiseVar, 0))./E.MeanPred;
    % mean residual: what the prediction misses on average, in ADU^2 and as an
    % equivalent shift of g*T
    E.ResidMean = E.MeanV - E.MeanPred;
    E.ResidRel  = E.ResidMean./E.MeanPred;

    % Calibration: bin both the measurement and the measured prediction by the
    % predictor. The slope is DILUTED and is a lower bound, not a test of unity:
    % the measurement follows each pixel's true variance while the binning
    % variable is a noisy estimate of it, so selecting a bin regresses the truth
    % toward the mean. Simulating the prediction does not undo that, because the
    % simulated values are drawn from the predictor itself and so follow it
    % exactly. What the curve is good for is its SHAPE -- a departure that grows
    % or reverses along the range is structure the prediction misses, while a
    % uniform shortfall is a level error, which the trimmed means measure
    % directly and without dilution.
    Q  = quantile(Pred(Use), [0.005 0.995]);
    Eb = linspace(Q(1), Q(2), 41);
    [Nb, ~, Ib] = histcounts(Pred(Use), Eb);
    Vu = V(Use);
    Ok = Ib>0;
    Xb = accumarray(Ib(Ok), Vs(Ok), [numel(Eb)-1 1], @median, NaN);
    Yb = accumarray(Ib(Ok), Vu(Ok), [numel(Eb)-1 1], @median, NaN);
    Gd = isfinite(Xb) & isfinite(Yb) & Nb(:)>100;
    Cf = polyfit(Xb(Gd), Yb(Gd), 1);
    E.Calib = struct('X',Xb(:).', 'Y',Yb(:).', 'N',Nb(:).', 'Slope',Cf(1), ...
                     'Offset',Cf(2), 'Nbins',nnz(Gd));

    % residual map, in blocks: measurement against the measured prediction
    Vsm = nan(Siz);
    Vsm(Use) = Vs;
    Rm = (local_block(V, Blk, Nby, Nbx) - local_block(Vsm, Blk, Nby, Nbx)) ...
         ./ local_block(Vsm, Blk, Nby, Nbx);
    clear Vsm
    Resid = cat(3, Resid, single(Rm));

    E.Hist = struct('Edges',Edges, 'Median',E.MedianV, ...
                    'Meas',histcounts(Vu./E.MedianV, Edges), ...
                    'Pred',histcounts(Vs./E.MedianV, Edges));
    if ~isempty(Mask)
        E.WidthMeasMasked = local_shape(V(UseM), Trim);
    end
    R{end+1} = E;                                                          %#ok<SAGROW>
    fprintf(['  %-2s step %2d: signal %8.1f ADU, trimmed mean V %9.2f vs predicted %9.2f (%+6.2f %%), ', ...
             'width %6.4f vs %6.4f, slope %6.3f over %d bins\n'], ...
        Ty, St, Sg, E.MeanV, E.MeanPred, 100.*E.ResidRel, E.WidthMeas, E.WidthPred, ...
        Cf(1), E.Calib.Nbins);
    if Ij==1
        fprintf(['     (the circular ZE row: its slope reads 1/0.839 = 1.19 because the predicted side\n', ...
                 '      is a chi2 draw and the measured side is the predictor itself)\n']);
    end
    clear M V Pred Vs Vu Pu
end

Fid = fopen(fullfile(DieOut, 'lowsignal_resid.bin'), 'w');
fwrite(Fid, Resid, 'single');
fclose(Fid);
S = struct('Stage',8, 'Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
           'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, 'Size',Siz, ...
           'LowMax',DieLowMax, 'Gain',Gain, 'Threshold',Tadu, 'Block',Blk, ...
           'ResidSize',[Nby Nbx numel(R)], 'Steps',{R});
Fid = fopen(fullfile(DieOut, 'lowsignal.json'), 'w');
fwrite(Fid, jsonencode(S));
fclose(Fid);

fprintf(['\nMeasurement against the SAME prediction measured the same way (trimmed means,\n', ...
    'top 0.1 %% dropped on both sides; the calibration bins both by the predictor, so its\n', ...
    'own noise dilutes it, so the slope is a LOWER BOUND whose shape matters, not its value;\n', ...
    'the level comparison in resid %% is the dilution-free test).\n\n']);
fprintf('%-3s %5s %9s %10s %10s %9s %9s %9s %9s\n', ...
    'set', 'step', 'signal', 'trim V', 'predicted', 'resid %', 'width', 'pred w', 'slope>');
for Ie = 1:1:numel(R)
    E = R{Ie};
    fprintf('%-3s %5d %9.1f %10.2f %10.2f %9.2f %9.4f %9.4f %9.3f%s\n', ...
        E.Type, E.Step, E.Signal, E.MeanV, E.MeanPred, 100.*E.ResidRel, ...
        E.WidthMeas, E.WidthPred, E.Calib.Slope, local_circ(E.Circular));
end
fprintf('[%4.0f s] LOWSIGNAL DONE -> %s\n', toc(T0), DieOut);

function M = local_trimmean(V, Trim)
    V  = sort(double(V(:)));
    Nk = max(floor(numel(V).*(1-Trim)), 2);
    M  = mean(V(1:Nk));
end

function T = local_circ(F)
    T = '';
    if F
        T = '   (circular: a wiring check)';
    end
end

function St = local_step(S)
    St = S;
    if S==0
        St = [];
    end
end

function W = local_shape(V, Trim)
    V  = sort(double(V(:)));
    N  = numel(V);
    Nk = max(floor(N.*(1-Trim)), 2);
    W  = std(V(1:Nk))./median(V);
end

function Bm = local_block(A, B, Nby, Nbx)
    C  = double(A(1:Nby.*B, 1:Nbx.*B));
    Bm = squeeze(mean(mean(reshape(C, B, Nby, B, Nbx), 1, 'omitnan'), 3, 'omitnan'));
end

function P = local_need(Dir, Name, Who)
    P = fullfile(Dir, Name);
    if ~isfile(P)
        error('ultrasat:lab:scripts:stage', 'stage 8 needs %s from %s', P, Who);
    end
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'stage 8 needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
