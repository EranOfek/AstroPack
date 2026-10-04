% Export the photon-transfer data of one die to a MATLAB file, for replotting.
%   Writes ptc_data.mat in DieOut: one entry per ladder point (the bias frames,
%   every dark step and every bright step) carrying both the point that appears
%   on the PTC figure and the full pixel distribution of the EXCESS variance
%     E_i = V_i - RN_i^2
%   which is the variance the signal added to that pixel, with the read noise
%   of stage 1 taken out pixel by pixel.
%   The distribution is stored three ways, so any plot can be remade without
%   the frames: a fine histogram (edges and counts, exact for any binned view),
%   a quantile table, and a random subsample of pixels with their signals for
%   scatter plots. The subsample uses the SAME pixels at every point, so a
%   pixel can be followed up the ladder; their indices and read noise are in
%   Meta.
%   Everything needed to redraw the figure is in the file, including the two
%   corrections applied to the plotted variance -- see Meta.Correction and the
%   README variable.
ultrasat.lab.scripts.desy_die_config;

T0 = tic;
fprintf('%s: exporting the photon-transfer data\n', DieTag);
P = ultrasat.lab.PTCAnalysis(DieDev, 'CCDSEC',[], 'Gain',DieGain, 'Parity','rawcol');
P.read;
P.subtractZero;
G    = P.rawColGeom;
Siz  = [G.Ny G.Nx];
Ptc  = jsondecode(fileread(local_need(DieOut, 'ptc.json', 'stage 5')));
RN   = readBin(fullfile(DieStage1, 'rn.bin'), Siz, 'stage 1');
RN2  = double(RN).^2;
Spread = local_spreads(DieOut);                 % stage 7, per step, [] if absent

Nsub = 200000;
rng(31);
Idx  = sort(randperm(numel(RN2), Nsub)).';
Qp   = [0.001 0.01 0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99 0.999];

Jobs = [{'ZE', 0, 0}];
for Ty = {'D','B'}
    Flag = strcmp(P.Frames.FrameType, Ty{1});
    for St = unique(P.Frames.Step(Flag)).'
        Row = find(Flag & P.Frames.Step==St, 1);
        if strcmp(Ty{1}, 'D')
            Xv = P.Frames.ExpTime(Row);
        else
            Xv = P.Frames.Intensity(Row).*P.IntensityScale;
        end
        Jobs(end+1,:) = {Ty{1}, St, Xv};                                   %#ok<SAGROW>
    end
end

PTC = struct([]);
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
    M   = double(mean(C, 3)) - double(P.Zero);
    V   = var(double(C), 0, 3);
    clear C
    E   = V - RN2;                              % excess variance: what the signal added

    Cf2 = Dof./(2.*gammaincinv(0.5, Dof./2));   % chi2 median -> mean
    Sg  = median(M(:), 'omitnan');
    Vm  = median(V(:), 'omitnan');
    Sp  = local_lookup(Spread, Ty, St);         % pixel-to-pixel spread of the TRUE variance

    S = struct();
    S.Type = Ty;  S.Step = St;  S.X = Jobs{Ij,3};
    if strcmp(Ty, 'D')
        S.XName = 'exposure time [s]';
    elseif strcmp(Ty, 'B')
        S.XName = 'intensity (config value x IntensityScale)';
    else
        S.XName = 'none (bias frames)';
    end
    S.Nframes = Nf;  S.Dof = Dof;
    S.Saturated = Sg > 0.9.*P.SatLevel;
    S.Circular  = strcmp(Ty, 'ZE');             % E is zero by construction there
    S.SignalMedian   = Sg;
    S.SignalTrimMean = local_trimmean(M(:), 1e-3);
    S.VarMedian        = Vm;
    S.VarMeanChi2      = Vm.*Cf2;
    S.SpreadRel        = Sp;
    S.VarMeanCorrected = Vm.*Cf2.*sqrt(1 + Sp.^2);
    S.Chi2MedianFactor = Cf2;

    Ev = double(E(:));
    Ev = Ev(isfinite(Ev));
    Lo = quantile(Ev, 1e-4);
    Hi = quantile(Ev, 1-1e-4);
    Ed = linspace(Lo, Hi, 801);
    Ex = struct('Edges',Ed, 'Counts',histcounts(Ev, Ed), ...
                'NUnder',nnz(Ev<Ed(1)), 'NOver',nnz(Ev>Ed(end)), 'N',numel(Ev), ...
                'QuantileP',Qp, 'Quantiles',quantile(Ev, Qp), ...
                'Mean',mean(Ev), 'TrimMean',local_trimmean(Ev, 1e-3), ...
                'Median',median(Ev), 'Std',std(Ev), ...
                'MAD',1.4826.*median(abs(Ev - median(Ev))));
    S.Excess = Ex;
    S.Sample = struct('Excess',single(E(Idx)), 'Signal',single(M(Idx)));
    PTC = [PTC, S];                                                        %#ok<AGROW>
    fprintf('  %-2s step %2d: signal %9.1f  median V %9.2f  excess mean %9.2f (trimmed %9.2f)\n', ...
        Ty, St, Sg, Vm, Ex.Mean, Ex.TrimMean);
    clear M V E Ev
end

Meta = struct('Tag',DieTag, 'Run',DieRun, 'Die',Die, 'GainHalf',DieGain, ...
              'Lot',P.Info.Lot, 'Wafer',P.Info.Wafer, 'Device',P.Info.Device, ...
              'Size',Siz, 'ReadoutDim',G.Dim, 'ExpSen',P.ExpSen, ...
              'IntensityScale',P.IntensityScale, 'SatLevel',P.SatLevel, ...
              'Gain',Ptc.GainEnsemble, 'GainOffset',Ptc.OffsetEnsemble, ...
              'GainWindow',Ptc.GainRange, 'GainPerPixelMean',Ptc.Unmasked.All.GainMean, ...
              'ReadNoiseMedian',median(double(RN(:))), 'ReadNoiseFile','rn.bin (stage 1)', ...
              'SampleIndex',int32(Idx), 'SampleRN2',single(RN2(Idx)), 'SampleN',Nsub, ...
              'Created',datestr(now, 31), 'Script','ultrasat.lab.scripts.desy_die_ptc_export');
Meta.Correction = ['Plotted variance = VarMedian * Chi2MedianFactor * sqrt(1+SpreadRel^2). ', ...
    'The first factor turns the median of a chi2-distributed per-pixel variance into its mean ', ...
    '(the plain mean is unusable: a cosmic ray in one of three frames puts a pixel at 1e7 ADU^2). ', ...
    'The second is needed because the first is exact only for identical pixels: when the true ', ...
    'variance itself spreads by a relative width SpreadRel, the corrected median estimates the ', ...
    'median rather than the mean. SpreadRel comes from stage 7 and is 0.27 on the dark ladder ', ...
    'against under 0.06 on the bright one, so omitting it biases the two ladders differently.'];

README = local_readme();
save(fullfile(DieOut, 'ptc_data.mat'), 'PTC', 'Meta', 'README', '-v7.3');
D = dir(fullfile(DieOut, 'ptc_data.mat'));
fprintf('\nwrote %s  (%d points, %.0f MB)\n', fullfile(DieOut,'ptc_data.mat'), numel(PTC), D.bytes./2^20);
fprintf('  load it and type  README  to see the field list\n');
fprintf('[%4.0f s] EXPORT DONE\n', toc(T0));

function R = local_readme()
    R = strjoin({
    'ptc_data.mat -- photon-transfer data of one DESY die, for replotting.'
    ''
    'PTC  : 1xN struct array, one entry per ladder point (bias, dark steps, bright steps).'
    '  Type, Step, X, XName   the ladder and the abscissa (exposure [s] or intensity)'
    '  Nframes, Dof           frames averaged, and Nframes-1'
    '  Saturated, Circular    flags; Circular marks the bias point, where the excess'
    '                         variance is zero by construction (the read-noise map is'
    '                         built from those same frames)'
    '  SignalMedian           median over pixels of the bias-subtracted mean signal [ADU]'
    '  SignalTrimMean         the same, trimmed mean (top 0.1 % dropped)'
    '  VarMedian              median over pixels of the per-pixel temporal variance [ADU^2]'
    '  Chi2MedianFactor       Dof/(2*gammaincinv(0.5,Dof/2)), the chi2 median-to-mean factor'
    '  VarMeanChi2            VarMedian * Chi2MedianFactor'
    '  SpreadRel              pixel-to-pixel spread of the TRUE variance (stage 7)'
    '  VarMeanCorrected       VarMeanChi2 * sqrt(1+SpreadRel^2)  <-- what the figure plots'
    '  Excess                 distribution of E_i = V_i - RN_i^2 over all pixels:'
    '                           Edges, Counts        801 edges, 800 counts'
    '                           NUnder, NOver, N     outside the histogram, and the total'
    '                           QuantileP, Quantiles percentile table'
    '                           Mean, TrimMean, Median, Std, MAD'
    '  Sample                 the same quantity for a fixed random subsample of pixels:'
    '                           Excess, Signal       [SampleN x 1] single'
    ''
    'Meta : the die, the gain and its window, the read-noise map used, and'
    '       SampleIndex / SampleRN2 -- the linear pixel indices of the subsample and their'
    '       read-noise variance. The SAME pixels are sampled at every ladder point, so a'
    '       pixel can be followed up the ladder.'
    '       Meta.Correction explains the two factors applied to the plotted variance.'
    ''
    'To redraw the PTC figure:'
    '  x = [PTC.SignalMedian];  y = [PTC.VarMeanCorrected];  t = {PTC.Type};'
    '  loglog(x(strcmp(t,"B")), y(strcmp(t,"B")), "s", x(strcmp(t,"D")), y(strcmp(t,"D")), "o")'
    'To draw the excess-variance distribution of one point:'
    '  k = 12;  E = PTC(k).Excess;  stairs(E.Edges(1:end-1), E.Counts)'
    }, newline);
end

function S = local_spreads(Dir)
    S = [];
    Pth = fullfile(Dir, 'varspread.json');
    if ~isfile(Pth)
        return
    end
    J = jsondecode(fileread(Pth));
    St = J.Steps;
    if ~iscell(St)
        St = num2cell(St);
    end
    S = St;
end

function V = local_lookup(S, Ty, St)
    V = 0;
    if isempty(S)
        return
    end
    for I = 1:1:numel(S)
        E = S{I};
        if strcmp(E.Type, Ty) && E.Step==St
            V = E.Unmasked.RelIntr;
            return
        end
    end
end

function M = local_trimmean(V, Trim)
    V  = sort(double(V(isfinite(V))));
    Nk = max(floor(numel(V).*(1-Trim)), 2);
    M  = mean(V(1:Nk));
end

function St = local_step(S)
    St = S;
    if S==0
        St = [];
    end
end

function P = local_need(Dir, Name, Who)
    P = fullfile(Dir, Name);
    if ~isfile(P)
        error('ultrasat:lab:scripts:stage', 'the export needs %s from %s', P, Who);
    end
end

function A = readBin(Path, Siz, Who)
    if ~isfile(Path)
        error('ultrasat:lab:scripts:stage', 'the export needs %s from %s', Path, Who);
    end
    Fid = fopen(Path, 'r');
    A   = fread(Fid, Siz, 'single');
    fclose(Fid);
end
