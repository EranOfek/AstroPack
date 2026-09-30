% Individual-pixel comparison of the TH02954 setups (runs 31-40).
%   For every die-run: bias / read-noise decomposition of the ZE frames,
%   per-pixel weighted ladder fits below the linearity limit with their
%   analytic fit noise removed, per-pixel thresholds and dark current, and
%   the resulting sigma_eff / SNR curves in electrons -- all split by
%   readout-column parity and with the bad readout columns masked.
%   High-gain half, DESY 100x100 region, and the SAME fit window as
%   desy_txscan_run.m (FitRange / FitSteps 'auto'), so the medians are
%   directly comparable with the published reports; what is new is the
%   per-pixel treatment, the parity split, the bad-column mask and the
%   removal of the fit noise from every pixel-to-pixel spread.
%   Run 35 is skipped (its TX / RST_H settings are not recorded).
%   Output: desy_perpixel/perpixel.json (scalars + curves, no pixel maps).
Root   = '/bigdata3/projects/ultrasat/DESY';
OutDir = '/home/sasha/claude/desy_perpixel';
if ~isfolder(OutDir), mkdir(OutDir); end
LinLimit = 2900;                 % [ADU] measured INL still <0.5% below this
Qgrid    = logspace(0, 3, 61);   % [e-] signal grid of the SNR curves
%        id      folder                                          settings  TX   RSTH
Runs = {'31',   'LOT_TH02954_31_FT_PTCint_-50_2026-08-25',   'AV',     3.3, 3.0;
        '32',   'LOT_TH02954_32_FT_PTCint_-50_2026-08-27',   'aSpect', 3.3, 3.0;
        '33',   'LOT_TH02954_33_FT_PTCint_-50_2026-08-28',   'AV',     3.0, 2.7;
        '34',   'LOT_TH02954_34_FT_PTCint_-50_2026-08-30',   'aSpect', 3.0, 2.7;
        '36',   'LOT_TH02954_36_FT_PTCint_-50_2026-09-02',   'aSpect', 3.3, 3.0;
        '36-2', 'LOT_TH02954_36-2_FT_PTCint_-50_2026-09-03', 'aSpect', 3.9, 2.7;
        '36-3', 'LOT_TH02954_36-3_FT_PTCint_-50_2026-09-05', 'aSpect', 3.9, 3.0;
        '38',   'LOT_TH02954_38_FT_PTCint_-50_2026-09-11',   'aSpect', 3.5, 3.0;
        '38-2', 'LOT_TH02954_38-2_FT_PTCint_-50_2026-09-13', 'aSpect', 3.7, 3.0;
        '40',   'LOT_TH02954_40_FT_PTCint_-50_2026-09-15',   'aSpect', 3.3, 3.0};
ZeRuns = {'39',   'LOT_TH02954_39_FT_ZE_-50_2026-09-14',   'aSpect', 3.6, 3.0;
          '39-2', 'LOT_TH02954_39-2_FT_ZE_-50_2026-09-14', 'aSpect', 3.8, 3.0};
Flav = containers.Map({'W04','W08'}, {6, 2});
Res  = struct('Full', {{}}, 'ZeroOnly', {{}}, 'LinLimit',LinLimit, 'Qgrid',Qgrid);
T0   = tic;

for Ir = 1:1:size(Runs,1)
    Dies = dir(fullfile(Root, Runs{Ir,2}, 'LOT_TH02954_W*'));
    for Id = 1:1:numel(Dies)
        Die = strrep(Dies(Id).name, 'LOT_TH02954_', '');
        Tag = sprintf('run%s_%s', Runs{Ir,1}, Die);
        fprintf('[%6.0f s] %s\n', toc(T0), Tag);
        try
            P = ultrasat.lab.PTCAnalysis(fullfile(Dies(Id).folder, Dies(Id).name), ...
                                         'FitSteps',struct('D','auto','B','auto'), 'Parity','rawcol');
            P.run;
            S = struct('Tag',Tag, 'Run',Runs{Ir,1}, 'Settings',Runs{Ir,3}, 'TX',Runs{Ir,4}, ...
                       'RSTH',Runs{Ir,5}, 'Die',Die, 'Flavour',Flav(Die(1:3)), ...
                       'Pass',P.Sidecar.Result.Trailer.Pass, 'GainUsed',P.PTC.GainUsed, ...
                       'GainTemporal',P.PTC.Fit.temporal.Gain, 'ExpSen',P.ExpSen);
            B = P.badColumns;
            S.Bad = struct('Nbad',B.Nbad, 'Nrawcol',B.Nrawcol, 'BadRawCol',B.BadRawCol, ...
                           'NoiseProfile',B.NoiseProfile, 'RespProfile',B.RespProfile);
            Zn = P.zeroNoiseStats('Mask',B.GoodMask);
            S.Zero = Zn;
            FD = P.perPixelFits('D', 'LinLimit',LinLimit, 'Mask',B.GoodMask);
            FB = P.perPixelFits('B', 'LinLimit',LinLimit, 'Mask',B.GoodMask);
            S.DarkFit   = stripMaps(FD);
            S.BrightFit = stripMaps(FB);
            S.PatternB  = P.stepFixedPattern('B', 'Mask',B.GoodMask);
            S.PatternD  = P.stepFixedPattern('D', 'Mask',B.GoodMask);
            Th = P.perPixelThreshold('DarkFit',FD, 'BrightFit',FB, 'Pattern',S.PatternB, 'Mask',B.GoodMask);
            S.Threshold = stripMaps(Th);
            for Pn = {'All','Even','Odd'}
                for Mt = {'light','dark'}
                    N = P.noiseBudget('Threshold',Th, 'Zero',Zn, 'Parity',Pn{1}, ...
                                      'Method',Mt{1}, 'Q',Qgrid);
                    S.Budget.(Pn{1}).(Mt{1}) = N;
                end
            end
            Res.Full{end+1} = S; %#ok<SAGROW>
        catch ME
            fprintf('  FAILED: %s\n', ME.message);
        end
        writeJSON(OutDir, Res);
    end
end

% ZE-only runs: bias and read noise, with a 5-frame subsample for a fair
% comparison against the 5-frame PTC runs
for Ir = 1:1:size(ZeRuns,1)
    Dies = dir(fullfile(Root, ZeRuns{Ir,2}, 'LOT_TH02954_W*'));
    for Id = 1:1:numel(Dies)
        Die = strrep(Dies(Id).name, 'LOT_TH02954_', '');
        Tag = sprintf('run%s_%s', ZeRuns{Ir,1}, Die);
        fprintf('[%6.0f s] %s (ZE only)\n', toc(T0), Tag);
        try
            P = ultrasat.lab.PTCAnalysis(fullfile(Dies(Id).folder, Dies(Id).name), 'Parity','rawcol');
            P.read;  P.subtractZero;
            B = P.badColumns;
            S = struct('Tag',Tag, 'Run',ZeRuns{Ir,1}, 'Settings',ZeRuns{Ir,3}, 'TX',ZeRuns{Ir,4}, ...
                       'RSTH',ZeRuns{Ir,5}, 'Die',Die, 'Flavour',Flav(Die(1:3)), ...
                       'Nbad',B.Nbad, 'BadRawCol',B.BadRawCol);
            S.Zero  = P.zeroNoiseStats('Mask',B.GoodMask);
            S.Zero5 = P.zeroNoiseStats('Mask',B.GoodMask, 'Frames',1:5);
            Res.ZeroOnly{end+1} = S; %#ok<SAGROW>
        catch ME
            fprintf('  FAILED: %s\n', ME.message);
        end
        writeJSON(OutDir, Res);
    end
end
fprintf('[%6.0f s] PERPIXEL DONE\n', toc(T0));

function S = stripMaps(S)
    % drop the per-pixel maps: only the summaries and the selection are kept
    for F = {'Slope','Intercept','VarSlope','VarIntercept','CovSlopeIntercept','ResidRMS', ...
             'Chi2Dof','Nused','DarkADU','DarkE','LightADU','LightE','VarDarkADU','VarLightADU', ...
             'DCADU','DCE','VarDCADU','RespADU','VarRespADU'}
        if isfield(S, F{1})
            S = rmfield(S, F{1});
        end
    end
end

function writeJSON(OutDir, Res)
    Fid = fopen(fullfile(OutDir, 'perpixel.json'), 'w');
    fwrite(Fid, jsonencode(Res));
    fclose(Fid);
end
