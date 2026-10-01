% Per-pixel mean and temporal variance of both ladders, one reference die per
% setup, for the variance-versus-mean figures of the comparison report.
%   Same reduction as desy_perpixel_run.m (high-gain half, DESY region, the
%   published fit window, Parity='rawcol'), and the bad-column mask and the
%   parity map are written out with the arrays so that the figures use exactly
%   the pixels the rest of the report uses.
%   Output per die-run in desy_perpixel/varmean/: <tag>_{dark,bright}_{mean,var}.bin
%   [Ny Nx Nstep] single, <tag>_zeronoise.bin, <tag>_mask.bin and
%   <tag>_parity.bin [Ny Nx] uint8, plus varmean_meta.json.
Root = '/Data1/DESY';
if ~isfolder(Root)
    Root = '/bigdata3/projects/ultrasat/DESY';
end
OutDir = '/home/sasha/claude/desy_perpixel/varmean';
if ~isfolder(OutDir), mkdir(OutDir); end
%        id      folder                                        settings TX   RSTH  die
Cases = {'31',   'LOT_TH02954_31_FT_PTCint_-50_2026-08-25',   'AV',     3.3, 3.0, 'W04_D07';
         '32',   'LOT_TH02954_32_FT_PTCint_-50_2026-08-27',   'aSpect', 3.3, 3.0, 'W04_D07';
         '33',   'LOT_TH02954_33_FT_PTCint_-50_2026-08-28',   'AV',     3.0, 2.7, 'W04_D07';
         '34',   'LOT_TH02954_34_FT_PTCint_-50_2026-08-30',   'aSpect', 3.0, 2.7, 'W04_D07';
         '36',   'LOT_TH02954_36_FT_PTCint_-50_2026-09-02',   'aSpect', 3.3, 3.0, 'W04_D07';
         '36-2', 'LOT_TH02954_36-2_FT_PTCint_-50_2026-09-03', 'aSpect', 3.9, 2.7, 'W04_D07';
         '36-3', 'LOT_TH02954_36-3_FT_PTCint_-50_2026-09-05', 'aSpect', 3.9, 3.0, 'W04_D07';
         '38',   'LOT_TH02954_38_FT_PTCint_-50_2026-09-11',   'aSpect', 3.5, 3.0, 'W04_D07';
         '38-2', 'LOT_TH02954_38-2_FT_PTCint_-50_2026-09-13', 'aSpect', 3.7, 3.0, 'W04_D07';
         '40',   'LOT_TH02954_40_FT_PTCint_-50_2026-09-15',   'aSpect', 3.3, 3.0, 'W04_D04'};
Meta = {};
T0   = tic;
for I = 1:1:size(Cases,1)
    Die = Cases{I,6};
    Tag = sprintf('run%s_%s', Cases{I,1}, Die);
    fprintf('[%6.0f s] %s\n', toc(T0), Tag);
    try
        P = ultrasat.lab.PTCAnalysis(fullfile(Root, Cases{I,2}, ['LOT_TH02954_', Die]), ...
                                     'FitSteps',struct('D','auto','B','auto'), 'Parity','rawcol');
        P.run;
        B = P.badColumns;
        for T = {'Dark','Bright'}
            L = P.(T{1});
            writeBin(fullfile(OutDir, sprintf('%s_%s_mean.bin', Tag, lower(T{1}))), L.Mean);
            writeBin(fullfile(OutDir, sprintf('%s_%s_var.bin',  Tag, lower(T{1}))), L.VarTemporal);
        end
        writeBin(fullfile(OutDir, [Tag '_zeronoise.bin']), P.ZeroNoise);
        writeBin(fullfile(OutDir, [Tag '_mask.bin']),   uint8(B.GoodMask), 'uint8');
        writeBin(fullfile(OutDir, [Tag '_parity.bin']), uint8(P.ParityMap), 'uint8');
        Zn = P.zeroNoiseStats('Mask',B.GoodMask);
        M = struct('Tag',Tag, 'Run',Cases{I,1}, 'Settings',Cases{I,3}, 'TX',Cases{I,4}, ...
                   'RSTH',Cases{I,5}, 'Die',Die, 'Size',size(P.Zero), ...
                   'DarkX',P.Dark.X, 'BrightX',P.Bright.X, ...
                   'DarkNframes',P.Dark.Nframes, 'BrightNframes',P.Bright.Nframes, ...
                   'DarkSteps',P.DarkFit.FitSteps, 'BrightSteps',P.BrightFit.FitSteps, ...
                   'Gain',P.PTC.GainUsed, 'GainOffset',P.PTC.Fit.temporal.Offset, ...
                   'GainRange',P.GainRange, 'SatLevel',P.SatLevel, 'ExpSen',P.ExpSen, ...
                   'BiasLevel',Zn.All.BiasLevel, 'ReadNoise',Zn.All.ReadNoiseMedian, ...
                   'ReadNoiseEven',Zn.Even.ReadNoiseMedian, 'ReadNoiseOdd',Zn.Odd.ReadNoiseMedian, ...
                   'Nbad',B.Nbad, 'Npix',nnz(B.GoodMask));
        Meta{end+1} = M; %#ok<SAGROW>
    catch ME
        fprintf('  FAILED: %s\n', ME.message);
    end
    Fid = fopen(fullfile(OutDir, 'varmean_meta.json'), 'w');
    fwrite(Fid, jsonencode(Meta));
    fclose(Fid);
end
fprintf('[%6.0f s] VARMEAN DONE\n', toc(T0));

function writeBin(Path, A, Type)
    if nargin<3
        Type = 'single';
        A = single(A);
    end
    Fid = fopen(Path, 'w');
    fwrite(Fid, A, Type);
    fclose(Fid);
end
