% TX-scan / settings runs of lot TH02954 (33, 34, 36, 36-2, 36-3, 38, 38-2, 40): same reduction
% as desy_ptc_report_run.m with the 'auto' fit-step rule; ZE-only runs 39, 39-2 for bias/read noise.
Root   = '/bigdata3/projects/ultrasat/DESY';
OutDir = '/home/sasha/claude/desy_txscan_report';
if ~isfolder(OutDir), mkdir(OutDir); end
set(0, 'DefaultFigureVisible', 'off');
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
FigDies = {'W04_D07','W08_D02'};
Res = struct('Region', {{}}, 'Zero', {{}});
T0 = tic;
for Ir = 1:size(Runs,1)
    Dies = dir(fullfile(Root, Runs{Ir,2}, 'LOT_TH02954_W*'));
    for Id = 1:numel(Dies)
        Die = strrep(Dies(Id).name, 'LOT_TH02954_', '');
        Dev = fullfile(Dies(Id).folder, Dies(Id).name);
        Tag = sprintf('run%s_%s', Runs{Ir,1}, Die);
        fprintf('[%6.0f s] %s\n', toc(T0), Tag);
        try
            P = ultrasat.lab.PTCAnalysis(Dev, 'FitSteps',struct('D','auto','B','auto'), 'Parity','rawcol');
            P.run;
            S = P.summary;
            S.Run = Runs{Ir,1}; S.Settings = Runs{Ir,3}; S.TX = Runs{Ir,4}; S.RSTH = Runs{Ir,5};
            S.Die = Die; S.Flavour = Flav(Die(1:3));
            S.ParityTable = table2struct(S.ParityTable);
            S.PTCcurve = struct('Mean',P.PTC.Mean, 'VarTemporal',P.PTC.VarTemporal, 'VarDiff',P.PTC.VarDiff, 'VarSpatial',P.PTC.VarSpatial);
            S.DarkLadder   = struct('X',P.Dark.X,   'Median',median(reshape(P.Dark.Mean,[],numel(P.Dark.X)),1));
            S.BrightLadder = struct('X',P.Bright.X, 'Median',median(reshape(P.Bright.Mean,[],numel(P.Bright.X)),1));
            S.Pass = P.Sidecar.Result.Trailer.Pass;  S.SoftBin = P.Sidecar.Result.Trailer.SoftBin;
            Res.Region{end+1} = S;
            if any(strcmp(FigDies, Die))
                F = figure('Position',[0 0 900 600]); P.plotResponse('D', 'Axes',axes(F), 'Parity',true); print(F, fullfile(OutDir, [Tag '_resp_D.png']), '-dpng', '-r90'); close(F);
                F = figure('Position',[0 0 900 600]); P.plotPTC('Axes',axes(F), 'Estimator','temporal', 'Parity',true, 'XLim',[0 3000]); print(F, fullfile(OutDir, [Tag '_ptc_zoom.png']), '-dpng', '-r90'); close(F);
            end
        catch ME
            fprintf('  FAILED: %s\n', ME.message);
        end
        fid = fopen(fullfile(OutDir, 'results.json'), 'w'); fwrite(fid, jsonencode(Res)); fclose(fid);
    end
end
% bias / read noise from ZE frames: all runs above + ZE-only runs
All = [Runs; ZeRuns];
for Ir = 1:size(All,1)
    Dies = dir(fullfile(Root, All{Ir,2}, 'LOT_TH02954_W*'));
    for Id = 1:numel(Dies)
        Die = strrep(Dies(Id).name, 'LOT_TH02954_', '');
        Dev = fullfile(Dies(Id).folder, Dies(Id).name);
        try
            AI = ultrasat.lab.readPTC(Dev, 'FrameType','ZE', 'CCDSEC',[1361 1460 1861 1960]);
            Cube = single(cat(3, AI.Image));  Zero = mean(Cube, 3);  Noise = std(Cube, 0, 3);
            H = AI(1).HeaderData;  RawSec = sscanf(H.getVal('RAWSEC'), '[%d:%d,%d:%d]').';
            Odd = repmat(mod(RawSec(2) - H.getVal('RAWXOFF') - (0:size(Zero,1)-1).', 2)==1, 1, size(Zero,2));
            Z = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise, true(size(Zero)));
            Z.Even = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise, ~Odd);
            Z.Odd  = ultrasat.lab.PTCAnalysis.zeroStats(Cube, Zero, Noise,  Odd);
            Z.Run = All{Ir,1}; Z.Settings = All{Ir,3}; Z.TX = All{Ir,4}; Z.RSTH = All{Ir,5}; Z.Die = Die; Z.Flavour = Flav(Die(1:3));
            Res.Zero{end+1} = Z;
        catch ME
            fprintf('  ZE FAILED %s %s: %s\n', All{Ir,1}, Die, ME.message);
        end
    end
end
fid = fopen(fullfile(OutDir, 'results.json'), 'w'); fwrite(fid, jsonencode(Res)); fclose(fid);
save(fullfile(OutDir, 'results.mat'), 'Res');
fprintf('[%6.0f s] done: %d die-runs, %d ZE sets\n', toc(T0), numel(Res.Region), numel(Res.Zero));
