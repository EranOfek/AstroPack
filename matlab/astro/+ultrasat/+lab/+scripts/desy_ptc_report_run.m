% Driver for the DESY PTC reproduction report: runs ultrasat.lab.PTCAnalysis on
% the TH02954 flavour-test dies (runs 31, 32), saves figures and a JSON of the
% summaries into OutDir. Report text is assembled from the JSON afterwards.
Root   = '/bigdata3/projects/ultrasat/DESY';
OutDir = '/home/sasha/claude/desy_ptc_report';
if ~isfolder(OutDir), mkdir(OutDir); end
set(0, 'DefaultFigureVisible', 'off');

Runs = {'31', 'LOT_TH02954_31_FT_PTCint_-50_2026-08-25', 'AV',     struct('D',4:7, 'B',[]);
        '32', 'LOT_TH02954_32_FT_PTCint_-50_2026-08-27', 'aSpect', struct('D',8:9, 'B',[])};
Dies = {'W04_D03','W04_D05','W04_D06','W04_D07','W08_D02','W08_D03','W08_D04'};
Flav = [6 6 6 6 2 2 2];
DeckDies = {'31','W04_D07'; '31','W08_D02'; '32','W04_D07'};
BigSec   = [1161 1560 1661 2060];                    % 400x400 around the DESY region, for the parity comparison

Res = struct('Region', [], 'Parity400', [], 'FullDie', []);
Region = {};
Tstart = tic;
for Ir = 1:size(Runs,1)
    for Id = 1:numel(Dies)
        Dev = fullfile(Root, Runs{Ir,2}, ['LOT_TH02954_', Dies{Id}]);
        Tag = sprintf('run%s_%s', Runs{Ir,1}, Dies{Id});
        fprintf('[%6.0f s] region %s\n', toc(Tstart), Tag);
        P = ultrasat.lab.PTCAnalysis(Dev, 'FitSteps',Runs{Ir,4}, 'Parity','rawcol');
        P.run;
        S = P.summary;
        S.Run = Runs{Ir,1};  S.Settings = Runs{Ir,3};  S.Die = Dies{Id};  S.Flavour = Flav(Id);
        S.ParityTable = table2struct(S.ParityTable);
        S.PTCcurve = struct('Mean',P.PTC.Mean, 'VarTemporal',P.PTC.VarTemporal, 'VarDiff',P.PTC.VarDiff, 'VarSpatial',P.PTC.VarSpatial);
        S.DarkLadder   = struct('X',P.Dark.X,   'Median',median(reshape(P.Dark.Mean,[],numel(P.Dark.X)),1));
        S.BrightLadder = struct('X',P.Bright.X, 'Median',median(reshape(P.Bright.Mean,[],numel(P.Bright.X)),1));
        S.Pass = P.Sidecar.Result.Trailer.Pass;  S.SoftBin = P.Sidecar.Result.Trailer.SoftBin;
        Region{end+1} = S; %#ok<SAGROW>
        IsDeck = any(strcmp(DeckDies(:,1), Runs{Ir,1}) & strcmp(DeckDies(:,2), Dies{Id}));
        if IsDeck
            F = figure('Position',[0 0 900 600]); P.plotResponse('D', 'Axes',axes(F), 'Parity',true); print(F, fullfile(OutDir, [Tag '_resp_D.png']), '-dpng', '-r90'); close(F);
            F = figure('Position',[0 0 900 600]); P.plotResponse('B', 'Axes',axes(F), 'Parity',true); xlim([0 0.4]); print(F, fullfile(OutDir, [Tag '_resp_B.png']), '-dpng', '-r90'); close(F);
            H = P.plotHistograms('D'); set(ancestor(H(1),'figure'), 'Position',[0 0 1300 420]); print(ancestor(H(1),'figure'), fullfile(OutDir, [Tag '_hist_D.png']), '-dpng', '-r90'); close(ancestor(H(1),'figure'));
            H = P.plotHistograms('B'); set(ancestor(H(1),'figure'), 'Position',[0 0 1300 420]); print(ancestor(H(1),'figure'), fullfile(OutDir, [Tag '_hist_B.png']), '-dpng', '-r90'); close(ancestor(H(1),'figure'));
            F = figure('Position',[0 0 900 600]); P.plotPTC('Axes',axes(F), 'XLim',[0 14500]); print(F, fullfile(OutDir, [Tag '_ptc.png']), '-dpng', '-r90'); close(F);
            F = figure('Position',[0 0 900 600]); P.plotPTC('Axes',axes(F), 'Estimator','temporal', 'Parity',true, 'XLim',[0 3000]); print(F, fullfile(OutDir, [Tag '_ptc_zoom.png']), '-dpng', '-r90'); close(F);
        end
    end
end
Res.Region = Region;

% parity on a 400x400 region for the deck dies
Par = {};
for Ic = 1:size(DeckDies,1)
    Ir  = find(strcmp(Runs(:,1), DeckDies{Ic,1}));
    Dev = fullfile(Root, Runs{Ir,2}, ['LOT_TH02954_', DeckDies{Ic,2}]);
    Tag = sprintf('run%s_%s', DeckDies{Ic,1}, DeckDies{Ic,2});
    fprintf('[%6.0f s] parity 400x400 %s\n', toc(Tstart), Tag);
    P = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',BigSec, 'FitSteps',Runs{Ir,4}, 'Parity','rawcol');
    P.run;
    S = P.summary;
    S.Run = DeckDies{Ic,1};  S.Die = DeckDies{Ic,2};  S.CCDSEC = BigSec;
    S.ParityTable = table2struct(S.ParityTable);
    Par{end+1} = S; %#ok<SAGROW>
    F = figure('Position',[0 0 900 600]); P.plotResponse('D', 'Axes',axes(F), 'Parity',true, 'Npix',200); print(F, fullfile(OutDir, [Tag '_400_resp_D_parity.png']), '-dpng', '-r90'); close(F);
    F = figure('Position',[0 0 900 600]); P.plotPTC('Axes',axes(F), 'Estimator','temporal', 'Parity',true, 'XLim',[0 3000]); print(F, fullfile(OutDir, [Tag '_400_ptc_parity.png']), '-dpng', '-r90'); close(F);
end
Res.Parity400 = Par;
save(fullfile(OutDir, 'results_partial.mat'), 'Res');
fid = fopen(fullfile(OutDir, 'results.json'), 'w'); fwrite(fid, jsonencode(Res)); fclose(fid);

% full die, streamed: W04_D07 run 31
fprintf('[%6.0f s] full die W04_D07 run 31\n', toc(Tstart));
Dev = fullfile(Root, Runs{1,2}, 'LOT_TH02954_W04_D07');
F = ultrasat.lab.PTCAnalysis(Dev, 'CCDSEC',[], 'FitSteps',Runs{1,4}, 'Parity','rawcol', 'Verbosity',1);
Tf = tic; F.run; Tfull = toc(Tf);
S = F.summary;  S.Run = '31';  S.Die = 'W04_D07';  S.Seconds = Tfull;
S.ParityTable = table2struct(S.ParityTable);
% quantiles of the maps and the fraction of pixels with a valid fit
for Q = {'DarkFit','BrightFit'}
    M = F.(Q{1});
    S.(Q{1}).SlopeP = prctile(M.Slope(:), [1 16 50 84 99]);
    S.(Q{1}).InterceptP = prctile(M.Intercept(:), [1 16 50 84 99]);
    S.(Q{1}).FracValid = mean(isfinite(M.Slope(:)));
end
Res.FullDie = S;
H = F.plotMaps('D'); set(ancestor(H(1),'figure'), 'Position',[0 0 1500 520]); print(ancestor(H(1),'figure'), fullfile(OutDir, 'run31_W04_D07_full_maps_D.png'), '-dpng', '-r90'); close(ancestor(H(1),'figure'));
H = F.plotMaps('B'); set(ancestor(H(1),'figure'), 'Position',[0 0 1500 520]); print(ancestor(H(1),'figure'), fullfile(OutDir, 'run31_W04_D07_full_maps_B.png'), '-dpng', '-r90'); close(ancestor(H(1),'figure'));
H = F.plotHistograms('D'); set(ancestor(H(1),'figure'), 'Position',[0 0 1300 420]); print(ancestor(H(1),'figure'), fullfile(OutDir, 'run31_W04_D07_full_hist_D.png'), '-dpng', '-r90'); close(ancestor(H(1),'figure'));
F2 = figure('Position',[0 0 900 600]); F.plotPTC('Axes',axes(F2), 'Estimator','temporal', 'Parity',true, 'XLim',[0 3000]); print(F2, fullfile(OutDir, 'run31_W04_D07_full_ptc_parity.png'), '-dpng', '-r90'); close(F2);
% row-median profile of the dark slope map in the DESY orientation (parity signature)
Prof = median(F.DarkFit.Slope, 2, 'omitnan');
F2 = figure('Position',[0 0 900 400]); plot(1:numel(Prof), Prof, '-'); hold on; plot(2:2:numel(Prof), Prof(2:2:end), '.', 'MarkerSize',4); grid on;
xlabel('DESY row (= raw TIFF column, reversed)'); ylabel('median dark slope [ADU/s]'); title('W04_D07 run 31: dark-current row profile (dots: odd raw columns)'); xlim([1800 2000]);
print(F2, fullfile(OutDir, 'run31_W04_D07_full_slope_profile.png'), '-dpng', '-r90'); close(F2);
S.SlopeProfile = Prof(1:2:end).';  Res.FullDie = S;

save(fullfile(OutDir, 'results.mat'), 'Res');
fid = fopen(fullfile(OutDir, 'results.json'), 'w'); fwrite(fid, jsonencode(Res)); fclose(fid);
fprintf('[%6.0f s] done\n', toc(Tstart));
