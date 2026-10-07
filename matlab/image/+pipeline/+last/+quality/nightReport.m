function [OutFile, Summary] = nightReport(Data, Args)
    % Build a one-night quality report (self-contained HTML) from coadd header metrics.
    %   Aggregates the per-crop table produced by pipeline.last.quality.harvestNight
    %   into: a session summary, a median +/- 5-95% table of the quality metrics, a
    %   per-camera breakdown, an anomaly census (failure classes with counts), and
    %   figures - distributions of the key metrics and their run across the night.
    %
    %   The HTML is self-contained: figures are embedded as base64 PNGs, so the
    %   single file can be mailed or served with no side-car images.
    %
    % Input  : - Data, one of:
    %            a table from pipeline.last.quality.harvestNight,
    %            a .mat file written by its 'CacheFile' argument,
    %            or [] / a night string to harvest on the fly.
    %          * ...,key,val,...
    %            'Info' - The Info structure from harvestNight. When Data is a
    %                   table and Info is empty, the session summary is derived
    %                   from the table alone. Default is [].
    %            'OutFile' - Output .html path. Default is '' ->
    %                   ./night_report_<YYYYMMDD>.html in the current directory.
    %            'HarvestArgs' - Cell array passed to harvestNight when Data is
    %                   empty or a night string. Default is {}.
    %            'HistKeys' - Metrics to show as distributions.
    %                   Default {'LIMMAG','AST_ARMS','PT_ARMS','PT_RMS','FWHM','PT_ZP'}.
    %            'TimeKeys' - Metrics to show against time of night.
    %                   Default {'LIMMAG','FWHM','BACKMAG','AIRMASS'}.
    %            'SummaryKeys' - Metrics listed in the median/range table.
    %                   Default is the union of the above plus the counts.
    %            'MinStars' - N_STARS below this counts as a sparse-crop anomaly.
    %                   Default is 100.
    %            'MinNCalib' - PT_NCALI below this counts as a weak-calibration
    %                   anomaly. Default is 10.
    %            'CameraKeys' - Metrics tabulated per camera.
    %                   Default {'LIMMAG','FWHM','AST_ARMS','PT_ARMS','N_STARS'}.
    %            'CameraOutlierZ' - A camera metric is highlighted when it sits
    %                   this many robust sigma (1.4826*MAD, computed across
    %                   cameras) from the across-camera median. Default is 3.
    %            'Title' - Report title. Default is '' -> 'LAST night report'.
    %            'Verbose' - Print progress. Default is true.
    % Output : - Path of the written HTML file.
    %          - A structure with the aggregated numbers (per-metric statistics,
    %            per-camera table, anomaly counts), for programmatic use.
    % Author : D. Kovaleva (Oct 2026)
    % Example: [T,Info] = pipeline.last.quality.harvestNight('Night','2026-10-04');
    %          pipeline.last.quality.nightReport(T, 'Info',Info);
    %          % or, end to end:
    %          pipeline.last.quality.nightReport('2026-10-04');

    arguments
        Data                      = []
        Args.Info                 = []
        Args.OutFile char         = ''
        Args.HarvestArgs cell     = {}
        Args.HistKeys cell        = {'LIMMAG','AST_ARMS','PT_ARMS','PT_RMS','FWHM','PT_ZP'}
        Args.TimeKeys cell        = {'LIMMAG','FWHM','BACKMAG','AIRMASS'}
        Args.SummaryKeys cell     = {'FWHM','LIMMAG','BACKMAG','AIRMASS','AST_ARMS', ...
                                     'AST_ERRM','AST_NSRC','N_STARS','PT_ZP','PT_ARMS', ...
                                     'PT_RMS','PT_NCALI','PT_DOF','PT_CTA','APC0_PS','RP_MRMS'}
        Args.MinStars (1,1) double  = 100
        Args.MinNCalib (1,1) double = 10
        Args.CameraKeys cell      = {'LIMMAG','FWHM','AST_ARMS','PT_ARMS','N_STARS'}
        Args.CameraOutlierZ (1,1) double = 3
        Args.Title char           = ''
        Args.Verbose logical      = true
    end

    % ---------- resolve the input into (T, Info)
    Info     = Args.Info;
    IsCache  = (ischar(Data) || isstring(Data)) && isfile(char(Data)) && ...
               endsWith(char(Data), '.mat', 'IgnoreCase',true);
    if istable(Data)
        T = Data;
    elseif IsCache
        Loaded = load(char(Data));
        T = Loaded.T;
        if isempty(Info) && isfield(Loaded, 'Info')
            Info = Loaded.Info;
        end
    else
        % empty, or a night string: harvest now
        HA = Args.HarvestArgs;
        if ~isempty(Data)
            HA = [HA, {'Night', char(Data)}];
        end
        [T, Info] = pipeline.last.quality.harvestNight(HA{:});
    end
    if isempty(Info)
        Info = struct('Night','', 'BasePath','', 'Ncam',numel(unique(T.Camera)), ...
                      'Nvisit',numel(unique(strcat(T.Camera,'|',T.Visit))), ...
                      'Ncrop',height(T), 'NcropExpect',NaN, 'HarvestSec',NaN, ...
                      'TimeSpanUT',[NaN NaN], 'CadenceSec',NaN);
    end
    Vars = T.Properties.VariableNames;

    % ---------- per-metric statistics
    Summary          = struct;
    Summary.Night    = Info.Night;
    Summary.Info     = Info;
    Keys             = Args.SummaryKeys(ismember(Args.SummaryKeys, Vars));
    Stats            = cell(numel(Keys), 1);
    for Ikey = 1:numel(Keys)
        V  = T.(Keys{Ikey});
        Vf = V(isfinite(V));
        S  = struct('Key', Keys{Ikey}, 'N', numel(Vf), 'Median', NaN, ...
                    'P05', NaN, 'P95', NaN, 'NaNFrac', 1 - numel(Vf)/max(1,numel(V)));
        if ~isempty(Vf)
            S.Median = median(Vf);
            S.P05    = prctile(Vf, 5);
            S.P95    = prctile(Vf, 95);
        end
        Stats{Ikey} = S;
    end
    Summary.Stats = [Stats{:}];

    % ---------- anomaly census
    Anom = {};
    Anom = addAnomaly(Anom, T, Vars, 'PT_DOF', @(v) v <= 0, ...
        'Underdetermined photometric fit (PT_DOF <= 0): zero point and PT_ARMS not constrained');
    Anom = addAnomaly(Anom, T, Vars, 'PT_ARMS', @(v) v < 1e-6, ...
        'PT_ARMS at or below 1e-6 (reported as a perfect fit)');
    Anom = addAnomaly(Anom, T, Vars, 'PT_NCALI', @(v) v < Args.MinNCalib, ...
        sprintf('Fewer than %g photometric calibrators', Args.MinNCalib));
    Anom = addAnomaly(Anom, T, Vars, 'AST_ARMS', @(v) ~isfinite(v), ...
        'No astrometric solution (AST_ARMS blank)');
    Anom = addAnomaly(Anom, T, Vars, 'N_STARS', @(v) v < Args.MinStars, ...
        sprintf('Fewer than %g detected sources', Args.MinStars));
    Anom = addAnomaly(Anom, T, Vars, 'LIMMAG', @(v) ~isfinite(v), ...
        'No limiting magnitude (LIMMAG blank)');
    Anom = addAnomaly(Anom, T, Vars, 'PT_ZP', @(v) ~isfinite(v), ...
        'No photometric zero point (PT_ZP blank)');
    if isfinite(Info.NcropExpect) && Info.NcropExpect > Info.Ncrop
        Anom{end+1} = struct('Key','products', 'N', Info.NcropExpect - Info.Ncrop, ...
            'Frac', (Info.NcropExpect - Info.Ncrop)/Info.NcropExpect, ...
            'Text', 'Coadd crops missing with respect to visits x crops-per-visit', ...
            'Examples', ""); %#ok<AGROW>
    end
    Summary.Anomalies = [Anom{:}];

    % ---------- per-camera breakdown
    CamKeys  = Args.CameraKeys(ismember(Args.CameraKeys, Vars));
    [Cam, ~, Icam] = unique(T.Camera);
    Ncam     = numel(Cam);
    CamStat  = nan(Ncam, numel(CamKeys));
    CamNcrop = accumarray(Icam, 1);
    for Ikey = 1:numel(CamKeys)
        V = T.(CamKeys{Ikey});
        for Ic = 1:Ncam
            Vc = V(Icam == Ic);
            Vc = Vc(isfinite(Vc));
            if ~isempty(Vc)
                CamStat(Ic, Ikey) = median(Vc);
            end
        end
    end
    Summary.Camera      = Cam;
    Summary.CameraKeys  = CamKeys;
    Summary.CameraStat  = CamStat;
    Summary.CameraNcrop = CamNcrop;

    % ---------- figures
    if Args.Verbose
        fprintf('nightReport: rendering figures\n');
    end
    HistKeys = Args.HistKeys(ismember(Args.HistKeys, Vars));
    TimeKeys = Args.TimeKeys(ismember(Args.TimeKeys, Vars));
    ImgHist  = figureHist(T, HistKeys);
    ImgTime  = figureTime(T, TimeKeys);
    ImgCam   = figureCamera(Cam, CamStat, CamKeys);

    % ---------- write the HTML
    if isempty(Args.OutFile)
        Tag     = regexprep(Info.Night, '\D', '');
        if isempty(Tag)
            Tag = datestr(now, 'yyyymmdd'); %#ok<DATST,TNOW1>
        end
        OutFile = fullfile(pwd, ['night_report_' Tag '.html']);
    else
        OutFile = Args.OutFile;
    end
    Title = Args.Title;
    if isempty(Title)
        Title = 'LAST night report';
    end
    writeHtml(OutFile, Title, Info, Summary, ImgHist, ImgTime, ImgCam, Args);
    if Args.Verbose
        fprintf('nightReport: wrote %s\n', OutFile);
    end
end

% ======================================================================
function Anom = addAnomaly(Anom, T, Vars, Key, TestFun, Text)
    % Append one anomaly class when its keyword exists and the test fires.
    if ismember(Key, Vars)
        Flag = TestFun(T.(Key));
        Flag = Flag(:) & true;
        if any(Flag)
            Idx = find(Flag);
            Ex  = strings(min(3, numel(Idx)), 1);
            for Ie = 1:numel(Ex)
                Ex(Ie) = T.Camera(Idx(Ie)) + " " + T.Visit(Idx(Ie));
            end
            Anom{end+1} = struct('Key', Key, 'N', numel(Idx), ...
                                 'Frac', numel(Idx)/height(T), 'Text', Text, ...
                                 'Examples', strjoin(Ex, ', ')); %#ok<AGROW>
        end
    end
end

% ----------------------------------------------------------------------
function B64 = figureHist(T, Keys)
    % Distribution panel, one histogram per metric.
    B64 = '';
    if ~isempty(Keys)
        Nk  = numel(Keys);
        Ncl = min(3, Nk);
        Nrw = ceil(Nk/Ncl);
        Fig = figure('Visible','off', 'Position',[10 10 420*Ncl 300*Nrw], 'Color','w');
        for Ik = 1:Nk
            V = T.(Keys{Ik});
            V = V(isfinite(V));
            subplot(Nrw, Ncl, Ik);
            if ~isempty(V)
                histogram(V, 40, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
                hold on;
                xline(median(V), 'r-', 'LineWidth',1.4);
                Lo = prctile(V,5);  Hi = prctile(V,95);
                xline(Lo, 'r--');   xline(Hi, 'r--');
                title(sprintf('%s: %.4g (%.4g-%.4g)', strrep(Keys{Ik},'_','\_'), ...
                              median(V), Lo, Hi), 'FontSize',9);
                xlim([min(Lo - 3*(Hi-Lo), min(V)), max(Hi + 3*(Hi-Lo), min(V))]);
            end
            grid on;
            xlabel(strrep(Keys{Ik},'_','\_'));  ylabel('N crops');
        end
        B64 = fig2base64(Fig);
    end
end

% ----------------------------------------------------------------------
function B64 = figureTime(T, Keys)
    % Run of the metrics across the night (per crop, UT hours).
    B64 = '';
    if ~isempty(Keys) && ismember('MIDJD', T.Properties.VariableNames)
        UT  = mod(T.MIDJD - 0.5, 1) * 24;
        Nk  = numel(Keys);
        Fig = figure('Visible','off', 'Position',[10 10 1100 230*Nk], 'Color','w');
        for Ik = 1:Nk
            V  = T.(Keys{Ik});
            Ok = isfinite(V) & isfinite(UT);
            subplot(Nk, 1, Ik);
            if any(Ok)
                plot(UT(Ok), V(Ok), '.', 'Color',[.45 .55 .8], 'MarkerSize',4);
                hold on;
                [Ub, ~, Ib] = unique(round(UT(Ok)*6)/6);   % 10-min bins
                Vb = accumarray(Ib, V(Ok), [], @median);
                plot(Ub, Vb, 'r-', 'LineWidth',1.5);
            end
            grid on;
            ylabel(strrep(Keys{Ik},'_','\_'));
            if Ik == Nk
                xlabel('UT [h]');
            end
        end
        B64 = fig2base64(Fig);
    end
end

% ----------------------------------------------------------------------
function B64 = figureCamera(Cam, CamStat, CamKeys)
    % Per-camera medians, each metric normalised to its across-camera median.
    B64 = '';
    if ~isempty(CamKeys) && ~isempty(Cam)
        Fig = figure('Visible','off', 'Position',[10 10 1100 420], 'Color','w');
        Rel = CamStat ./ median(CamStat, 1, 'omitnan');
        imagesc(Rel', [0.8 1.2]);
        colormap(parula);  Cb = colorbar;
        Cb.Label.String = 'median / across-camera median';
        set(gca, 'YTick',1:numel(CamKeys), 'YTickLabel',strrep(CamKeys,'_','\_'), ...
                 'XTick',1:numel(Cam), 'XTickLabel',strrep(cellstr(Cam),'LAST.01.',''), ...
                 'XTickLabelRotation',90, 'FontSize',8);
        title('Per-camera medians, relative to the night');
        B64 = fig2base64(Fig);
    end
end

% ----------------------------------------------------------------------
function B64 = fig2base64(Fig)
    % Render a figure to PNG and return its base64 encoding; always closes Fig.
    Tmp = [tempname '.png'];
    try
        exportgraphics(Fig, Tmp, 'Resolution',110);
    catch
        print(Fig, Tmp, '-dpng', '-r110');
    end
    close(Fig);
    Fid  = fopen(Tmp, 'rb');
    Byte = fread(Fid, Inf, '*uint8');
    fclose(Fid);
    delete(Tmp);
    B64 = char(matlab.net.base64encode(Byte));
end

% ----------------------------------------------------------------------
function writeHtml(OutFile, Title, Info, Summary, ImgHist, ImgTime, ImgCam, Args)
    % Compose the self-contained HTML document.
    Fid = fopen(OutFile, 'w');
    OC  = onCleanup(@() fclose(Fid));

    fprintf(Fid, '<!doctype html><html><head><meta charset="utf-8">\n');
    fprintf(Fid, '<title>%s %s</title>\n', Title, Info.Night);
    fprintf(Fid, ['<style>body{font-family:system-ui,Arial,sans-serif;margin:24px;' ...
                  'max-width:1200px;color:#222}h1{font-size:22px}h2{font-size:17px;' ...
                  'margin-top:28px;border-bottom:1px solid #ddd;padding-bottom:4px}' ...
                  'table{border-collapse:collapse;margin:8px 0;font-size:13px}' ...
                  'th,td{border:1px solid #ccc;padding:4px 9px;text-align:right}' ...
                  'th{background:#f2f4f8}td:first-child,th:first-child{text-align:left}' ...
                  'img{max-width:100%%;height:auto;margin:8px 0}' ...
                  '.bad{color:#b00;font-weight:600}.note{color:#555;font-size:13px}' ...
                  '</style></head><body>\n']);
    fprintf(Fid, '<h1>%s &mdash; night %s</h1>\n', Title, Info.Night);
    fprintf(Fid, '<p class="note">Generated %s from %s</p>\n', ...
            datestr(now, 'yyyy-mm-dd HH:MM'), Info.BasePath); %#ok<DATST,TNOW1>

    % --- session summary
    fprintf(Fid, '<h2>Session</h2>\n<table>\n');
    fprintf(Fid, '<tr><td>Cameras</td><td>%d</td></tr>\n', Info.Ncam);
    fprintf(Fid, '<tr><td>Visits</td><td>%d</td></tr>\n', Info.Nvisit);
    if isfinite(Info.NcropExpect)
        fprintf(Fid, '<tr><td>Coadd crops</td><td>%d / %d (%.1f%%)</td></tr>\n', ...
                Info.Ncrop, Info.NcropExpect, 100*Info.Ncrop/Info.NcropExpect);
    else
        fprintf(Fid, '<tr><td>Coadd crops</td><td>%d</td></tr>\n', Info.Ncrop);
    end
    if all(isfinite(Info.TimeSpanUT))
        fprintf(Fid, '<tr><td>Time span</td><td>%05.2f&ndash;%05.2f UT</td></tr>\n', ...
                Info.TimeSpanUT(1), Info.TimeSpanUT(2));
    end
    if isfinite(Info.CadenceSec)
        fprintf(Fid, '<tr><td>Cadence</td><td>%.0f s</td></tr>\n', Info.CadenceSec);
    end
    fprintf(Fid, '</table>\n');

    % --- metric table
    fprintf(Fid, '<h2>Quality metrics</h2>\n');
    fprintf(Fid, '<p class="note">Median over all crops, with the 5&ndash;95%% range.</p>\n');
    fprintf(Fid, '<table><tr><th>Metric</th><th>Median</th><th>5&ndash;95%%</th>');
    fprintf(Fid, '<th>N crops</th><th>blank</th></tr>\n');
    for Is = 1:numel(Summary.Stats)
        S = Summary.Stats(Is);
        Cls = '';
        if S.NaNFrac > 0.02
            Cls = ' class="bad"';
        end
        fprintf(Fid, '<tr><td>%s</td><td>%.4g</td><td>%.4g &ndash; %.4g</td><td>%d</td><td%s>%.1f%%</td></tr>\n', ...
                S.Key, S.Median, S.P05, S.P95, S.N, Cls, 100*S.NaNFrac);
    end
    fprintf(Fid, '</table>\n');

    % --- anomalies
    fprintf(Fid, '<h2>Anomalies</h2>\n');
    if isempty(Summary.Anomalies)
        fprintf(Fid, '<p>None of the configured failure classes fired.</p>\n');
    else
        fprintf(Fid, '<table><tr><th>Class</th><th>Keyword</th><th>N crops</th>');
        fprintf(Fid, '<th>of total</th><th>examples</th></tr>\n');
        for Ia = 1:numel(Summary.Anomalies)
            A = Summary.Anomalies(Ia);
            fprintf(Fid, '<tr><td>%s</td><td>%s</td><td class="bad">%d</td><td>%.2f%%</td><td>%s</td></tr>\n', ...
                    A.Text, A.Key, A.N, 100*A.Frac, A.Examples);
        end
        fprintf(Fid, '</table>\n');
    end

    % --- per-camera
    fprintf(Fid, '<h2>Per camera</h2>\n<table><tr><th>Camera</th><th>crops</th>');
    for Ik = 1:numel(Summary.CameraKeys)
        fprintf(Fid, '<th>%s</th>', Summary.CameraKeys{Ik});
    end
    fprintf(Fid, '</tr>\n');
    % Outlier test per metric: robust z against the across-camera spread, so
    % each metric is judged on its own natural scatter rather than a fixed
    % percentage (AST_ARMS and N_STARS vary far more between cameras than LIMMAG).
    MedAll = median(Summary.CameraStat, 1, 'omitnan');
    MadAll = 1.4826 * median(abs(Summary.CameraStat - MedAll), 1, 'omitnan');
    for Ic = 1:numel(Summary.Camera)
        fprintf(Fid, '<tr><td>%s</td><td>%d</td>', Summary.Camera(Ic), Summary.CameraNcrop(Ic));
        for Ik = 1:numel(Summary.CameraKeys)
            V   = Summary.CameraStat(Ic, Ik);
            Cls = '';
            if isfinite(V) && isfinite(MadAll(Ik)) && MadAll(Ik) > 0 && ...
                    abs(V - MedAll(Ik)) > Args.CameraOutlierZ * MadAll(Ik)
                Cls = ' class="bad"';
            end
            fprintf(Fid, '<td%s>%.4g</td>', Cls, V);
        end
        fprintf(Fid, '</tr>\n');
    end
    fprintf(Fid, '</table>\n');
    fprintf(Fid, ['<p class="note">Highlighted: more than %g robust sigma ' ...
                  '(1.4826&middot;MAD) from the across-camera median of that metric.</p>\n'], ...
            Args.CameraOutlierZ);
    if ~isempty(ImgCam)
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', ImgCam);
    end

    % --- figures
    if ~isempty(ImgHist)
        fprintf(Fid, '<h2>Distributions</h2>\n');
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', ImgHist);
    end
    if ~isempty(ImgTime)
        fprintf(Fid, '<h2>Across the night</h2>\n');
        fprintf(Fid, '<p class="note">Grey: one point per crop. Red: 10-minute median.</p>\n');
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', ImgTime);
    end

    fprintf(Fid, '<h2>Settings</h2>\n<p class="note">Sparse-crop threshold N_STARS &lt; %g; ', Args.MinStars);
    fprintf(Fid, 'weak-calibration threshold PT_NCALI &lt; %g.</p>\n', Args.MinNCalib);
    fprintf(Fid, '</body></html>\n');
end
