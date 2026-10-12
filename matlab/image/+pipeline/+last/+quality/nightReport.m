function [OutFile, Summary] = nightReport(Data, Args)
    % Build a one-night quality report (self-contained HTML) from coadd header metrics.
    %   Aggregates the per-crop table produced by pipeline.last.quality.harvestNight
    %   into: a session summary, a median +/- 5-95% table of the quality metrics, a
    %   per-camera breakdown, an anomaly census (failure classes with counts), and
    %   figures - distributions of the key metrics and their run across the night.
    %
    %   Photometric calibration has two modes (issue #1381), and the report keeps
    %   them apart. The Tran2D fit flag PT_P_F1 says which one a crop got: 1 for
    %   the regular mode (the full transmission model was fitted), 0 for the
    %   reduced mode (the calibrator count fell below MinCalibrators and only
    %   Norm was fitted, leaving a flat zero point), blank for a crop that was
    %   not photometrically calibrated. Reduced-mode crops carry the unfitted
    %   field term in their residuals, so their PT_ARMS is several times larger
    %   by construction. They are therefore excluded from the PT_* medians - of
    %   the night and of each camera - and reported in a section of their own,
    %   rather than being pooled in and dragging the night's photometric numbers
    %   or flagging whichever camera happened to observe spectrum-poor fields. A
    %   table harvested before PT_P_F1 was recorded carries no mode information,
    %   and then every crop is treated as regular, as before.
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
    %                   anomaly. Default is 10. Independent of the pipeline's own
    %                   MinCalibrators, which decides the calibration mode.
    %            'CameraKeys' - Metrics tabulated per camera.
    %                   Default {'LIMMAG','FWHM','AST_ARMS','PT_ARMS','N_STARS'}.
    %            'CameraOutlierZ' - A camera metric is highlighted when it sits
    %                   this many robust sigma (1.4826*MAD, computed across
    %                   cameras) from the across-camera median. Default is 3.
    %            'Title' - Report title. Default is '' -> 'LAST night report'.
    %            'Verbose' - Print progress. Default is true.
    % Output : - Path of the written HTML file.
    %          - A structure with the aggregated numbers (per-metric statistics,
    %            per-camera table, anomaly counts, and the regular / reduced /
    %            uncalibrated crop counts), for programmatic use.
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
    if (~isfield(Info,'DurationHr') || ~isfinite(Info.DurationHr)) && ...
            ismember('MIDJD', T.Properties.VariableNames)
        % Older caches predate DurationHr: derive it from the table.
        UTd = mod(mod(T.MIDJD - 0.5, 1) * 24 - 12, 24) + 12;
        UTd = UTd(isfinite(UTd));
        if ~isempty(UTd)
            Info.DurationHr = max(UTd) - min(UTd);
        end
    end

    Vars = T.Properties.VariableNames;

    % ---------- calibration mode
    % Photometric calibration runs in one of two modes (issue #1381). PT_P_F*
    % is the Tran2D fit flag: 1 when the full transmission model was fitted
    % (regular mode), 0 when the calibrator count fell below MinCalibrators and
    % the fit fell back to Norm alone, with a spatially flat zero point (reduced
    % mode). A blank flag means the crop was not photometrically calibrated at
    % all. Reduced-mode crops ARE calibrated, but they carry the unfitted field
    % term in their residuals, so their PT_ARMS is several times larger; pooled
    % with the regular ones they would drag every photometric median and make
    % whichever camera happened to observe spectrum-poor fields look faulty.
    [IsReduced, IsRegular, HasMode] = calibMode(T);
    KeepPhot = ~IsReduced;

    % ---------- per-metric statistics
    Summary          = struct;
    Summary.Night    = Info.Night;
    Summary.Info     = Info;
    Summary.HasMode  = ismember('PT_P_F1', Vars);
    Summary.NRegular = sum(IsRegular);
    Summary.NReduced = sum(IsReduced);
    Summary.NNoMode  = sum(~HasMode);
    Keys             = Args.SummaryKeys(ismember(Args.SummaryKeys, Vars));
    % The PT_* rows describe the regular-mode population; the reduced-mode
    % crops are summarised separately so neither distribution hides the other.
    IsPhot           = startsWith(Keys, 'PT_');
    Rows             = repmat({true(height(T),1)}, 1, numel(Keys));
    Rows(IsPhot)     = {KeepPhot};
    Summary.Stats    = statsFor(T, Keys, Rows);
    if any(IsReduced)
        Summary.StatsReduced = statsFor(T, Keys(IsPhot), ...
                                        repmat({IsReduced}, 1, sum(IsPhot)));
    else
        Summary.StatsReduced = [];
    end

    % ---------- anomaly census
    Anom = {};
    Anom = addAnomaly(Anom, T, Vars, 'PT_P_F1', @(v) isfinite(v) & v == 0, ...
        'Reduced-mode calibration: fewer calibrators than MinCalibrators, Norm only, flat zero point');
    Anom = addAnomaly(Anom, T, Vars, 'PT_DOF', @(v) v <= 0, ...
        'Underdetermined photometric fit (PT_DOF <= 0): superseded by the reduced mode, should no longer fire');
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
    CamFracRed  = accumarray(Icam, double(IsReduced), [Ncam 1]) ./ CamNcrop;
    IsPhotCam   = startsWith(CamKeys, 'PT_');
    for Ikey = 1:numel(CamKeys)
        V = T.(CamKeys{Ikey});
        if IsPhotCam(Ikey)
            Keep = KeepPhot;
        else
            Keep = true(height(T), 1);
        end
        for Ic = 1:Ncam
            Vc = V(Icam == Ic & Keep);
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
    Summary.CameraFracReduced = CamFracRed;

    % ---------- figures
    if Args.Verbose
        fprintf('nightReport: rendering figures\n');
    end
    HistKeys = Args.HistKeys(ismember(Args.HistKeys, Vars));
    TimeKeys = Args.TimeKeys(ismember(Args.TimeKeys, Vars));
    ImgHist  = figureHist(T, HistKeys, IsReduced);
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

% ======================================================================
function [IsReduced, IsRegular, HasMode] = calibMode(T)
    % Split the crops by photometric calibration mode using the Tran2D fit
    % flag PT_P_F1: 1 = regular (full model), 0 = reduced (Norm only),
    % blank = not calibrated. Tables harvested before the flag was recorded
    % carry no mode information, and then every crop counts as regular.
    N         = height(T);
    IsReduced = false(N, 1);
    HasMode   = false(N, 1);
    if ismember('PT_P_F1', T.Properties.VariableNames)
        V         = T.PT_P_F1;
        HasMode   = isfinite(V);
        IsReduced = HasMode & V == 0;
    end
    IsRegular = HasMode & ~IsReduced;
end

% ======================================================================
function Stats = statsFor(T, Keys, Rows)
    % Median, 5-95 per cent range and blank fraction of each key, each over
    % its own subset of rows (Rows{Ikey} is a logical column into T).
    Out = cell(numel(Keys), 1);
    for Ikey = 1:numel(Keys)
        V  = T.(Keys{Ikey});
        V  = V(Rows{Ikey});
        Vf = V(isfinite(V));
        S  = struct('Key', Keys{Ikey}, 'N', numel(Vf), 'Median', NaN, ...
                    'P05', NaN, 'P95', NaN, 'NaNFrac', 1 - numel(Vf)/max(1,numel(V)));
        if ~isempty(Vf)
            S.Median = median(Vf);
            S.P05    = prctile(Vf, 5);
            S.P95    = prctile(Vf, 95);
        end
        Out{Ikey} = S;
    end
    Stats = [Out{:}];
end

% ----------------------------------------------------------------------
function B64 = figureHist(T, Keys, IsReduced)
    % Distribution panel, one histogram per metric. A PT_* panel describes the
    % regular-mode crops, with the reduced-mode ones overlaid in orange: the
    % two populations have genuinely different widths and a single histogram
    % of their union would misrepresent both.
    B64 = '';
    if ~isempty(Keys)
        Nk  = numel(Keys);
        Ncl = min(3, Nk);
        Nrw = ceil(Nk/Ncl);
        Fig = figure('Visible','off', 'Position',[10 10 420*Ncl 300*Nrw], 'Color','w');
        for Ik = 1:Nk
            V = T.(Keys{Ik});
            if startsWith(Keys{Ik}, 'PT_')
                Vred = V(IsReduced);
                V    = V(~IsReduced);
                Vred = Vred(isfinite(Vred));
            else
                Vred = [];
            end
            V = V(isfinite(V));
            subplot(Nrw, Ncl, Ik);
            if ~isempty(V)
                histogram(V, 40, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
                hold on;
                xline(median(V), 'r-', 'LineWidth',1.4);
                Lo = prctile(V,5);  Hi = prctile(V,95);
                xline(Lo, 'r--');   xline(Hi, 'r--');
                XHi = max(Hi + 3*(Hi-Lo), min(V));
                if ~isempty(Vred)
                    histogram(Vred, 'FaceColor',[.90 .50 .15], 'EdgeColor','none');
                    XHi = max(XHi, prctile(Vred, 95));
                end
                title(sprintf('%s: %.4g (%.4g-%.4g)%s', strrep(Keys{Ik},'_','\_'), ...
                              median(V), Lo, Hi, redTag(Vred)), 'FontSize',9);
                xlim([min(Lo - 3*(Hi-Lo), min(V)), XHi]);
            end
            grid on;
            xlabel(strrep(Keys{Ik},'_','\_'));  ylabel('N crops');
        end
        B64 = fig2base64(Fig);
    end
end

% ----------------------------------------------------------------------
function Tag = redTag(Vred)
    % Annotation appended to a PT_* panel title when reduced-mode crops exist.
    if isempty(Vred)
        Tag = '';
    else
        Tag = sprintf('  |  %d reduced: %.4g', numel(Vred), median(Vred));
    end
end

% ----------------------------------------------------------------------
function B64 = figureTime(T, Keys)
    % Run of the metrics across the night (per crop, UT hours).
    B64 = '';
    if ~isempty(Keys) && ismember('MIDJD', T.Properties.VariableNames)
        % Night-continuous time axis: a night starts in the evening of its date
        % and runs past midnight into the next, so plotting raw UT would split
        % it, with the morning hours drawn to the LEFT of the evening ones.
        % Shift to a local-noon origin (evening 12..24, morning 24..36) and
        % label the ticks back in UT.
        UT  = mod(mod(T.MIDJD - 0.5, 1) * 24 - 12, 24) + 12;
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
            % Ticks every two hours, labelled as the UT hour they represent.
            Lo = floor(min(UT(Ok))); Hi = ceil(max(UT(Ok)));
            if isfinite(Lo) && isfinite(Hi) && Hi > Lo
                Tk = Lo:2:Hi;
                set(gca, 'XTick', Tk, 'XTickLabel', ...
                    arrayfun(@(h) sprintf('%02d', mod(h,24)), Tk, 'UniformOutput',false));
                xlim([Lo Hi]);
            end
            if Ik == Nk
                xlabel('UT [h]  (night of the title date, continuing past midnight)');
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
    fprintf(Fid, '<h1>%s &mdash; night of %s</h1>\n', Title, Info.Night);
    fprintf(Fid, ['<p class="note">The date is the evening the night began; ' ...
                  'visits after midnight belong to the following day.</p>\n']);
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
        % Round to the minute BEFORE splitting, otherwise 19.000 - 1e-9 prints
        % as 18:60.
        HM = @(H) deal(mod(floor(round(H*60)/60), 24), mod(round(H*60), 60));
        [H1, M1] = HM(Info.TimeSpanUT(1));
        [H2, M2] = HM(Info.TimeSpanUT(2));
        fprintf(Fid, '<tr><td>First visit</td><td>%02d:%02d UT</td></tr>\n', H1, M1);
        fprintf(Fid, '<tr><td>Last visit</td><td>%02d:%02d UT%s</td></tr>\n', H2, M2, ...
                repmat(' (next day)', 1, double(Info.TimeSpanUT(2) < Info.TimeSpanUT(1))));
    end
    if isfield(Info,'DurationHr') && isfinite(Info.DurationHr)
        fprintf(Fid, '<tr><td>Duration</td><td>%.1f h</td></tr>\n', Info.DurationHr);
    end
    if isfinite(Info.CadenceSec)
        fprintf(Fid, '<tr><td>Cadence</td><td>%.0f s</td></tr>\n', Info.CadenceSec);
    end
    % Calibration modes (issue #1381). Only meaningful when the Tran2D fit flag
    % was harvested; tables cached before that carry no mode information.
    if isfield(Summary, 'HasMode') && Summary.HasMode
        NCal = Summary.NRegular + Summary.NReduced;
        fprintf(Fid, '<tr><td>Calibrated crops: regular / reduced mode</td><td>%d / %d', ...
                Summary.NRegular, Summary.NReduced);
        if NCal > 0
            fprintf(Fid, ' (%.2f%% reduced)', 100*Summary.NReduced/NCal);
        end
        fprintf(Fid, '</td></tr>\n');
        if Summary.NNoMode > 0
            fprintf(Fid, '<tr><td>Not photometrically calibrated</td><td>%d</td></tr>\n', ...
                    Summary.NNoMode);
        end
    end
    fprintf(Fid, '</table>\n');

    % --- metric table
    fprintf(Fid, '<h2>Quality metrics</h2>\n');
    fprintf(Fid, '<p class="note">Median over all crops, with the 5&ndash;95%% range.');
    if isfield(Summary, 'NReduced') && Summary.NReduced > 0
        fprintf(Fid, [' The PT_* rows cover the regular-mode crops only; the %d ' ...
                      'reduced-mode ones follow in their own table.'], Summary.NReduced);
    end
    fprintf(Fid, '</p>\n');
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

    % --- the reduced-mode population, reported next to the regular one
    if isfield(Summary, 'StatsReduced') && ~isempty(Summary.StatsReduced)
        fprintf(Fid, '<h2>Reduced-mode calibration</h2>\n');
        fprintf(Fid, ['<p class="note">%d crops (%.2f%% of the calibrated ones) fell ' ...
                      'below MinCalibrators and were calibrated with Norm alone, so the ' ...
                      'zero point is flat across the crop and the unfitted field term ' ...
                      'stays in the residuals. A larger PT_ARMS here is the expected cost ' ...
                      'of that, not a fault. PT_DOF is PT_NCALI&nbsp;&minus;&nbsp;1 and ' ...
                      'every PT_P_F* is 0.</p>\n'], Summary.NReduced, ...
                100*Summary.NReduced/max(1, Summary.NRegular + Summary.NReduced));
        fprintf(Fid, '<table><tr><th>Metric</th><th>Median</th><th>5&ndash;95%%</th>');
        fprintf(Fid, '<th>N crops</th><th>blank</th></tr>\n');
        for Is = 1:numel(Summary.StatsReduced)
            S = Summary.StatsReduced(Is);
            fprintf(Fid, '<tr><td>%s</td><td>%.4g</td><td>%.4g &ndash; %.4g</td><td>%d</td><td>%.1f%%</td></tr>\n', ...
                    S.Key, S.Median, S.P05, S.P95, S.N, 100*S.NaNFrac);
        end
        fprintf(Fid, '</table>\n');
    end

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
    ShowRed = isfield(Summary, 'CameraFracReduced') && ...
              isfield(Summary, 'NReduced') && Summary.NReduced > 0;
    fprintf(Fid, '<h2>Per camera</h2>\n<table><tr><th>Camera</th><th>crops</th>');
    if ShowRed
        fprintf(Fid, '<th>reduced</th>');
    end
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
        if ShowRed
            fprintf(Fid, '<td>%.1f%%</td>', 100*Summary.CameraFracReduced(Ic));
        end
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
                  '(1.4826&middot;MAD) from the across-camera median of that metric. ' ...
                  'The PT_* medians are taken over the regular-mode crops, so a camera is ' ...
                  'not flagged for the reduced-mode crops it was handed; the reduced ' ...
                  'column says how many those were.</p>\n'], ...
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
    fprintf(Fid, 'weak-calibration threshold PT_NCALI &lt; %g; ', Args.MinNCalib);
    fprintf(Fid, ['calibration mode read from the Tran2D fit flag PT_P_F1 ' ...
                  '(1 = regular, 0 = reduced, blank = not calibrated).</p>\n']);
    fprintf(Fid, '</body></html>\n');
end
