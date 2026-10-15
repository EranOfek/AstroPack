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
    %            'LogXKeys' - Metrics whose histogram gets a log x axis with
    %                   log-spaced bins, for quantities that span decades.
    %                   Default is the RMS family
    %                   {'AST_ARMS','PT_ARMS','PT_RMS','PH_RMS','RP_MRMS'}.
    %            'LogY' - Log count axis on the histograms, so a tail of a few
    %                   crops stays visible next to a peak of thousands.
    %                   Default is true.
    %            'WorstKey' - Metric whose worst crops are listed and marked in
    %                   the distribution panel. Default is 'FWHM'.
    %            'WorstHigh' - true when a LARGE value of WorstKey is the bad
    %                   one (FWHM, ARMS), false when a small one is (LIMMAG).
    %                   Default is true.
    %            'NWorst' - How many worst crops to list. Default is 10.
    %            'FieldCalibFile' - Table of the expected calibrator count per
    %                   crop and field, read through
    %                   pipeline.last.quality.fieldCalibrators. Default '' uses
    %                   the committed last_field_calibrators.csv; 'none' skips
    %                   the field check.
    %            'MinExpectedCalib' - A field whose expected calibrator count
    %                   per crop is below this is starved: its crops cannot be
    %                   calibrated whatever the night does, and their reduced
    %                   mode is not an anomaly. At or above it, a reduced-mode
    %                   crop is one. Default is 16, the pipeline's
    %                   MinCalibrators (issue #1381).
    %            'Title' - Report title. Default is '' -> 'LAST night report'.
    %            'Verbose' - Print progress. Default is true.
    %   Fields are checked against the expected calibrator count of their
    %   pointing, which splits the reduced-mode crops in two. Under a thin
    %   calibrator sky the fallback is the designed behaviour and nothing is
    %   wrong: those crops are calibrated, with a flat zero point, and the table
    %   is reported as context for their larger PT_ARMS. On a field expected to
    %   hold calibrators the same fallback IS a fault, because the night took
    %   them away rather than the sky never having them, and only that case
    %   enters the anomaly census.
    %
    %   The report opens with a problem summary of two or three sentences -
    %   failure classes, coverage and calibration mode, cameras and the worst
    %   crops - which states explicitly when nothing is wrong. Every plotted
    %   and tabulated metric carries its unit.
    %
    % Output : - Path of the written HTML file.
    %          - A structure with the aggregated numbers (per-metric statistics,
    %            per-camera table, anomaly counts, the regular / reduced /
    %            uncalibrated crop counts, the camera-outlier mask, the worst
    %            crops by WorstKey and the problem summary), for programmatic use.
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
        Args.LogXKeys cell        = {'AST_ARMS','PT_ARMS','PT_RMS','PH_RMS','RP_MRMS'}
        Args.LogY logical         = true
        Args.WorstKey char        = 'FWHM'
        Args.WorstHigh logical    = true
        Args.NWorst (1,1) double  = 10
        Args.FieldCalibFile char  = ''
        Args.MinExpectedCalib (1,1) double = 16
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

    % ---------- expected calibrators of the fields that were observed
    % A field short of Gaia XP spectra cannot reach the calibrator floor
    % however good the night is (issue #1381), so the shortage belongs to the
    % pointing and can be named instead of being discovered per visit.
    [ExpCalib, Summary.Fields] = fieldExpected(T, IsReduced, Args);
    IsStarved = false(height(T), 1);
    % A starved field falling back to the reduced mode is the designed
    % behaviour and no anomaly at all. The anomaly is the opposite case: a field
    % that holds calibrators in abundance and still went reduced, which means
    % the night, not the sky, took them away - cloud, extinction, defocus,
    % a masked region. Crops whose field is not in the table stay unknown and
    % are counted apart rather than accused.
    Summary.NReducedStarved = 0;
    Summary.NReducedRich    = 0;
    Summary.NReducedUnknown = sum(IsReduced);
    if ~isempty(ExpCalib)
        T.EXPCALIB   = ExpCalib;
        KnownField   = isfinite(ExpCalib);
        IsStarved    = KnownField & ExpCalib < Args.MinExpectedCalib;
        ReducedRich  = IsReduced & KnownField & ~IsStarved;
        T.REDUCED_RICH = double(ReducedRich);
        Vars         = T.Properties.VariableNames;
        Summary.NReducedStarved = sum(IsReduced & IsStarved);
        Summary.NReducedRich    = sum(ReducedRich);
        Summary.NReducedUnknown = sum(IsReduced & ~KnownField);
    end

    % ---------- anomaly census
    % Only things that went wrong belong here. The reduced mode as such does
    % not: on a field whose sky is thin it is the designed behaviour, and where
    % it is not, REDUCED_RICH below says so. Each class also reports how many of
    % its crops sit on a calibrator-poor field, which answers "is this just the
    % sky?" for the classes that a thin field drags along with it (PT_NCALI,
    % PT_DOF, PT_ARMS).
    Anom = {};
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_DOF', @(v) v <= 0, ...
        'Underdetermined photometric fit (PT_DOF <= 0): superseded by the reduced mode, should no longer fire');
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_ARMS', @(v) v < 1e-6, ...
        'PT_ARMS at or below 1e-6 (reported as a perfect fit)');
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_NCALI', @(v) v < Args.MinNCalib, ...
        sprintf('Fewer than %g photometric calibrators', Args.MinNCalib));
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'REDUCED_RICH', @(v) v == 1, ...
        sprintf(['Reduced-mode calibration on a field that is NOT calibrator-poor ' ...
                 '(at least %g expected per crop): the calibrators were lost by the ' ...
                 'night, not missing from the sky'], Args.MinExpectedCalib));
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'AST_ARMS', @(v) ~isfinite(v), ...
        'No astrometric solution (AST_ARMS blank)');
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'N_STARS', @(v) v < Args.MinStars, ...
        sprintf('Fewer than %g detected sources', Args.MinStars));
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'LIMMAG', @(v) ~isfinite(v), ...
        'No limiting magnitude (LIMMAG blank)');
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_ZP', @(v) ~isfinite(v), ...
        'No photometric zero point (PT_ZP blank)');
    if isfinite(Info.NcropExpect) && Info.NcropExpect > Info.Ncrop
        Anom{end+1} = struct('Key','products', 'N', Info.NcropExpect - Info.Ncrop, ...
            'Frac', (Info.NcropExpect - Info.Ncrop)/Info.NcropExpect, ...
            'NStarved', 0, ...
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
    % Which (camera, metric) cells are outliers is needed twice - by the
    % problem summary at the top and by the per-camera table - so decide once.
    Summary.CameraOutlier = cameraOutliers(CamStat, Args.CameraOutlierZ);

    % ---------- worst crops by one metric (default FWHM)
    Summary.Worst = worstCrops(T, Args.WorstKey, Args.NWorst, Args.WorstHigh);

    % ---------- problem summary (the opening paragraph of the report)
    Summary.Clean = isempty(Summary.Anomalies) && ...
                    ~any(Summary.CameraOutlier(:)) && ...
                    ~(isfinite(Info.NcropExpect) && Info.NcropExpect > Info.Ncrop);
    Summary.Problems = problemLines(Summary, Info, Args);

    % ---------- figures
    if Args.Verbose
        fprintf('nightReport: rendering figures\n');
    end
    HistKeys = Args.HistKeys(ismember(Args.HistKeys, Vars));
    TimeKeys = Args.TimeKeys(ismember(Args.TimeKeys, Vars));
    ImgHist  = figureHist(T, HistKeys, IsReduced, Args, Summary);
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
function Anom = addAnomaly(Anom, T, Vars, IsStarved, Key, TestFun, Text)
    % Append one anomaly class when its keyword exists and the test fires.
    %   NStarved counts the crops of the class that sit on a field whose sky is
    %   thin anyway: for PT_NCALI and its relatives a class that is mostly
    %   starved-field crops is the sky being read correctly, not a fault.
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
                                 'Frac', numel(Idx)/height(T), ...
                                 'NStarved', sum(Flag & IsStarved), 'Text', Text, ...
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
function B64 = figureHist(T, Keys, IsReduced, Args, Summary)
    % Distribution panel, one histogram per metric.
    %   Every panel is split by calibration mode: the reduced-mode crops
    %   (PT_P_F1 = 0) are drawn in orange over the others. For the PT_* metrics
    %   the two populations have genuinely different widths, and for the rest
    %   the overlay answers a different question - what the fields that starved
    %   the calibration look like in seeing, depth and source count.
    %   Counts are on a log axis (Args.LogY), so a tail of a few crops stays
    %   visible next to a peak of thousands, and the metrics in Args.LogXKeys
    %   get a log x axis with log-spaced bins. In the Args.WorstKey panel the
    %   cut of the worst-crop table is marked, so those crops can be placed in
    %   the distribution.
    B64 = '';
    if ~isempty(Keys)
        Nk  = numel(Keys);
        Ncl = min(3, Nk);
        Nrw = ceil(Nk/Ncl);
        Fig = figure('Visible','off', 'Position',[10 10 420*Ncl 300*Nrw], 'Color','w');
        for Ik = 1:Nk
            Key    = Keys{Ik};
            Vall   = T.(Key);
            Vred   = Vall(IsReduced);
            V      = Vall(~IsReduced);
            V      = V(isfinite(V));
            Vred   = Vred(isfinite(Vred));
            IsLog  = ismember(Key, Args.LogXKeys);
            Nbelow = 0;
            if IsLog
                % A log axis cannot carry zero or negative values, and PT_ARMS
                % does reach ~1e-16 on an underdetermined fit. Count what drops
                % out instead of letting it disappear.
                Nbelow = sum(V <= 0) + sum(Vred <= 0);
                V      = V(V > 0);
                Vred   = Vred(Vred > 0);
            end
            subplot(Nrw, Ncl, Ik);
            if ~isempty(V)
                Lo = prctile(V, 5);  Hi = prctile(V, 95);
                if IsLog
                    [Edges, NbelowAxis] = logEdges([V; Vred]);
                    Nbelow = Nbelow + NbelowAxis;
                    histogram(V, Edges, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
                    hold on;
                    if ~isempty(Vred)
                        histogram(Vred, Edges, 'FaceColor',[.90 .50 .15], 'EdgeColor','none');
                    end
                    set(gca, 'XScale','log');
                    xlim([Edges(1) Edges(end)]);
                    % decade ticks: MATLAB labels only one of them on a panel
                    % this small, which leaves the axis unreadable
                    set(gca, 'XTick', 10.^(floor(log10(Edges(1))):ceil(log10(Edges(end)))));
                else
                    histogram(V, 40, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
                    hold on;
                    XHi = max(Hi + 3*(Hi-Lo), min(V));
                    if ~isempty(Vred)
                        histogram(Vred, 'FaceColor',[.90 .50 .15], 'EdgeColor','none');
                        XHi = max(XHi, prctile(Vred, 95));
                    end
                    if strcmp(Key, Args.WorstKey) && isfinite(Summary.Worst.Cut)
                        XHi = max(XHi, Summary.Worst.Cut);   % keep the cut in view
                    end
                    % Start at the data: a margin three spreads below the 5th
                    % percentile only pushed narrow distributions into a corner,
                    % and for FWHM it ran the axis into negative arcsec.
                    XLo = min(V);
                    if XHi > XLo
                        XLo = XLo - 0.02*(XHi - XLo);
                    end
                    xlim([XLo, XHi]);
                end
                xline(median(V), 'r-', 'LineWidth',1.4);
                xline(Lo, 'r--');   xline(Hi, 'r--');
                % The cut of the worst-crop table, so the listed crops can be
                % found in the distribution.
                if strcmp(Key, Args.WorstKey) && isfinite(Summary.Worst.Cut)
                    xline(Summary.Worst.Cut, 'k-', 'LineWidth',1.2, ...
                          'Label',sprintf('worst %d', Args.NWorst), ...
                          'LabelVerticalAlignment','bottom', ...
                          'LabelHorizontalAlignment','left', 'FontSize',7);
                end
                if Args.LogY
                    set(gca, 'YScale','log');
                    YL   = ylim;
                    YTop = YL(2) * 2.5;          % headroom for the legend
                    ylim([0.7 YTop]);
                    set(gca, 'YTick', 10.^(0:max(0, floor(log10(YL(2))))));
                end
                Note = strtrim(strrep(redTag(Vred), '|', ''));
                if Nbelow > 0
                    Note = sprintf('%s  %d below axis', Note, Nbelow);
                end
                % Two lines: a one-line title of this length collides with the
                % neighbouring panel.
                title({sprintf('%s: %.4g (%.4g-%.4g)', strrep(Key,'_','\_'), median(V), Lo, Hi), ...
                       Note}, 'FontSize',8, 'FontWeight','normal');
                if ~isempty(Vred)
                    legend({'regular + uncalibrated','reduced mode'}, ...
                           'FontSize',7, 'Location','best', 'Box','off');
                end
            end
            grid on;
            xlabel(axisLabel(Key));  ylabel('N crops');
        end
        B64 = fig2base64(Fig);
    end
end

% ----------------------------------------------------------------------
function [Edges, Nbelow] = logEdges(V)
    % Log-spaced bin edges over the positive values of V.
    %   The low end is clamped when the dynamic range is extreme - PT_ARMS
    %   reaches 1e-16 on an underdetermined fit, and spanning that would
    %   squash the whole night into the last bin. Nbelow reports how many
    %   values fall below the clamped edge.
    V      = V(isfinite(V) & V > 0);
    Nbelow = 0;
    Edges  = [];
    if ~isempty(V)
        Lo = min(V);
        Hi = max(V);
        if Hi/Lo > 1e4
            Lo = prctile(V, 1)/3;
        end
        if ~(Hi > Lo)                  % all values equal
            Lo = Hi/2;  Hi = Hi*2;
        end
        Nbelow = sum(V < Lo);
        Edges  = logspace(log10(Lo), log10(Hi), 41);
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
            ylabel(axisLabel(Keys{Ik}));
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
function U = unitFor(Key)
    % Physical unit of a header metric, for axis labels and table headings.
    %   FWHM and the astrometric residuals are written in arcsec (imProc.psf.fwhm
    %   converts with the WCS scale; AST_ARMS/AST_ERRM are degrees x ARCSEC_DEG),
    %   BACKMAG is a surface brightness, and the photometric residuals are
    %   magnitudes. Counts and bare coefficients (AIRMASS, PT_CHI2, PT_CTA, the
    %   fit flags) have no unit and return ''.
    switch upper(Key)
        case {'FWHM','AST_ARMS','AST_ERRM'}
            U = 'arcsec';
        case {'LIMMAG','PT_ZP','PT_ARMS','PT_RMS','PH_RMS','RP_MRMS','APC0_PS'}
            U = 'mag';
        case 'BACKMAG'
            U = 'mag/arcsec^2';
        case {'MED_X2','MED_Y2','MED_XY'}
            U = 'pix^2';
        case {'MNTTEMP','CAMTEMP'}
            U = 'deg C';
        case 'EXPTIME'
            U = 's';
        case 'MIDJD'
            U = 'd';
        case {'N_STARS','AST_NSRC','PT_NCALI','PT_DOF','NCOADD','EXPCALIB'}
            U = 'count';
        otherwise
            U = '';
    end
end

% ----------------------------------------------------------------------
function L = axisLabel(Key)
    % Axis label: the keyword with its unit, TeX-escaped for the figure.
    U = unitFor(Key);
    L = strrep(Key, '_', '\_');
    if ~isempty(U)
        L = [L ' [' strrep(U, '_', '\_') ']'];
    end
end

% ----------------------------------------------------------------------
function L = tableLabel(Key)
    % Table heading: the keyword with its unit, as plain text for the HTML.
    U = unitFor(Key);
    L = Key;
    if ~isempty(U)
        L = [Key ' [' U ']'];
    end
end

% ----------------------------------------------------------------------
function Mask = cameraOutliers(CamStat, Z)
    % Logical [Ncam x Nkey]: camera medians more than Z robust sigma
    % (1.4826*MAD across cameras) from the across-camera median of that metric,
    % so each metric is judged on its own natural scatter.
    MedAll = median(CamStat, 1, 'omitnan');
    MadAll = 1.4826 * median(abs(CamStat - MedAll), 1, 'omitnan');
    Mask   = isfinite(CamStat) & isfinite(MadAll) & MadAll > 0 & ...
             abs(CamStat - MedAll) > Z .* MadAll;
end

% ----------------------------------------------------------------------
function W = worstCrops(T, Key, N, High)
    % The N worst crops by one metric, and where the worst tail sits.
    %   .Rows     - the N worst crops, worst first, with context columns.
    %   .Cut      - value of the Nth worst crop (marked in the histogram).
    %   .Tail     - the 95th percentile (5th when High is false): the tail cut.
    %   .ByCrop   - per CROPID: median of the metric and how many tail crops it
    %               holds. One CROPID across many cameras means a persistently
    %               soft focal-plane position rather than a bad night.
    %   .ByCamera - per camera: how many tail crops, and the fraction of its own.
    W = struct('Key',Key, 'High',High, 'Cut',NaN, 'Tail',NaN, 'Ntail',0, ...
               'Rows',table(), 'ByCrop',table(), 'ByCamera',table());
    Vars = T.Properties.VariableNames;
    if ismember(Key, Vars)
        V  = T.(Key);
        Ok = isfinite(V);
        if any(Ok)
            Sgn = 1;
            Pct = 95;
            if ~High
                Sgn = -1;
                Pct = 5;
            end
            Idx      = find(Ok);
            [~, Ord] = sort(Sgn * V(Idx), 'descend');
            Idx      = Idx(Ord);
            Nw       = min(N, numel(Idx));
            W.Cut    = V(Idx(Nw));
            W.Tail   = prctile(V(Ok), Pct);
            IsTail   = Ok & (Sgn * V > Sgn * W.Tail);
            W.Ntail  = sum(IsTail);

            % the worst crops themselves, with whatever context exists
            Ctx  = {'CROPID','FIELDID','LIMMAG','N_STARS','AST_ARMS','PT_ARMS'};
            Ctx  = Ctx(ismember(Ctx, Vars) & ~strcmp(Ctx, Key));
            Rows = T(Idx(1:Nw), [{'Camera','Visit'}, {Key}, Ctx]);
            W.Rows = Rows;

            % tail composition by focal-plane position
            if ismember('CROPID', Vars)
                OkC = Ok & isfinite(T.CROPID);   % a blank CROPID cannot be grouped
                if any(OkC)
                    [Cid, ~, Ic] = unique(T.CROPID(OkC));
                    Med = accumarray(Ic, V(OkC), [], @median);
                    Cnt = accumarray(Ic, double(IsTail(OkC)), [numel(Cid) 1]);
                    Byc = table(Cid, Med, Cnt, 'VariableNames', {'CROPID','Median','Ntail'});
                    W.ByCrop = sortrows(Byc, 'Ntail', 'descend');
                end
            end

            % tail composition by camera
            [Cam, ~, Icam] = unique(T.Camera);
            Ncam  = numel(Cam);
            Ntot  = accumarray(Icam, 1, [Ncam 1]);
            Ntl   = accumarray(Icam, double(IsTail), [Ncam 1]);
            Bycam = table(Cam, Ntot, Ntl, 100*Ntl./max(1,Ntot), ...
                          'VariableNames', {'Camera','Ncrop','Ntail','PctTail'});
            W.ByCamera = sortrows(Bycam, 'Ntail', 'descend');
        end
    end
end

% ----------------------------------------------------------------------
function [ExpCalib, Fields] = fieldExpected(T, IsReduced, Args)
    % Expected calibrators per crop for the fields this night observed.
    %   ExpCalib is one value per crop (NaN when the field is unknown), and
    %   Fields is one row per observed field, poorest first:
    %   FIELDID, Expected, SpecDens, Ncrop, Nvisit, NReduced, MedNCALI.
    %   NReduced next to Expected is the point of the table: it says whether
    %   the reduced-mode crops of the night were the predictable consequence of
    %   where the telescope was pointed.
    ExpCalib = [];
    Fields   = table();
    Vars     = T.Properties.VariableNames;
    if ~ismember('FIELDID', Vars) || strcmpi(Args.FieldCalibFile, 'none')
        return
    end
    F = pipeline.last.quality.fieldCalibrators('FileName', Args.FieldCalibFile);
    if isempty(F)
        return
    end

    % FIELDID is harvested as a string and is a bare number for the survey grid,
    % where it indexes the field table; a named field has no entry and stays NaN.
    Fid      = str2double(T.FIELDID);
    ExpCalib = nan(height(T), 1);
    Dens     = nan(height(T), 1);
    [Tf, Loc] = ismember(Fid, F.Index);
    ExpCalib(Tf) = F.ExpectedPerCrop(Loc(Tf));
    if ismember('SpecDens', F.Properties.VariableNames)
        Dens(Tf) = F.SpecDens(Loc(Tf));
    end

    % --- one row per observed field
    Known = Tf & isfinite(Fid);
    if any(Known)
        [Uf, ~, Iu] = unique(Fid(Known));
        Nf    = numel(Uf);
        Exp   = accumarray(Iu, ExpCalib(Known), [Nf 1], @(v) v(1));
        Dns   = accumarray(Iu, Dens(Known),     [Nf 1], @(v) v(1));
        Ncrop = accumarray(Iu, 1,               [Nf 1]);
        Nred  = accumarray(Iu, double(IsReduced(Known)), [Nf 1]);
        Nvis  = zeros(Nf, 1);
        VisKey = strcat(T.Camera(Known), '|', T.Visit(Known));
        for If = 1:Nf
            Nvis(If) = numel(unique(VisKey(Iu == If)));
        end
        if ismember('PT_NCALI', Vars)
            Nc  = T.PT_NCALI(Known);
            Med = accumarray(Iu, Nc, [Nf 1], @(v) median(v(isfinite(v))));
        else
            Med = nan(Nf, 1);
        end
        Fields = table(Uf, Exp, Dns, Ncrop, Nvis, Nred, Med, 'VariableNames', ...
                       {'FIELDID','Expected','SpecDens','Ncrop','Nvisit','NReduced','MedNCALI'});
        Fields = sortrows(Fields, 'Expected', 'ascend');
    end
end

% ----------------------------------------------------------------------
function Lines = problemLines(Summary, Info, Args)
    % Two or three sentences for the head of the report: what is wrong with
    % this night, stating explicitly when nothing is.
    Lines = {};

    % --- 1. failure classes. Everything in the census is a fault: a thin
    % calibrator sky is not one, and is reported as context below.
    A = Summary.Anomalies;
    if isempty(A)
        Lines{end+1} = sprintf(['No failure class fired: all %d crops passed every ' ...
            'configured check (blank LIMMAG, PT_ZP or AST_ARMS, sparse crops, ' ...
            'weak or underdetermined photometric calibration).'], Info.Ncrop);
    else
        [~, Ord] = sort([A.N], 'descend');
        A    = A(Ord);
        Ntop = min(3, numel(A));
        Txt  = cell(Ntop, 1);
        for Ia = 1:Ntop
            Txt{Ia} = sprintf('%s on %d crops (%.2f%%)', A(Ia).Key, A(Ia).N, 100*A(Ia).Frac);
        end
        Lines{end+1} = sprintf('%d failure class(es) fired, worst first: %s.', ...
                               numel(A), strjoin(Txt, '; '));
    end

    % --- 2. coverage and calibration mode
    Parts = {};
    if isfinite(Info.NcropExpect) && Info.NcropExpect > Info.Ncrop
        Miss = Info.NcropExpect - Info.Ncrop;
        Parts{end+1} = sprintf('%d of %d expected coadd crops are missing (%.1f%%)', ...
                               Miss, Info.NcropExpect, 100*Miss/Info.NcropExpect);
    else
        Parts{end+1} = 'all expected coadd crops are present';
    end
    if isfield(Summary, 'HasMode') && Summary.HasMode
        NCal = Summary.NRegular + Summary.NReduced;
        if Summary.NReduced > 0
            Why = '';
            if isfield(Summary, 'NReducedStarved') && ~isempty(Summary.Fields)
                Why = sprintf([' - %d of them under a thin calibrator sky (by design), ' ...
                    '%d on fields that should have had calibrators (anomalous), %d on fields ' ...
                    'of unknown expectation'], Summary.NReducedStarved, ...
                    Summary.NReducedRich, Summary.NReducedUnknown);
            end
            Parts{end+1} = sprintf('%d crops (%.1f%% of the calibrated ones) fell back to the reduced mode%s', ...
                Summary.NReduced, 100*Summary.NReduced/max(1,NCal), Why);
        else
            Parts{end+1} = 'every calibrated crop got the full transmission fit';
        end
        if Summary.NNoMode > 0
            Parts{end+1} = sprintf('%d crops are not photometrically calibrated', ...
                                   Summary.NNoMode);
        end
    end
    if ~isempty(Summary.Fields)
        Poor = Summary.Fields(Summary.Fields.Expected < Args.MinExpectedCalib, :);
        if isempty(Poor)
            Parts{end+1} = sprintf('every field observed is expected to hold at least %g calibrators per crop', ...
                                   Args.MinExpectedCalib);
        else
            Parts{end+1} = sprintf(['%d of %d observed fields lie under a thin calibrator sky ' ...
                '(%d crops, calibrated Norm-only by design; poorest FIELDID %g at %.3g expected)'], ...
                height(Poor), height(Summary.Fields), sum(Poor.Ncrop), ...
                Poor.FIELDID(1), Poor.Expected(1));
        end
    end
    Lines{end+1} = sprintf('Coverage and calibration: %s.', strjoin(Parts, ', '));

    % --- 3. cameras and the worst crops
    Parts = {};
    if any(Summary.CameraOutlier(:))
        Rows = find(any(Summary.CameraOutlier, 2));
        Ntop = min(3, numel(Rows));
        Txt  = cell(Ntop, 1);
        for Ir = 1:Ntop
            Ic      = Rows(Ir);
            Txt{Ir} = sprintf('%s (%s)', Summary.Camera(Ic), ...
                       strjoin(Summary.CameraKeys(Summary.CameraOutlier(Ic,:)), ', '));
        end
        Parts{end+1} = sprintf('%d camera(s) more than %g robust sigma off the night: %s', ...
                               numel(Rows), Args.CameraOutlierZ, strjoin(Txt, '; '));
    else
        Parts{end+1} = 'no camera deviates from the night median';
    end
    W = Summary.Worst;
    if ~isempty(W.Rows) && isfinite(W.Cut)
        U = unitFor(W.Key);
        if ~isempty(U)
            U = [' ' U];
        end
        Extreme = W.Rows.(W.Key)(1);
        Parts{end+1} = sprintf('worst %s %.3g%s (%s)', W.Key, Extreme, U, W.Rows.Camera(1));
        if ~isempty(W.ByCrop) && W.ByCrop.Ntail(1) > 0
            Parts{end+1} = sprintf('the %s tail peaks on CROPID %g', ...
                                   W.Key, W.ByCrop.CROPID(1));
        end
    end
    Lines{end+1} = sprintf('Cameras and worst crops: %s.', strjoin(Parts, '; '));
end

% ----------------------------------------------------------------------
function S = htmlEscape(S)
    % Make a plain string safe to drop into the document.
    S = char(S);
    S = strrep(S, '&', '&amp;');
    S = strrep(S, '<', '&lt;');
    S = strrep(S, '>', '&gt;');
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
                  '.probs{border-left:5px solid #b00;background:#fdf3f3;padding:10px 14px;' ...
                  'margin:14px 0;font-size:14px}.probs.ok{border-left-color:#2a8a55;' ...
                  'background:#f3faf5}.probs p{margin:5px 0}.probs b{font-size:14px}' ...
                  '</style></head><body>\n']);
    fprintf(Fid, '<h1>%s &mdash; night of %s</h1>\n', Title, Info.Night);
    fprintf(Fid, ['<p class="note">The date is the evening the night began; ' ...
                  'visits after midnight belong to the following day.</p>\n']);
    fprintf(Fid, '<p class="note">Generated %s from %s</p>\n', ...
            datestr(now, 'yyyy-mm-dd HH:MM'), Info.BasePath); %#ok<DATST,TNOW1>

    % --- problem summary: the first thing to read, green when there is nothing
    % to report. The reduced-mode crops are named here but do not make a night
    % faulty: Norm-only is the designed answer to a spectrum-poor field.
    if isfield(Summary, 'Problems')
        Cls = '';
        Hdr = 'Problems';
        if isfield(Summary, 'Clean') && Summary.Clean
            Cls = ' ok';
            Hdr = 'No problems found';
        end
        fprintf(Fid, '<div class="probs%s">\n', Cls);
        fprintf(Fid, '<p><b>%s</b></p>\n', Hdr);
        for Ip = 1:numel(Summary.Problems)
            fprintf(Fid, '<p>%s</p>\n', Summary.Problems{Ip});
        end
        fprintf(Fid, '</div>\n');
    end

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
                tableLabel(S.Key), S.Median, S.P05, S.P95, S.N, Cls, 100*S.NaNFrac);
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
                      'every PT_P_F* is 0. Whether a given one was unavoidable is answered ' ...
                      'by its field, in the two sections below.</p>\n'], Summary.NReduced, ...
                100*Summary.NReduced/max(1, Summary.NRegular + Summary.NReduced));
        fprintf(Fid, '<table><tr><th>Metric</th><th>Median</th><th>5&ndash;95%%</th>');
        fprintf(Fid, '<th>N crops</th><th>blank</th></tr>\n');
        for Is = 1:numel(Summary.StatsReduced)
            S = Summary.StatsReduced(Is);
            fprintf(Fid, '<tr><td>%s</td><td>%.4g</td><td>%.4g &ndash; %.4g</td><td>%d</td><td>%.1f%%</td></tr>\n', ...
                    tableLabel(S.Key), S.Median, S.P05, S.P95, S.N, 100*S.NaNFrac);
        end
        fprintf(Fid, '</table>\n');
    end

    % --- fields short of calibrators by their own sky
    if isfield(Summary, 'Fields') && ~isempty(Summary.Fields)
        Poor = Summary.Fields(Summary.Fields.Expected < Args.MinExpectedCalib, :);
        fprintf(Fid, '<h2>Calibrator supply of the fields observed</h2>\n');
        if isempty(Poor)
            fprintf(Fid, ['<p>All %d field(s) observed sit under a sky expected to hold at ' ...
                          'least %g spectral calibrators per crop.</p>\n'], ...
                    height(Summary.Fields), Args.MinExpectedCalib);
        else
            fprintf(Fid, ['<p class="note"><b>Context, not a finding.</b> %d field(s) of the ' ...
                          '%d observed sit where Gaia DR3 published fewer than %g sampled XP ' ...
                          'spectra per crop (expected counts from last_field_calibrators.csv, ' ...
                          'built by pipeline.last.quality.specDensityMap). Nothing is wrong ' ...
                          'with them and nothing was lost: their crops are calibrated, by the ' ...
                          'Norm-only fit, which gives a valid zero point that is flat across ' ...
                          'the crop instead of a fitted field term. The night behaved as it ' ...
                          'should; the table is here so the larger PT_ARMS of those crops is ' ...
                          'read as the sky they were pointed at. <b>Reduced</b> says how many ' ...
                          'of each field''s crops took that route, and <b>median PT_NCALI</b> ' ...
                          'what the fit really found - the expected count is an ' ...
                          'order-of-magnitude figure (the 0.35 survival fraction behind it was ' ...
                          'measured between 0.12 and 0.57), so treat a factor of two as ' ...
                          'agreement.</p>\n'], height(Poor), height(Summary.Fields), ...
                    Args.MinExpectedCalib);
            fprintf(Fid, ['<table><tr><th>FIELDID</th><th>expected per crop</th>' ...
                          '<th>GAIADR3spec [deg^-2]</th><th>visits</th><th>crops</th>' ...
                          '<th>reduced</th><th>median PT_NCALI</th></tr>\n']);
            Nsh = min(12, height(Poor));
            for Ir = 1:Nsh
                fprintf(Fid, ['<tr><td>%g</td><td>%.3g</td><td>%.4g</td>' ...
                              '<td>%d</td><td>%d</td><td>%d</td><td>%.4g</td></tr>\n'], ...
                        Poor.FIELDID(Ir), Poor.Expected(Ir), Poor.SpecDens(Ir), ...
                        Poor.Nvisit(Ir), Poor.Ncrop(Ir), Poor.NReduced(Ir), Poor.MedNCALI(Ir));
            end
            fprintf(Fid, '</table>\n');
            if height(Poor) > Nsh
                fprintf(Fid, '<p class="note">%d further fields omitted.</p>\n', ...
                        height(Poor) - Nsh);
            end
        end
    end

    % --- the real anomaly: reduced mode on a field that has calibrators
    if isfield(Summary, 'Fields') && ~isempty(Summary.Fields)
        Rich = Summary.Fields(Summary.Fields.Expected >= Args.MinExpectedCalib & ...
                              Summary.Fields.NReduced > 0, :);
        Rich = sortrows(Rich, 'NReduced', 'descend');
        fprintf(Fid, '<h2>Reduced mode without a reason</h2>\n');
        if isempty(Rich)
            fprintf(Fid, ['<p>None: every reduced-mode crop of this night was on a field ' ...
                          'whose sky holds fewer than %g calibrators per crop anyway.</p>\n'], ...
                    Args.MinExpectedCalib);
        else
            fprintf(Fid, ['<p class="note">%d crops on %d field(s) fell back to the Norm-only ' ...
                          'fit although their field is expected to hold at least %g calibrators ' ...
                          'per crop. Here the calibrators were lost by the night rather than ' ...
                          'missing from the sky - cloud, extinction, defocus, a masked region - ' ...
                          'so these are the crops worth chasing. Compare <b>median PT_NCALI</b> ' ...
                          'with <b>expected</b>: a large gap is the measure of what was lost.</p>\n'], ...
                    sum(Rich.NReduced), height(Rich), Args.MinExpectedCalib);
            fprintf(Fid, ['<table><tr><th>FIELDID</th><th>expected per crop</th>' ...
                          '<th>visits</th><th>crops</th><th>reduced</th>' ...
                          '<th>median PT_NCALI</th></tr>\n']);
            Nsh = min(12, height(Rich));
            for Ir = 1:Nsh
                fprintf(Fid, ['<tr><td>%g</td><td>%.3g</td><td>%d</td><td>%d</td>' ...
                              '<td class="bad">%d</td><td>%.4g</td></tr>\n'], ...
                        Rich.FIELDID(Ir), Rich.Expected(Ir), Rich.Nvisit(Ir), ...
                        Rich.Ncrop(Ir), Rich.NReduced(Ir), Rich.MedNCALI(Ir));
            end
            fprintf(Fid, '</table>\n');
            if height(Rich) > Nsh
                fprintf(Fid, '<p class="note">%d further fields omitted.</p>\n', ...
                        height(Rich) - Nsh);
            end
        end
        if isfield(Summary, 'NReducedUnknown') && Summary.NReducedUnknown > 0
            fprintf(Fid, ['<p class="note">%d further reduced-mode crops are on fields with no ' ...
                          'entry in the expectation table (a named or non-survey pointing), so ' ...
                          'nothing is claimed about them either way.</p>\n'], ...
                    Summary.NReducedUnknown);
        end
    end

    % --- anomalies
    fprintf(Fid, '<h2>Anomalies</h2>\n');
    if isempty(Summary.Anomalies)
        fprintf(Fid, '<p>None of the configured failure classes fired.</p>\n');
    else
        ShowStarved = isfield(Summary, 'Fields') && ~isempty(Summary.Fields);
        fprintf(Fid, '<table><tr><th>Class</th><th>Keyword</th><th>N crops</th>');
        fprintf(Fid, '<th>of total</th>');
        if ShowStarved
            fprintf(Fid, '<th>on thin-sky fields</th>');
        end
        fprintf(Fid, '<th>examples</th></tr>\n');
        for Ia = 1:numel(Summary.Anomalies)
            A = Summary.Anomalies(Ia);
            % The class texts carry bare '<=' and '<', which a browser would
            % swallow as the start of a tag.
            fprintf(Fid, '<tr><td>%s</td><td>%s</td><td class="bad">%d</td><td>%.2f%%</td>', ...
                    htmlEscape(A.Text), htmlEscape(A.Key), A.N, 100*A.Frac);
            if ShowStarved
                fprintf(Fid, '<td>%d</td>', A.NStarved);
            end
            fprintf(Fid, '<td>%s</td></tr>\n', htmlEscape(A.Examples));
        end
        fprintf(Fid, '</table>\n');
        if ShowStarved
            fprintf(Fid, ['<p class="note">"On thin-sky fields" counts the crops of each class ' ...
                          'whose field is expected to hold fewer than %g calibrators per crop. ' ...
                          'A class that is mostly those is the sky being measured correctly, ' ...
                          'not a fault of the night.</p>\n'], Args.MinExpectedCalib);
        end
    end

    % --- per-camera
    ShowRed = isfield(Summary, 'CameraFracReduced') && ...
              isfield(Summary, 'NReduced') && Summary.NReduced > 0;
    fprintf(Fid, '<h2>Per camera</h2>\n<table><tr><th>Camera</th><th>crops</th>');
    if ShowRed
        fprintf(Fid, '<th>reduced</th>');
    end
    for Ik = 1:numel(Summary.CameraKeys)
        fprintf(Fid, '<th>%s</th>', tableLabel(Summary.CameraKeys{Ik}));
    end
    fprintf(Fid, '</tr>\n');
    % Outlier test per metric: robust z against the across-camera spread, so
    % each metric is judged on its own natural scatter rather than a fixed
    % percentage (AST_ARMS and N_STARS vary far more between cameras than
    % LIMMAG). Decided once in the main function, and reused by the problem
    % summary at the top of the page.
    Outlier = Summary.CameraOutlier;
    for Ic = 1:numel(Summary.Camera)
        fprintf(Fid, '<tr><td>%s</td><td>%d</td>', Summary.Camera(Ic), Summary.CameraNcrop(Ic));
        if ShowRed
            fprintf(Fid, '<td>%.1f%%</td>', 100*Summary.CameraFracReduced(Ic));
        end
        for Ik = 1:numel(Summary.CameraKeys)
            V   = Summary.CameraStat(Ic, Ik);
            Cls = '';
            if Outlier(Ic, Ik)
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

    % --- worst crops by one metric (FWHM by default)
    if isfield(Summary, 'Worst') && ~isempty(Summary.Worst.Rows)
        W   = Summary.Worst;
        U   = unitFor(W.Key);
        if ~isempty(U)
            U = [' ' U];
        end
        Dir = 'largest';
        if ~W.High
            Dir = 'smallest';
        end
        fprintf(Fid, '<h2>Worst crops by %s</h2>\n', W.Key);
        fprintf(Fid, ['<p class="note">The %d crops with the %s %s of the night, worst ' ...
                      'first; the same cut is marked in the %s distribution below. The ' ...
                      'tail is the %.3g%s level (95th percentile), reached by %d crops ' ...
                      '(%.2f%% of the night). Where that tail sits tells the two causes ' ...
                      'apart: spread over the crops of one camera it is that mount or that ' ...
                      'visit, concentrated on one CROPID it is a focal-plane position that ' ...
                      'is soft all night.</p>\n'], ...
                height(W.Rows), Dir, W.Key, W.Key, W.Tail, U, W.Ntail, ...
                100*W.Ntail/max(1, Info.Ncrop));
        VarsW = W.Rows.Properties.VariableNames;
        fprintf(Fid, '<table><tr>');
        for Iv = 1:numel(VarsW)
            fprintf(Fid, '<th>%s</th>', tableLabel(VarsW{Iv}));
        end
        fprintf(Fid, '</tr>\n');
        for Ir = 1:height(W.Rows)
            fprintf(Fid, '<tr>');
            for Iv = 1:numel(VarsW)
                Val = W.Rows.(VarsW{Iv})(Ir);
                if isnumeric(Val)
                    fprintf(Fid, '<td>%.4g</td>', Val);
                else
                    fprintf(Fid, '<td>%s</td>', Val);
                end
            end
            fprintf(Fid, '</tr>\n');
        end
        fprintf(Fid, '</table>\n');

        % where the tail sits: focal-plane position, then camera
        if ~isempty(W.ByCrop)
            Nsh = min(8, height(W.ByCrop));
            fprintf(Fid, ['<p class="note">Tail by focal-plane position, worst first ' ...
                          '(%d of %d CROPIDs shown; night median %s = %.3g%s).</p>\n'], ...
                    Nsh, height(W.ByCrop), W.Key, median(W.ByCrop.Median, 'omitnan'), U);
            fprintf(Fid, '<table><tr><th>CROPID</th><th>median %s</th><th>crops in tail</th></tr>\n', ...
                    tableLabel(W.Key));
            for Ir = 1:Nsh
                fprintf(Fid, '<tr><td>%g</td><td>%.4g</td><td>%d</td></tr>\n', ...
                        W.ByCrop.CROPID(Ir), W.ByCrop.Median(Ir), W.ByCrop.Ntail(Ir));
            end
            fprintf(Fid, '</table>\n');
        end
        if ~isempty(W.ByCamera) && W.ByCamera.Ntail(1) > 0
            Nsh = min(5, height(W.ByCamera));
            fprintf(Fid, '<p class="note">Tail by camera, worst first (%d of %d shown).</p>\n', ...
                    Nsh, height(W.ByCamera));
            fprintf(Fid, '<table><tr><th>Camera</th><th>crops</th><th>in tail</th><th>of its own</th></tr>\n');
            for Ir = 1:Nsh
                fprintf(Fid, '<tr><td>%s</td><td>%d</td><td>%d</td><td>%.1f%%</td></tr>\n', ...
                        W.ByCamera.Camera(Ir), W.ByCamera.Ncrop(Ir), ...
                        W.ByCamera.Ntail(Ir), W.ByCamera.PctTail(Ir));
            end
            fprintf(Fid, '</table>\n');
        end
    end

    % --- figures
    if ~isempty(ImgHist)
        fprintf(Fid, '<h2>Distributions</h2>\n');
        fprintf(Fid, ['<p class="note">Blue: regular-mode and uncalibrated crops. ' ...
                      'Orange: reduced-mode crops, overlaid on every metric, not only ' ...
                      'the photometric ones. Red: median with the 5&ndash;95%% range. ' ...
                      'Counts are logarithmic; the RMS metrics also use a logarithmic ' ...
                      'value axis. Black: the worst-crop cut of the table above.</p>\n']);
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
                  '(1 = regular, 0 = reduced, blank = not calibrated); ']);
    fprintf(Fid, 'worst crops listed by %s (%d of them); ', Args.WorstKey, Args.NWorst);
    fprintf(Fid, 'logarithmic value axis on %s; ', strjoin(Args.LogXKeys, ', '));
    fprintf(Fid, 'starved-field threshold %g expected calibrators per crop.</p>\n', ...
            Args.MinExpectedCalib);
    fprintf(Fid, '</body></html>\n');
end
