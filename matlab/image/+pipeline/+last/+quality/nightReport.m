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
    %            'TransPar' - Add the section on the fitted transmission-model
    %                   parameters: PT_FNORM (throughput), PT_1_V1 (Norm),
    %                   PT_3_V2 (TauAod500), PT_5_V2 (PWV_cm) and PT_8_V2 (QE
    %                   Center_Ang), as histograms and a median/range table.
    %                   Needs a table harvested with those keywords (the
    %                   harvestNight default since Oct 2026); without them the
    %                   section is skipped. Default is true.
    %            'TransParOnly' - Write ONLY that section (plus a one-line
    %                   session summary), for a table that holds just the
    %                   calibration keywords - e.g. a re-calibration of a night
    %                   with changed fit settings. Default is false.
    %            'Title' - Report title. Default is '' -> 'LAST night report'.
    %            'Verbose' - Print progress. Default is true.
    %   Fields are checked against the expected calibrator count of their
    %   pointing, which splits the reduced-mode crops in two. Under a thin
    %   calibrator sky the fallback is the designed behaviour and nothing is
    %   wrong: those crops are calibrated, with a flat zero point, and the table
    %   is reported as context for their larger PT_ARMS. On a field expected to
    %   hold calibrators the same fallback is unexplained and enters the
    %   anomaly census, with the observing conditions of those crops printed
    %   beside the night's own: worse seeing or depth means the night took the
    %   calibrators, while conditions as good as the night's mean the per-FIELD
    %   expectation is too coarse for the crop, Gaia XP coverage being patchy
    %   well inside one 11.3 deg^2 field.
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
        Args.CameraKeys cell      = {'LIMMAG','FWHM','AST_ARMS','PT_ARMS','N_STARS'}
        Args.CameraOutlierZ (1,1) double = 3
        Args.LogXKeys cell        = {'AST_ARMS','PT_ARMS','PT_RMS','PH_RMS','RP_MRMS'}
        Args.LogY logical         = true
        Args.WorstKey char        = 'FWHM'
        Args.WorstHigh logical    = true
        Args.NWorst (1,1) double  = 10
        Args.FieldCalibFile char  = ''
        Args.MinExpectedCalib (1,1) double = 16
        Args.TransPar logical     = true
        Args.TransParOnly logical = false
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

    if Args.TransParOnly
        [OutFile, Summary] = transParOnlyReport(T, Info, IsReduced, IsRegular, HasMode, Args);
        return
    end

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
        % What the sky looked like when those crops lost their calibrators. If
        % the seeing and depth of the rich-field fallbacks are no worse than the
        % night's own, the calibrators were not taken by cloud or extinction,
        % and the per-FIELD expectation is simply too coarse for the crop -
        % GAIADR3spec is patchy on scales well inside one 11.3 deg^2 field.
        Summary.RichReduced = compareConditions(T, ReducedRich, IsRegular, ExpCalib);
        % How many calibrators those crops actually had, per crop and per
        % focal-plane position, and what the OTHER crops of the same exposures
        % got. Same visit means same sky and same transparency, so a gap there
        % is positional - the focal plane or the pipeline - and neither the
        % night nor the field can explain it.
        Summary.RichReduced.Shortfall = shortfallByCrop(T, ReducedRich, IsRegular, ExpCalib);
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
        ['Zero point from a single calibrator (PT_DOF <= 0; in the reduced mode ' ...
         'PT_DOF is PT_NCALI - 1): nothing constrains it and its residual is machine noise']);
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_ARMS', @(v) v < 1e-6, ...
        'PT_ARMS at or below 1e-6 (reported as a perfect fit)');
    % Same for PT_RMS: with one or two calibrators the median-of-N zero point
    % reproduces them exactly and the scatter comes out at machine epsilon
    % (~1e-14), which is then published as the crop's photometric quality.
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'PT_RMS', @(v) v < 1e-6, ...
        'PT_RMS at or below 1e-6: a zero point that reproduces its calibrators exactly, not a measurement');
    % A low PT_NCALI is deliberately NOT a class of its own. Since the reduced
    % mode exists (issue #1381), few calibrators is not a failure but a fact
    % with two possible causes, and both are reported as what they are: the sky
    % was thin (context, under the calibrator supply of the fields) or the night
    % took the calibrators away (REDUCED_RICH, a fault). A separate '< N
    % calibrators' class only renamed the same crops as something broken, and
    % PT_NCALI remains in the metric table and in both field tables.
    Anom = addAnomaly(Anom, T, Vars, IsStarved, 'REDUCED_RICH', @(v) v == 1, ...
        sprintf(['Reduced-mode calibration although the field average promises at least %g ' ...
                 'calibrators per crop: a shortfall the sky does not account for, to be read ' ...
                 'with the conditions of those crops'], Args.MinExpectedCalib));
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
    ImgHist  = figureHist(T, HistKeys, IsReduced, ~HasMode, Args, Summary);
    ImgTime  = figureTime(T, TimeKeys);
    ImgCam   = figureCamera(Cam, CamStat, CamKeys);
    ImgTrans = '';
    Summary.TransPar = [];
    if Args.TransPar
        [Summary.TransPar, ImgTrans] = transParams(T, IsReduced, Args);
    end

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
    writeHtml(OutFile, Title, Info, Summary, ImgHist, ImgTime, ImgCam, ImgTrans, Args);
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
function B64 = figureHist(T, Keys, IsReduced, IsUncal, Args, Summary)
    % Distribution panel, one histogram per metric.
    %   Every panel is split by calibration mode: the reduced-mode crops
    %   (PT_P_F1 = 0) are drawn in orange over the others. For the PT_* metrics
    %   the two populations have genuinely different widths, and for the rest
    %   the overlay answers a different question - what the fields that starved
    %   the calibration look like in seeing, depth and source count.
    %
    %   The blue population is every crop that is not reduced-mode, but what
    %   that holds depends on the metric, so each panel counts its own and says
    %   so in the legend. A crop that was never photometrically calibrated has
    %   no PT_ZP, PT_ARMS, PT_RMS or LIMMAG to contribute - the value is blank -
    %   so those panels are regular-mode crops alone. FWHM and the astrometric
    %   residuals are measured before the photometric fit, so there the
    %   uncalibrated crops are present and counted.
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
            IsLog  = ismember(Key, Args.LogXKeys);
            Usable = isfinite(Vall);
            Nbelow = 0;
            if IsLog
                % A log axis cannot carry zero or negative values, and PT_ARMS
                % does reach ~1e-16 on an underdetermined fit. Count what drops
                % out instead of letting it disappear.
                Nbelow = sum(Usable & Vall <= 0);
                Usable = Usable & Vall > 0;
            end
            SelBlue = Usable & ~IsReduced;
            V       = Vall(SelBlue);
            Vred    = Vall(Usable & IsReduced);
            % How many crops in blue were never photometrically calibrated: for
            % a PT_* or LIMMAG panel this is 0, because such a crop has nothing
            % to plot there.
            Nuncal  = sum(SelBlue & IsUncal);
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
                LabBlue = 'regular mode';
                if Nuncal > 0
                    LabBlue = sprintf('regular mode + %d uncalibrated', Nuncal);
                end
                if ~isempty(Vred)
                    legend({LabBlue, 'reduced mode'}, ...
                           'FontSize',7, 'Location','best', 'Box','off');
                elseif Nuncal > 0
                    legend({LabBlue}, 'FontSize',7, 'Location','best', 'Box','off');
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
            % A keyword that is blank in every header leaves Ok all false, and
            % min/max of nothing is empty rather than NaN - which an && test
            % cannot take - so the panel is left bare instead.
            Lo = floor(min(UT(Ok)));  Hi = ceil(max(UT(Ok)));
            if ~isempty(Lo) && ~isempty(Hi) && isfinite(Lo) && isfinite(Hi) && Hi > Lo
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
function S = shortfallByCrop(T, Sel, Ref, ExpCalib)
    % Calibrator statistics of a selection of crops.
    %   .Quant    - percentiles of PT_NCALI over the selection.
    %   .NFew     - how many had 2 calibrators or fewer, where the Norm-only
    %               zero point reproduces them and PT_RMS stops meaning
    %               anything.
    %   .Ratio    - median PT_NCALI / expected.
    %   .ByCrop   - per CROPID: how many, what share of that position's crops,
    %               median PT_NCALI, median expected, ratio.
    %   .SibNCALI - median PT_NCALI of the REGULAR crops of the same visits:
    %               the control that holds sky, night and field fixed.
    S = struct('N', sum(Sel), 'Quant', [], 'NFew', 0, 'Ratio', NaN, ...
               'ByCrop', table(), 'SibNCALI', NaN, 'SibN', 0, ...
               'MedPerVisit', NaN, 'NVisit', 0);
    Vars = T.Properties.VariableNames;
    if ~any(Sel) || ~ismember('PT_NCALI', Vars)
        return
    end
    N       = T.PT_NCALI;
    S.Quant = prctile(N(Sel & isfinite(N)), [0 5 25 50 75 95 100]);
    S.NFew  = sum(Sel & isfinite(N) & N <= 2);
    S.Ratio = median(N(Sel) ./ ExpCalib(Sel), 'omitnan');

    if ismember('CROPID', Vars)
        Cid  = T.CROPID;
        Ok   = isfinite(Cid);
        Uc   = unique(Cid(Ok & Sel));
        Rows = cell(numel(Uc), 1);
        for Ic = 1:numel(Uc)
            Here  = Ok & Cid == Uc(Ic);
            Mine  = Here & Sel;
            Rows{Ic} = [Uc(Ic), sum(Mine), 100*sum(Mine)/max(1, sum(Here)), ...
                        median(N(Mine), 'omitnan'), median(ExpCalib(Mine), 'omitnan'), ...
                        median(N(Mine) ./ ExpCalib(Mine), 'omitnan')];
        end
        M = cell2mat(Rows);
        S.ByCrop = array2table(M, 'VariableNames', ...
                   {'CROPID','N','PctOfPosition','MedNCALI','MedExpected','Ratio'});
        S.ByCrop = sortrows(S.ByCrop, 'PctOfPosition', 'descend');
    end

    % the sibling control: regular crops of the very same visits
    [~, ~, Iv] = unique(strcat(T.Camera, '|', T.Visit));
    Hit        = accumarray(Iv, double(Sel)) > 0;
    InHit      = Hit(Iv);
    S.NVisit   = sum(Hit);
    S.SibN     = sum(InHit & Ref);
    S.SibNCALI = median(N(InHit & Ref), 'omitnan');
    Per        = accumarray(Iv, double(Sel));
    S.MedPerVisit = median(Per(Hit));
end

% ----------------------------------------------------------------------
function S = compareConditions(T, Sel, Ref, ExpCalib)
    % Median of the observing conditions for a selection and for a reference
    % set, so the two can be read side by side. Keys that the night does not
    % carry come back NaN.
    S = struct('N', sum(Sel), 'Keys', {{}}, 'Sel', [], 'Ref', [], ...
               'Expected', NaN, 'Measured', NaN);
    if ~any(Sel)
        return
    end
    Keys = {'FWHM','LIMMAG','AIRMASS','N_STARS','BACKMAG'};
    Keys = Keys(ismember(Keys, T.Properties.VariableNames));
    S.Keys = Keys;
    S.Sel  = nan(1, numel(Keys));
    S.Ref  = nan(1, numel(Keys));
    for Ik = 1:numel(Keys)
        V = T.(Keys{Ik});
        S.Sel(Ik) = median(V(Sel & isfinite(V)), 'omitnan');
        S.Ref(Ik) = median(V(Ref & isfinite(V)), 'omitnan');
    end
    S.Expected = median(ExpCalib(Sel), 'omitnan');
    if ismember('PT_NCALI', T.Properties.VariableNames)
        S.Measured = median(T.PT_NCALI(Sel), 'omitnan');
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
                    '%d unexplained by their field average, %d on fields of unknown ' ...
                    'expectation'], Summary.NReducedStarved, Summary.NReducedRich, ...
                    Summary.NReducedUnknown);
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
function writeHtml(OutFile, Title, Info, Summary, ImgHist, ImgTime, ImgCam, ImgTrans, Args)
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
        % Where they came from: the whole point of keeping the field table.
        if isfield(Summary, 'NReducedStarved') && ~isempty(Summary.Fields)
            NRd = max(1, Summary.NReduced);
            fprintf(Fid, '<table><tr><th>Reduced-mode crops by the sky they were pointed at</th>');
            fprintf(Fid, '<th>crops</th><th>of the reduced</th></tr>\n');
            fprintf(Fid, ['<tr><td>Field expected to hold fewer than %g calibrators per crop ' ...
                          '- the fallback is by design</td><td>%d</td><td>%.1f%%</td></tr>\n'], ...
                    Args.MinExpectedCalib, Summary.NReducedStarved, ...
                    100*Summary.NReducedStarved/NRd);
            fprintf(Fid, ['<tr><td>Field expected to hold them - the shortfall is not explained ' ...
                          'by the field average</td><td class="bad">%d</td><td>%.1f%%</td></tr>\n'], ...
                    Summary.NReducedRich, 100*Summary.NReducedRich/NRd);
            fprintf(Fid, ['<tr><td>Field not in the expectation table - no claim either way</td>' ...
                          '<td>%d</td><td>%.1f%%</td></tr>\n'], ...
                    Summary.NReducedUnknown, 100*Summary.NReducedUnknown/NRd);
            fprintf(Fid, '</table>\n');
        end
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
                          'what the fit really found - the expected count is good to a ' ...
                          'factor of about 1.5 (the 0.65 survival fraction behind it runs ' ...
                          'from 0.55 at the sharpest seeing to 0.69 at the softest), so ' ...
                          'treat that as agreement.</p>\n'], height(Poor), height(Summary.Fields), ...
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
                          'per crop. Compare <b>median PT_NCALI</b> with <b>expected</b>: the gap ' ...
                          'is what was not there. Two things produce it and the conditions below ' ...
                          'tell them apart - the night took the calibrators away (cloud, ' ...
                          'extinction, defocus, a masked region), or the crop''s own patch of sky ' ...
                          'is poorer than its field average, since the expectation is a mean over ' ...
                          '11.3 deg<sup>2</sup> and Gaia XP coverage is patchy well inside ' ...
                          'that.</p>\n'], sum(Rich.NReduced), height(Rich), Args.MinExpectedCalib);
            % the conditions comparison, which is what separates the two readings
            if isfield(Summary, 'RichReduced') && ~isempty(Summary.RichReduced.Keys)
                R = Summary.RichReduced;
                fprintf(Fid, '<table><tr><th>Median</th>');
                for Ik = 1:numel(R.Keys)
                    fprintf(Fid, '<th>%s</th>', tableLabel(R.Keys{Ik}));
                end
                fprintf(Fid, '<th>calibrators found / expected</th></tr>\n');
                fprintf(Fid, '<tr><td>These crops</td>');
                fprintf(Fid, '<td>%.4g</td>', R.Sel);
                fprintf(Fid, '<td>%.3g / %.3g</td></tr>\n', R.Measured, R.Expected);
                fprintf(Fid, '<tr><td>Regular-mode crops</td>');
                fprintf(Fid, '<td>%.4g</td>', R.Ref);
                fprintf(Fid, '<td>&ndash;</td></tr>\n');
                fprintf(Fid, '</table>\n');
                fprintf(Fid, ['<p class="note">Worse seeing, shallower depth or higher airmass ' ...
                              'here means the night took the calibrators. Conditions as good as ' ...
                              'the night''s own, or better, mean it did not - and then the ' ...
                              'field-average expectation is what is too coarse, not the fit that ' ...
                              'is at fault.</p>\n']);
                % the calibrator statistics of those crops
                if isfield(R, 'Shortfall') && R.Shortfall.N > 0
                    Sf = R.Shortfall;
                    fprintf(Fid, ['<p class="note"><b>Calibrators per crop.</b> PT_NCALI over ' ...
                                  'these crops: median %g, quartiles %g&ndash;%g, 5&ndash;95%% ' ...
                                  '%g&ndash;%g, range %g&ndash;%g; %d of them (%.1f%%) had two or ' ...
                                  'fewer, where the zero point reproduces its calibrators and ' ...
                                  'PT_RMS stops being a measurement. Median measured/expected ' ...
                                  '%.3g.</p>\n'], Sf.Quant(4), Sf.Quant(3), Sf.Quant(5), ...
                            Sf.Quant(2), Sf.Quant(6), Sf.Quant(1), Sf.Quant(7), Sf.NFew, ...
                            100*Sf.NFew/max(1,Sf.N), Sf.Ratio);
                    if isfinite(Sf.SibNCALI)
                        fprintf(Fid, ['<p class="note"><b>The control.</b> These crops are spread ' ...
                                      'over %d visits, a median of %g crops of the 24 in each. ' ...
                                      'The REGULAR crops of those same exposures - same sky, same ' ...
                                      'transparency, same field - found a median of %g ' ...
                                      'calibrators against these crops'' %g. A gap there is ' ...
                                      'positional, and neither the night nor the field ' ...
                                      'accounts for it.</p>\n'], Sf.NVisit, Sf.MedPerVisit, ...
                                Sf.SibNCALI, Sf.Quant(4));
                    end
                    if ~isempty(Sf.ByCrop)
                        fprintf(Fid, ['<table><tr><th>CROPID</th><th>crops</th>' ...
                                      '<th>of that position''s crops</th><th>median PT_NCALI</th>' ...
                                      '<th>median expected</th><th>found / expected</th></tr>\n']);
                        for Ir = 1:height(Sf.ByCrop)
                            fprintf(Fid, ['<tr><td>%g</td><td>%d</td><td>%.1f%%</td><td>%g</td>' ...
                                          '<td>%.3g</td><td>%.3g</td></tr>\n'], ...
                                    Sf.ByCrop.CROPID(Ir), Sf.ByCrop.N(Ir), ...
                                    Sf.ByCrop.PctOfPosition(Ir), Sf.ByCrop.MedNCALI(Ir), ...
                                    Sf.ByCrop.MedExpected(Ir), Sf.ByCrop.Ratio(Ir));
                        end
                        fprintf(Fid, '</table>\n');
                        fprintf(Fid, ['<p class="note">Sorted by how much of each focal-plane ' ...
                                      'position is affected. A flat column is the sky; a strong ' ...
                                      'gradient across CROPID is the instrument or the pipeline - ' ...
                                      'a ghost, a halo, a mask or a selection cut that bites at ' ...
                                      'particular positions.</p>\n']);
                    end
                end
            end
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
        fprintf(Fid, ['<p class="note">Blue: every crop that is not reduced-mode. What ' ...
                      'that holds depends on the metric, so each panel counts its own and ' ...
                      'says so in its legend: a crop that was never photometrically ' ...
                      'calibrated has no PT_ZP, PT_ARMS, PT_RMS or LIMMAG to contribute, so ' ...
                      'those panels are regular-mode crops alone, while FWHM and the ' ...
                      'astrometric residuals are measured before the photometric fit and do ' ...
                      'carry them. Orange: reduced-mode crops, overlaid on every metric, not ' ...
                      'only the photometric ones. Red: median with the 5&ndash;95%% range. ' ...
                      'Counts are logarithmic; the RMS metrics also use a logarithmic value ' ...
                      'axis. Black: the worst-crop cut of the table above.</p>\n']);
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', ImgHist);
    end
    writeTransPar(Fid, Summary.TransPar, ImgTrans);
    if ~isempty(ImgTime)
        fprintf(Fid, '<h2>Across the night</h2>\n');
        fprintf(Fid, '<p class="note">Grey: one point per crop. Red: 10-minute median.</p>\n');
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', ImgTime);
    end

    fprintf(Fid, '<h2>Settings</h2>\n<p class="note">Sparse-crop threshold N_STARS &lt; %g; ', Args.MinStars);
    fprintf(Fid, ['calibration mode read from the Tran2D fit flag PT_P_F1 ' ...
                  '(1 = regular, 0 = reduced, blank = not calibrated); ']);
    fprintf(Fid, 'worst crops listed by %s (%d of them); ', Args.WorstKey, Args.NWorst);
    fprintf(Fid, 'logarithmic value axis on %s; ', strjoin(Args.LogXKeys, ', '));
    fprintf(Fid, 'starved-field threshold %g expected calibrators per crop.</p>\n', ...
            Args.MinExpectedCalib);
    fprintf(Fid, '</body></html>\n');
end

% ======================================================================
function Def = transParDefs()
    % The transmission-model parameters shown in the report.
    %   Key     - table column holding the value (TP_* are derived here).
    %   Flag    - fit flag keyword: 1 = fitted on this crop, 0 = held at its
    %             default (every parameter but Norm in the reduced mode).
    %   NameKey - PT_<i>_N column naming function <i>, checked against Fun:
    %             the index is the position in LASTFunList of
    %             imUtil.calib.predefSeqCompositeFun, and a crop calibrated
    %             with another function list must not be mixed in.
    %   Cat/Ipar - entry and parameter index in that catalogue, for bounds.
    Def = struct( ...
        'Label',   {'PT_FNORM', 'Norm (PT_1_V1)', 'TauAod500 (PT_3_V2)', 'PWV_cm (PT_5_V2)', 'Center_Ang (PT_8_V2)'}, ...
        'Key',     {'TP_FNORM', 'TP_NORM', 'PT_3_V2', 'PT_5_V2', 'PT_8_V2'}, ...
        'Flag',    {'PT_1_F1', 'PT_1_F1', 'PT_3_F2', 'PT_5_F2', 'PT_8_F2'}, ...
        'NameKey', {'PT_1_N', 'PT_1_N', 'PT_3_N', 'PT_5_N', 'PT_8_N'}, ...
        'Fun',     {'astro.transmission.normalization', 'astro.transmission.normalization', ...
                    'astro.transmission.aerosolTransmission', 'astro.transmission.waterTransmission', ...
                    'telescope.detector.qeSkewedGaussianLAST'}, ...
        'Cat',     {'', '', 'Aerosol', 'Water', 'QE_SkewedGaussian'}, ...
        'Ipar',    {0, 0, 2, 2, 2}, ...
        'Unit',    {'', 'm^2', '', 'cm', 'Angstrom'});
end

% ----------------------------------------------------------------------
function [Stats, B64] = transParams(T, IsReduced, Args)
    % Statistics and histograms of the fitted transmission-model parameters.
    %   Norm is reported in m^2 whatever the header's age: a header without
    %   PT_NORMU predates issue #1431 and its Norm is relative to LAST's area,
    %   so it is multiplied by PhotCalibTrans.LegacyNormArea. PT_FNORM is read
    %   when present and otherwise derived as Norm [m^2] / CollectingAreaLAST,
    %   which for an old header is its Norm unchanged.
    %   Only crops on which a parameter was actually fitted (its fit flag is 1)
    %   enter its histogram: in the reduced mode every parameter but Norm sits
    %   at its default, and those identical values would show up as a false
    %   spike. Norm is fitted in both modes, so its panels keep the orange
    %   reduced-mode overlay. Fits pinned at a catalogue bound are counted.
    Stats = [];
    B64   = '';
    Vars  = T.Properties.VariableNames;
    if ~ismember('PT_1_V1', Vars)
        return
    end
    Ncrop = height(T);

    % --- Norm in m^2 and the throughput
    NormRaw = T.PT_1_V1;
    IsM2    = false(Ncrop, 1);
    if ismember('PT_NORMU', Vars)
        IsM2 = strcmpi(strtrim(T.PT_NORMU), 'm2');
    end
    T.TP_NORM = NormRaw;
    T.TP_NORM(~IsM2) = NormRaw(~IsM2) * PhotCalibTrans.LegacyNormArea;
    T.TP_FNORM = T.TP_NORM / PhotCalibTrans.CollectingAreaLAST;
    if ismember('PT_FNORM', Vars)
        Has = isfinite(T.PT_FNORM);
        T.TP_FNORM(Has) = T.PT_FNORM(Has);
    end

    % --- catalogue bounds (the defaults; a run may have overridden them)
    try
        FunCat = imUtil.calib.predefSeqCompositeFun();
    catch
        FunCat = struct();
    end

    Def   = transParDefs();
    Npar  = numel(Def);
    Out   = cell(Npar, 1);
    Sel   = cell(Npar, 2);
    for Ip = 1:Npar
        D = Def(Ip);
        S = struct('Label',D.Label, 'Unit',D.Unit, 'Key',D.Key, 'Present',false, ...
                   'N',0, 'Median',NaN, 'P05',NaN, 'P95',NaN, ...
                   'NReduced',0, 'MedianReduced',NaN, 'NNotFitted',0, ...
                   'NOtherFun',0, 'Min',NaN, 'Max',NaN, 'NAtMin',0, 'NAtMax',0, ...
                   'NLegacyNorm',sum(~IsM2 & isfinite(NormRaw)));
        if ismember(D.Key, T.Properties.VariableNames)
            S.Present = true;
            V     = T.(D.Key);
            Ok    = isfinite(V);
            % A table harvested without the fit flag cannot tell a fitted
            % value from a default one: take every finite value as fitted.
            if ismember(D.Flag, Vars)
                Flag = T.(D.Flag);
            else
                Flag = double(Ok);
            end
            % function at that index is the expected one (blank name = old
            % table without the names: assume it is)
            if ismember(D.NameKey, Vars)
                Name    = strtrim(T.(D.NameKey));
                Other   = strlength(Name) > 0 & ~strcmp(Name, D.Fun);
                S.NOtherFun = sum(Ok & Other);
                Ok      = Ok & ~Other;
            end
            Fitted       = Ok & Flag == 1;
            S.NNotFitted = sum(Ok & ~Fitted);
            SelReg       = Fitted & ~IsReduced;
            SelRed       = Fitted & IsReduced;
            Vr           = V(SelReg);
            S.N          = numel(Vr);
            S.NReduced   = sum(SelRed);
            if S.N > 0
                S.Median = median(Vr);
                S.P05    = prctile(Vr, 5);
                S.P95    = prctile(Vr, 95);
            end
            if S.NReduced > 0
                S.MedianReduced = median(V(SelRed));
            end
            if ~isempty(D.Cat) && isfield(FunCat, D.Cat)
                PI    = FunCat.(D.Cat).ParamInfo(D.Ipar);
                S.Min = PI.Min;
                S.Max = PI.Max;
                Tol   = 1e-6 * max(1, abs([S.Min S.Max]));
                S.NAtMin = sum(abs(V(Fitted) - S.Min) <= Tol(1));
                S.NAtMax = sum(abs(V(Fitted) - S.Max) <= Tol(2));
            end
            Sel{Ip,1} = V(SelReg);
            Sel{Ip,2} = V(SelRed);
        end
        Out{Ip} = S;
    end
    Stats = [Out{:}];
    if ~any([Stats.Present]) || all([Stats.N] + [Stats.NReduced] == 0)
        return
    end

    % --- histograms
    Show = find([Stats.Present] & ([Stats.N] + [Stats.NReduced]) > 0);
    Nk   = numel(Show);
    Ncl  = min(3, Nk);
    Nrw  = ceil(Nk/Ncl);
    Fig  = figure('Visible','off', 'Position',[10 10 420*Ncl 300*Nrw], 'Color','w');
    for Is = 1:Nk
        Ip   = Show(Is);
        S    = Stats(Ip);
        V    = Sel{Ip,1};
        Vred = Sel{Ip,2};
        subplot(Nrw, Ncl, Is);
        Vall = [V; Vred];
        % Axis over the bulk, not the extremes: a handful of runaway fits
        % (Center_Ang out to 9600 A) would squash the core into two bins. The
        % reach is three 5-95% widths past the range, kept inside the data,
        % and widened to a bound that fits actually reached.
        P   = prctile(Vall, [5 95]);
        Wd  = max(P(2) - P(1), eps(max(abs(P))));
        XLo = max(min(Vall), P(1) - 3*Wd);
        XHi = min(max(Vall), P(2) + 3*Wd);
        if S.NAtMin > 0
            XLo = min(XLo, S.Min);
        end
        if S.NAtMax > 0
            XHi = max(XHi, S.Max);
        end
        Noff  = sum(Vall < XLo | Vall > XHi);
        Edges = linspace(XLo, XHi, 41);
        if ~(Edges(end) > Edges(1))
            Edges = Edges(1) + [-0.5 0.5] * max(abs(Edges(1))*1e-3, 1e-6);
        end
        Leg = {};
        if ~isempty(V)
            histogram(V, Edges, 'FaceColor',[.35 .45 .75], 'EdgeColor','none');
            Leg{end+1} = sprintf('regular mode (%d)', numel(V)); %#ok<AGROW>
        end
        hold on;
        if ~isempty(Vred)
            histogram(Vred, Edges, 'FaceColor',[.90 .50 .15], 'EdgeColor','none');
            Leg{end+1} = sprintf('reduced mode (%d)', numel(Vred)); %#ok<AGROW>
        end
        if ~isempty(V)
            xline(S.Median, '-', 'Color','r', 'LineWidth',1.4);
            xline(S.P05, '--', 'Color','r', 'LineWidth',1);
            xline(S.P95, '--', 'Color','r', 'LineWidth',1);
        end
        % Norm in m^2 is ~0.02: tick labels of that length collide on a
        % panel this narrow, so keep every other one.
        Xmax = max(abs(Vall));
        if Xmax > 0 && Xmax < 0.1
            Ax = gca;
            Ax.XTick = Ax.XTick(1:2:end);
        end
        % a catalogue bound is drawn only when fits actually sit on it
        if S.NAtMin > 0
            xline(S.Min, 'k:', 'LineWidth',1.2);
        end
        if S.NAtMax > 0
            xline(S.Max, 'k:', 'LineWidth',1.2);
        end
        if Args.LogY
            set(gca, 'YScale','log');
            YL = ylim;
            ylim([0.7 YL(2)*2.5]);
            set(gca, 'YTick', 10.^(0:max(0, floor(log10(YL(2))))));
        end
        Note = '';
        if S.NAtMin + S.NAtMax > 0
            Note = sprintf('%d at lower, %d at upper bound', S.NAtMin, S.NAtMax);
        end
        if Noff > 0
            Note = strtrim(sprintf('%s  %d off axis', Note, Noff));
        end
        title({sprintf('%s: %.4g (%.4g-%.4g)', strrep(S.Label,'_','\_'), S.Median, S.P05, S.P95), ...
               Note}, 'FontSize',8, 'FontWeight','normal');
        legend(Leg, 'FontSize',7, 'Location','northwest', 'Box','off');
        grid on;
        XL = strrep(S.Label, '_', '\_');
        if ~isempty(S.Unit)
            XL = [XL ' [' strrep(S.Unit, '^2', '^{2}') ']'];
        end
        xlabel(XL);  ylabel('N crops');
    end
    B64 = fig2base64(Fig);
end

% ----------------------------------------------------------------------
function writeTransPar(Fid, Stats, Img)
    % HTML section of the transmission-model parameters.
    if isempty(Stats) || ~any([Stats.Present])
        return
    end
    fprintf(Fid, '<h2>Transmission-model parameters</h2>\n');
    fprintf(Fid, ['<p class="note">The parameters the photometric fit adjusts, over the ' ...
                  'crops on which each one was actually fitted (its PT_*_F* flag is 1). ' ...
                  'In the reduced mode only Norm is fitted, so the atmosphere and QE ' ...
                  'panels hold regular-mode crops alone, and the crops that kept the ' ...
                  'default are counted under <b>not fitted</b>. Norm is the effective ' ...
                  'collecting area in m<sup>2</sup>; a header older than issue #1431 ' ...
                  '(no PT_NORMU) has it relative to LAST''s area and is converted. ' ...
                  'PT_FNORM, the throughput, is Norm over the geometric area ' ...
                  '&pi;&middot;0.1397<sup>2</sup> m<sup>2</sup>, derived the same way ' ...
                  'where the header predates it. Bounds are the catalogue defaults of ' ...
                  'imUtil.calib.predefSeqCompositeFun; a fit sitting on one did not ' ...
                  'converge inside the allowed range.</p>\n']);
    fprintf(Fid, ['<table><tr><th>Parameter</th><th>Median</th><th>5&ndash;95%%</th>' ...
                  '<th>N fitted</th><th>reduced: N / median</th><th>not fitted</th>' ...
                  '<th>at lower / upper bound</th></tr>\n']);
    for Ip = find([Stats.Present])
        S   = Stats(Ip);
        Lab = S.Label;
        if ~isempty(S.Unit)
            Lab = sprintf('%s [%s]', Lab, strrep(S.Unit, '^2', '<sup>2</sup>'));
        end
        if S.NReduced > 0
            Red = sprintf('%d / %.4g', S.NReduced, S.MedianReduced);
        else
            Red = '&ndash;';
        end
        if isfinite(S.Min)
            Bnd    = sprintf('%d / %d', S.NAtMin, S.NAtMax);
            BndCls = '';
            if S.NAtMin + S.NAtMax > 0
                BndCls = ' class="bad"';
            end
        else
            Bnd    = '&ndash;';
            BndCls = '';
        end
        fprintf(Fid, ['<tr><td>%s</td><td>%.4g</td><td>%.4g &ndash; %.4g</td><td>%d</td>' ...
                      '<td>%s</td><td>%d</td><td%s>%s</td></tr>\n'], ...
                Lab, S.Median, S.P05, S.P95, S.N, Red, S.NNotFitted, BndCls, Bnd);
    end
    fprintf(Fid, '</table>\n');
    Nother = max([Stats.NOtherFun]);
    if Nother > 0
        fprintf(Fid, ['<p class="note">%d crop(s) were calibrated with a different function ' ...
                      'list (PT_&lt;i&gt;_N is not the expected function) and are left out.</p>\n'], Nother);
    end
    if ~isempty(Img)
        fprintf(Fid, ['<p class="note">Blue: regular mode. Orange: reduced mode (Norm only). ' ...
                      'Red: median with the 5&ndash;95%% range of the regular mode. Black ' ...
                      'dotted: a bound that fits reached. Counts are logarithmic. Each axis ' ...
                      'spans the bulk, three 5&ndash;95%% widths either side; crops beyond ' ...
                      'it are counted as off axis in the panel title.</p>\n']);
        fprintf(Fid, '<img src="data:image/png;base64,%s">\n', Img);
    end
end

% ----------------------------------------------------------------------
function [OutFile, Summary] = transParOnlyReport(T, Info, IsReduced, IsRegular, HasMode, Args)
    % The transmission-parameter section alone, as its own HTML page.
    [Summary.TransPar, Img] = transParams(T, IsReduced, Args);
    Summary.Night    = Info.Night;
    Summary.NRegular = sum(IsRegular);
    Summary.NReduced = sum(IsReduced);
    Summary.NNoMode  = sum(~HasMode);
    if isempty(Args.OutFile)
        OutFile = fullfile(pwd, ['transpar_report_' regexprep(Info.Night, '\D', '') '.html']);
    else
        OutFile = Args.OutFile;
    end
    Title = Args.Title;
    if isempty(Title)
        Title = 'LAST photometric calibration parameters';
    end
    Fid = fopen(OutFile, 'w');
    OC  = onCleanup(@() fclose(Fid));
    fprintf(Fid, '<!doctype html><html><head><meta charset="utf-8">\n');
    fprintf(Fid, '<title>%s %s</title>\n', htmlEscape(Title), Info.Night);
    fprintf(Fid, ['<style>body{font-family:system-ui,Arial,sans-serif;margin:24px;max-width:1300px}' ...
                  'table{border-collapse:collapse;margin:8px 0}td,th{border:1px solid #ccc;' ...
                  'padding:3px 8px;text-align:right}th{background:#f2f2f2}td:first-child{text-align:left}' ...
                  '.note{color:#555;font-size:90%%}.bad{background:#fde2e2}img{max-width:100%%}</style>\n']);
    fprintf(Fid, '</head><body>\n<h1>%s &mdash; %s</h1>\n', htmlEscape(Title), Info.Night);
    fprintf(Fid, ['<p>%d crops: %d regular mode, %d reduced mode (Norm only), %d not ' ...
                  'photometrically calibrated.</p>\n'], height(T), Summary.NRegular, ...
            Summary.NReduced, Summary.NNoMode);
    writeTransPar(Fid, Summary.TransPar, Img);
    fprintf(Fid, '</body></html>\n');
    if Args.Verbose
        fprintf('nightReport: wrote %s\n', OutFile);
    end
end
