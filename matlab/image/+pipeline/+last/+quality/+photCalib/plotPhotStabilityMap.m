function Result = plotPhotStabilityMap(MS, Args)
    % Per-source photometric scatter as a function of detector position (X,Y).
    % Description: The positional analogue of stabilityN3 / plotPhotStability:
    %              instead of plotting per-source epoch-to-epoch scatter versus
    %              magnitude, it plots it versus detector position, as two 1-D
    %              marginal panels - std-vs-XField and std-vs-YField - each with
    %              a binned-median trend line. This exposes SPATIAL structure in
    %              the stability (field-correction residuals, flat-field errors,
    %              vignetting / PSF variation) that a magnitude plot averages
    %              over.
    %
    %              The per-source scatter is computed exactly as in stabilityN3
    %              (same reject mask: bad FLAGS bits OR SN < SNmin, then robust
    %              or plain std over epochs, with a MinEpochs cut). Only the
    %              independent variable (position) and the visualisation differ.
    %
    %              Frame-agnostic: cross-epoch matching is by sky position, so
    %              the position columns are just carried data. Point XField/
    %              YField at the frame that matches the calibration:
    %                * per-crop calibration -> 'X','Y'  (native crop pixels,
    %                  within-crop map)
    %                * joint calibration     -> 'XFULL','YFULL' (full focal-plane
    %                  map; requires a multi-crop MS carrying XFULL/YFULL)
    %
    %              Pure consumer: it does not load or match - pass a
    %              MatchedSources built by stabilityN3 (which returns MS) or
    %              matchEpochs. The chosen XField/YField/Mag must exist as full
    %              [Nepoch x Nsrc] matrices in MS.Data.
    % Input  : - MS - a MatchedSources object, an array of them, or a cell of
    %                  such (all pooled). Its .Data must contain Mag, XField,
    %                  YField (and FLAGS/SN if used for masking).
    %          * ...,key,val,...
    %            'Mag'        - Magnitude field whose epoch scatter is mapped.
    %                           Default 'MAG_APER_3'.
    %            'XField'     - Position field for the first panel's x-axis.
    %                           Default 'X'. Use 'XFULL' for the joint frame.
    %            'YField'     - Position field for the second panel's x-axis.
    %                           Default 'Y'. Use 'YFULL' for the joint frame.
    %            'StdMethod'  - 'robust' (1.4826*MAD) | 'plain' (sample std).
    %                           Default 'robust'.
    %            'MinEpochs'  - Drop sources with <= this many finite Mag
    %                           epochs. Default 10.
    %            'BadFlags'   - FLAGS bit names to mask per (epoch,source) before
    %                           the std. Default {'Saturated','NearEdge'}. {} to
    %                           disable.
    %            'SNmin'      - Mask (epoch,source) with SN < SNmin. 0 disables.
    %                           Default 10.
    %            'NBins'      - Number of position bins per axis (used to derive
    %                           the bin width from each axis' data range when the
    %                           explicit BinWidth is empty). Default 20.
    %            'BinWidthX'  - Explicit x-bin width for the XField panel.
    %                           Default [] (= XField range / NBins).
    %            'BinWidthY'  - Explicit x-bin width for the YField panel.
    %                           Default [] (= YField range / NBins).
    %            'CropSize'   - Pin the position axes to a fixed crop tile
    %                           extent instead of auto-fitting to the observed
    %                           data range. Scalar N -> [0.5, N+0.5] on both
    %                           panels (LAST per-crop = 1716 or 1726 depending
    %                           on pipeline). 1x2 [Nx Ny] for asymmetric tiles.
    %                           When using XFULL/YFULL, pass [6388 9576] (or
    %                           the pipeline-specific full-frame size) to hold
    %                           the focal-plane extent constant across calls.
    %                           Default [] (auto-fit to data range).
    %            'MagBinWidth'- Width [mag] of magnitude bins for the third panel
    %                           "median STD vs magnitude". Default 1 (bins
    %                           12-13, 13-14, ...). Set 0 to skip the panel.
    %                           The per-source median of 'Mag' (post-reject) is
    %                           the bin variable; each bin's dot is the median
    %                           (or mean, per 'MagBinStat') of the per-source
    %                           STD values that fell inside it.
    %            'MagBinRange'- [min max] of magnitudes for the third panel.
    %                           Default [] (= observed range of the mag data).
    %                           Pin to hold the axis fixed across calls.
    %            'MagBinStat' - Statistic reduced per magnitude bin: 'median'
    %                           (default) or 'mean'.
    %            'TrendsByMag'- Overlay one binned-STD trend PER magnitude bin
    %                           on the X and Y panels, colour-matched to the
    %                           scatter colormap so brightness reads left-to-
    %                           right. Default true when MagBinWidth > 0.
    %                           Bin edges follow MagBinWidth + MagBinRange.
    %            'ShowGlobalTrend' - Also draw the single all-source binned
    %                           trend on the X and Y panels (red by default).
    %                           Default true.
    %            'TrendBand'  - Shaded envelope drawn UNDER each per-mag-bin
    %                           trend line on the X and Y panels (style
    %                           borrowed from plotMagCurves's OverlayMedian
    %                           band):
    %                             'none' (default) - no band.
    %                             'q13'   - Q1..Q3 band (25th/75th percentile).
    %                             'mad'   - median +- 1.4826*MAD (robust +-1 sigma).
    %                           Band colour matches the trend line for that
    %                           mag bin; MinPerBin gates the per-position
    %                           bin sample count.
    %            'TrendBandAlpha' - Face alpha of the band. Default 0.18.
    %            'TrendMarkers'- Draw black-edged filled markers at every
    %                           position-bin centre on the per-mag-bin trend
    %                           lines. Default true. Useful when scatter is
    %                           on (dots + line share the mag-encoded colour,
    %                           so the line-only version vanishes into the
    %                           cloud). Pass false for a clean "lines only"
    %                           look when ShowScatter=false.
    %            'ShowScatter'- Draw the per-source scatter dots on every panel.
    %                           Default true. Pass false for a trends-only view
    %                           (only the binned-median lines + global trend
    %                           remain — useful when the scatter cloud drowns
    %                           the lines or when overlaying several runs on
    %                           the same axes). When off, the colorbar is also
    %                           suppressed since nothing on the axes needs it.
    %            'MinPerBin'  - Minimum sources per trend bin (applies to all
    %                           trends: global, per-magnitude, and mag panel).
    %                           Default 20.
    %            'Color'      - Scatter RGB used when 'ColorBy' is empty (solid
    %                           dot colour). Default light blue.
    %            'ColorBy'    - Field name whose per-source median value drives
    %                           the dot colour (colormap-encoded). Default
    %                           'MAG_APER_3' so bright/faint sources are easy to
    %                           tell apart. Set '' to disable colour coding and
    %                           use the solid 'Color' above instead.
    %            'ColorMap'   - Colormap name for the ColorBy encoding. Default
    %                           'parula'. Anything accepted by colormap() works.
    %            'ColorLimits'- 2-element [cmin cmax] clamp on the colour axis.
    %                           Default [] (auto: [min max] of the ColorBy pool).
    %                           Useful to hold the mag-colour scale fixed across
    %                           several calls for like-for-like comparison.
    %            'TrendColor' - Trend-line RGB. Default red.
    %            'MarkerSize' - Scatter dot size. Default 6.
    %            'LineWidth'  - Trend line width. Default 2.
    %            'LogY'       - Log scatter/std y-axis. Default true.
    %            'Title'      - Figure title override. Default ''.
    %            'Plot'       - Draw the figure. Default true. Set false to only
    %                           return Result (headless / reuse).
    % Output : - Result - struct with fields:
    %            .X, .Y     - per-source median position (surviving sources).
    %            .Std       - per-source epoch scatter (same order).
    %            .Mag       - per-source median of Args.Mag (same order); used
    %                         as the bin anchor for TrendMag.
    %            .TrendX    - binnedTrend struct for std-vs-XField (.X centres,
    %                         .Val median, .Count, .Std).
    %            .TrendY    - binnedTrend struct for std-vs-YField.
    %            .TrendMag  - binnedTrend struct for the median-STD-vs-mag bins
    %                         (12-13, 13-14, ... by default). Empty .X when
    %                         MagBinWidth=0.
    %            .TrendXbyMag / .TrendYbyMag - 1xNmagbin struct arrays with per-
    %                         mag-bin STD-vs-XField / STD-vs-YField binned trends.
    %                         Each element has .BinEdges [lo hi], .BinCenter,
    %                         .X, .Val, .Count, .Std, and (when TrendBand ~=
    %                         'none') .Lo / .Hi (the shaded envelope edges,
    %                         Q1/Q3 or Med +- 1.4826*MAD). Empty when
    %                         TrendsByMag off.
    %            .Color     - per-source median of the ColorBy field, same order
    %                         as .X/.Y/.Std. Present only when ColorBy is set.
    %            .NSources  - number of surviving sources.
    %            .Args      - the resolved Args (for reproducibility).
    % Author : D. Kovaleva (Jul 2026)
    % See also: stabilityN3 (loader + std-vs-mag), plotPhotStability.
    % Example:
    %   % --- Per-crop calibration: map scatter across ONE crop's native X/Y.
    %   %     stabilityN3 now carries X,Y in MS.Data, so no rebuild is needed.
    %   MS = pipeline.last.quality.photCalib.stabilityN3('DataPath', PerCropDir);
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS);        % X,Y, robust std
    %
    %   % --- Joint calibration: map across the whole focal plane (all crops)
    %   %     using the full-frame coordinates carried by a multi-crop MS.
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSfull, ...
    %       'XField','XFULL', 'YField','YFULL');
    %
    %   % --- Plain sample std instead of robust (1.4826*MAD):
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, 'StdMethod','plain');
    %
    %   % --- Map a different magnitude column (e.g. the instrumental one):
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, 'Mag','MAG_APER_3');
    %
    %   % --- Finer position binning + require >=50 sources per trend bin:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, ...
    %       'NBins', 40, 'MinPerBin', 50);
    %
    %   % --- Explicit bin widths (e.g. 100 px bins along each axis) + title:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, ...
    %       'BinWidthX', 100, 'BinWidthY', 100, ...
    %       'Title', 'Field 1716.c crop 10 - stability vs position');
    %
    %   % --- Loosen quality cuts (keep flagged epochs, drop the S/N floor):
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, ...
    %       'BadFlags', {}, 'SNmin', 0, 'MinEpochs', 5);
    %
    %   % --- Headless: get the binned trends without drawing, then plot yourself
    %   %     (e.g. to overlay several crops / calibrations on one axes):
    %   R = pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, 'Plot', false);
    %   figure; semilogy(R.TrendX.X, R.TrendX.Val, '-o'); hold on;
    %   xlabel('X [pix]'); ylabel('median robust std [mag]');
    %   fprintf('%d sources; median scatter = %.4f mag\n', R.NSources, median(R.Std));
    %
    %   % --- Presentation-friendly colours and thicker line:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS, ...
    %       'Color',[0.6 0.6 0.6], 'TrendColor',[0.85 0.10 0.10], ...
    %       'MarkerSize', 4, 'LineWidth', 3);
    %
    %   % ================================================================
    %   % Multi-crop aggregation: pool all 24 crops of a visit into ONE plot.
    %   % ================================================================
    %   %
    %   % The function's input contract accepts a scalar MS, an MS array, OR
    %   % a cell of MS. All elements are flattened by i_flattenMS and
    %   % pooled per-source (median X, median Y, per-source STD) into a
    %   % single (Xs, Ys, Zs) vector before binning. Each crop can bring
    %   % its own X/Y range; per-crop pooling overlays all tiles on the
    %   % same [1, CropSize] axes, so common in-crop structure across crops
    %   % shows up as a coherent pattern.
    %
    %   % --- 1a. Fast path: load a whole visit's 24 MergedMat files at once
    %   %     as a 1x24 MatchedSources array, then pool.
    %   MSarr = pipeline.last.load.loadMergedMat( ...
    %               'MergedMatDir', '/data/2025/06/22/proc/223444v0');   % 1x24
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSarr, ...
    %       'XField','X','YField','Y', 'CropSize', 1726);
    %
    %   % --- 1b. Calibrated coadd catalogs (no MergedMat): loop stabilityN3
    %   %     over each crop's pattern and pass the MS cell straight in.
    %   BaseDir = '/home/dana/tmp/JointRun/joint__LAST_Joint_2Iter_Split3__cheby1_4';
    %   MSc = cell(1, 24);
    %   for k = 1:24
    %       Pat  = sprintf('LAST*_1716.c_*_%03d_sci_coadd_Cat_1.fits', k);
    %       MSc{k} = pipeline.last.quality.photCalib.stabilityN3( ...
    %                   'DataPath', BaseDir, 'Pattern', Pat);
    %   end
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSc, ...
    %       'XField','X','YField','Y', ...
    %       'CropSize',   1716, ...      % 1726 for the old pipeline
    %       'TrendBand',  'q13', ...
    %       'ShowScatter', false, ...
    %       'TrendMarkers', false, ...
    %       'ShowGlobalTrend', false, ...
    %       'ColorMap',   'turbo');       % diverse per-mag-bin line colours
    %
    %   % --- 2. Joint calibration output that already carries XFULL/YFULL:
    %   %     one MS, full focal plane.
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSjoint, ...
    %       'XField','XFULL','YField','YFULL', ...
    %       'CropSize',  [6388 9576], ...
    %       'TrendBand', 'q13');
    %
    %   % --- 3. Same 24-crop pool, robust +-MAD band under each per-mag-bin
    %   %     trend instead of Q1/Q3:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSc, ...
    %       'TrendBand', 'mad', 'CropSize', 1716);
    arguments
        MS
        Args.Mag        (1,:) char = 'MAG_APER_3'
        Args.XField     (1,:) char = 'X'
        Args.YField     (1,:) char = 'Y'
        Args.StdMethod  (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs  (1,1) double = 10
        Args.BadFlags        cell   = {'Saturated','NearEdge'}
        Args.SNmin      (1,1) double = 10
        Args.NBins      (1,1) double = 20
        Args.BinWidthX             = []
        Args.BinWidthY             = []
        Args.CropSize              = []
        Args.MagBinWidth (1,1) double = 1
        Args.MagBinRange              = []
        Args.MagBinStat  (1,:) char {mustBeMember(Args.MagBinStat,{'median','mean'})} = 'median'
        Args.TrendsByMag                = []
        Args.ShowGlobalTrend (1,1) logical = true
        Args.TrendBand       (1,:) char {mustBeMember(Args.TrendBand,{'none','q13','mad'})} = 'none'
        Args.TrendBandAlpha  (1,1) double = 0.18
        Args.TrendMarkers    (1,1) logical = true
        Args.ShowScatter     (1,1) logical = true
        Args.MinPerBin  (1,1) double = 5
        Args.Color      (1,3) double = [0.30 0.55 0.85]
        Args.ColorBy    (1,:) char   = 'MAG_APER_3'
        Args.ColorMap   (1,:) char   = 'parula'
        Args.ColorLimits             = []
        Args.TrendColor (1,3) double = [0.85 0.10 0.10]
        Args.MarkerSize (1,1) double = 6
        Args.LineWidth  (1,1) double = 2
        Args.LogY       (1,1) logical = true
        Args.Title      (1,:) char = ''
        Args.Plot       (1,1) logical = true
    end

    MSarr = i_flattenMS(MS);
    if isempty(MSarr)
        error('pipeline:last:quality:photCalib:plotPhotStabilityMap:NoMS', ...
              'No MatchedSources supplied.');
    end

    % --- Pool per-source (median position, epoch scatter) over all elements ---
    UseColor = ~isempty(Args.ColorBy);
    ColorIsMag = UseColor && strcmp(Args.ColorBy, Args.Mag);
    Xs = []; Ys = []; Zs = []; Cs = []; Ms = [];
    for K = 1:numel(MSarr)
        M = MSarr(K);
        Need = {Args.Mag, Args.XField, Args.YField};
        if UseColor && ~ColorIsMag; Need{end+1} = Args.ColorBy; end %#ok<AGROW>
        i_requireFields(M, Need, K);

        Mag = M.Data.(Args.Mag);

        % Reject mask: bad FLAGS bits OR low S/N (identical to stabilityN3).
        Reject = flagBadEpochs(M, Args.BadFlags, 'SizeRefField', Args.Mag);
        if Args.SNmin > 0 && isfield(M.Data, 'SN')
            Reject = Reject | ~(M.Data.SN >= Args.SNmin);   % also catches NaN SN
        end
        Mag(Reject) = NaN;

        Std = i_perSourceStd(Mag, Args.MinEpochs, Args.StdMethod);
        Xmed = median(M.Data.(Args.XField), 1, 'omitnan');
        Ymed = median(M.Data.(Args.YField), 1, 'omitnan');
        Mmed = median(Mag, 1, 'omitnan');   % per-source median mag for the mag-binned trend

        Xs = [Xs, Xmed(:).']; %#ok<AGROW>
        Ys = [Ys, Ymed(:).']; %#ok<AGROW>
        Zs = [Zs, Std(:).'];  %#ok<AGROW>
        Ms = [Ms, Mmed(:).']; %#ok<AGROW>
        if UseColor
            if ColorIsMag
                Cs = [Cs, Mmed(:).']; %#ok<AGROW>
            else
                % Apply the same reject mask before medianing the colour column
                % so a bright saturated epoch doesn't drag a source's colour.
                Cval = M.Data.(Args.ColorBy);
                Cval(Reject) = NaN;
                Cmed = median(Cval, 1, 'omitnan');
                Cs = [Cs, Cmed(:).']; %#ok<AGROW>
            end
        end
    end

    Good = isfinite(Xs) & isfinite(Ys) & isfinite(Zs);
    if UseColor; Good = Good & isfinite(Cs); end
    Xs = Xs(Good); Ys = Ys(Good); Zs = Zs(Good); Ms = Ms(Good);
    if UseColor; Cs = Cs(Good); end

    % --- Binned-median trends (bin width from range/NBins when not given) ---
    BwX = i_binWidth(Args.BinWidthX, Xs, Args.NBins);
    BwY = i_binWidth(Args.BinWidthY, Ys, Args.NBins);
    TrendX = binnedTrend(Xs, Zs, 'BinWidth', BwX, 'MinCount', Args.MinPerBin, 'Stat', 'median');
    TrendY = binnedTrend(Ys, Zs, 'BinWidth', BwY, 'MinCount', Args.MinPerBin, 'Stat', 'median');

    % Median-STD-vs-magnitude trend (fixed 1-mag bins by default; the anchor is
    % each source's post-reject median mag).
    ShowMagPanel = Args.MagBinWidth > 0;
    if ShowMagPanel
        TrendMag = binnedTrend(Ms, Zs, ...
                    'BinWidth', Args.MagBinWidth, ...
                    'Range',    Args.MagBinRange, ...
                    'MinCount', Args.MinPerBin, ...
                    'Stat',     Args.MagBinStat);
    else
        TrendMag = struct('X', [], 'Val', [], 'Count', [], 'Std', []);
    end

    % Per-magnitude-bin trends of STD vs XField and vs YField (one line per
    % mag bin, drawn on top of the two position panels). Bin edges follow
    % MagBinWidth + MagBinRange (defaults auto-fit to the observed mag range).
    if isempty(Args.TrendsByMag)
        DoTrendsByMag = ShowMagPanel;   % default: on iff mag panel is on
    else
        DoTrendsByMag = logical(Args.TrendsByMag);
    end
    TrendXbyMag = struct('BinEdges', {}, 'BinCenter', {}, 'X', {}, 'Val', {}, ...
                         'Count', {}, 'Std', {}, 'Lo', {}, 'Hi', {});
    TrendYbyMag = TrendXbyMag;
    DoBand = ~strcmpi(Args.TrendBand, 'none');
    if DoTrendsByMag
        MagEdges = i_magEdges(Ms, Args.MagBinRange, Args.MagBinWidth);
        for B = 1:numel(MagEdges)-1
            InBin = Ms >= MagEdges(B) & Ms < MagEdges(B+1);
            if nnz(InBin) < Args.MinPerBin
                Tx = struct('X', [], 'Val', [], 'Count', [], 'Std', [], 'Lo', [], 'Hi', []);
                Ty = Tx;
            else
                Tx = i_binnedMedianBand(Xs(InBin), Zs(InBin), BwX, ...
                                        Args.MinPerBin, Args.TrendBand);
                Ty = i_binnedMedianBand(Ys(InBin), Zs(InBin), BwY, ...
                                        Args.MinPerBin, Args.TrendBand);
            end
            Center = (MagEdges(B) + MagEdges(B+1)) / 2;
            TrendXbyMag(B) = struct('BinEdges', [MagEdges(B) MagEdges(B+1)], ...
                                    'BinCenter', Center, ...
                                    'X', Tx.X, 'Val', Tx.Val, ...
                                    'Count', Tx.Count, 'Std', Tx.Std, ...
                                    'Lo', Tx.Lo, 'Hi', Tx.Hi);
            TrendYbyMag(B) = struct('BinEdges', [MagEdges(B) MagEdges(B+1)], ...
                                    'BinCenter', Center, ...
                                    'X', Ty.X, 'Val', Ty.Val, ...
                                    'Count', Ty.Count, 'Std', Ty.Std, ...
                                    'Lo', Ty.Lo, 'Hi', Ty.Hi);
        end
    end

    Result = struct('X', Xs, 'Y', Ys, 'Std', Zs, 'Mag', Ms, ...
                    'TrendX', TrendX, 'TrendY', TrendY, 'TrendMag', TrendMag, ...
                    'TrendXbyMag', TrendXbyMag, 'TrendYbyMag', TrendYbyMag, ...
                    'NSources', numel(Zs), 'Args', Args);
    if UseColor
        Result.Color = Cs;
    end

    if ~Args.Plot
        return;
    end

    % --- Colour-axis limits (empty -> auto). Pin once so both panels share.
    if UseColor
        if isempty(Args.ColorLimits)
            CLim = [min(Cs, [], 'omitnan'), max(Cs, [], 'omitnan')];
            if ~all(isfinite(CLim)) || CLim(1) == CLim(2)
                CLim = [];   % fall back to Matlab default caxis
            end
        else
            CLim = Args.ColorLimits(:).';
        end
    end

    % Resolve the CLim actually used for colour-matching the per-mag-bin
    % trend lines (so bin colours match dot colours). Fall back to the mag
    % pool range when scatter isn't colour-coded.
    if DoTrendsByMag
        if UseColor && exist('CLim','var') && ~isempty(CLim)
            TrendCLim = CLim;
        else
            TrendCLim = [min(Ms, [], 'omitnan'), max(Ms, [], 'omitnan')];
        end
        MagCmap = feval(Args.ColorMap, 256);
    end

    % --- Marginal panels: std vs XField, std vs YField, and (optional)
    %     median STD vs magnitude in fixed mag bins.
    figure('WindowStyle','docked','Color',[1 1 1]);
    Panels = {Args.XField, Xs, TrendX, TrendXbyMag; ...
              Args.YField, Ys, TrendY, TrendYbyMag};
    NPanels = 2 + ShowMagPanel;
    for P = 1:2
        subplot(1, NPanels, P); hold on; box on; grid on;
        LegLabels = {}; LegHandles = [];
        if Args.ShowScatter
            if UseColor
                H = scatter(Panels{P,2}, Zs, Args.MarkerSize^2, Cs, 'filled');
                colormap(gca, Args.ColorMap);
                if ~isempty(CLim); caxis(CLim); end
                CB = colorbar;
                CB.Label.String       = Args.ColorBy;
                CB.Label.Interpreter  = 'none';
            else
                H = plot(Panels{P,2}, Zs, '.', 'MarkerSize', Args.MarkerSize, 'Color', Args.Color);
            end
            LegHandles(end+1) = H;      LegLabels{end+1} = 'per-source';
        end
        if Args.ShowGlobalTrend
            T = Panels{P,3};
            if ~isempty(T.X)
                H = plot(T.X, T.Val, '-', ...
                    'LineWidth', Args.LineWidth, 'Color', Args.TrendColor);
                LegHandles(end+1) = H; LegLabels{end+1} = 'binned median (all mag)';
            end
        end
        if DoTrendsByMag
            TArr = Panels{P,4};
            % Collect per-mag-bin legend entries into their own list so we
            % can add them to the main legend REVERSED (faint bins at the
            % top of the legend, bright bins at the bottom). Draw order is
            % unchanged - bright bins are drawn first, faint bins on top.
            BinHandles = gobjects(0);
            BinLabels  = {};
            for B = 1:numel(TArr)
                if isempty(TArr(B).X); continue; end
                Col = i_colorFromMap(MagCmap, TArr(B).BinCenter, TrendCLim);
                % Optional Q1/Q3 or MAD envelope, drawn UNDER the line so
                % the trend + markers stay on top of the band. Style mirrors
                % plotMagCurves's OverlayMedian shaded overlay.
                if DoBand && ~isempty(TArr(B).Lo)
                    Xb = TArr(B).X(:).';
                    Lo = TArr(B).Lo(:).';
                    Hi = TArr(B).Hi(:).';
                    patch([Xb, fliplr(Xb)], [Hi, fliplr(Lo)], ...
                        Col, 'FaceAlpha', Args.TrendBandAlpha, ...
                        'EdgeColor', 'none', 'HandleVisibility', 'off');
                end
                % Thick coloured line + (optionally) black-edged filled
                % circles at every bin centre. Without the markers + black
                % edge the line vanishes on top of a same-coloured scatter
                % cloud, so markers are on by default. When ShowScatter is
                % off (or the caller explicitly toggles TrendMarkers off),
                % switch to a plain line for a cleaner look.
                if Args.TrendMarkers
                    H = plot(TArr(B).X, TArr(B).Val, '-o', ...
                        'LineWidth',       Args.LineWidth + 1, ...
                        'Color',           Col, ...
                        'MarkerFaceColor', Col, ...
                        'MarkerEdgeColor', 'k', ...
                        'MarkerSize',      max(6, Args.MarkerSize + 2));
                else
                    H = plot(TArr(B).X, TArr(B).Val, '-', ...
                        'LineWidth', Args.LineWidth + 1, 'Color', Col);
                end
                BinHandles(end+1) = H; %#ok<AGROW>
                % Compact per-bin label: just the mag range (the "median in"
                % prefix is redundant when the whole group of lines has the
                % same meaning). The legend title below carries the "what".
                BinLabels{end+1}  = sprintf('%g-%g mag', ...
                    TArr(B).BinEdges(1), TArr(B).BinEdges(2)); %#ok<AGROW>
            end
            % Faint bins first in the legend (top), bright bins last (bottom).
            LegHandles = [LegHandles, fliplr(BinHandles)];
            LegLabels  = [LegLabels,  fliplr(BinLabels)];
        end
        if Args.LogY; set(gca, 'YScale', 'log'); end
        xlabel(Panels{P,1}, 'Interpreter','none');
        ylabel(sprintf('STD(%s)', Args.Mag), 'Interpreter','none');
        % Pin the position axis to the full crop tile when CropSize is set
        % (P=1 uses the X extent, P=2 uses the Y extent), so the axis width
        % is comparable across visits/calibrations that sample the tile
        % non-uniformly. Auto-fit when empty.
        Extent = i_cropExtent(Args.CropSize, P);
        if ~isempty(Extent); xlim(Extent); end
        Lg = legend(LegHandles, LegLabels, 'Location','best');
        if DoTrendsByMag
            Lg.Title.String = sprintf('%s STD per mag bin', Args.MagBinStat);
        end
    end
    if ShowMagPanel
        subplot(1, NPanels, 3); hold on; box on; grid on;
        MagLegLabels = {}; MagLegHandles = [];
        if Args.ShowScatter
            % Faint per-source cloud in the background, then the bin markers on top.
            H = plot(Ms, Zs, '.', 'MarkerSize', max(2, Args.MarkerSize-2), ...
                 'Color', [0.75 0.75 0.75]);
            MagLegHandles(end+1) = H;
            MagLegLabels{end+1}  = 'per-source';
        end
        if ~isempty(TrendMag.X)
            H = plot(TrendMag.X, TrendMag.Val, 'o-', ...
                 'LineWidth',      Args.LineWidth, ...
                 'Color',          Args.TrendColor, ...
                 'MarkerFaceColor',Args.TrendColor, ...
                 'MarkerSize',     max(6, Args.MarkerSize+2));
            MagLegHandles(end+1) = H;
            MagLegLabels{end+1}  = sprintf('%s per %g-mag bin', ...
                Args.MagBinStat, Args.MagBinWidth);
        end
        if Args.LogY; set(gca, 'YScale', 'log'); end
        xlabel(sprintf('%s (per-source median)', Args.Mag), 'Interpreter','none');
        ylabel(sprintf('%s STD(%s)', Args.MagBinStat, Args.Mag), ...
               'Interpreter','none');
        legend(MagLegHandles, MagLegLabels, 'Location','best');
    end
    if ~isempty(Args.Title)
        sgtitle(Args.Title, 'Interpreter','none');
    else
        sgtitle(sprintf('Stability vs position - %s (%s std, %d sources)', ...
                Args.Mag, Args.StdMethod, numel(Zs)), 'Interpreter','none');
    end
end


% =========================================================================
function A = i_flattenMS(MS)
    % Normalise scalar / array / cell-of-arrays input to a 1xN MatchedSources.
    if iscell(MS)
        A = MatchedSources.empty(1, 0);
        for I = 1:numel(MS)
            A = [A, reshape(MS{I}, 1, [])]; %#ok<AGROW>
        end
    else
        A = reshape(MS, 1, []);
    end
end


% =========================================================================
function i_requireFields(MS, Fields, K)
    % Error clearly when a required Data field is missing (e.g. XField absent
    % because the MS was built without X/Y in ColNamesAll).
    for I = 1:numel(Fields)
        if ~isfield(MS.Data, Fields{I})
            error('pipeline:last:quality:photCalib:plotPhotStabilityMap:MissingField', ...
                ['MS element %d has no Data field "%s". Rebuild the MatchedSources ', ...
                 'so this field is a full [Nepoch x Nsrc] matrix (e.g. add X/Y or ', ...
                 'XFULL/YFULL to the merge ColNamesAll).'], K, Fields{I});
        end
    end
end


% =========================================================================
function S = i_perSourceStd(M, MinEpochs, Method)
    % Per-source epoch scatter along dim 1 of M ([Nepoch x Nsrc]); NaN for
    % sources with <= MinEpochs finite epochs. Mirrors stabilityN3.perSourceStats.
    Med = median(M, 1, 'omitnan');
    switch lower(Method)
        case 'robust'
            S = 1.4826 * median(abs(M - Med), 1, 'omitnan');   % scaled MAD (robust std)
        case 'plain'
            S = std(M, 0, 1, 'omitnan');
    end
    S(sum(~isnan(M), 1) <= MinEpochs) = NaN;
end


% =========================================================================
function T = i_binnedMedianBand(X, Y, BinWidth, MinCount, BandKind)
    % Per-bin median of Y across bins of X, with a Q1/Q3 or +-MAD envelope
    % (Lo, Hi) around the median. Same bin geometry as binnedTrend, but
    % also carries the band edges so the plot layer can shade an envelope.
    T = struct('X', [], 'Val', [], 'Count', [], 'Std', [], 'Lo', [], 'Hi', []);
    if ~(BinWidth > 0); return; end
    OK = isfinite(X) & isfinite(Y);
    X = X(OK);  Y = Y(OK);
    if numel(X) < MinCount; return; end
    Lo = min(X);  Hi = max(X);
    if ~(Hi > Lo); return; end
    Edges = Lo:BinWidth:Hi;
    if Edges(end) < Hi; Edges(end+1) = Hi; end
    Nbin = numel(Edges) - 1;
    BinId = discretize(X, Edges);
    BinId(isnan(BinId)) = Nbin;               % right-edge sample -> last bin
    Med = nan(1, Nbin);
    LoV = nan(1, Nbin);
    HiV = nan(1, Nbin);
    Std = nan(1, Nbin);
    Cnt = zeros(1, Nbin);
    for B = 1:Nbin
        Chunk = Y(BinId == B);
        Cnt(B) = numel(Chunk);
        if Cnt(B) < MinCount; continue; end
        Med(B) = median(Chunk);
        Std(B) = std(Chunk);
        switch lower(BandKind)
            case 'none'
                % leave Lo/Hi as NaN
            case 'q13'
                Q = quantile(Chunk, [0.25 0.75]);
                LoV(B) = Q(1);   HiV(B) = Q(2);
            case 'mad'
                Mad    = 1.4826 * median(abs(Chunk - Med(B)));
                LoV(B) = Med(B) - Mad;
                HiV(B) = Med(B) + Mad;
        end
    end
    Ctr = 0.5 * (Edges(1:end-1) + Edges(2:end));
    Keep = ~isnan(Med);
    T.X     = Ctr(Keep);
    T.Val   = Med(Keep);
    T.Std   = Std(Keep);
    T.Count = Cnt(Keep);
    if strcmpi(BandKind, 'none')
        T.Lo = [];
        T.Hi = [];
    else
        T.Lo = LoV(Keep);
        T.Hi = HiV(Keep);
    end
end


% =========================================================================
function Edges = i_magEdges(Ms, Range, Width)
    % Fixed-width magnitude bin edges. If Range is empty, snap it to whole
    % multiples of Width around the observed [min, max] so 1-mag bins land
    % at integer magnitudes (12, 13, 14, ...).
    if isempty(Range)
        V = Ms(isfinite(Ms));
        if isempty(V)
            Edges = [];
            return;
        end
        Lo = floor(min(V) / Width) * Width;
        Hi = ceil (max(V) / Width) * Width;
    else
        Lo = Range(1);  Hi = Range(2);
    end
    if ~(Hi > Lo) || ~(Width > 0)
        Edges = [];
        return;
    end
    Edges = Lo:Width:Hi;
    if Edges(end) < Hi
        Edges(end+1) = Hi;   % ensure the last partial bin is included
    end
end


% =========================================================================
function C = i_colorFromMap(Cmap, Value, CLim)
    % Look up an RGB triplet by mapping Value linearly into CLim over Cmap
    % rows. Clamped to [0, 1] and to the map's row range.
    N = size(Cmap, 1);
    if ~isfinite(Value) || CLim(2) <= CLim(1)
        Idx = 1;
    else
        F   = (Value - CLim(1)) / (CLim(2) - CLim(1));
        Idx = 1 + round(min(max(F, 0), 1) * (N - 1));
    end
    C = Cmap(Idx, :);
end


% =========================================================================
function Xl = i_cropExtent(CropSize, Axis)
    % Return a 1x2 [lo, hi] xlim for the given axis (1 = X, 2 = Y) derived
    % from CropSize. CropSize may be:
    %   []      -> [] (auto-fit; caller skips xlim)
    %   scalar  -> square tile [0.5, N+0.5] on both axes
    %   1x2     -> [Nx, Ny], each panel gets its own extent
    if isempty(CropSize)
        Xl = [];
        return;
    end
    CropSize = double(CropSize(:).');
    if isscalar(CropSize)
        N = CropSize;
    else
        N = CropSize(min(Axis, numel(CropSize)));
    end
    if ~(isfinite(N) && N > 0)
        Xl = [];
        return;
    end
    Xl = [0.5, N + 0.5];
end


% =========================================================================
function Bw = i_binWidth(BwArg, Vals, NBins)
    % Explicit bin width if given, else (data range)/NBins with a positive
    % fallback so binnedTrend always receives a usable width.
    if ~isempty(BwArg)
        Bw = BwArg;
        return;
    end
    V = Vals(isfinite(Vals));
    if numel(V) < 2 || range(V) == 0
        Bw = 1;
    else
        Bw = range(V) / max(1, NBins);
    end
end
