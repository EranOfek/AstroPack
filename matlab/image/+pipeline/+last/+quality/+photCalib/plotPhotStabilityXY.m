function Result = plotPhotStabilityXY(MS, Args)
    % 2D focal-plane map of per-source photometric scatter (STD) colour-coded by
    % magnitude of scatter. Each source lands at its median (X, Y); the colour
    % encodes the per-source epoch-to-epoch STD of the chosen magnitude column.
    %
    % Description: The XY-heatmap sibling of plotPhotStabilityMap - same input
    %              contract (single MS, MS array, or cell of MS - all pooled),
    %              same reject / MinEpochs / StdMethod semantics. Where
    %              plotPhotStabilityMap draws two 1-D marginal panels
    %              (STD-vs-X and STD-vs-Y with binned trends), this one draws
    %              a single 2-D scatter (X, Y) with dot colour = STD, so
    %              you can see the SPATIAL layout of the scatter rather than
    %              just its two projections.
    %
    %              Frame-agnostic - point XField/YField at whichever coordinate
    %              set matches your calibration:
    %                * per-crop calibration -> 'X','Y'   (per-crop native, one
    %                  1716 x 1716 tile; multi-crop MS overlays all tiles on
    %                  the same [1,1716]x[1,1716] axes, which is useful for
    %                  seeing common in-crop structure across crops)
    %                * joint calibration     -> 'XFULL','YFULL' (full 6388 x
    %                  9576 focal plane; requires MS that carries the full-
    %                  frame coords)
    %
    %              Pure consumer - it does not load or match. Pass MS built by
    %              stabilityN3, loadMergedMat, or matchEpochs; the chosen
    %              XField/YField/Mag must exist as full [Nepoch x Nsrc]
    %              matrices in MS.Data.
    %
    % Input  : - MS - a MatchedSources scalar, array, or cell (all pooled).
    %                 MS.Data must contain Mag, XField, YField (and FLAGS/SN
    %                 if used for masking).
    %          * ...,key,val,...
    %            'Mag'         - Magnitude field whose epoch STD is mapped.
    %                            Default 'MAG_APER_3'.
    %            'XField'      - Position field for the x-axis. Default 'X'.
    %                            Use 'XFULL' for the joint frame.
    %            'YField'      - Position field for the y-axis. Default 'Y'.
    %                            Use 'YFULL' for the joint frame.
    %            'StdMethod'   - 'robust' (1.4826*MAD) | 'plain' (sample std).
    %                            Default 'robust'.
    %            'MinEpochs'   - Drop sources with <= this many finite Mag
    %                            epochs. Default 10.
    %            'BadFlags'    - FLAGS bit names to mask per (epoch, source)
    %                            before the STD. Default {'Saturated','NearEdge'}.
    %                            {} to disable.
    %            'SNmin'       - Mask (epoch, source) with SN < SNmin.
    %                            0 disables. Default 10.
    %            'ColorMap'    - Colormap name for STD encoding. Default
    %                            'parula'. Anything accepted by colormap() works.
    %            'ColorLimits' - 2-element [cmin cmax] clamp on the STD axis
    %                            [mag]. Default [] (auto: robust percentiles
    %                            [2, 98] of the STD pool, so a few outliers
    %                            don't wash out the map). Pin to hold the
    %                            colour scale fixed across calls.
    %            'LogColor'    - Encode STD on a log scale in the colorbar.
    %                            Default true (magnitudes usually span several
    %                            decades and log makes the field-edge tail
    %                            readable).
    %            'MarkerSize'  - Scatter dot area. Default 12.
    %            'AxisEqual'   - Set 'axis equal' so pixels are square. Default
    %                            true (usually right for a focal-plane view).
    %            'CropSize'    - Pin the position axes to a fixed crop tile
    %                            extent instead of auto-fitting to data.
    %                            Scalar N -> [0.5, N+0.5] on both axes (LAST
    %                            per-crop = 1716 or 1726 depending on pipeline).
    %                            1x2 [Nx Ny] for asymmetric tiles. Full-frame
    %                            view: pass [6388 9576] (or the pipeline-
    %                            specific full-frame size). Default [] (auto).
    %            'Title'       - Figure title override. Default '' (auto).
    %            'Plot'        - Draw the figure. Default true. Set false to
    %                            just return the pooled Result (headless).
    % Output : - Result - struct with fields:
    %            .X, .Y     - per-source median position (surviving sources).
    %            .Std       - per-source epoch STD (same order).
    %            .Mag       - per-source median of Args.Mag (same order).
    %            .NSources  - number of surviving sources.
    %            .Args      - the resolved Args (for reproducibility).
    % Author : D. Kovaleva (Jul 2026)
    % See also: plotPhotStabilityMap (1-D marginal STD-vs-X and STD-vs-Y),
    %           stabilityN3 (loader + std-vs-mag),
    %           plotMagCurves (per-source MAG vs airmass/JD/time overlay).
    % Example:
    %   % --- Per-crop stability map (native crop pixels). Multi-crop MS is
    %   %     pooled so all crops overlay on the same [1, 1716] axes.
    %   MS = pipeline.last.quality.photCalib.stabilityN3(...);
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS);
    %
    %   % --- Full-frame map from a joint calibration or a loaded 24-crop MS
    %   %     that carries XFULL/YFULL:
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %       'XField','XFULL','YField','YFULL');
    %
    %   % --- Pin the STD colour scale for like-for-like comparison between
    %   %     two calibration runs; also linear (not log) colour axis:
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS_new, ...
    %       'ColorLimits', [0 0.05], 'LogColor', false);
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS_old, ...
    %       'ColorLimits', [0 0.05], 'LogColor', false);
    %
    %   % --- Plain std, bigger dots, aspect 1:1 disabled (letterbox):
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %       'StdMethod', 'plain', 'MarkerSize', 24, 'AxisEqual', false);
    %
    %   % --- Loosen the reject mask (keep flagged epochs, drop SN floor):
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %       'BadFlags', {}, 'SNmin', 0, 'MinEpochs', 5);
    %
    %   % --- Headless: get the pooled (X,Y,Std) without plotting:
    %   R = pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, 'Plot', false);
    %   scatter(R.X, R.Y, 20, R.Std, 'filled');
    %   axis equal; colorbar;
    %
    %   % --- Pixel-binned heatmap (median STD per 100-px cell) instead of
    %   %     one-dot-per-source. Only sources with median MAG_APER_3 in
    %   %     [12, 17] contribute, and the axes are pinned to the full LAST
    %   %     focal plane so runs are directly comparable:
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %       'XField','XFULL','YField','YFULL', ...
    %       'Mode','heatmap', 'BinSize', 100, 'MinPerBin', 5, ...
    %       'MagRange', [12 17], 'CropSize', [6388 9576]);
    %
    %   % --- Sweep mag windows manually (one plot per range), pinning the
    %   %     colour scale so cells are comparable across windows:
    %   Ranges = {[12 15], [12 16], [12 17], [12 18]};
    %   for R = Ranges
    %       pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %           'Mode','heatmap', 'MagRange', R{1}, ...
    %           'ColorLimits', [0 0.05], 'LogColor', false, ...
    %           'BinSize', 100, 'CropSize', [6388 9576]);
    %   end
    %
    %   % --- Density sanity check for a given mag range (source count per
    %   %     bin, not STD). Useful before trusting a MagRange choice:
    %   pipeline.last.quality.photCalib.plotPhotStabilityXY(MS, ...
    %       'Mode','heatmap', 'BinStat','count', 'MagRange', [12 17]);

    arguments
        MS
        Args.Mag         (1,:) char = 'MAG_APER_3'
        Args.XField      (1,:) char = 'X'
        Args.YField      (1,:) char = 'Y'
        Args.StdMethod   (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs   (1,1) double  = 10
        Args.BadFlags         cell     = {'Saturated','NearEdge'}
        Args.SNmin       (1,1) double  = 10
        Args.MagRange                  = []
        % [MagMin MagMax] filter on per-source median Args.Mag. Sources
        % outside this window are dropped before rendering. Empty (default)
        % = no filter. Loop externally over MagRange values to sweep depth.
        Args.Mode        (1,:) char {mustBeMember(Args.Mode,{'scatter','heatmap'})} = 'scatter'
        % 'scatter' (default) - one dot per source at its (X, Y), colour
        %                       encodes per-source STD (current behaviour).
        % 'heatmap' - bin (X, Y) into pixel cells of size Args.BinSize;
        %             colour encodes BinStat (median by default) of the
        %             per-source STD in each cell. Cells with fewer than
        %             Args.MinPerBin sources are drawn NaN (blank).
        Args.BinSize                   = 100
        % Scalar (square bins) or 1x2 [Bx By] pixel size for the heatmap
        % grid. Only used when Mode='heatmap'.
        Args.BinStat     (1,:) char {mustBeMember(Args.BinStat,{'median','mean','count'})} = 'median'
        % Per-bin reduction of the per-source STDs. 'count' shows source
        % density instead (useful sanity check for a MagRange choice).
        Args.MinPerBin   (1,1) double  = 5
        % Minimum sources per heatmap cell; below this the cell is NaN.
        Args.ColorMap    (1,:) char    = 'parula'
        Args.ColorLimits              = []
        Args.LogColor    (1,1) logical = true
        Args.MarkerSize  (1,1) double  = 12
        Args.AxisEqual   (1,1) logical = true
        Args.CropSize                  = []
        Args.Title       (1,:) char    = ''
        Args.Plot        (1,1) logical = true
        Args.OutFile     (1,:) char    = ''
        % When non-empty, save the returned Result struct to this .mat
        % path (via `save`). Enables the `diffPhotStabilityHeatmap` sibling
        % to load two heatmaps computed under different conditions (e.g.
        % joint vs per-crop calibration, or two mag windows) and plot the
        % pixel-wise difference / ratio. No effect on the figure.
    end

    MSarr = i_flattenMS(MS);
    if isempty(MSarr)
        error('pipeline:last:quality:photCalib:plotPhotStabilityXY:NoMS', ...
              'No MatchedSources supplied.');
    end

    % --- Pool per-source (median X, median Y, per-source STD, median Mag) ---
    Xs = []; Ys = []; Zs = []; Ms = [];
    for K = 1:numel(MSarr)
        M = MSarr(K);
        i_requireFields(M, {Args.Mag, Args.XField, Args.YField}, K);

        Mag = M.Data.(Args.Mag);

        % Reject mask: bad FLAGS bits OR low S/N (identical to stabilityN3
        % / plotPhotStabilityMap so the three tools stay consistent).
        Reject = flagBadEpochs(M, Args.BadFlags, 'SizeRefField', Args.Mag);
        if Args.SNmin > 0 && isfield(M.Data, 'SN')
            Reject = Reject | ~(M.Data.SN >= Args.SNmin);   % also catches NaN SN
        end
        Mag(Reject) = NaN;

        Std  = i_perSourceStd(Mag, Args.MinEpochs, Args.StdMethod);
        Xmed = median(M.Data.(Args.XField), 1, 'omitnan');
        Ymed = median(M.Data.(Args.YField), 1, 'omitnan');
        Mmed = median(Mag, 1, 'omitnan');

        Xs = [Xs, Xmed(:).']; %#ok<AGROW>
        Ys = [Ys, Ymed(:).']; %#ok<AGROW>
        Zs = [Zs, Std(:).'];  %#ok<AGROW>
        Ms = [Ms, Mmed(:).']; %#ok<AGROW>
    end
    Good = isfinite(Xs) & isfinite(Ys) & isfinite(Zs);
    Xs = Xs(Good); Ys = Ys(Good); Zs = Zs(Good); Ms = Ms(Good);

    % Optional mag-window cut on per-source median mag. Loop this arg from
    % the caller to sweep depth ('for mr = ... plotPhotStabilityXY(...,
    % 'MagRange', [12 mr])'). Empty = no filter (all sources plotted).
    if ~isempty(Args.MagRange)
        InRange = Ms >= Args.MagRange(1) & Ms <= Args.MagRange(2);
        Xs = Xs(InRange); Ys = Ys(InRange); Zs = Zs(InRange); Ms = Ms(InRange);
    end

    Result = struct('X', Xs, 'Y', Ys, 'Std', Zs, 'Mag', Ms, ...
                    'NSources', numel(Zs), 'Args', Args);

    if ~Args.Plot
        return;
    end

    % Build the plot payload first so we can compute CLim from what will
    % actually be shown (per-source Zs in scatter mode; binned HeatVal in
    % heatmap mode - very different distributions and log-scale ranges).
    figure('WindowStyle','docked','Color',[1 1 1]);
    switch lower(Args.Mode)
        case 'scatter'
            ColorSource = Zs;
            IsCount = false;
        case 'heatmap'
            [HeatVal, HeatCount, HeatX, HeatY] = i_binHeatmap( ...
                Xs, Ys, Zs, Args.BinSize, Args.BinStat, Args.MinPerBin, Args.CropSize);
            % Record heatmap product on Result for headless reuse.
            Result.HeatX      = HeatX;
            Result.HeatY      = HeatY;
            Result.HeatVal    = HeatVal;
            Result.HeatCount  = HeatCount;
            IsCount = strcmpi(Args.BinStat, 'count');
            if IsCount
                ColorSource = HeatCount(:);
            else
                ColorSource = HeatVal(:);
            end
    end
    % --- Colour-axis limits for the encoding shown -----------------------
    if isempty(Args.ColorLimits)
        Finite = ColorSource(isfinite(ColorSource) & ColorSource > 0);
        if isempty(Finite)
            CLim = [0 1];
        else
            CLim = quantile(Finite, [0.02, 0.98]);
            if ~all(isfinite(CLim)) || CLim(1) == CLim(2)
                CLim = [min(Finite), max(Finite)];
            end
        end
    else
        CLim = Args.ColorLimits(:).';
    end
    DoLog = Args.LogColor && ~IsCount;
    % When log-encoding is on, values stay LINEAR (mag) - we set the axis'
    % ColorScale to 'log' after plotting so MATLAB draws a log-spaced
    % colour axis with linear tick labels (0.001, 0.01, 0.1 ...). CLim is
    % kept in linear units and clamped away from zero so log(0) is safe.
    if DoLog
        CLimEff = [max(CLim(1), eps), max(CLim(2), CLim(1) + eps)];
    else
        CLimEff = CLim;
    end

    switch lower(Args.Mode)
        case 'scatter'
            scatter(Xs, Ys, Args.MarkerSize, Zs, 'filled');
        case 'heatmap'
            % HeatVal is indexed (Xbin, Ybin); pcolor expects (row=Y, col=X)
            % so transpose. NaN cells (below MinPerBin) render blank.
            H = pcolor(HeatX, HeatY, HeatVal.');
            H.EdgeColor = 'none';
            shading flat;
    end
    colormap(gca, Args.ColorMap);
    if all(isfinite(CLimEff)) && CLimEff(2) > CLimEff(1)
        caxis(CLimEff);
    end
    if DoLog
        % Requires R2019b+. If unavailable, fall back to a manual log-tick
        % remap on the linear-value colorbar so at least the ticks read
        % as 10^k mag.
        try
            set(gca, 'ColorScale', 'log');
        catch
            % older MATLAB - handled below by setting explicit ticks.
        end
    end
    CB = colorbar;
    if IsCount
        CB.Label.String = 'sources per bin';
    else
        StatTag = '';
        if strcmpi(Args.Mode, 'heatmap'); StatTag = [Args.BinStat ' ']; end
        % Values are always linear (ColorScale handles the log rendering).
        % So the label reads in linear mag either way; DoLog just controls
        % whether the axis is drawn log-spaced. The scale (log vs linear)
        % is visible in the tick spacing itself.
        CB.Label.String = sprintf('%sSTD(%s) [mag]', StatTag, Args.Mag);
    end
    CB.Label.Interpreter = 'none';

    box on; grid on;
    if Args.AxisEqual; axis equal tight; end
    % Pin axes to a fixed crop tile if CropSize is set; overrides the
    % auto-fit above so the axis extent is comparable across visits or
    % calibrations that sample the tile non-uniformly. Applied AFTER
    % axis-equal so aspect + limits both take effect.
    ExtX = i_cropExtent(Args.CropSize, 1);
    ExtY = i_cropExtent(Args.CropSize, 2);
    if ~isempty(ExtX); xlim(ExtX); end
    if ~isempty(ExtY); ylim(ExtY); end
    xlabel(Args.XField, 'Interpreter','none');
    ylabel(Args.YField, 'Interpreter','none');
    if ~isempty(Args.Title)
        title(Args.Title, 'Interpreter','none');
    else
        MagTag = '';
        if ~isempty(Args.MagRange)
            MagTag = sprintf(', mag in [%g, %g]', Args.MagRange(1), Args.MagRange(2));
        end
        title(sprintf('Stability map (%s) - %s (%s std, %d sources%s)', ...
              Args.Mode, Args.Mag, Args.StdMethod, numel(Zs), MagTag), ...
              'Interpreter','none');
    end
    if ~isempty(Args.OutFile)
        [D, ~, ~] = fileparts(Args.OutFile);
        if ~isempty(D) && ~exist(D, 'dir'); mkdir(D); end
        save(Args.OutFile, 'Result', '-v7.3');
        fprintf('plotPhotStabilityXY: Result saved to %s\n', Args.OutFile);
    end
end


% =========================================================================
function [Val, Cnt, Xc, Yc] = i_binHeatmap(Xs, Ys, Zs, BinSize, Stat, MinPerBin, CropSize)
    % Reduce per-source (X, Y, Z) into a 2D pixel-binned map.
    % BinSize scalar -> square bins; 1x2 -> [Bx By].
    % CropSize (scalar or 1x2) pins the grid extent to [0.5, N+0.5]; empty
    %   auto-fits to the observed X/Y range.
    % Cells with < MinPerBin sources are set to NaN.
    if isscalar(BinSize); Bxy = double([BinSize BinSize]);
    else;                 Bxy = double(BinSize(:).');   end
    if isempty(CropSize)
        Xlo = floor(min(Xs, [], 'omitnan'));
        Xhi = ceil (max(Xs, [], 'omitnan'));
        Ylo = floor(min(Ys, [], 'omitnan'));
        Yhi = ceil (max(Ys, [], 'omitnan'));
    else
        C = double(CropSize(:).');
        if isscalar(C); C = [C C]; end
        Xlo = 0.5;  Xhi = C(1) + 0.5;
        Ylo = 0.5;  Yhi = C(2) + 0.5;
    end
    Nx = max(1, ceil((Xhi - Xlo) / Bxy(1)));
    Ny = max(1, ceil((Yhi - Ylo) / Bxy(2)));
    Xedges = Xlo + (0:Nx) * Bxy(1);
    Yedges = Ylo + (0:Ny) * Bxy(2);
    Xc = 0.5 * (Xedges(1:end-1) + Xedges(2:end));
    Yc = 0.5 * (Yedges(1:end-1) + Yedges(2:end));

    Ix = discretize(Xs, Xedges);
    Iy = discretize(Ys, Yedges);
    OK = isfinite(Ix) & isfinite(Iy);
    Ix = Ix(OK); Iy = Iy(OK); Zs = Zs(OK);

    % Count per bin.
    Cnt = accumarray([Ix(:), Iy(:)], 1, [Nx, Ny], @sum, 0);

    if strcmpi(Stat, 'count')
        Val = double(Cnt);
    else
        switch lower(Stat)
            case 'median'; Fun = @median;
            case 'mean';   Fun = @mean;
        end
        Val = accumarray([Ix(:), Iy(:)], Zs(:), [Nx, Ny], Fun, NaN);
    end
    % Blank cells with too few sources.
    Val(Cnt < MinPerBin) = NaN;
end


% =========================================================================
function S = ternary(Cond, ATrue, AFalse)
    % Inline branch for building log strings.
    if Cond; S = ATrue; else; S = AFalse; end
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
            error('pipeline:last:quality:photCalib:plotPhotStabilityXY:MissingField', ...
                ['MS element %d has no Data field "%s". Rebuild the MatchedSources ', ...
                 'so this field is a full [Nepoch x Nsrc] matrix (e.g. add X/Y or ', ...
                 'XFULL/YFULL to the merge ColNamesAll).'], K, Fields{I});
        end
    end
end


% =========================================================================
function Xl = i_cropExtent(CropSize, Axis)
    % Return a 1x2 [lo, hi] extent for the given axis (1 = X, 2 = Y) derived
    % from CropSize. CropSize may be:
    %   []      -> [] (auto-fit; caller skips xlim/ylim)
    %   scalar  -> square tile [0.5, N+0.5] on both axes
    %   1x2     -> [Nx, Ny], each axis gets its own extent
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
function S = i_perSourceStd(M, MinEpochs, Method)
    % Per-source epoch scatter along dim 1 of M ([Nepoch x Nsrc]); NaN for
    % sources with <= MinEpochs finite epochs. Mirrors stabilityN3 /
    % plotPhotStabilityMap so the three tools stay numerically consistent.
    Med = median(M, 1, 'omitnan');
    switch lower(Method)
        case 'robust'
            S = 1.4826 * median(abs(M - Med), 1, 'omitnan');   % scaled MAD (robust std)
        case 'plain'
            S = std(M, 0, 1, 'omitnan');
    end
    S(sum(~isnan(M), 1) <= MinEpochs) = NaN;
end
