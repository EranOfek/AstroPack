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
    %           plotMagAirmass (per-source MAG vs AIRMASS overlay).
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

    arguments
        MS
        Args.Mag         (1,:) char = 'MAG_APER_3'
        Args.XField      (1,:) char = 'X'
        Args.YField      (1,:) char = 'Y'
        Args.StdMethod   (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs   (1,1) double  = 10
        Args.BadFlags         cell     = {'Saturated','NearEdge'}
        Args.SNmin       (1,1) double  = 10
        Args.ColorMap    (1,:) char    = 'parula'
        Args.ColorLimits              = []
        Args.LogColor    (1,1) logical = true
        Args.MarkerSize  (1,1) double  = 12
        Args.AxisEqual   (1,1) logical = true
        Args.CropSize                  = []
        Args.Title       (1,:) char    = ''
        Args.Plot        (1,1) logical = true
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

    Result = struct('X', Xs, 'Y', Ys, 'Std', Zs, 'Mag', Ms, ...
                    'NSources', numel(Zs), 'Args', Args);

    if ~Args.Plot
        return;
    end

    % --- Colour-axis limits for the STD encoding -----------------------
    if isempty(Args.ColorLimits)
        Finite = Zs(isfinite(Zs) & Zs > 0);
        if isempty(Finite)
            CLim = [0 1];
        else
            % Robust percentile clamp so a handful of outliers don't
            % squash the map into one colour bin.
            CLim = quantile(Finite, [0.02, 0.98]);
            if ~all(isfinite(CLim)) || CLim(1) == CLim(2)
                CLim = [min(Finite), max(Finite)];
            end
        end
    else
        CLim = Args.ColorLimits(:).';
    end

    % Log-encode STD: map to log10(STD) for the colour axis so the scale
    % spans decades cleanly. Non-positive STD values are dropped from the
    % colour (drawn as NaN -> invisible on the scatter), consistent with
    % the "sources with degenerate scatter should not compete for colour"
    % convention. Colorbar tick labels are formatted back into linear mag.
    if Args.LogColor
        C = nan(size(Zs));
        Pos = isfinite(Zs) & Zs > 0;
        C(Pos) = log10(Zs(Pos));
        CLimEff = log10(max(CLim, eps));
    else
        C = Zs;
        CLimEff = CLim;
    end

    figure('WindowStyle','docked','Color',[1 1 1]);
    scatter(Xs, Ys, Args.MarkerSize, C, 'filled');
    colormap(gca, Args.ColorMap);
    if all(isfinite(CLimEff)) && CLimEff(2) > CLimEff(1)
        caxis(CLimEff);
    end
    CB = colorbar;
    if Args.LogColor
        CB.Label.String = sprintf('log_{10} STD(%s) [mag]', Args.Mag);
    else
        CB.Label.String = sprintf('STD(%s) [mag]', Args.Mag);
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
        title(sprintf('Stability map - %s (%s std, %d sources)', ...
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
