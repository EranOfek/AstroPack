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
    %                           Default 'MAGAB__APER_3'.
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
    %            'MinPerBin'  - Minimum sources per trend bin. Default 20.
    %            'Color'      - Scatter RGB. Default light blue.
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
    %            .TrendX    - binnedTrend struct for std-vs-XField (.X centres,
    %                         .Val median, .Count, .Std).
    %            .TrendY    - binnedTrend struct for std-vs-YField.
    %            .NSources  - number of surviving sources.
    %            .Args      - the resolved Args (for reproducibility).
    % Author : D. Kovaleva (Jul 2026)
    % See also: stabilityN3 (loader + std-vs-mag), plotPhotStability.
    % Example:
    %   MS = pipeline.last.quality.photCalib.stabilityN3('DataPath', PerCropDir);
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MS);          % X,Y
    %   % joint full-frame MS:
    %   pipeline.last.quality.photCalib.plotPhotStabilityMap(MSfull, ...
    %       'XField','XFULL', 'YField','YFULL');
    arguments
        MS
        Args.Mag        (1,:) char = 'MAGAB__APER_3'
        Args.XField     (1,:) char = 'X'
        Args.YField     (1,:) char = 'Y'
        Args.StdMethod  (1,:) char {mustBeMember(Args.StdMethod,{'robust','plain'})} = 'robust'
        Args.MinEpochs  (1,1) double = 10
        Args.BadFlags        cell   = {'Saturated','NearEdge'}
        Args.SNmin      (1,1) double = 10
        Args.NBins      (1,1) double = 20
        Args.BinWidthX             = []
        Args.BinWidthY             = []
        Args.MinPerBin  (1,1) double = 20
        Args.Color      (1,3) double = [0.30 0.55 0.85]
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
    Xs = []; Ys = []; Zs = [];
    for K = 1:numel(MSarr)
        M = MSarr(K);
        i_requireFields(M, {Args.Mag, Args.XField, Args.YField}, K);

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

        Xs = [Xs, Xmed(:).']; %#ok<AGROW>
        Ys = [Ys, Ymed(:).']; %#ok<AGROW>
        Zs = [Zs, Std(:).'];  %#ok<AGROW>
    end

    Good = isfinite(Xs) & isfinite(Ys) & isfinite(Zs);
    Xs = Xs(Good); Ys = Ys(Good); Zs = Zs(Good);

    % --- Binned-median trends (bin width from range/NBins when not given) ---
    BwX = i_binWidth(Args.BinWidthX, Xs, Args.NBins);
    BwY = i_binWidth(Args.BinWidthY, Ys, Args.NBins);
    TrendX = binnedTrend(Xs, Zs, 'BinWidth', BwX, 'MinCount', Args.MinPerBin, 'Stat', 'median');
    TrendY = binnedTrend(Ys, Zs, 'BinWidth', BwY, 'MinCount', Args.MinPerBin, 'Stat', 'median');

    Result = struct('X', Xs, 'Y', Ys, 'Std', Zs, ...
                    'TrendX', TrendX, 'TrendY', TrendY, ...
                    'NSources', numel(Zs), 'Args', Args);

    if ~Args.Plot
        return;
    end

    % --- Two marginal panels: std vs XField, std vs YField ---
    figure('WindowStyle','docked','Color',[1 1 1]);
    Panels = {Args.XField, Xs, TrendX; Args.YField, Ys, TrendY};
    for P = 1:2
        subplot(1, 2, P); hold on; box on; grid on;
        plot(Panels{P,2}, Zs, '.', 'MarkerSize', Args.MarkerSize, 'Color', Args.Color);
        T = Panels{P,3};
        if ~isempty(T.X)
            plot(T.X, T.Val, '-', 'LineWidth', Args.LineWidth, 'Color', Args.TrendColor);
        end
        if Args.LogY; set(gca, 'YScale', 'log'); end
        xlabel(Panels{P,1}, 'Interpreter','none');
        ylabel(sprintf('STD(%s)', Args.Mag), 'Interpreter','none');
        legend({'per-source', 'binned median'}, 'Location','best');
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
