function Indices = plotMagCurves(MS, Args)
    % Overlay per-source mag-vs-airmass curves for sources passing an epoch-
    % count + median-magnitude cut. Per-epoch AIRMASS is read directly from
    % the MatchedSources object (no external CSV): first MS.Data.AIRMASS is
    % tried, then MS.SrcData.AIRMASS_perEpoch, then Args.Airmass. If none
    % is available the function errors with a clear message.
    %
    % stabilityN3 auto-populates MS.Data.AIRMASS as a broadcast
    % [Nepoch x Nsrc] matrix (every column carries the same per-epoch AM),
    % so the default entry point is:
    %   MS = stabilityN3(...);   plotMagCurves(MS);
    %
    % Sources are filtered by min-epoch count and by their median-magnitude
    % window; the survivors are drawn as per-source lines colour-coded by
    % median magnitude, with an optional binned-median + Q1/Q3 overlay on
    % top. Migrated from /home/dana/tmp/N3/plotMagAirmass.m; the CSV /
    % 'Visit'-filename JD-parsing plumbing is retired.
    %
    % Input  : - MS - a single MatchedSources object (e.g. output of
    %                 stabilityN3, matchEpochs, loadMergedMat).
    %                 Required fields: MS.Data.(MagField) [Nep x Nsrc], MS.JD.
    %                 Per-epoch AIRMASS resolved from (in order):
    %                   1. Args.Airmass (explicit override)
    %                   2. MS.Data.AIRMASS(:,1) if present
    %                   3. MS.SrcData.AIRMASS_perEpoch if present
    %          * ...,key,val,...
    %            'MagField'      - magnitude matrix to plot.
    %                              Default 'MAGAB__APER_3'.
    %            'XField'        - x-axis quantity. One of:
    %                                'airmass' (default) - per-epoch AIRMASS
    %                                'jd'                - raw Julian Date
    %                                'time'              - hours since JD0:
    %                                                      (MS.JD - JD0)*24
    %                                <fieldname>         - any per-epoch column
    %                                                      of MS.Data
    %                              Bin edges, sort order and x-label follow the
    %                              chosen quantity. 'Airmass' override applies
    %                              only when XField='airmass'.
    %            'JD0'           - reference JD subtracted when XField='time'.
    %                              Default NaN -> use MS.JD(1) (hours from the
    %                              first epoch).
    %            'Airmass'       - 1xNepoch numeric override. Default []
    %                              (auto-pull from MS as above). Length
    %                              must match numel(MS.JD).
    %            'MinEpochs'     - min # of non-NaN epochs per source.
    %                              Default 10.
    %            'MagRange'      - [min, max] median-magnitude filter.
    %                              Default [12, 16].
    %            'MagRangeField' - MS.Data column whose per-source median the
    %                              MagRange cut (and the median-magnitude
    %                              colour code / colorbar) act on. Default ''
    %                              -> use MagField. Lets you select on one
    %                              magnitude (e.g. 'MAGAB__APER_3') while
    %                              plotting another (e.g. 'MAGAB__PSF').
    %            'OutFile'       - if non-empty, save the figure here.
    %                              Default '' (interactive show).
    %            'ColorBy'       - 'median' | 'flat'. Default 'median'.
    %            'LineAlpha'     - per-line alpha. Default 0.4.
    %            'Subtract'      - 'none' | 'median' (per-source median mag
    %                              subtraction, residuals around 0).
    %                              Default 'none'.
    %            'OverlayMedian' - true (default) to overlay a binned-median
    %                              + Q1/Q3 band over the per-source cloud.
    %            'NBins'         - number of equal-width x-axis bins for
    %                              the overlay. Default 20.
    %            'OverlayColor'  - RGB for the overlay median line + band.
    %                              Default [1 0 0].
    % Output : - Indices - column indices in MS.Data.<MagField> of the
    %                       sources drawn.
    % Author : D. Kovaleva (Jul 2026)
    % See also: stabilityN3 (loader + std-vs-mag), plotPhotStabilityMap
    %           (per-source scatter vs detector position),
    %           plotPhotStability, matchEpochs.
    % Example:
    %   % --- Default: absolute MAGAB__APER_3 vs airmass, sources with
    %   %             median in [12, 16] and > 10 epochs, colour-coded by
    %   %             median mag. AIRMASS is pulled from MS.Data.AIRMASS.
    %   MS = pipeline.last.quality.photCalib.stabilityN3(...);
    %   pipeline.last.quality.photCalib.plotMagCurves(MS);
    %
    %   % --- Subtract each source's median magnitude (residuals around 0).
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, 'Subtract','median');
    %
    %   % --- Deviation from median vs time (hours from a reference JD):
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, ...
    %       'XField','time', 'JD0', 2460864.2408504, 'Subtract','median');
    %
    %   % --- ... or vs raw Julian Date:
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, 'XField','jd');
    %
    %   % --- Narrower mag window + stricter epoch cut, save to file.
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, ...
    %       'MagRange', [13 15], 'MinEpochs', 50, ...
    %       'OutFile',  '/home/dana/tmp/MagAM_13_15.png');
    %
    %   % --- Different aperture / calibration column.
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, 'MagField','MAG_APER_3');
    %
    %   % --- Flat black lines (no median colour code) - good for B/W prints.
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, ...
    %       'ColorBy','flat', 'LineAlpha', 0.2);
    %
    %   % --- Explicit AIRMASS vector (e.g. MS came from a loader that did
    %   %     not stash airmass; you have your own per-epoch AM).
    %   pipeline.last.quality.photCalib.plotMagCurves(MS, 'Airmass', AMvec);
    %
    %   % --- Capture indices of plotted sources for follow-up:
    %   idx = pipeline.last.quality.photCalib.plotMagCurves(MS, ...
    %             'MagRange', [14 14.2]);
    %   MS.SrcData.SOURCE_ID(idx)        % source IDs that made the cut

    arguments
        MS (1,1) MatchedSources
        Args.MagField      (1,:) char    = 'MAGAB__APER_3'
        Args.XField        (1,:) char    = 'airmass'
        Args.JD0           (1,1) double  = NaN
        Args.Airmass                     = []
        Args.MinEpochs     (1,1) double  = 10
        Args.MagRange      (1,2) double  = [12, 16]
        Args.MagRangeField (1,:) char    = ''
        Args.OutFile       (1,:) char    = ''
        Args.ColorBy       (1,:) char    {mustBeMember(Args.ColorBy,{'median','flat'})} = 'median'
        Args.LineAlpha     (1,1) double  = 0.4
        Args.Subtract      (1,:) char    {mustBeMember(Args.Subtract,{'none','median'})} = 'none'
        Args.OverlayMedian (1,1) logical = true
        Args.NBins         (1,1) double  = 20
        Args.OverlayColor  (1,3) double  = [1 0 0]
    end

    if ~isfield(MS.Data, Args.MagField)
        error('pipeline:last:quality:photCalib:plotMagCurves:NoMagField', ...
              'MatchedSources has no Data.%s. Available: %s', ...
              Args.MagField, strjoin(fieldnames(MS.Data), ', '));
    end

    Nep = numel(MS.JD);

    % ---- Resolve the per-epoch x-axis quantity + its label --------------
    [X, XLabel] = i_resolveXAxis(MS, Args, Nep);
    if strcmpi(Args.XField, 'airmass') && all(isnan(X))
        error('pipeline:last:quality:photCalib:plotMagCurves:NoAirmass', ...
             ['No per-epoch AIRMASS available on this MS. Either\n', ...
              '  (a) pass ''Airmass'' as a 1xNepoch vector, or\n', ...
              '  (b) load MS with stabilityN3 (auto-populates MS.Data.AIRMASS), or\n', ...
              '  (c) stash your AM vector as MS.SrcData.AIRMASS_perEpoch.']);
    end

    % ---- Source selection (epoch-count + median-magnitude window) --------
    % The MagRange cut and the median-magnitude colour code act on
    % MagRangeField (default: the plotted MagField); the plotted quantity Y
    % and the 'Subtract','median' residual always use MagField itself.
    MagMat = MS.Data.(Args.MagField);

    RangeField = Args.MagRangeField;
    if isempty(RangeField); RangeField = Args.MagField; end
    if ~isfield(MS.Data, RangeField)
        error('pipeline:last:quality:photCalib:plotMagCurves:NoMagRangeField', ...
              'MatchedSources has no Data.%s (MagRangeField). Available: %s', ...
              RangeField, strjoin(fieldnames(MS.Data), ', '));
    end
    SelMat = MS.Data.(RangeField);

    Nobs   = sum(~isnan(MagMat), 1);
    MedSel = median(SelMat, 1, 'omitnan');   % selection + colour magnitude

    Sel = find(Nobs(:) > Args.MinEpochs ...
             & MedSel(:) >= Args.MagRange(1) ...
             & MedSel(:) <= Args.MagRange(2));

    fprintf('plotMagCurves: %d sources match (Nobs>%d, median(%s) in [%.2f, %.2f])\n', ...
        numel(Sel), Args.MinEpochs, RangeField, Args.MagRange(1), Args.MagRange(2));

    if isempty(Sel)
        Indices = [];
        return;
    end
    Indices = Sel;

    Y = MagMat(:, Sel);
    if strcmpi(Args.Subtract, 'median')
        Y    = Y - median(MagMat(:, Sel), 1, 'omitnan');
        Ylab = sprintf('%s - median(source)', strrep(Args.MagField, '_', '\_'));
    else
        Ylab = strrep(Args.MagField, '_', '\_');
    end

    % ---- Sort by the x-quantity so per-source lines connect cleanly -----
    [Xsorted, SortIdx] = sort(X);
    Y = Y(SortIdx, :);
    OK = ~isnan(Xsorted);
    Xsorted = Xsorted(OK);
    Y       = Y(OK, :);
    if isempty(Xsorted)
        warning('pipeline:last:quality:photCalib:plotMagCurves:NoValidX', ...
                'No epochs have a valid %s value - nothing to plot.', Args.XField);
        return;
    end

    Visible = 'on';
    if ~isempty(Args.OutFile); Visible = 'off'; end
    Fig = figure('Visible', Visible, 'Position', [100 100 1100 700]);
    hold on;

    switch lower(Args.ColorBy)
        case 'median'
            Cmap = parula(256);
            MLo = Args.MagRange(1); MHi = Args.MagRange(2);
            Cidx = round((MedSel(Sel) - MLo) / max(MHi-MLo, eps) * 255) + 1;
            Cidx = max(1, min(256, Cidx));
            for J = 1:numel(Sel)
                Col = Cmap(Cidx(J), :);
                Ph  = plot(Xsorted, Y(:,J), '-', 'Color', Col, 'LineWidth', 0.8);
                Ph.Color(4) = Args.LineAlpha;
            end
            CB = colorbar; colormap(Cmap);
            CB.Label.String = sprintf('median %s [mag]', ...
                strrep(RangeField, '_', '\_'));
            caxis([MLo, MHi]);
        case 'flat'
            for J = 1:numel(Sel)
                Ph = plot(Xsorted, Y(:,J), '-', 'Color', [0 0 0], 'LineWidth', 0.5);
                Ph.Color(4) = Args.LineAlpha;
            end
    end

    % ---- Overlay binned median with transparent Q1-Q3 band --------------
    if Args.OverlayMedian && numel(Xsorted) > 1
        % NBins equal-width bins across the x-quantity range.
        Edges = linspace(Xsorted(1), Xsorted(end), Args.NBins + 1);
        Ctr   = 0.5 * (Edges(1:end-1) + Edges(2:end));
        BinId = discretize(Xsorted, Edges);
        BinId(isnan(BinId)) = Args.NBins;   % right-edge sample -> last bin
        MedY = nan(1, Args.NBins);
        Q1Y  = nan(1, Args.NBins);
        Q3Y  = nan(1, Args.NBins);
        for B = 1:Args.NBins
            Rows = BinId == B;
            if ~any(Rows); continue; end
            Chunk = Y(Rows, :);
            Chunk = Chunk(:);
            Chunk = Chunk(~isnan(Chunk));
            if numel(Chunk) < 5; continue; end
            Q      = quantile(Chunk, [0.25, 0.5, 0.75]);
            Q1Y(B) = Q(1);
            MedY(B)= Q(2);
            Q3Y(B) = Q(3);
        end
        Good = ~isnan(MedY);
        if any(Good)
            Xb = Ctr(Good);
            patch([Xb, fliplr(Xb)], [Q3Y(Good), fliplr(Q1Y(Good))], ...
                  Args.OverlayColor, 'FaceAlpha', 0.2, 'EdgeColor', 'none', ...
                  'HandleVisibility', 'off');
            plot(Xb, MedY(Good), '-', 'Color', Args.OverlayColor, ...
                 'LineWidth', 2.5);
        end
    end

    xlabel(XLabel);
    ylabel(Ylab);
    grid on;
    title(sprintf('%d sources, median in [%.2f, %.2f], N_{ep}>%d', ...
        numel(Sel), Args.MagRange(1), Args.MagRange(2), Args.MinEpochs));
    hold off;

    if ~isempty(Args.OutFile)
        Outdir = fileparts(Args.OutFile);
        if ~isempty(Outdir) && ~exist(Outdir, 'dir'); mkdir(Outdir); end
        print(Fig, Args.OutFile, '-dpng', '-r150');
        close(Fig);
        fprintf('plotMagCurves: saved %s\n', Args.OutFile);
    end
end


% =========================================================================
function [X, XLabel] = i_resolveXAxis(MS, Args, Nep)
    % Return the per-epoch x-axis vector (Nep x 1) and its axis label,
    % dispatching on Args.XField:
    %   'airmass' - per-epoch AIRMASS (i_resolveAirmass; honours Args.Airmass)
    %   'jd'      - raw Julian Date (MS.JD)
    %   'time'    - hours since a reference JD: (MS.JD - JD0)*24, where JD0 is
    %               Args.JD0 (default MS.JD(1) when NaN)
    %   <field>   - any per-epoch column of MS.Data (broadcast-safe: first
    %               finite column is used)
    switch lower(Args.XField)
        case 'airmass'
            X      = i_resolveAirmass(MS, Args.Airmass, Nep);
            XLabel = 'AIRMASS';
        case 'jd'
            X      = double(MS.JD(:));
            XLabel = 'JD [day]';
        case 'time'
            JD0 = Args.JD0;
            if isnan(JD0); JD0 = MS.JD(1); end
            X      = (double(MS.JD(:)) - JD0) * 24;
            XLabel = sprintf('Time - JD %.7f [hr]', JD0);
        otherwise
            if ~isfield(MS.Data, Args.XField)
                error('pipeline:last:quality:photCalib:plotMagCurves:BadXField', ...
                    ['Unknown XField ''%s''. Use ''airmass'', ''jd'', ''time'', ', ...
                     'or a per-epoch field of MS.Data (available: %s).'], ...
                    Args.XField, strjoin(fieldnames(MS.Data), ', '));
            end
            A = MS.Data.(Args.XField);
            X = nan(Nep, 1);
            if size(A, 1) == Nep
                for C = 1:size(A, 2)
                    Col = double(A(:, C));
                    if any(isfinite(Col)); X = Col; break; end
                end
            end
            XLabel = strrep(Args.XField, '_', '\_');
    end
end


% =========================================================================
function AM = i_resolveAirmass(MS, Override, Nep)
    % Return per-epoch AIRMASS as an Nep x 1 vector, searching (in order):
    %   1. Explicit Override vector.
    %   2. MS.Data.AIRMASS -- broadcast [Nep x Nsrc] (each column identical)
    %      OR bare [Nep x 1]. First non-empty column is used.
    %   3. MS.SrcData.AIRMASS_perEpoch -- convention for callers that stashed
    %      the per-epoch AM on SrcData rather than broadcasting it.
    %   4. NaN vector (caller errors with a helpful message).
    if ~isempty(Override)
        if numel(Override) ~= Nep
            error('pipeline:last:quality:photCalib:plotMagCurves:AirmassSize', ...
                'Args.Airmass has %d elements; expected %d (numel(MS.JD)).', ...
                numel(Override), Nep);
        end
        AM = double(Override(:));
        return;
    end
    if isfield(MS.Data, 'AIRMASS') && ~isempty(MS.Data.AIRMASS)
        A = MS.Data.AIRMASS;
        if size(A, 1) == Nep
            % Pick the first column with any finite value (broadcast rows
            % share the same per-epoch value in every column).
            for C = 1:size(A, 2)
                Col = double(A(:, C));
                if any(isfinite(Col))
                    AM = Col;
                    return;
                end
            end
        end
    end
    if isfield(MS.SrcData, 'AIRMASS_perEpoch') && ~isempty(MS.SrcData.AIRMASS_perEpoch)
        V = MS.SrcData.AIRMASS_perEpoch(:);
        if numel(V) == Nep
            AM = double(V);
            return;
        end
    end
    AM = nan(Nep, 1);
end
