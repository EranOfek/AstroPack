function Indices = plotCurvesMS(MS, Args)
    % Overlay per-source curves of ANY MatchedSources quantity vs ANY per-epoch
    % quantity. Y is any MS.Data column (magnitude, flux, FWHM, position, ...);
    % X is airmass / Julian Date / time-in-hours / any per-epoch MS.Data column.
    % Sources are gated by epoch count and (optionally) by a median-value window
    % on a selection field, and drawn colour-coded by that median, with an
    % optional binned-median + Q1/Q3 overlay on top.
    %
    % Per-epoch AIRMASS (for XField='airmass') is read from the MS itself: first
    % MS.Data.AIRMASS, then MS.SrcData.AIRMASS_perEpoch, then Args.Airmass.
    % stabilityN3 auto-populates MS.Data.AIRMASS as a broadcast [Nep x Nsrc]
    % matrix, so the default entry point is:
    %   MS = stabilityN3(...);   plotCurvesMS(MS);
    %
    % Renamed from plotMagCurves (Aug 2026) when the Y-axis was generalised
    % beyond magnitudes. Migrated from /home/dana/tmp/N3/plotMagAirmass.m; the
    % CSV / 'Visit'-filename JD-parsing plumbing is retired.
    %
    % Input  : - MS - a single MatchedSources object (e.g. output of
    %                 stabilityN3, matchEpochs, loadMergedMat).
    %                 Required fields: MS.Data.(YField) [Nep x Nsrc], MS.JD.
    %          * ...,key,val,...
    %            'YField'        - MS.Data column plotted on the Y-axis (any
    %                              [Nep x Nsrc] quantity). Default 'MAGAB__APER_3'.
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
    %            'MinEpochs'     - min # of non-NaN epochs per source (counted
    %                              on YField). Default 10.
    %            'SelField'      - MS.Data column whose per-source median drives
    %                              the SelRange cut AND the colour code / colorbar.
    %                              Default '' -> use YField. Lets you select /
    %                              colour on one quantity (e.g. 'MAGAB__APER_3')
    %                              while plotting another (e.g. 'FLUX_PSF').
    %            'SelRange'      - [min, max] median-of-SelField window. Default
    %                              [12, 16] (magnitude-oriented). Pass [] to
    %                              disable the value cut entirely (keep only the
    %                              MinEpochs cut) - useful for non-magnitude Y.
    %            'OutFile'       - if non-empty, save the figure here.
    %                              Default '' (interactive show).
    %            'ColorBy'       - 'median' | 'flat'. Default 'median' (colour
    %                              each source by its median SelField value;
    %                              colorbar spans SelRange, or the data range
    %                              when SelRange=[]).
    %            'LineAlpha'     - per-line alpha. Default 0.4.
    %            'Subtract'      - 'none' | 'median' (per-source median
    %                              subtraction of Y, residuals around 0).
    %                              Default 'none'.
    %            'OverlayMedian' - true (default) to overlay a binned-median
    %                              + Q1/Q3 band over the per-source cloud.
    %            'NBins'         - number of equal-width x-axis bins for
    %                              the overlay. Default 20.
    %            'OverlayColor'  - RGB for the overlay median line + band.
    %                              Default [1 0 0].
    % Output : - Indices - column indices in MS.Data.<YField> of the sources drawn.
    % Author : D. Kovaleva (Jul 2026)
    % See also: stabilityN3 (loader + std-vs-mag), plotPhotStabilityMap
    %           (per-source scatter vs detector position),
    %           plotPhotStability, matchEpochs.
    % Example:
    %   % --- Default: MAGAB__APER_3 vs airmass, median in [12,16], >10 epochs.
    %   MS = pipeline.last.quality.photCalib.stabilityN3(...);
    %   pipeline.last.quality.photCalib.plotCurvesMS(MS);
    %
    %   % --- Deviation from median vs time (hours from a reference JD):
    %   pipeline.last.quality.photCalib.plotCurvesMS(MS, ...
    %       'XField','time', 'JD0', 2460864.2408504, 'Subtract','median');
    %
    %   % --- A NON-magnitude quantity: FWHM vs airmass, no value cut,
    %   %     coloured by median MAGAB__APER_3.
    %   pipeline.last.quality.photCalib.plotCurvesMS(MS, ...
    %       'YField','FWHM', 'SelRange',[], ...
    %       'SelField','MAGAB__APER_3', 'ColorBy','median');
    %
    %   % --- Plot FLUX_PSF but select on median MAGAB__APER_3 in [13 15]:
    %   pipeline.last.quality.photCalib.plotCurvesMS(MS, ...
    %       'YField','FLUX_PSF', 'SelField','MAGAB__APER_3', 'SelRange',[13 15]);
    %
    %   % --- Flat black lines (no colour code) - good for B/W prints.
    %   pipeline.last.quality.photCalib.plotCurvesMS(MS, ...
    %       'ColorBy','flat', 'LineAlpha', 0.2);
    %
    %   % --- Capture indices of plotted sources for follow-up:
    %   idx = pipeline.last.quality.photCalib.plotCurvesMS(MS, 'SelRange', [14 14.2]);
    %   MS.SrcData.SOURCE_ID(idx)        % source IDs that made the cut

    arguments
        MS (1,1) MatchedSources
        Args.YField        (1,:) char    = 'MAGAB__APER_3'
        Args.XField        (1,:) char    = 'airmass'
        Args.JD0           (1,1) double  = NaN
        Args.Airmass                     = []
        Args.MinEpochs     (1,1) double  = 10
        Args.SelRange      double        = [12, 16]
        Args.SelField      (1,:) char    = ''
        Args.OutFile       (1,:) char    = ''
        Args.ColorBy       (1,:) char    {mustBeMember(Args.ColorBy,{'median','flat'})} = 'median'
        Args.LineAlpha     (1,1) double  = 0.4
        Args.Subtract      (1,:) char    {mustBeMember(Args.Subtract,{'none','median'})} = 'none'
        Args.OverlayMedian (1,1) logical = true
        Args.NBins         (1,1) double  = 20
        Args.OverlayColor  (1,3) double  = [1 0 0]
    end

    Tex = @(S) strrep(S, '_', '\_');   % escape underscores for axis text

    if ~isempty(Args.SelRange) && numel(Args.SelRange) ~= 2
        error('pipeline:last:quality:photCalib:plotCurvesMS:BadSelRange', ...
              'SelRange must be [] (no cut) or a [min max] pair; got %d elements.', ...
              numel(Args.SelRange));
    end
    if ~isfield(MS.Data, Args.YField)
        error('pipeline:last:quality:photCalib:plotCurvesMS:NoYField', ...
              'MatchedSources has no Data.%s. Available: %s', ...
              Args.YField, strjoin(fieldnames(MS.Data), ', '));
    end

    Nep = numel(MS.JD);

    % ---- Resolve the per-epoch x-axis quantity + its label --------------
    [X, XLabel] = i_resolveXAxis(MS, Args, Nep);
    if strcmpi(Args.XField, 'airmass') && all(isnan(X))
        error('pipeline:last:quality:photCalib:plotCurvesMS:NoAirmass', ...
             ['No per-epoch AIRMASS available on this MS. Either\n', ...
              '  (a) pass ''Airmass'' as a 1xNepoch vector, or\n', ...
              '  (b) load MS with stabilityN3 (auto-populates MS.Data.AIRMASS), or\n', ...
              '  (c) stash your AM vector as MS.SrcData.AIRMASS_perEpoch.']);
    end

    % ---- Source selection (epoch-count + optional median-value window) ---
    % SelRange cut and the median colour code act on SelField (default: the
    % plotted YField); the plotted quantity Y and 'Subtract','median' always
    % use YField itself.
    YMat = MS.Data.(Args.YField);

    SelFieldName = Args.SelField;
    if isempty(SelFieldName); SelFieldName = Args.YField; end
    if ~isfield(MS.Data, SelFieldName)
        error('pipeline:last:quality:photCalib:plotCurvesMS:NoSelField', ...
              'MatchedSources has no Data.%s (SelField). Available: %s', ...
              SelFieldName, strjoin(fieldnames(MS.Data), ', '));
    end
    SelMat = MS.Data.(SelFieldName);

    Nobs     = sum(~isnan(YMat), 1);
    MedSel   = median(SelMat, 1, 'omitnan');   % selection + colour value
    UseRange = ~isempty(Args.SelRange);

    if UseRange
        Sel = find(Nobs(:) > Args.MinEpochs ...
                 & MedSel(:) >= Args.SelRange(1) ...
                 & MedSel(:) <= Args.SelRange(2));
        fprintf('plotCurvesMS: %d sources match (Nobs>%d, median(%s) in [%.4g, %.4g])\n', ...
            numel(Sel), Args.MinEpochs, SelFieldName, Args.SelRange(1), Args.SelRange(2));
    else
        Sel = find(Nobs(:) > Args.MinEpochs & isfinite(MedSel(:)));
        fprintf('plotCurvesMS: %d sources match (Nobs>%d, no %s value cut)\n', ...
            numel(Sel), Args.MinEpochs, SelFieldName);
    end

    if isempty(Sel)
        Indices = [];
        return;
    end
    Indices = Sel;

    Y = YMat(:, Sel);
    if strcmpi(Args.Subtract, 'median')
        Y    = Y - median(YMat(:, Sel), 1, 'omitnan');
        Ylab = sprintf('%s - median(source)', Tex(Args.YField));
    else
        Ylab = Tex(Args.YField);
    end

    % ---- Sort by the x-quantity so per-source lines connect cleanly -----
    [Xsorted, SortIdx] = sort(X);
    Y = Y(SortIdx, :);
    OK = ~isnan(Xsorted);
    Xsorted = Xsorted(OK);
    Y       = Y(OK, :);
    if isempty(Xsorted)
        warning('pipeline:last:quality:photCalib:plotCurvesMS:NoValidX', ...
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
            if UseRange
                MLo = Args.SelRange(1);  MHi = Args.SelRange(2);
            else
                MLo = min(MedSel(Sel));  MHi = max(MedSel(Sel));   % data-driven range
            end
            if ~(MHi > MLo); MHi = MLo + eps; end                 % guard flat range
            Cidx = round((MedSel(Sel) - MLo) / max(MHi-MLo, eps) * 255) + 1;
            Cidx = max(1, min(256, Cidx));
            for J = 1:numel(Sel)
                Col = Cmap(Cidx(J), :);
                Ph  = plot(Xsorted, Y(:,J), '-', 'Color', Col, 'LineWidth', 0.8);
                Ph.Color(4) = Args.LineAlpha;
            end
            CB = colorbar; colormap(Cmap);
            CB.Label.String = sprintf('median %s', Tex(SelFieldName));
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
    if UseRange
        title(sprintf('%d sources, median(%s) in [%.4g, %.4g], N_{ep}>%d', ...
            numel(Sel), Tex(SelFieldName), Args.SelRange(1), Args.SelRange(2), Args.MinEpochs));
    else
        title(sprintf('%d sources, N_{ep}>%d', numel(Sel), Args.MinEpochs));
    end
    hold off;

    if ~isempty(Args.OutFile)
        Outdir = fileparts(Args.OutFile);
        if ~isempty(Outdir) && ~exist(Outdir, 'dir'); mkdir(Outdir); end
        print(Fig, Args.OutFile, '-dpng', '-r150');
        close(Fig);
        fprintf('plotCurvesMS: saved %s\n', Args.OutFile);
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
                error('pipeline:last:quality:photCalib:plotCurvesMS:BadXField', ...
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
            error('pipeline:last:quality:photCalib:plotCurvesMS:AirmassSize', ...
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
