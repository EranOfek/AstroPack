function Result = plotPhotCatHist(DataPath, Args)
    % Histogram of one or more catalog columns pooled over many visit dirs.
    % Description: Discovers visit subdirectories under DataPath (one level
    %              or recursively), loads each visit's catalogs via
    %              loadVisitCatHdr (CatData + HeaderData only — no pixels),
    %              pulls the requested column(s) from every AstroImage's
    %              CatData using getCol, pools the rows into one (or several
    %              overlaid) distribution(s), and plots a histogram with
    %              optional summary statistics.
    %
    %              Counterpart of plotPhotHistogram: that function plots one
    %              scalar per PhotCalibTrans (Chi2_DOF, NCalib, AirMass, ...);
    %              this function plots per-source quantities pulled directly
    %              from the catalog rows (e.g. MAG_PSF, MAG_AB_APER_3, FWHM,
    %              SN_3, FLUX_APER_3, X, Y, RA, Dec).
    %
    % Input  : - DataPath char/string. Parent directory containing one or
    %            more visit subdirectories.
    %          * ...,key,val,...
    %            'Columns'      - Catalog column name (char) or cell of names
    %                             to overlay (e.g. {'MAG_PSF','MAG_AB_APER_3'}).
    %                             Required.
    %            'VisitPattern' - Glob for visit subdirectory names.
    %                             Default '*v[0-9]*'.
    %            'Recursive'    - If true, search for visit dirs at any depth
    %                             under DataPath; otherwise only direct
    %                             children. Default true.
    %            'FileType'     - 'coadd' or 'proc'. Forwarded to
    %                             loadVisitCatHdr. Default 'coadd'.
    %            'CropsToAnalyze' - Crop indices (1..Ncrop) to include from
    %                             each visit. Default [] (all).
    %            'RowFilter'    - Function handle @(AI) -> logical row mask
    %                             applied to each AstroImage.CatData before
    %                             pooling. Useful e.g. to require finite
    %                             SN_3 or to exclude flagged sources. Default
    %                             [] (keep all rows).
    %            'MaxRowsPerImage' - If set to a finite N, randomly subsample
    %                             N rows per AI (after RowFilter) to cap
    %                             memory/plotting cost. Default Inf.
    %            'Bins'         - Number of bins or vector of edges. Default 60.
    %            'XLim'         - x-limits [min max]. Default [] (auto).
    %            'LogX'         - Log-transformed x axis. Default false.
    %            'Stats'        - Overlay median + 16/84 percentiles + N as
    %                             text (single-column mode only). Default true.
    %            'Color'        - Bar color for single-column mode. Default
    %                             [0.30 0.55 0.90].
    %            'Verbose'      - Print progress / per-column N. Default false.
    % Output : - Result struct with fields:
    %            .Columns   - 1xK cell of column names actually plotted.
    %            .Values    - 1xK cell, each a column vector of pooled values
    %                         (NaNs and (LogX=true) non-positives dropped).
    %            .Stats     - 1xK struct array: .N .Median .Mean .Std .P16
    %                         .P84 .Min .Max  per column.
    %            .DataPath, .VisitPattern, .Recursive, .FileType - echoed.
    %            .NVisits   - number of visit dirs found.
    %            .NImages   - number of AstroImages processed (sum over visits).
    % Author : D. Kovaleva (Apr 2026)
    % Example: % Single column over a date tree:
    %          DP = '/archimedes/LASTunitTest/2025/04/26';
    %          R = pipeline.last.quality.photCalib.plotPhotCatHist(DP, ...
    %                  'Columns', 'MAG_PSF');
    %
    %          % Overlay PSF vs aperture-3 magnitudes:
    %          R = pipeline.last.quality.photCalib.plotPhotCatHist(DP, ...
    %                  'Columns', {'MAG_PSF','MAG_AB_APER_3'}, ...
    %                  'Bins', 80, 'XLim', [10 22]);
    %
    %          % Whole year, only v0 coadd visits, S/N>5 sources only:
    %          R = pipeline.last.quality.photCalib.plotPhotCatHist( ...
    %                  '/archimedes/LASTunitTest/2025', ...
    %                  'VisitPattern', '*v0', ...
    %                  'Columns', 'MAG_AB_APER_3', ...
    %                  'RowFilter', @(AI) AI.CatData.getCol('SN_3') > 5);
    %
    %          % Restrict to a single observation field (token after '_clear_'):
    %          R = pipeline.last.quality.photCalib.plotPhotCatHist(DP, ...
    %                  'Columns', 'MAG_PSF', 'FieldId', '1781');

    arguments
        DataPath                          {mustBeText}
        Args.Columns                                              % char or cell
        Args.VisitPattern    {mustBeText}              = '*v[0-9]*'
        Args.Recursive       logical                    = true
        Args.FileType        {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.FieldId                                      = ''
        Args.CropID          double {mustBeInteger, mustBeNonnegative} = []
        Args.CropsToAnalyze                              = []
        Args.RowFilter                                   = []
        Args.MaxRowsPerImage                             = Inf
        Args.Bins                                        = 60
        Args.XLim                                        = []
        Args.LogX            logical                    = false
        Args.Stats           logical                    = true
        Args.Color                                       = [0.30 0.55 0.90]
        Args.Verbose         logical                    = false
    end

    % --- Normalize column list ------------------------------------------
    if ischar(Args.Columns) || (isstring(Args.Columns) && isscalar(Args.Columns))
        Cols = {char(Args.Columns)};
    elseif iscell(Args.Columns) || isstring(Args.Columns)
        Cols = cellfun(@char, cellstr(Args.Columns), 'UniformOutput', false);
    else
        error('plotPhotCatHist:BadColumns', ...
            'Columns must be a char, string, or cell of names.');
    end
    Ncols = numel(Cols);

    % --- Discover visit dirs --------------------------------------------
    if Args.Recursive
        Listing = dir(fullfile(char(DataPath), '**', Args.VisitPattern));
    else
        Listing = dir(fullfile(char(DataPath), Args.VisitPattern));
    end
    Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));
    if isempty(Listing)
        ScopeStr = char(DataPath);
        if Args.Recursive; ScopeStr = [ScopeStr ' (recursive)']; end
        error('plotPhotCatHist:NoVisits', ...
            'No subdirectories matching "%s" found under %s', ...
            Args.VisitPattern, ScopeStr);
    end
    Nvisits = numel(Listing);
    VisitDirs = strings(1, Nvisits);
    for I = 1:Nvisits
        VisitDirs(I) = string(fullfile(Listing(I).folder, Listing(I).name));
    end
    if Args.Verbose
        fprintf('plotPhotCatHist: %d visit dirs, FileType=%s, Cols={%s}\n', ...
            Nvisits, Args.FileType, strjoin(Cols, ','));
    end

    % --- Load catalogs --------------------------------------------------
    AIc = pipeline.last.load.loadVisitCatHdr( ...
        'VisitDirs', VisitDirs, ...
        'FileType',  Args.FileType, ...
        'FieldId',   Args.FieldId, ...
        'CropID',    Args.CropID, ...
        'Verbose',   false);

    % --- Pool column values across visits and AIs -----------------------
    Vals  = repmat({zeros(0,1)}, 1, Ncols);
    NImg  = 0;
    for Iv = 1:numel(AIc)
        AIvis = AIc{Iv};
        if isempty(AIvis); continue; end
        if isempty(Args.CropsToAnalyze)
            CropIdx = 1:numel(AIvis);
        else
            CropIdx = Args.CropsToAnalyze(Args.CropsToAnalyze <= numel(AIvis));
        end
        for Ic = CropIdx
            AI = AIvis(Ic);
            if isempty(AI.CatData) || isempty(AI.CatData.Catalog); continue; end
            NImg = NImg + 1;

            % Row mask
            if isempty(Args.RowFilter)
                Mask = true(size(AI.CatData.Catalog, 1), 1);
            else
                try
                    Mask = logical(Args.RowFilter(AI));
                    Mask = Mask(:);
                catch ME
                    warning('plotPhotCatHist:RowFilterFailed', ...
                        'RowFilter threw on visit %d crop %d (%s); skipping.', ...
                        Iv, Ic, ME.message);
                    continue;
                end
            end

            % Optional subsampling
            KeepIdx = find(Mask);
            if numel(KeepIdx) > Args.MaxRowsPerImage
                Pick = randperm(numel(KeepIdx), Args.MaxRowsPerImage);
                KeepIdx = KeepIdx(Pick);
            end
            if isempty(KeepIdx); continue; end

            for Icol = 1:Ncols
                try
                    V = AI.CatData.getCol(Cols{Icol});
                catch
                    continue;   % column missing in this catalog
                end
                if isempty(V); continue; end
                V = V(KeepIdx);
                Vals{Icol} = [Vals{Icol}; V(:)];
            end
        end
    end

    % --- Drop NaNs (and non-positives if LogX) --------------------------
    for Icol = 1:Ncols
        V = Vals{Icol};
        Keep = isfinite(V);
        if Args.LogX; Keep = Keep & V > 0; end
        Vals{Icol} = V(Keep);
    end

    Empty = cellfun(@isempty, Vals);
    if all(Empty)
        error('plotPhotCatHist:Empty', ...
            'No finite values collected for any of: %s', strjoin(Cols, ', '));
    end
    if any(Empty)
        warning('plotPhotCatHist:EmptyColumns', ...
            'No finite values for: %s', strjoin(Cols(Empty), ', '));
    end

    % --- Per-column stats -----------------------------------------------
    Stats = repmat(struct('N',0,'Median',NaN,'Mean',NaN,'Std',NaN, ...
                          'P16',NaN,'P84',NaN,'Min',NaN,'Max',NaN), 1, Ncols);
    for Icol = 1:Ncols
        V = Vals{Icol};
        Stats(Icol).N = numel(V);
        if ~isempty(V)
            Stats(Icol).Median = median(V);
            Stats(Icol).Mean   = mean(V);
            Stats(Icol).Std    = std(V);
            Stats(Icol).P16    = prctile(V, 16);
            Stats(Icol).P84    = prctile(V, 84);
            Stats(Icol).Min    = min(V);
            Stats(Icol).Max    = max(V);
        end
    end

    if Args.Verbose
        for Icol = 1:Ncols
            fprintf('  %-20s : N = %d  med = %.4g  [P16 P84] = [%.4g %.4g]\n', ...
                Cols{Icol}, Stats(Icol).N, ...
                Stats(Icol).Median, Stats(Icol).P16, Stats(Icol).P84);
        end
    end

    % --- Plot ------------------------------------------------------------
    figure; hold on;
    Cmap = lines(max(Ncols, 1));

    % Common edge grid (so overlaid histograms share bins)
    AllVals = vertcat(Vals{:});
    if Args.LogX
        AllPlot = log10(AllVals);
    else
        AllPlot = AllVals;
    end
    if isscalar(Args.Bins)
        if isempty(Args.XLim)
            EdgeLo = min(AllPlot); EdgeHi = max(AllPlot);
        else
            EL = Args.XLim;
            if Args.LogX; EL = log10(EL); end
            EdgeLo = EL(1); EdgeHi = EL(2);
        end
        Edges = linspace(EdgeLo, EdgeHi, Args.Bins + 1);
    else
        Edges = Args.Bins;
        if Args.LogX; Edges = log10(Edges); end
    end

    Hbar = gobjects(Ncols, 1);
    for Icol = 1:Ncols
        V = Vals{Icol};
        if isempty(V); continue; end
        if Args.LogX; Vplot = log10(V); else; Vplot = V; end
        if Ncols == 1
            FaceCol = Args.Color;
            FaceAlpha = 1.0;
            EdgeCol = 'k';
        else
            FaceCol = Cmap(Icol,:);
            FaceAlpha = 0.45;
            EdgeCol = Cmap(Icol,:) * 0.6;
        end
        Hbar(Icol) = histogram(Vplot, Edges, ...
            'FaceColor', FaceCol, 'FaceAlpha', FaceAlpha, ...
            'EdgeColor', EdgeCol, ...
            'DisplayName', sprintf('%s (N=%d)', Cols{Icol}, Stats(Icol).N));
    end

    if Ncols == 1
        XLab = Cols{1};
        if Args.LogX; XLab = sprintf('log10(%s)', XLab); end
    else
        XLab = 'value';
        if Args.LogX; XLab = 'log10(value)'; end
    end
    xlabel(XLab, 'Interpreter', 'none');
    ylabel('Count'); grid on; box on;
    if ~isempty(Args.XLim); xlim(Args.XLim); end

    if Ncols == 1
        title(sprintf('%s  (N = %d, %d images, %d visits)', ...
            Cols{1}, Stats(1).N, NImg, Nvisits), 'Interpreter', 'none');
    else
        title(sprintf('Catalog columns over %d images (%d visits)', ...
            NImg, Nvisits), 'Interpreter', 'none');
        legend(Hbar(arrayfun(@(h) isgraphics(h), Hbar)), ...
            'Location', 'best', 'Interpreter', 'none');
    end

    % --- Stats overlay (single-column only) ------------------------------
    if Args.Stats && Ncols == 1 && Stats(1).N > 0
        S = Stats(1);
        YLim = ylim;
        if Args.LogX
            MedX = log10(S.Median);
            P16X = log10(S.P16);
            P84X = log10(S.P84);
        else
            MedX = S.Median; P16X = S.P16; P84X = S.P84;
        end
        plot([MedX MedX], YLim, 'k-',  'LineWidth', 1.4);
        plot([P16X P16X], YLim, 'k--', 'LineWidth', 0.8);
        plot([P84X P84X], YLim, 'k--', 'LineWidth', 0.8);
        Txt = sprintf( ...
            'N = %d\nMed = %.4g\n[P16, P84] = [%.4g, %.4g]\nMean = %.4g\nStd = %.4g', ...
            S.N, S.Median, S.P16, S.P84, S.Mean, S.Std);
        XL = xlim;
        text(XL(1) + 0.65*(XL(2)-XL(1)), YLim(1) + 0.92*(YLim(2)-YLim(1)), ...
            Txt, 'VerticalAlignment','top', 'HorizontalAlignment','left', ...
            'BackgroundColor', [1 1 1 0.85], 'EdgeColor','k', 'FontSize', 9, ...
            'Interpreter', 'none');
    end
    hold off;

    % --- Assemble output -------------------------------------------------
    Result.Columns      = Cols;
    Result.Values       = Vals;
    Result.Stats        = Stats;
    Result.DataPath     = char(DataPath);
    Result.VisitPattern = Args.VisitPattern;
    Result.Recursive    = Args.Recursive;
    Result.FileType     = Args.FileType;
    Result.NVisits      = Nvisits;
    Result.NImages      = NImg;
end
