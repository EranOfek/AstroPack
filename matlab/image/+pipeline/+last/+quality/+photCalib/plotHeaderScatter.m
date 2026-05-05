function Result = plotHeaderScatter(DataPath, Args)
    % 2D scatter of two FITS header keywords pooled over many visit dirs.
    % Description: Discovers visit subdirectories under DataPath (one level
    %              or recursively), loads each visit's AstroImages with
    %              loadVisitCatHdr (HeaderData populated), pulls the two
    %              requested header keywords via extractHeaderData, then
    %              plots one (visit, crop) point per AstroImage. Reports
    %              Pearson correlation. No calibration round-trip needed.
    %
    %              Counterpart of plotHeaderHist (1D histograms). Use this
    %              for any pair of header-only quantities, e.g.
    %                  ('PT_DOF', 'PT_CHI2')      - fit-quality scatter
    %                  ('AIRMASS', 'PT_ARMS')     - atm-vs-asymptotic-RMS
    %                  ('FWHM', 'PH_RMS')         - seeing-vs-rel-photometry
    %                  ('BACKMAG', 'LIMMAG')      - background-vs-depth
    %
    % Input  : - DataPath char/string. Parent directory containing one or
    %            more visit subdirectories.
    %          * ...,key,val,...
    %            'XKey'         - Header keyword for x-axis. Required.
    %            'YKey'         - Header keyword for y-axis. Required.
    %            'VisitPattern' - Glob for visit subdirectory names.
    %                             Default '*v[0-9]*'. Set to '' (empty) to
    %                             skip discovery entirely and treat
    %                             DataPath itself as a single visit dir.
    %            'Recursive'    - Search at any depth under DataPath.
    %                             Default true.
    %            'FileType'     - 'coadd' or 'proc'. Default 'coadd'.
    %            'Ncrop'        - Number of crops per visit. Default 24.
    %            'CropsToAnalyze' - Crop indices to include. Default []
    %                             (all 1..Ncrop).
    %            'LogX','LogY'  - Log-transformed axis. Default false.
    %            'XLim','YLim'  - Axis limits [min max]. Default [] (auto).
    %            'ColorByCrop'  - One color per crop ID. Default false.
    %            'CentralEdge'  - Central crops as filled dots, peripheral
    %                             as open circles. Default false.
    %            'TileOrder'    - For CentralEdge: 'colmajor'|'rowmajor'.
    %                             Default 'rowmajor'.
    %            'Color'        - Marker color (single-color mode).
    %                             Default [0.30 0.55 0.90].
    %            'MarkerSize'   - Marker size. Default 6.
    %            'ShowTitle'    - Show the auto-generated title (X vs Y, N,
    %                             Pearson R). Default true.
    %            'Title'        - Override the auto title with a custom
    %                             string. Empty = use auto. Ignored when
    %                             ShowTitle=false. Default ''.
    %            'Chi2Quantiles' - Vector of quantiles (each in (0,1)) at
    %                             which to overlay chi^2(q, DOF) reference
    %                             curves. Empty = no curves. Use
    %                             [0.16 0.5 0.84] for median +/- 1-sigma
    %                             bands. Meaningful only when one axis is
    %                             CHI2 and the other is DOF: whichever of
    %                             XKey/YKey contains 'DOF' (case-insens.)
    %                             is taken as the DOF axis, and the curves
    %                             are oriented accordingly. Skipped with a
    %                             warning if neither key matches.
    %                             Default [].
    %            'Chi2DofMin'   - Lower bound on the DOF range used to draw
    %                             the chi^2 reference curves. Curves only
    %                             extend over DOF >= this value; data
    %                             points below it are still scattered.
    %                             Default 30 (chi^2 distribution is well
    %                             approximated by its asymptotics there).
    %            'Verbose'      - Print summary. Default false.
    % Output : - Result struct with fields:
    %            .X, .Y       - column vectors of pooled values
    %                           (NaNs dropped jointly; LogX/LogY applied
    %                           non-positives are dropped before plotting).
    %            .CropID      - 1xN crop index per point.
    %            .VisitID     - 1xN visit index per point.
    %            .Pearson     - struct .R (correlation coefficient),
    %                           .RCentral, .RPeripheral (when CentralEdge).
    %            .HeaderData  - struct returned by extractHeaderData.
    %            .DataPath, .VisitPattern, .Recursive, .FileType - echoed.
    %            .NVisits     - number of visit dirs found.
    % Author : D. Kovaleva (Apr 2026)
    % Example: % PT_CHI2 vs PT_DOF over a date tree:
    %          DP = '/archimedes/LASTunitTest/2025/04/26';
    %          R  = pipeline.last.quality.photCalib.plotHeaderScatter(DP, ...
    %                  'XKey', 'PT_DOF', 'YKey', 'PT_CHI2');
    %
    %          % Whole year, only v0 visits, color central vs peripheral:
    %          R  = pipeline.last.quality.photCalib.plotHeaderScatter( ...
    %                  '/archimedes/LASTunitTest/2025', ...
    %                  'XKey', 'PT_DOF', 'YKey', 'PT_CHI2', ...
    %                  'VisitPattern', '*v1', 'CentralEdge', true);
    %
    %
    %          DataPath = '/archimedes/LASTunitTest/2025';
    %          pipeline.last.quality.photCalib.plotHeaderScatter(DataPath, 'XKey', 'PT_DOF', 'YKey', 'PT_CHI2', 'CentralEdge', true, 'visitPattern', '*v1');
    %          pipeline.last.quality.photCalib.plotHeaderScatter(DataPath, 'XKey', 'PT_DOF', 'YKey', 'PT_CHI2', 'CentralEdge', true, 'visitPattern', '*v1', 'LogY', true);
    %

    arguments
        DataPath                          {mustBeText}
        Args.XKey            (1,:) char
        Args.YKey            (1,:) char
        Args.VisitPattern    {mustBeText}              = '*v[0-9]*'
        Args.Recursive       logical                    = true
        Args.FileType        {mustBeMember(Args.FileType, {'coadd','proc'})} = 'coadd'
        Args.Ncrop                                       = 24
        Args.CropsToAnalyze                              = []
        Args.LogX            logical                    = false
        Args.LogY            logical                    = false
        Args.XLim                                        = []
        Args.YLim                                        = []
        Args.ColorByCrop     logical                    = false
        Args.CentralEdge     logical                    = false
        Args.TileOrder       (1,:) char                 = 'rowmajor'
        Args.Color                                       = [0.30 0.55 0.90]
        Args.MarkerSize                                  = 6
        Args.ShowTitle       logical                    = false
        Args.Title           (1,:) char                 = ''
        Args.Chi2Quantiles                              = []
        Args.Chi2DofMin                                  = 1
        Args.Verbose         logical                    = false
    end

    % --- Discover visit dirs --------------------------------------------
    if isempty(Args.VisitPattern)
        % Single-visit mode: DataPath IS the visit dir.
        if ~isfolder(char(DataPath))
            error('plotHeaderScatter:NoVisits', ...
                'DataPath %s is not a directory.', char(DataPath));
        end
        VisitDirs = string(DataPath);
        Nvisits   = 1;
    else
        if Args.Recursive
            Listing = dir(fullfile(char(DataPath), '**', Args.VisitPattern));
        else
            Listing = dir(fullfile(char(DataPath), Args.VisitPattern));
        end
        Listing = Listing([Listing.isdir] & ~startsWith({Listing.name}, '.'));
        if isempty(Listing)
            ScopeStr = char(DataPath);
            if Args.Recursive; ScopeStr = [ScopeStr ' (recursive)']; end
            error('plotHeaderScatter:NoVisits', ...
                'No subdirectories matching "%s" found under %s', ...
                Args.VisitPattern, ScopeStr);
        end
        Nvisits = numel(Listing);
        VisitDirs = strings(1, Nvisits);
        for I = 1:Nvisits
            VisitDirs(I) = string(fullfile(Listing(I).folder, Listing(I).name));
        end
    end
    if Args.Verbose
        fprintf('plotHeaderScatter: %d visit dirs, FileType=%s, %s vs %s\n', ...
            Nvisits, Args.FileType, Args.XKey, Args.YKey);
    end

    % --- Load headers ---------------------------------------------------
    AIc = pipeline.last.load.loadVisitCatHdr( ...
        'VisitDirs', VisitDirs, ...
        'FileType',  Args.FileType, ...
        'Verbose',   false);

    Keys = {Args.XKey, Args.YKey};
    HD = pipeline.last.load.extractHeaderData(AIc, ...
        'HeaderKeys', Keys, ...
        'Ncrop',      Args.Ncrop, ...
        'Verbose',    false);

    XField = matlab.lang.makeValidName(Args.XKey);
    YField = matlab.lang.makeValidName(Args.YKey);
    Xmat = HD.(XField);   % [Nvisits x Ncrop]
    Ymat = HD.(YField);

    % --- Crop selection -------------------------------------------------
    if isempty(Args.CropsToAnalyze)
        CropIdx = 1:Args.Ncrop;
    else
        CropIdx = Args.CropsToAnalyze;
        CropIdx = CropIdx(CropIdx >= 1 & CropIdx <= Args.Ncrop);
    end
    Xsub = Xmat(:, CropIdx);
    Ysub = Ymat(:, CropIdx);

    % --- Build (visit, crop) labels for each cell -----------------------
    [VisGrid, CropGrid] = ndgrid(1:Nvisits, CropIdx);
    Xv  = Xsub(:);  Yv  = Ysub(:);
    Cid = CropGrid(:);  Vid = VisGrid(:);

    % --- NaN/non-positive filter ----------------------------------------
    Keep = isfinite(Xv) & isfinite(Yv);
    if Args.LogX; Keep = Keep & Xv > 0; end
    if Args.LogY; Keep = Keep & Yv > 0; end
    if ~any(Keep)
        error('plotHeaderScatter:Empty', ...
            'No finite (%s, %s) pairs collected.', Args.XKey, Args.YKey);
    end
    Xv  = Xv(Keep);   Yv  = Yv(Keep);
    Cid = Cid(Keep);  Vid = Vid(Keep);

    % --- Central-vs-peripheral classification ---------------------------
    switch lower(Args.TileOrder)
        case 'colmajor', CentralCrops = [8 9 10 11 14 15 16 17];
        case 'rowmajor', CentralCrops = [6 7 10 11 14 15 18 19];
        otherwise,        CentralCrops = [];
    end
    IsCentral = ismember(Cid, CentralCrops);

    % --- Pearson correlations -------------------------------------------
    Pearson.R = corr(Xv, Yv);
    if Args.CentralEdge && any(IsCentral) && any(~IsCentral)
        Pearson.RCentral    = corr(Xv(IsCentral),  Yv(IsCentral));
        Pearson.RPeripheral = corr(Xv(~IsCentral), Yv(~IsCentral));
    else
        Pearson.RCentral    = NaN;
        Pearson.RPeripheral = NaN;
    end

    if Args.Verbose
        fprintf('  N points       = %d (visits=%d, crops=%d)\n', ...
            numel(Xv), Nvisits, numel(CropIdx));
        fprintf('  median %s = %.4g\n', Args.XKey, median(Xv));
        fprintf('  median %s = %.4g\n', Args.YKey, median(Yv));
        fprintf('  Pearson R      = %+.3f\n', Pearson.R);
        if Args.CentralEdge
            fprintf('  R central      = %+.3f\n', Pearson.RCentral);
            fprintf('  R peripheral   = %+.3f\n', Pearson.RPeripheral);
        end
    end

    % --- Plot -----------------------------------------------------------
    figure; hold on;
    if Args.ColorByCrop
        Ncuse = numel(CropIdx);
        Cmap = lines(Ncuse);
        for Iic = 1:Ncuse
            Sel = Cid == CropIdx(Iic);
            if ~any(Sel); continue; end
            plot(Xv(Sel), Yv(Sel), 'o', ...
                'MarkerFaceColor', Cmap(Iic,:), ...
                'MarkerEdgeColor', Cmap(Iic,:), ...
                'MarkerSize', Args.MarkerSize, ...
                'DisplayName', sprintf('crop %d', CropIdx(Iic)));
        end
        legend('Location', 'eastoutside', 'NumColumns', 2, 'Interpreter', 'none');
    elseif Args.CentralEdge
        plot(Xv(IsCentral),  Yv(IsCentral), 'o', ...
            'MarkerFaceColor', Args.Color, ...
            'MarkerEdgeColor', Args.Color, ...
            'MarkerSize', Args.MarkerSize, ...
            'DisplayName', 'central');
        plot(Xv(~IsCentral), Yv(~IsCentral), 'o', ...
            'MarkerFaceColor', 'none', ...
            'MarkerEdgeColor', Args.Color, ...
            'LineWidth', 1.0, ...
            'MarkerSize', Args.MarkerSize, ...
            'DisplayName', 'peripheral');
        legend('Location', 'best', 'Interpreter', 'none');
    else
        plot(Xv, Yv, 'o', ...
            'MarkerFaceColor', Args.Color, ...
            'MarkerEdgeColor', Args.Color, ...
            'MarkerSize', Args.MarkerSize);
    end

    % --- Optional chi^2(q, DOF) reference curves ------------------------
    %   Meaningful only when one axis is CHI2 and the other is DOF.
    %   Auto-detect: whichever of XKey/YKey contains 'DOF' (case-insens.)
    %   is taken as the DOF axis; iterate over that axis and compute the
    %   chi^2 quantile on the opposite axis. Skip with a warning if
    %   neither key looks like a DOF.
    if ~isempty(Args.Chi2Quantiles)
        Q = Args.Chi2Quantiles(:).';
        XIsDof = contains(Args.XKey, 'DOF', 'IgnoreCase', true);
        YIsDof = contains(Args.YKey, 'DOF', 'IgnoreCase', true);
        if any(Q <= 0 | Q >= 1)
            warning('plotHeaderScatter:BadChi2Quantiles', ...
                'Chi2Quantiles must lie strictly in (0,1); ignoring.');
        elseif ~XIsDof && ~YIsDof
            warning('plotHeaderScatter:NoDofAxis', ...
                ['Chi2Quantiles requires one axis to be a DOF (key name ', ...
                 'containing ''DOF''). XKey=%s, YKey=%s; skipping curves.'], ...
                Args.XKey, Args.YKey);
        else
            % Iterate over the DOF axis; chi2inv requires positive DOF.
            % Restrict the curve to DOF >= Chi2DofMin (default 30, where
            % the chi^2 distribution is well-behaved).
            if XIsDof
                Dof = Xv;
            else
                Dof = Yv;
            end
            DofFloor = max(Args.Chi2DofMin, 1);
            DofKept  = Dof(Dof >= DofFloor);
            if isempty(DofKept)
                DofMin = NaN; DofMax = NaN;
            else
                DofMin = min(DofKept);
                DofMax = max(DofKept);
            end
            if isfinite(DofMin) && DofMax > DofMin
                DofGrid = linspace(DofMin, DofMax, 400);
                for Iq = 1:numel(Q)
                    q   = Q(Iq);
                    Chi = chi2inv(q, DofGrid);
                    if abs(q - 0.5) < 1e-9
                        Style = '-'; LW = 2.0;
                    else
                        Style = '--'; LW = 1.0;
                    end
                    if XIsDof
                        plot(DofGrid, Chi, Style, 'Color', [0.85 0.20 0.20], ...
                            'LineWidth', LW, ...
                            'DisplayName', sprintf('chi2inv(%.2f, %s)', q, Args.XKey));
                    else
                        plot(Chi, DofGrid, Style, 'Color', [0.85 0.20 0.20], ...
                            'LineWidth', LW, ...
                            'DisplayName', sprintf('chi2inv(%.2f, %s)', q, Args.YKey));
                    end
                end
            end
        end
    end

    if Args.LogX; set(gca, 'XScale', 'log'); end
    if Args.LogY; set(gca, 'YScale', 'log'); end
    if ~isempty(Args.XLim); xlim(Args.XLim); end
    if ~isempty(Args.YLim); ylim(Args.YLim); end
    grid on; box on;

    xlabel(Args.XKey, 'Interpreter', 'none');
    ylabel(Args.YKey, 'Interpreter', 'none');

    if Args.ShowTitle
        if ~isempty(Args.Title)
            TStr = Args.Title;
        elseif Args.CentralEdge && ~isnan(Pearson.RCentral)
            TStr = sprintf('%s vs %s   N=%d, R=%+.2f (cen=%+.2f, per=%+.2f)', ...
                Args.YKey, Args.XKey, numel(Xv), ...
                Pearson.R, Pearson.RCentral, Pearson.RPeripheral);
        else
            TStr = sprintf('%s vs %s   N=%d, R=%+.2f', ...
                Args.YKey, Args.XKey, numel(Xv), Pearson.R);
        end
        title(TStr, 'Interpreter', 'none');
    end
    hold off;

    % --- Output ---------------------------------------------------------
    Result.X            = Xv;
    Result.Y            = Yv;
    Result.CropID       = Cid;
    Result.VisitID      = Vid;
    Result.Pearson      = Pearson;
    Result.HeaderData   = HD;
    Result.DataPath     = char(DataPath);
    Result.VisitPattern = Args.VisitPattern;
    Result.Recursive    = Args.Recursive;
    Result.FileType     = Args.FileType;
    Result.NVisits      = Nvisits;
    Result.XKey         = Args.XKey;
    Result.YKey         = Args.YKey;
end
