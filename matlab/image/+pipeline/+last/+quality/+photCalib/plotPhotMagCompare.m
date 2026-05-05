function Result = plotPhotMagCompare(AI_A, AI_B, Args)
    % Pairwise scatter of catalog magnitudes from two AstroImage arrays
    % Description: For two AstroImage arrays of equal crop count
    %              (typically 1x24 each — one per crop of two coadds /
    %              versions / pipelines), cross-matches sources between
    %              corresponding crops by sky position via imProc.match.match,
    %              then plots Mag_A vs Mag_B for matched sources. Two
    %              layouts: 'pooled' (all crops in one panel) or 'grid'
    %              (one subplot per crop with its own 1:1 line).
    %
    % Input  : - AI_A AstroImage array (1xNcrop). Catalog must have the
    %            requested 'MagField' column.
    %          - AI_B AstroImage array (1xNcrop). Same requirement.
    %          * ...,key,val,...
    %            'MagField'        - Catalog column to compare.
    %                                Default 'MAG_AB_APER_3'.
    %            'MatchRadius'     - Match radius (arcsec). Default 1.
    %            'MagRange'        - [min max] mag window applied to both
    %                                A and B (after match). Default [] (no
    %                                two-sided cut). Combines with
    %                                MagMin/MagMax via intersection.
    %            'MagMin'          - Lower magnitude cap (inclusive). Both
    %                                Mag_A and Mag_B must be >= MagMin.
    %                                Default -Inf (no floor).
    %            'MagMax'          - Upper magnitude cap (inclusive). Both
    %                                Mag_A and Mag_B must be <= MagMax.
    %                                Default Inf (no ceiling). Use this for
    %                                one-sided cuts like 'MAG_AB_APER_3<23'.
    %                                The plot and all reported statistics
    %                                (PerCrop.MedDiff/StdDiff, DiffStats,
    %                                Pearson R) are computed on the
    %                                filtered set.
    %            'CropsToAnalyze'  - Crop indices to include. Default []
    %                                (all 1..min(numel(AI_A),numel(AI_B))).
    %            'Layout'          - 'pooled' (single panel, all crops)
    %                                or 'grid' (one subplot per crop).
    %                                Applies to Mode='scatter' and 'diff'.
    %                                Default 'pooled'.
    %            'Mode'            - 'scatter' (default): Mag_B vs Mag_A
    %                                with 1:1 line.
    %                                'diff'             : (Mag_B - Mag_A)
    %                                vs Mag_A with zero + median lines.
    %                                'hist'             : pooled histogram
    %                                of (Mag_B - Mag_A) (Layout ignored).
    %            'Bins'            - Number of bins for Mode='hist'.
    %                                Default 60.
    %            'LabelA','LabelB' - Axis labels for the two AIs.
    %                                Defaults 'A' and 'B'.
    %            'ShowOneToOne'    - Draw 1:1 dashed line. Default true.
    %            'MarkerSize'      - Marker size. Default 4.
    %            'Color'           - Marker color. Default [0.30 0.55 0.90].
    %            'ShowTitle'       - Show auto title. Default true.
    %            'Title'           - Override the auto title (pooled only).
    %                                Default ''.
    %            'Verbose'         - Print summary. Default false.
    % Output : - Result struct with fields:
    %            .PerCrop   - struct array (one per analyzed crop):
    %                .CropID, .MagA, .MagB, .RA, .Dec, .X, .Y, .N,
    %                .MedDiff (= median(B-A)), .StdDiff (= std(B-A)).
    %                Positions come from AI_B(Ic).CatData (rows aligned).
    %            .AllA, .AllB, .AllCrop - pooled values across all crops.
    %            .AllDiff   - pooled (B - A).
    %            .AllRA, .AllDec, .AllX, .AllY - per-source positions
    %                          (from AI_B's catalog) for the pooled set.
    %                          NaN where the source's catalog lacks the
    %                          column.
    %            .DiffStats - struct .N .Mean .Median .Std of AllDiff.
    %            .Pearson   - struct .R for the pooled cloud.
    %            .MagField, .LabelA, .LabelB, .Mode - echoed.
    % Author : D. Kovaleva (May 2026)
    % Example: % Compare MAG_AB_APER_3 between two coadds:
    %          R = pipeline.last.quality.photCalib.plotPhotMagCompare( ...
    %                  Coadd_v0, Coadd_v1, ...
    %                  'MagField', 'MAG_AB_APER_3', ...
    %                  'LabelA','v0','LabelB','v1', 'MagRange',[12 18]);
    %
    %          % Per-crop grid layout for closer inspection:
    %          R = pipeline.last.quality.photCalib.plotPhotMagCompare( ...
    %                  Coadd_v0, Coadd_v1, 'Layout','grid');

    arguments
        AI_A   AstroImage
        AI_B   AstroImage
        Args.MagField        (1,:) char         = 'MAG_AB_APER_3'
        Args.MatchRadius                         = 1
        Args.MagRange                            = []
        Args.MagMin                              = -Inf
        Args.MagMax                              = Inf
        Args.CropsToAnalyze                      = []
        Args.Layout          (1,:) char ...
            {mustBeMember(Args.Layout, {'pooled','grid'})} = 'pooled'
        Args.Mode            (1,:) char ...
            {mustBeMember(Args.Mode, {'scatter','diff','hist'})} = 'scatter'
        Args.Bins                                = 60
        Args.LabelA          (1,:) char         = 'A'
        Args.LabelB          (1,:) char         = 'B'
        Args.ShowOneToOne    logical            = true
        Args.MarkerSize                          = 4
        Args.Color                               = [0.30 0.55 0.90]
        Args.ShowTitle       logical            = true
        Args.Title           (1,:) char         = ''
        Args.Verbose         logical            = false
    end

    Ncrop = min(numel(AI_A), numel(AI_B));
    if isempty(Args.CropsToAnalyze)
        CropIdx = 1:Ncrop;
    else
        CropIdx = Args.CropsToAnalyze(:).';
        CropIdx = CropIdx(CropIdx >= 1 & CropIdx <= Ncrop);
    end

    PerCrop = repmat(struct('CropID',0, 'MagA',[], 'MagB',[], 'N',0, ...
        'RA',[], 'Dec',[], 'X',[], 'Y',[], ...
        'MedDiff',NaN, 'StdDiff',NaN), 1, numel(CropIdx));

    AllA = []; AllB = []; AllCrop = [];
    AllRA = []; AllDec = []; AllX = []; AllY = [];

    for K = 1:numel(CropIdx)
        Ic = CropIdx(K);
        PerCrop(K).CropID = Ic;

        % Match AI_A(Ic) sources against AI_B(Ic). Output is a copy of
        % AI_A whose CatData rows are aligned with AI_B(Ic).CatData.
        try
            Mat1 = imProc.match.match(AI_A(Ic), AI_B(Ic), ...
                'Radius',      Args.MatchRadius, ...
                'RadiusUnits', 'arcsec', ...
                'CooType',     'sphere', ...
                'CreateNewObj', true);
        catch ME
            warning('plotPhotMagCompare:MatchFailed', ...
                'Crop %d: match failed (%s); skipping.', Ic, ME.message);
            continue;
        end

        % Pull magnitudes from both, aligned row-for-row
        try
            Ma = Mat1.CatData.getCol(Args.MagField);
            Mb = AI_B(Ic).CatData.getCol(Args.MagField);
        catch
            warning('plotPhotMagCompare:NoColumn', ...
                'Crop %d: %s missing in one of the catalogs; skipping.', ...
                Ic, Args.MagField);
            continue;
        end
        Ma = Ma(:); Mb = Mb(:);

        % Build cut. Filters apply to BOTH A and B values; a source is
        % kept only if both its Mag_A and Mag_B fall inside the window.
        % Stats and plot are then computed on the filtered set.
        Lo = Args.MagMin;
        Hi = Args.MagMax;
        if ~isempty(Args.MagRange)
            Lo = max(Lo, Args.MagRange(1));
            Hi = min(Hi, Args.MagRange(2));
        end
        Keep = isfinite(Ma) & isfinite(Mb) & ...
               Ma >= Lo & Ma <= Hi & Mb >= Lo & Mb <= Hi;

        % Source positions from AI_B (rows align with Mb / Mat1 rows).
        Ra = nan(size(Mb)); De = nan(size(Mb));
        Xc = nan(size(Mb)); Yc = nan(size(Mb));
        try; Ra = AI_B(Ic).CatData.getCol('RA');  Ra = Ra(:); catch; end
        try; De = AI_B(Ic).CatData.getCol('Dec'); De = De(:); catch; end
        for XCand = {'X1','X','XPEAK'}
            try; Xc = AI_B(Ic).CatData.getCol(XCand{1}); Xc = Xc(:); break; catch; end %#ok<NASGU>
        end
        for YCand = {'Y1','Y','YPEAK'}
            try; Yc = AI_B(Ic).CatData.getCol(YCand{1}); Yc = Yc(:); break; catch; end %#ok<NASGU>
        end

        Ma = Ma(Keep); Mb = Mb(Keep);
        Ra = Ra(Keep); De = De(Keep);
        Xc = Xc(Keep); Yc = Yc(Keep);
        if isempty(Ma); continue; end

        PerCrop(K).MagA    = Ma;
        PerCrop(K).MagB    = Mb;
        PerCrop(K).RA      = Ra;
        PerCrop(K).Dec     = De;
        PerCrop(K).X       = Xc;
        PerCrop(K).Y       = Yc;
        PerCrop(K).N       = numel(Ma);
        PerCrop(K).MedDiff = median(Mb - Ma);
        PerCrop(K).StdDiff = std(Mb - Ma);

        AllA    = [AllA;    Ma];                  %#ok<AGROW>
        AllB    = [AllB;    Mb];                  %#ok<AGROW>
        AllRA   = [AllRA;   Ra];                  %#ok<AGROW>
        AllDec  = [AllDec;  De];                  %#ok<AGROW>
        AllX    = [AllX;    Xc];                  %#ok<AGROW>
        AllY    = [AllY;    Yc];                  %#ok<AGROW>
        AllCrop = [AllCrop; repmat(Ic, numel(Ma), 1)]; %#ok<AGROW>
    end

    if isempty(AllA)
        error('plotPhotMagCompare:Empty', ...
            'No matched sources collected for %s.', Args.MagField);
    end

    Pearson.R = corr(AllA, AllB);

    if Args.Verbose
        fprintf('plotPhotMagCompare: %s\n', Args.MagField);
        fprintf('  %d matched sources over %d crops\n', ...
            numel(AllA), numel(unique(AllCrop)));
        fprintf('  Pearson R = %+.3f\n', Pearson.R);
        fprintf('  median (B - A) = %+.4g  std = %.4g\n', ...
            median(AllB - AllA), std(AllB - AllA));
    end

    % --- Plot -----------------------------------------------------------
    AllDiff = AllB - AllA;
    DiffStats.N      = numel(AllDiff);
    DiffStats.Mean   = mean(AllDiff);
    DiffStats.Median = median(AllDiff);
    DiffStats.Std    = std(AllDiff);

    switch lower(Args.Mode)
        case 'scatter'
            renderScatterFig(AllA, AllB, PerCrop, CropIdx, Args, Pearson);

        case 'diff'
            % Layout still applies: pooled diff vs A, or per-crop grid of
            % diff vs A in each subplot.
            if strcmpi(Args.Layout, 'grid')
                Ncols = ceil(sqrt(numel(CropIdx)));
                Nrows = ceil(numel(CropIdx) / Ncols);
                figure('Position', [50 50 280*Ncols 250*Nrows], ...
                    'Name', sprintf('%s diff (%s - %s) per crop', ...
                        Args.MagField, Args.LabelB, Args.LabelA));
                for K = 1:numel(CropIdx)
                    subplot(Nrows, Ncols, K); hold on;
                    P = PerCrop(K);
                    if P.N == 0
                        title(sprintf('crop %d: no matches', CropIdx(K)), ...
                            'Interpreter','none');
                        box on; grid on; continue;
                    end
                    plot(P.MagA, P.MagB - P.MagA, 'o', ...
                        'MarkerFaceColor', Args.Color, ...
                        'MarkerEdgeColor', Args.Color, ...
                        'MarkerSize', Args.MarkerSize);
                    XL = xlim;
                    plot(XL, [0 0], 'k-', 'HandleVisibility','off');
                    plot(XL, [P.MedDiff P.MedDiff], 'r--', 'LineWidth', 1.2);
                    grid on; box on;
                    title(sprintf('crop %d  N=%d  med=%+.3f', ...
                        P.CropID, P.N, P.MedDiff), 'Interpreter','none');
                    xlabel(sprintf('%s [%s]', Args.MagField, Args.LabelA), ...
                        'Interpreter','none');
                    ylabel(sprintf('%s [%s - %s]', Args.MagField, ...
                        Args.LabelB, Args.LabelA), 'Interpreter','none');
                    hold off;
                end
                sgtitle(sprintf('%s diff: %s - %s', Args.MagField, ...
                    Args.LabelB, Args.LabelA), 'Interpreter','none');
            else
                figure('Name', sprintf('%s diff (%s - %s)', ...
                    Args.MagField, Args.LabelB, Args.LabelA));
                hold on;
                plot(AllA, AllDiff, 'o', ...
                    'MarkerFaceColor', Args.Color, ...
                    'MarkerEdgeColor', Args.Color, ...
                    'MarkerSize', Args.MarkerSize);
                XL = xlim;
                plot(XL, [0 0], 'k-', 'HandleVisibility','off');
                plot(XL, [DiffStats.Median DiffStats.Median], 'r--', 'LineWidth', 1.2);
                grid on; box on;
                xlabel(sprintf('%s [%s]', Args.MagField, Args.LabelA), ...
                    'Interpreter','none');
                ylabel(sprintf('%s [%s - %s]', Args.MagField, ...
                    Args.LabelB, Args.LabelA), 'Interpreter','none');
                if Args.ShowTitle
                    if ~isempty(Args.Title)
                        TStr = Args.Title;
                    else
                        TStr = sprintf('%s diff (%s - %s)  N=%d  med=%+.4g  std=%.4g', ...
                            Args.MagField, Args.LabelB, Args.LabelA, ...
                            DiffStats.N, DiffStats.Median, DiffStats.Std);
                    end
                    title(TStr, 'Interpreter','none');
                end
                hold off;
            end

        case 'hist'
            figure('Name', sprintf('%s diff distribution (%s - %s)', ...
                Args.MagField, Args.LabelB, Args.LabelA));
            hold on;
            histogram(AllDiff, Args.Bins, ...
                'FaceColor', Args.Color, 'EdgeColor', 'k');
            grid on; box on;
            YL = ylim;
            plot([DiffStats.Median DiffStats.Median], YL, 'k-', 'LineWidth', 1.4);
            plot([0 0], YL, 'k--', 'LineWidth', 0.8);
            xlabel(sprintf('%s [%s - %s]', Args.MagField, Args.LabelB, Args.LabelA), ...
                'Interpreter','none');
            ylabel('Count');
            if Args.ShowTitle
                if ~isempty(Args.Title)
                    TStr = Args.Title;
                else
                    TStr = sprintf('%s diff distribution  N=%d  med=%+.4g  std=%.4g', ...
                        Args.MagField, DiffStats.N, DiffStats.Median, DiffStats.Std);
                end
                title(TStr, 'Interpreter','none');
            end
            hold off;
    end

    % --- Output ---------------------------------------------------------
    Result.PerCrop   = PerCrop;
    Result.AllA      = AllA;
    Result.AllB      = AllB;
    Result.AllDiff   = AllDiff;
    Result.DiffStats = DiffStats;
    Result.AllCrop   = AllCrop;
    Result.AllRA     = AllRA;
    Result.AllDec    = AllDec;
    Result.AllX      = AllX;
    Result.AllY      = AllY;
    Result.Pearson   = Pearson;
    Result.MagField  = Args.MagField;
    Result.LabelA    = Args.LabelA;
    Result.LabelB    = Args.LabelB;
    Result.Mode      = Args.Mode;
end

% =========================================================================
function renderScatterFig(AllA, AllB, PerCrop, CropIdx, Args, Pearson)
    if strcmpi(Args.Layout, 'grid')
        Ncols = ceil(sqrt(numel(CropIdx)));
        Nrows = ceil(numel(CropIdx) / Ncols);
        figure('Position', [50 50 280*Ncols 250*Nrows], ...
            'Name', sprintf('%s: %s vs %s (per crop)', ...
                Args.MagField, Args.LabelB, Args.LabelA));
        for K = 1:numel(CropIdx)
            subplot(Nrows, Ncols, K); hold on;
            P = PerCrop(K);
            if P.N == 0
                title(sprintf('crop %d: no matches', CropIdx(K)), ...
                    'Interpreter','none');
                box on; grid on; continue;
            end
            plot(P.MagA, P.MagB, 'o', ...
                'MarkerFaceColor', Args.Color, ...
                'MarkerEdgeColor', Args.Color, ...
                'MarkerSize', Args.MarkerSize);
            if Args.ShowOneToOne
                XL = xlim; YL = ylim;
                Lo = min(XL(1), YL(1)); Hi = max(XL(2), YL(2));
                plot([Lo Hi], [Lo Hi], 'k--');
                xlim([Lo Hi]); ylim([Lo Hi]);
            end
            grid on; box on;
            title(sprintf('crop %d  N=%d  med(B-A)=%+.3f', ...
                P.CropID, P.N, P.MedDiff), 'Interpreter','none');
            xlabel(sprintf('%s [%s]', Args.MagField, Args.LabelA), ...
                'Interpreter','none');
            ylabel(sprintf('%s [%s]', Args.MagField, Args.LabelB), ...
                'Interpreter','none');
            hold off;
        end
        sgtitle(sprintf('%s: %s vs %s', Args.MagField, Args.LabelB, Args.LabelA), ...
            'Interpreter','none');
    else
        figure('Name', sprintf('%s: %s vs %s', ...
            Args.MagField, Args.LabelB, Args.LabelA));
        hold on;
        plot(AllA, AllB, 'o', ...
            'MarkerFaceColor', Args.Color, ...
            'MarkerEdgeColor', Args.Color, ...
            'MarkerSize', Args.MarkerSize);
        if Args.ShowOneToOne
            XL = xlim; YL = ylim;
            Lo = min(XL(1), YL(1)); Hi = max(XL(2), YL(2));
            plot([Lo Hi], [Lo Hi], 'k--', 'HandleVisibility','off');
            xlim([Lo Hi]); ylim([Lo Hi]);
        end
        grid on; box on;
        xlabel(sprintf('%s [%s]', Args.MagField, Args.LabelA), 'Interpreter','none');
        ylabel(sprintf('%s [%s]', Args.MagField, Args.LabelB), 'Interpreter','none');
        if Args.ShowTitle
            if ~isempty(Args.Title)
                TStr = Args.Title;
            else
                TStr = sprintf('%s: %s vs %s   N=%d, R=%+.2f', ...
                    Args.MagField, Args.LabelB, Args.LabelA, ...
                    numel(AllA), Pearson.R);
            end
            title(TStr, 'Interpreter','none');
        end
        hold off;
    end
end
