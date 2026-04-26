function Result = plotOutliersScatter(MS, Cats, Args)
    % Select and analyze scatter-plot outliers (bright + high-std sources)
    % Description: From MatchedSources, selects sources whose median
    %   magnitude < MaxMag and epoch-to-epoch std > MinStd (the
    %   bright outliers in the Mag-vs-Std scatter plot). For each
    %   selected source, cross-matches back to per-epoch catalogs
    %   to extract FLAGS and other columns. Plots lightcurves and
    %   returns the subset data for further investigation.
    %
    % Input  : - MS struct with MS.(Mode){crop} = MatchedSources.
    %          - Cats struct with Cats.(Mode){visit}(crop) = AstroCatalog.
    %            Pass [] to skip catalog cross-match.
    %          * ...,key,val,...
    %            'Mode'       - PhotSys mode. Default is 'percrop'.
    %            'MagField'   - Magnitude field in MS.Data. Default is 'MAG_AB_APER_3'.
    %            'MaxMag'     - Maximum median magnitude. Default is 15.
    %            'MinStd'     - Minimum std [mag]. Default is 0.05.
    %            'MinEpochsSummary' - Min non-NaN epochs per source for inclusion
    %                        in Summary table / statistics. Default is 2.
    %            'MinEpochsPlot' - Min non-NaN epochs for a source to be plotted
    %                        as an individual lightcurve. Default is 5.
    %            'CropsToAnalyze' - Crop indices. Default is [] (all).
    %            'MatchRadius'- Cross-match radius [arcsec] for Cats lookup.
    %                        Default is 2.
    %            'PlotLC'     - Plot lightcurves. Default is true.
    %            'PlotStats'  - Plot Nepochs + flag histograms comparing
    %                        outliers vs. the non-outlier reference
    %                        population (sources passing MinEpochsSummary
    %                        but not the MaxMag/MinStd outlier cut).
    %                        Default is true.
    %            'PlotPerEpoch' - Plot per-epoch histogram (sum over outliers
    %                        of flagged vs. valid detections at each epoch).
    %                        Default is false.
    %            'PlotXY'     - Plot source positions per crop (subplot grid):
    %                        non-outlier sources in grey (dot size grows with
    %                        brightness), outliers as small red dots.
    %                        Default is false.
    %            'PlotDS9'    - Overlay outlier positions on images in DS9.
    %                        One frame per crop in CropsToAnalyze. Outliers
    %                        shown as red circles with flag-name text labels.
    %                        Requires 'AI' argument. Default is false.
    %            'AI'         - Images to display under the outlier overlay
    %                        when PlotDS9 is true. One of:
    %                          - AstroImage array indexed by crop: AI(Ic)
    %                          - cell array: AI{Ic} = AstroImage
    %                          - struct with AI.(Mode){Iv}(Ic) (e.g. R.AI)
    %                        Default is [].
    %            'DS9Epoch'   - Epoch selection for DS9 display.
    %                          - scalar (default 1): single epoch per crop.
    %                          - vector: with a single crop in DS9Crops,
    %                            show one ds9 frame per epoch in the vector,
    %                            each overlaid with that epoch's detected
    %                            outlier positions (no median).
    %                          - [] with single crop in DS9Crops: all epochs.
    %                        Default is 1.
    %            'DS9Crops'   - Subset of crop indices to display in DS9
    %                        (outlier detection still runs on CropsToAnalyze).
    %                        Default is [] (use CropsToAnalyze).
    %            'DS9Clear'   - Delete all existing ds9 frames before plotting.
    %                        Default is true.
    %            'MaxPlotSrc' - Max sources to plot individually. Default is 20.
    %            'Verbose'    - Print summary. Default is true.
    % Output : - Result struct with:
    %            .Outliers{crop} — struct per crop with:
    %              .SrcIdx    — source indices in MS
    %              .MedMag    — median magnitudes [1 × Nsel]
    %              .StdMag    — std magnitudes [1 × Nsel]
    %              .MSsub     — MatchedSources subset (selected sources)
    %              .CatRows{visit} — table rows from Cats for selected sources
    %            .Summary    — aggregate table of all outliers
    % Author : D. Kovaleva (Apr 2026)
    % Example: R = pipeline.last.quality.photCalib.plotOutliersScatter(R.MS, R.Cats);
    %          R = pipeline.last.quality.photCalib.plotOutliersScatter(R.MS, R.Cats, ...
    %              'MaxMag', 14, 'MinStd', 0.1, 'MagField', 'MAG_AB_PSF');
    %          R = pipeline.last.quality.photCalib.plotOutliersScatter(R.MS, [], ...
    %              'MaxMag', 15, 'MinStd', 0.05);  % MS only, no Cats

    arguments
        MS struct
        Cats = []
        Args.Mode           = 'percrop'
        Args.MagField       = 'MAG_AB_APER_3'
        Args.MaxMag         = 15
        Args.MinStd         = 0.05
        Args.MinEpochsSummary = 2
        Args.MinEpochsPlot    = 5
        Args.CropsToAnalyze = []
        Args.MatchRadius    = 2       % arcsec
        Args.FilterFlags cell = {'Saturated', 'NearEdge', 'NaN'}  % NaN out epochs with these flags
        Args.BackgroundMag  = 22   % Mag fainter than this treated as bad epoch
        Args.PlotLC logical = true
        Args.PlotStats logical = true
        Args.PlotPerEpoch logical = false
        Args.PlotXY logical = false
        Args.PlotDS9 logical = false
        Args.AI              = []
        Args.DS9Epoch        = 1
        Args.DS9Crops        = []
        Args.DS9Clear logical = false
        Args.MaxPlotSrc     = 20
        Args.Verbose logical = true
    end

    Mode = Args.Mode;
    if ~isfield(MS, Mode)
        warning('plotOutliersScatter:NoMode', 'Mode %s not found in MS.', Mode);
        Result = struct();
        return;
    end

    CropsToUse = Args.CropsToAnalyze;
    if isempty(CropsToUse)
        CropsToUse = 1:numel(MS.(Mode));
    end

    MatchRadiusRad = Args.MatchRadius / 206264.806;
    HasCats = ~isempty(Cats) && isfield(Cats, Mode);

    Result.Outliers = cell(1, max(CropsToUse));
    AllMed = []; AllStd = []; AllCrop = []; AllNep = [];
    AllFlagsOR = []; AllFlagNames = {};
    AllFlagNamesRest = {};   % non-outlier sources (Nvalid >= MinEpochsSummary)
    AllNepRest = [];

    for Iic = 1:numel(CropsToUse)
        Ic = CropsToUse(Iic);
        if Ic > numel(MS.(Mode)) || isempty(MS.(Mode){Ic})
            Result.Outliers{Ic} = struct();
            if Args.Verbose
                fprintf('Crop %02d: skipped (empty)\n', Ic);
            end
        else
            MSobj = MS.(Mode){Ic};
            if ~isfield(MSobj.Data, Args.MagField)
                Result.Outliers{Ic} = struct();
                if Args.Verbose
                    fprintf('Crop %02d: skipped (%s not found)\n', Ic, Args.MagField);
                end
            else
                MagMat = MSobj.Data.(Args.MagField);  % [Nepochs × Nsrc]

                % NaN out epochs with bad flags (per source)
                if ~isempty(Args.FilterFlags) && isfield(MSobj.Data, 'FLAGS')
                    FlagMat = MSobj.Data.FLAGS;
                    FlagMat(isnan(FlagMat)) = 0;
                    try
                        BD = BitDictionary;
                        BadEpochMask = false(size(FlagMat));
                        for Ifl = 1:numel(Args.FilterFlags)
                            [~, ~, BitDec] = BD.name2bit(Args.FilterFlags{Ifl});
                            BadEpochMask = BadEpochMask | (bitand(uint32(FlagMat), uint32(BitDec)) > 0);
                        end
                        MagMat(BadEpochMask) = NaN;
                    catch
                    end
                end

                % NaN out epochs fainter than background
                if isfinite(Args.BackgroundMag)
                    MagMat(MagMat > Args.BackgroundMag) = NaN;
                end

                % Filter by MinEpochsSummary (after flag + background filtering)
                Nvalid = sum(~isnan(MagMat), 1);
                EpochMask = Nvalid >= Args.MinEpochsSummary;

                MedMag = median(MagMat, 1, 'omitnan');
                StdMag = std(MagMat, 0, 1, 'omitnan');

                % Select outliers
                SelMask = EpochMask & MedMag < Args.MaxMag & StdMag > Args.MinStd;
                SrcIdx = find(SelMask);

                if Args.Verbose
                    fprintf('Crop %02d: %d outliers (MedMag<%.1f, Std>%.3f)\n', ...
                        Ic, numel(SrcIdx), Args.MaxMag, Args.MinStd);
                end

                Out = struct();
                Out.SrcIdx = SrcIdx;
                Out.MagFiltered = MagMat(:, SrcIdx);  % flag-filtered lightcurves
                Out.MedMag = MedMag(SrcIdx);
                Out.StdMag = StdMag(SrcIdx);
                Out.Nepochs = Nvalid(SrcIdx);

                % Per-crop source positions (median over epochs) for PlotXY.
                % LAST MS stores centroids as X1/Y1; fall back to X/Y for
                % other conventions.
                XField = ''; YField = '';
                if     isfield(MSobj.Data, 'X1') && isfield(MSobj.Data, 'Y1'); XField = 'X1'; YField = 'Y1';
                elseif isfield(MSobj.Data, 'X')  && isfield(MSobj.Data, 'Y');  XField = 'X';  YField = 'Y';
                end
                if ~isempty(XField)
                    Out.XYAllX     = median(MSobj.Data.(XField), 1, 'omitnan');
                    Out.XYAllY     = median(MSobj.Data.(YField), 1, 'omitnan');
                    Out.XYAllMed   = MedMag;
                    Out.XYSelMask  = SelMask;
                end

                % Extract MS subset
                if ~isempty(SrcIdx)
                    Out.MSsub = MSobj.selectBySrcIndex(SrcIdx);
                else
                    Out.MSsub = [];
                end

                % Decode FLAGS: bitwise OR across epochs per source
                Out.FlagsOR = zeros(1, numel(SrcIdx));
                Out.FlagNames = cell(1, numel(SrcIdx));
                if isfield(MSobj.Data, 'FLAGS') && ~isempty(SrcIdx)
                    FlagMat = MSobj.Data.FLAGS(:, SrcIdx);  % [Nepochs × Nsel]
                    FlagMat(isnan(FlagMat)) = 0;
                    for Is = 1:numel(SrcIdx)
                        FlagVec = uint32(FlagMat(:, Is));
                        Out.FlagsOR(Is) = double(bitor_reduce(FlagVec));
                    end
                    try
                        BD = BitDictionary;
                        [BN, ~, ~] = BD.bitdec2name(Out.FlagsOR);
                        for Is = 1:numel(SrcIdx)
                            Out.FlagNames{Is} = strjoin(BN{Is}, ',');
                        end
                    catch
                        for Is = 1:numel(SrcIdx)
                            Out.FlagNames{Is} = sprintf('%d', Out.FlagsOR(Is));
                        end
                    end
                end

                % Cross-match to Cats
                Out.CatRows = {};
                if HasCats && ~isempty(SrcIdx)
                    Nvisits = numel(Cats.(Mode));

                    % Get RA/Dec of selected sources from MS
                    if isfield(MSobj.Data, 'RA') && isfield(MSobj.Data, 'Dec')
                        RA_sel  = median(MSobj.Data.RA(:, SrcIdx), 1, 'omitnan');
                        Dec_sel = median(MSobj.Data.Dec(:, SrcIdx), 1, 'omitnan');
                    else
                        RA_sel = []; Dec_sel = [];
                    end

                    if ~isempty(RA_sel)
                        Out.CatRows = cell(Nvisits, 1);
                        for Iv = 1:Nvisits
                            if isempty(Cats.(Mode){Iv}) || Ic > numel(Cats.(Mode){Iv})
                                Out.CatRows{Iv} = [];
                            else
                                Cat = Cats.(Mode){Iv}(Ic);
                                Tab = Cat.Table;
                                if isempty(Tab) || ~ismember('RA', Tab.Properties.VariableNames)
                                    Out.CatRows{Iv} = [];
                                else
                                    CatRA  = deg2rad(Tab.RA);
                                    CatDec = deg2rad(Tab.Dec);
                                    MatchIdx = nan(1, numel(SrcIdx));
                                    for Is = 1:numel(SrcIdx)
                                        Dist = celestial.coo.sphere_dist_fast( ...
                                            deg2rad(RA_sel(Is)), deg2rad(Dec_sel(Is)), CatRA, CatDec);
                                        [MinDist, MinI] = min(Dist);
                                        if MinDist < MatchRadiusRad
                                            MatchIdx(Is) = MinI;
                                        end
                                    end
                                    ValidMatch = MatchIdx(isfinite(MatchIdx));
                                    if ~isempty(ValidMatch)
                                        Out.CatRows{Iv} = Tab(ValidMatch, :);
                                    else
                                        Out.CatRows{Iv} = [];
                                    end
                                end
                            end
                        end
                    end
                end

                Result.Outliers{Ic} = Out;

                % Accumulate for summary
                AllMed = [AllMed, Out.MedMag];
                AllStd = [AllStd, Out.StdMag];
                AllNep = [AllNep, Out.Nepochs];
                AllFlagsOR = [AllFlagsOR, Out.FlagsOR];
                AllFlagNames = [AllFlagNames, Out.FlagNames];
                AllCrop = [AllCrop, Ic * ones(1, numel(SrcIdx))];

                % Reference population: non-outliers passing MinEpochsSummary
                RestIdx = find(EpochMask & ~SelMask);
                AllNepRest = [AllNepRest, Nvalid(RestIdx)];
                RestFlagNames = repmat({''}, 1, numel(RestIdx));
                if isfield(MSobj.Data, 'FLAGS') && ~isempty(RestIdx)
                    FMR = MSobj.Data.FLAGS(:, RestIdx);
                    FMR(isnan(FMR)) = 0;
                    RestFlagsOR = zeros(1, numel(RestIdx));
                    for Is = 1:numel(RestIdx)
                        RestFlagsOR(Is) = double(bitor_reduce(uint32(FMR(:, Is))));
                    end
                    try
                        BDr = BitDictionary;
                        [BNr, ~, ~] = BDr.bitdec2name(RestFlagsOR);
                        for Is = 1:numel(RestIdx)
                            RestFlagNames{Is} = strjoin(BNr{Is}, ',');
                        end
                    catch
                        for Is = 1:numel(RestIdx)
                            RestFlagNames{Is} = sprintf('%d', RestFlagsOR(Is));
                        end
                    end
                end
                AllFlagNamesRest = [AllFlagNamesRest, RestFlagNames];
            end
        end
    end

    % Summary table
    if ~isempty(AllMed)
        Result.Summary = table(AllCrop(:), AllMed(:), AllStd(:), AllNep(:), ...
            AllFlagsOR(:), AllFlagNames(:), ...
            'VariableNames', {'CropID', 'MedMag', 'StdMag', 'Nepochs', 'FlagsOR', 'FlagNames'});
        Result.Summary = sortrows(Result.Summary, 'StdMag', 'descend');
    else
        Result.Summary = table();
    end

    if Args.Verbose
        fprintf('\nTotal outliers: %d across %d crops\n', numel(AllMed), numel(CropsToUse));
        if ~isempty(AllMed)
            fprintf('Mag range: %.2f - %.2f\n', min(AllMed), max(AllMed));
            fprintf('Std range: %.3f - %.3f\n', min(AllStd), max(AllStd));
        end
    end

    % Collect per-outlier lightcurves, labels, and crop indices (in Summary order)
    AllLC = {};
    AllLabels = {};
    AllCropLC = [];
    Idx = 0;
    for Iic = 1:numel(CropsToUse)
        Ic = CropsToUse(Iic);
        if isempty(Result.Outliers{Ic}) || ~isfield(Result.Outliers{Ic}, 'MagFiltered')
            continue;
        end
        if isempty(Result.Outliers{Ic}.MagFiltered)
            continue;
        end
        Out = Result.Outliers{Ic};
        MagSub = Out.MagFiltered;
        for Is = 1:size(MagSub, 2)
            Idx = Idx + 1;
            AllLC{Idx} = MagSub(:, Is);
            FN = '';
            if Idx <= numel(AllFlagNames)
                FN = AllFlagNames{Idx};
            end
            AllLabels{Idx} = sprintf('C%02d M=%.1f S=%.3f Nep=%d %s', ...
                Ic, Out.MedMag(Is), Out.StdMag(Is), Out.Nepochs(Is), FN);
            AllCropLC(Idx) = Ic;
        end
    end

    % Plot Nepochs + flag statistics histograms (outliers vs. rest)
    if Args.PlotStats && ~isempty(AllNep)
        figure('Name', 'Outlier statistics', 'Position', [50, 50, 1100, 400]);
        Nout  = numel(AllFlagNames);
        Nrest = numel(AllFlagNamesRest);

        % Nepochs histogram — probability-normalized so N_out vs N_rest is comparable
        subplot(1, 2, 1);
        MaxEp = max([AllNep, AllNepRest]);
        Edges = 0.5:1:(MaxEp + 0.5);
        histogram(AllNep, Edges, 'Normalization', 'probability', ...
            'FaceColor', [0.8 0.4 0.3], 'FaceAlpha', 0.75);
        hold on;
        if ~isempty(AllNepRest)
            histogram(AllNepRest, Edges, 'Normalization', 'probability', ...
                'FaceColor', [0.5 0.5 0.5], 'FaceAlpha', 0.5);
        end
        YL = ylim;
        plot([Args.MinEpochsSummary Args.MinEpochsSummary]-0.5, YL, '--k', 'LineWidth', 1);
        plot([Args.MinEpochsPlot Args.MinEpochsPlot]-0.5, YL, '--r', 'LineWidth', 1);
        text(Args.MinEpochsSummary-0.5, YL(2), ' MinSum', 'VerticalAlignment', 'top');
        text(Args.MinEpochsPlot-0.5, YL(2), ' MinPlot', 'VerticalAlignment', 'top', 'Color', 'r');
        box on; grid on;
        xlabel('N valid epochs');
        ylabel('Fraction of sources');
        title('Nepochs distribution');
        legend({sprintf('Outliers (N=%d)', Nout), sprintf('Rest (N=%d)', Nrest)}, ...
            'Location', 'best');

        % Flag statistics: grouped bar — outlier vs rest fractions
        subplot(1, 2, 2);
        [KeysO, ValsO] = countFlagNames(AllFlagNames);
        [KeysR, ValsR] = countFlagNames(AllFlagNamesRest);
        AllKeys = union(KeysO, KeysR, 'stable');
        CountsO = zeros(1, numel(AllKeys));
        CountsR = zeros(1, numel(AllKeys));
        for Ik = 1:numel(AllKeys)
            io = find(strcmp(KeysO, AllKeys{Ik}), 1);
            ir = find(strcmp(KeysR, AllKeys{Ik}), 1);
            if ~isempty(io); CountsO(Ik) = ValsO(io); end
            if ~isempty(ir); CountsR(Ik) = ValsR(ir); end
        end
        FracO = CountsO / max(Nout, 1);
        FracR = CountsR / max(Nrest, 1);
        [~, Order] = sort(FracO, 'descend');
        AllKeys = AllKeys(Order); FracO = FracO(Order); FracR = FracR(Order);
        CountsO = CountsO(Order); CountsR = CountsR(Order);
        if ~isempty(AllKeys)
            Bh = bar(1:numel(AllKeys), [FracO; FracR]');
            Bh(1).FaceColor = [0.8 0.4 0.3];
            Bh(2).FaceColor = [0.5 0.5 0.5];
            set(gca, 'XTick', 1:numel(AllKeys), 'XTickLabel', AllKeys, ...
                'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');
            ylabel('Fraction of sources');
            title('Flag frequency (bitwise-OR per source)');
            legend({sprintf('Outliers (N=%d)', Nout), sprintf('Rest (N=%d)', Nrest)}, ...
                'Location', 'best');
            box on; grid on;
        end

        Result.NepochsHist     = AllNep;
        Result.NepochsHistRest = AllNepRest;
        Result.FlagCounts      = containers.Map(AllKeys, num2cell(CountsO));
        Result.FlagCountsRest  = containers.Map(AllKeys, num2cell(CountsR));
    end

    % Per-epoch histogram: sum across outliers of valid vs. bad at each epoch
    if Args.PlotPerEpoch && ~isempty(AllLC)
        Nep = max(cellfun(@numel, AllLC));
        ValidPerEp = zeros(1, Nep);
        BadPerEp   = zeros(1, Nep);
        for Is = 1:numel(AllLC)
            LC = AllLC{Is};
            Ne = numel(LC);
            V = isfinite(LC(:))';
            ValidPerEp(1:Ne) = ValidPerEp(1:Ne) + V;
            BadPerEp(1:Ne)   = BadPerEp(1:Ne)   + (~V);
        end

        figure('Name', 'Per-epoch outlier counts', 'Position', [50, 50, 900, 400]);
        Bh = bar(1:Nep, [ValidPerEp; BadPerEp]', 'stacked');
        Bh(1).FaceColor = [0.3 0.6 0.9];
        Bh(2).FaceColor = [0.9 0.4 0.3];
        legend({'Valid', 'Flagged/Bkg'}, 'Location', 'best');
        xlabel('Epoch');
        ylabel('Number of outliers');
        title(sprintf('Per-epoch detection status across %d outliers', numel(AllLC)));
        box on; grid on;

        Result.PerEpoch.Valid = ValidPerEp;
        Result.PerEpoch.Bad   = BadPerEp;
    end

    % Plot source positions per crop: rest = grey (size ∝ flux), outliers = red
    if Args.PlotXY
        HasAnyXY = false;
        for Iic = 1:numel(CropsToUse)
            Ic = CropsToUse(Iic);
            if Ic <= numel(Result.Outliers) && isstruct(Result.Outliers{Ic}) ...
                    && isfield(Result.Outliers{Ic}, 'XYAllX')
                HasAnyXY = true;
                break;
            end
        end
        if HasAnyXY
            Ncrops = numel(CropsToUse);
            Ncols  = ceil(sqrt(Ncrops));
            Nrows  = ceil(Ncrops / Ncols);
            figure('Name', 'Outlier X,Y positions', ...
                'Position', [50, 50, 260*Ncols, 240*Nrows]);
            for Iic = 1:Ncrops
                Ic = CropsToUse(Iic);
                subplot(Nrows, Ncols, Iic);
                if Ic > numel(Result.Outliers) || ~isstruct(Result.Outliers{Ic}) ...
                        || ~isfield(Result.Outliers{Ic}, 'XYAllX')
                    title(sprintf('Crop %02d (no XY)', Ic), 'FontSize', 8);
                    continue;
                end
                Out = Result.Outliers{Ic};
                X      = Out.XYAllX;
                Y      = Out.XYAllY;
                MedAll = Out.XYAllMed;
                SelMsk = Out.XYSelMask;

                RestMsk = ~SelMsk & ~isnan(MedAll);
                % Map magnitude to marker area: brighter = larger.
                % 5th/95th percentile of rest mags gives a robust size range.
                MagRef = MedAll(RestMsk);
                if numel(MagRef) >= 10
                    Lo = prctile(MagRef,  5);
                    Hi = prctile(MagRef, 95);
                else
                    Lo = 10; Hi = 20;
                end
                if Hi <= Lo; Hi = Lo + 1; end
                Sz = 2 + 50 * (Hi - MedAll) / (Hi - Lo);
                Sz = max(1, min(Sz, 80));

                hold on;
                scatter(X(RestMsk), Y(RestMsk), Sz(RestMsk), ...
                    [0.7 0.7 0.7], 'filled', 'MarkerFaceAlpha', 0.6);
                if any(SelMsk)
                    scatter(X(SelMsk), Y(SelMsk), Sz(SelMsk), ...
                        'MarkerEdgeColor', [0.9 0.2 0.2], 'LineWidth', 1);
                    % Label each outlier with its flag names (if any)
                    OutSrcIdx = find(SelMsk);
                    for Is = 1:numel(OutSrcIdx)
                        si = OutSrcIdx(Is);
                        Lbl = '';
                        if isfield(Out, 'FlagNames') && Is <= numel(Out.FlagNames)
                            Lbl = Out.FlagNames{Is};
                        end
                        if ~isempty(Lbl)
                            text(X(si), Y(si), [' ' Lbl], ...
                                'FontSize', 6, 'Color', [0.7 0.1 0.1], ...
                                'Interpreter', 'none', ...
                                'VerticalAlignment', 'middle', ...
                                'HorizontalAlignment', 'left');
                        end
                    end
                end
                axis equal; axis tight;
                box on;
                title(sprintf('Crop %02d  (out=%d / rest=%d)', ...
                    Ic, sum(SelMsk), sum(RestMsk)), 'FontSize', 8);
                set(gca, 'FontSize', 7);
            end
            sgtitle(sprintf('Outliers (red) over sources (grey, dot size \\propto flux) — %s', ...
                strrep(Args.MagField, '_', '\_')), 'FontSize', 10);
        end
    end

    % Overlay outliers on images in DS9 — one frame per crop
    if Args.PlotDS9
        if isempty(Args.AI)
            warning('plotOutliersScatter:NoAI', ...
                'PlotDS9 requires Args.AI (AstroImage per crop).');
        else
            % Ensure xpans (XPA nameserver) is running — some installs do
            % not auto-start it, which makes every xpaget/xpaset fail.
            [~, AccOut] = system('xpaaccess -n xpans 2>/dev/null');
            XpansUp = (str2double(strtrim(AccOut)) >= 1);
            if ~XpansUp
                fprintf('DS9: xpans not running — starting it\n');
                % Try several spawn forms; some MATLAB /bin/sh setups
                % swallow nohup-backgrounded jobs.
                SpawnCmds = {'xpans &', ...
                             'bash -lc ''xpans </dev/null >/dev/null 2>&1 &''', ...
                             'nohup xpans >/dev/null 2>&1 &'};
                for Ic_sp = 1:numel(SpawnCmds)
                    [Sp, ~] = system(SpawnCmds{Ic_sp});
                    fprintf('DS9: spawn ''%s'' status=%d\n', SpawnCmds{Ic_sp}, Sp);
                    for Itry = 1:8   % ~2s per try
                        pause(0.25);
                        [~, AccOut] = system('xpaaccess -n xpans 2>/dev/null');
                        XpansUp = (str2double(strtrim(AccOut)) >= 1);
                        if XpansUp
                            fprintf('DS9: xpans up after spawn %d (%.1fs)\n', ...
                                Ic_sp, Itry*0.25);
                            break;
                        end
                    end
                    if XpansUp; break; end
                end
                if ~XpansUp
                    [~, Pgr] = system('pgrep -af xpans || echo "(no xpans proc)"');
                    fprintf('DS9: xpans did not respond. pgrep xpans:\n%s', Pgr);
                end
            end
            if ~ds9.isopen
                % ds9.open uses a 3s blind wait; we'll poll below regardless
                ds9.open;
            end
            % Pre-flight: poll XPA until ds9 actually answers commands.
            % ds9.isopen can return true before the XPA server is ready,
            % and can also return stale-true if ds9 was closed externally.
            DS9Ready = false;
            for Itry = 1:20   % up to ~10s
                try
                    ds9.system('xpaget ds9 frame frameno');
                    DS9Ready = true;
                    break;
                catch
                    pause(0.5);
                end
            end
            if ~DS9Ready
                fprintf('DS9: ds9 not reachable after ds9.open — diagnosing\n');
                [~, W] = system('which ds9'); fprintf('    which ds9: %s', W);
                [~, P] = system('pgrep -af ds9 || echo "(no ds9 process)"');
                fprintf('    pgrep: %s', P);
                [~, X] = system('xpaget xpans 2>&1 || echo "(xpans not reachable)"');
                fprintf('    xpaget xpans:\n%s', X);
                [~, A] = system('xpaaccess -v ds9 2>&1');
                fprintf('    xpaaccess -v ds9: %s', A);
                fprintf('DS9: attempting fresh launch\n');
                try; system('pkill -f "^ds9$" >/dev/null 2>&1'); catch; end
                pause(0.5);
                try; [St, ~] = system('nohup ds9 >/dev/null 2>&1 &'); ...
                     fprintf('    ds9 launch status=%d\n', St); catch; end
                for Itry = 1:30   % up to ~15s
                    try
                        ds9.system('xpaget ds9 frame frameno');
                        DS9Ready = true;
                        fprintf('DS9: became reachable after %.1fs\n', Itry*0.5);
                        break;
                    catch
                        pause(0.5);
                    end
                end
            end
            if ~DS9Ready
                warning('plotOutliersScatter:DS9Unreachable', ...
                    'ds9 is not responding to XPA. See diagnostics above.');
            end
            if DS9Ready && Args.DS9Clear
                try
                    ds9.delete_frame(Inf);
                catch
                end
            end
            NdispedCrops = 0;
            NskippedNoAI = 0;
            NskippedNoXY = 0;
            DS9CropsToUse = Args.DS9Crops;
            if isempty(DS9CropsToUse)
                DS9CropsToUse = CropsToUse;
            end
            % Build task list [Ic, Iv] per frame
            SingleCrop = numel(DS9CropsToUse) == 1;
            Epochs = Args.DS9Epoch;
            PerEpochMode = SingleCrop && (isempty(Epochs) || numel(Epochs) > 1);
            if PerEpochMode
                Ic1 = DS9CropsToUse(1);
                if isempty(Epochs) && Ic1 <= numel(MS.(Mode)) && ~isempty(MS.(Mode){Ic1}) ...
                        && isfield(MS.(Mode){Ic1}.Data, Args.MagField)
                    Epochs = 1:size(MS.(Mode){Ic1}.Data.(Args.MagField), 1);
                end
                Tasks = [repmat(Ic1, numel(Epochs), 1), Epochs(:)];
            else
                Iv = Epochs;
                if isempty(Iv); Iv = 1; end
                Iv = Iv(1);
                Tasks = [DS9CropsToUse(:), repmat(Iv, numel(DS9CropsToUse), 1)];
            end
            if Args.Verbose
                if PerEpochMode; ModeStr = 'per-epoch mode'; else; ModeStr = 'per-crop mode'; end
                fprintf('DS9: %s — %d frames (Result.Outliers has %d cells)\n', ...
                    ModeStr, size(Tasks, 1), numel(Result.Outliers));
            end
            for It = 1:size(Tasks, 1)
                if ~DS9Ready; break; end
                Ic = Tasks(It, 1);
                Iv = Tasks(It, 2);
                HasOutField = Ic <= numel(Result.Outliers) && isstruct(Result.Outliers{Ic}) ...
                              && isfield(Result.Outliers{Ic}, 'SrcIdx');
                fprintf('DS9: crop %02d epoch %02d — HasOutStruct=%d\n', Ic, Iv, HasOutField);
                if ~HasOutField
                    continue;
                end
                try
                    Out = Result.Outliers{Ic};
                    MSobj = MS.(Mode){Ic};
                    XField = ''; YField = '';
                    if     isfield(MSobj.Data, 'X1') && isfield(MSobj.Data, 'Y1'); XField='X1'; YField='Y1';
                    elseif isfield(MSobj.Data, 'X')  && isfield(MSobj.Data, 'Y');  XField='X';  YField='Y';
                    end
                    fprintf('DS9: crop %02d epoch %02d — XField=''%s'' SrcIdxLen=%d\n', ...
                        Ic, Iv, XField, numel(Out.SrcIdx));
                    if isempty(XField) || isempty(Out.SrcIdx)
                        NskippedNoXY = NskippedNoXY + 1;
                        continue;
                    end
                    if PerEpochMode
                        Xrow = MSobj.Data.(XField)(Iv, Out.SrcIdx);
                        Yrow = MSobj.Data.(YField)(Iv, Out.SrcIdx);
                    else
                        Xrow = median(MSobj.Data.(XField)(:, Out.SrcIdx), 1, 'omitnan');
                        Yrow = median(MSobj.Data.(YField)(:, Out.SrcIdx), 1, 'omitnan');
                    end
                    GoodXY = isfinite(Xrow) & isfinite(Yrow);
                    XYidx = find(GoodXY);
                    Xsel = Xrow(GoodXY);
                    Ysel = Yrow(GoodXY);
                    fprintf('DS9: crop %02d epoch %02d — %d finite points (of %d outliers)\n', ...
                        Ic, Iv, numel(Xsel), numel(Out.SrcIdx));

                    AIc = localPickAI(Args.AI, Ic, Iv, Mode);
                    fprintf('DS9: crop %02d epoch %02d — AIc class=%s, isempty=%d\n', ...
                        Ic, Iv, class(AIc), isempty(AIc));
                    if isempty(AIc)
                        NskippedNoAI = NskippedNoAI + 1;
                        continue;
                    end
                    if ~isprop(AIc, 'Image') || isempty(AIc.Image)
                        fprintf('DS9: crop %02d epoch %02d — AI empty .Image, skipping\n', Ic, Iv);
                        continue;
                    end

                    FrameNum = NdispedCrops + 1;
                    ds9.disp(AIc.Image, FrameNum);
                    NdispedCrops = NdispedCrops + 1;
                    pause(0.1);
                    try; ds9.frame(FrameNum); catch; end

                    if ~isempty(Xsel)
                        ds9.plotXY([Xsel(:), Ysel(:)], [], ...
                            'Color', 'red', 'Marker', 'o', ...
                            'MarkerSize', 22, 'Width', 2, 'CooType', 'image');
                        if isfield(Out, 'FlagNames')
                            for Is = 1:numel(Xsel)
                                SrcI = XYidx(Is);
                                if SrcI > numel(Out.FlagNames); break; end
                                Lbl = Out.FlagNames{SrcI};
                                if isempty(Lbl); Lbl = sprintf('src%d', Out.SrcIdx(SrcI)); end
                                try
                                    ds9.text(Xsel(Is) + 15, Ysel(Is), Lbl, ...
                                        'Color', 'red', 'FontSize', 10);
                                catch
                                end
                            end
                        end
                    end
                    if PerEpochMode
                        fprintf('DS9: crop %02d epoch %02d → frame %d (%d markers)\n', ...
                            Ic, Iv, FrameNum, numel(Xsel));
                    else
                        fprintf('DS9: crop %02d → frame %d (%d markers)\n', ...
                            Ic, FrameNum, numel(Xsel));
                    end
                catch ME
                    fprintf('DS9: crop %02d epoch %02d — EXCEPTION: %s\n', Ic, Iv, ME.message);
                    if ~isempty(ME.stack)
                        fprintf('    at %s line %d\n', ME.stack(1).name, ME.stack(1).line);
                    end
                end
            end
            if NdispedCrops > 1
                try
                    ds9.tile('grid');
                catch
                end
            end
            if Args.Verbose
                fprintf('DS9: displayed %d crop frames (skipped %d for missing AI, %d for missing XY)\n', ...
                    NdispedCrops, NskippedNoAI, NskippedNoXY);
            end
        end
    end

    % Plot lightcurves — one subplot per source, sorted by StdMag descending
    if Args.PlotLC && ~isempty(AllMed)
        % Filter to sources meeting MinEpochsPlot
        PlotMask = AllNep >= Args.MinEpochsPlot;
        LC_plot = AllLC(PlotMask);
        Labels_plot = AllLabels(PlotMask);
        Std_plot = AllStd(PlotMask);

        Ntotal = numel(LC_plot);
        if Args.Verbose
            fprintf('Plotting %d sources (Nepochs >= %d)\n', Ntotal, Args.MinEpochsPlot);
        end
        if Ntotal == 0
            return;
        end

        NperPanel = Args.MaxPlotSrc;
        Npages = ceil(Ntotal / NperPanel);

        % Sort by StdMag descending (restricted to plotted subset)
        [~, SortOrder] = sort(Std_plot, 'descend');
        LC_plot = LC_plot(SortOrder);
        Labels_plot = Labels_plot(SortOrder);

        for Ipage = 1:Npages
            IdxStart = (Ipage - 1) * NperPanel + 1;
            IdxEnd = min(Ipage * NperPanel, Ntotal);
            Nsub = IdxEnd - IdxStart + 1;

            Ncols = ceil(sqrt(Nsub));
            Nrows = ceil(Nsub / Ncols);

            figure('Name', sprintf('Outlier lightcurves (%d/%d)', Ipage, Npages), ...
                   'Position', [50, 50, 250*Ncols, 200*Nrows]);

            for Isub = 1:Nsub
                Iglobal = IdxStart + Isub - 1;
                subplot(Nrows, Ncols, Isub);
                LC = LC_plot{Iglobal};
                EpVec = (1:numel(LC))';
                Valid = isfinite(LC);
                hold on;
                % Connecting line through valid points (skipping NaN gaps)
                if sum(Valid) >= 2
                    plot(EpVec(Valid), LC(Valid), '-', 'Color', [0.4 0.6 1], 'LineWidth', 1);
                end
                % Large markers on valid epochs
                plot(EpVec(Valid), LC(Valid), '.', 'MarkerSize', 12, 'Color', [0 0.3 0.8]);
                box on; grid on;
                title(Labels_plot{Iglobal}, 'FontSize', 7, 'Interpreter', 'none');
                if Isub > (Nrows-1)*Ncols
                    xlabel('Epoch');
                end
            end

            sgtitle(sprintf('Outliers: MedMag<%.1f, Std>%.3f, Nep>=%d (page %d/%d)', ...
                Args.MaxMag, Args.MinStd, Args.MinEpochsPlot, Ipage, Npages));
        end
    end
end

function Val = bitor_reduce(Vec)
    Val = uint32(0);
    for I = 1:numel(Vec)
        Val = bitor(Val, uint32(Vec(I)));
    end
end

function AIc = localPickAI(AI, Ic, Iv, Mode)
    % Resolve one AstroImage for crop Ic from an array, cell, or nested
    % pipeline-result struct. Iv selects epoch when AI is per-epoch.
    AIc = [];
    if isa(AI, 'AstroImage')
        Sz = size(AI);
        if numel(Sz) == 2 && Sz(1) > 1 && Sz(2) > 1
            % 2D AstroImage array: interpret as (Iv, Ic) or (Ic, Iv)
            if Sz(2) >= Ic && Sz(1) >= Iv
                AIc = AI(Iv, Ic);
            elseif Sz(1) >= Ic && Sz(2) >= Iv
                AIc = AI(Ic, Iv);
            end
        elseif numel(AI) >= Ic
            AIc = AI(Ic);
        end
    elseif iscell(AI)
        % Could be {Ic} = AstroImage, or {Iv}(Ic) = AstroImage
        if Ic <= numel(AI) && isa(AI{Ic}, 'AstroImage')
            AIc = AI{Ic};
        elseif Iv <= numel(AI) && isa(AI{Iv}, 'AstroImage') && numel(AI{Iv}) >= Ic
            AIc = AI{Iv}(Ic);
        end
    elseif isstruct(AI) && isfield(AI, Mode)
        % e.g. R.AI with same layout as R.Cats: AI.(Mode){Iv}(Ic)
        Container = AI.(Mode);
        if iscell(Container) && Iv <= numel(Container) ...
                && isa(Container{Iv}, 'AstroImage') && numel(Container{Iv}) >= Ic
            AIc = Container{Iv}(Ic);
        end
    end
end

function [Keys, Vals] = countFlagNames(FlagNameCell)
    % Count per-flag-name occurrences across a cell array of comma-joined
    % flag-name strings (one entry per source). Bitwise-OR decoding is
    % already done upstream, so each source contributes 1 per named flag.
    M = containers.Map();
    NoFlag = 0;
    for Is = 1:numel(FlagNameCell)
        Nm = FlagNameCell{Is};
        if isempty(Nm) || strcmp(Nm, '0')
            NoFlag = NoFlag + 1;
        else
            Parts = strsplit(Nm, ',');
            for Ip = 1:numel(Parts)
                K = strtrim(Parts{Ip});
                if isempty(K); continue; end
                if isKey(M, K)
                    M(K) = M(K) + 1;
                else
                    M(K) = 1;
                end
            end
        end
    end
    Keys = M.keys();
    Vals = zeros(1, numel(Keys));
    for Ik = 1:numel(Keys)
        Vals(Ik) = M(Keys{Ik});
    end
    if NoFlag > 0
        Keys = [Keys, {'(none)'}];
        Vals = [Vals, NoFlag];
    end
end
