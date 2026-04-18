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
    %            'PlotStats'  - Plot Nepochs + flag histograms. Default is true.
    %            'PlotPerEpoch' - Plot per-epoch histogram (sum over outliers
    %                        of flagged vs. valid detections at each epoch).
    %                        Default is false.
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
    % Example: R = pipeline.last.quality.plotOutliersScatter(R.MS, R.Cats);
    %          R = pipeline.last.quality.plotOutliersScatter(R.MS, R.Cats, ...
    %              'MaxMag', 14, 'MinStd', 0.1, 'MagField', 'MAG_AB_PSF');
    %          R = pipeline.last.quality.plotOutliersScatter(R.MS, [], ...
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

                MedMag = nanmedian(MagMat, 1);
                StdMag = nanstd(MagMat, 0, 1);

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
                        RA_sel  = nanmedian(MSobj.Data.RA(:, SrcIdx), 1);
                        Dec_sel = nanmedian(MSobj.Data.Dec(:, SrcIdx), 1);
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

    % Plot Nepochs + flag statistics histograms
    if Args.PlotStats && ~isempty(AllNep)
        figure('Name', 'Outlier statistics', 'Position', [50, 50, 1100, 400]);

        % Nepochs histogram
        subplot(1, 2, 1);
        Edges = 0.5:1:(max(AllNep)+0.5);
        histogram(AllNep, Edges, 'FaceColor', [0.3 0.5 0.8]);
        hold on;
        YL = ylim;
        plot([Args.MinEpochsSummary Args.MinEpochsSummary]-0.5, YL, '--k', 'LineWidth', 1);
        plot([Args.MinEpochsPlot Args.MinEpochsPlot]-0.5, YL, '--r', 'LineWidth', 1);
        text(Args.MinEpochsSummary-0.5, YL(2), ' MinSum', 'VerticalAlignment', 'top');
        text(Args.MinEpochsPlot-0.5, YL(2), ' MinPlot', 'VerticalAlignment', 'top', 'Color', 'r');
        box on; grid on;
        xlabel('N valid epochs');
        ylabel('Number of outliers');
        title(sprintf('Nepochs distribution (N=%d outliers)', numel(AllNep)));

        % Flag statistics histogram: per-flag-name frequency across outliers
        subplot(1, 2, 2);
        FlagCounts = containers.Map();
        NoFlag = 0;
        for Is = 1:numel(AllFlagNames)
            Nm = AllFlagNames{Is};
            if isempty(Nm) || strcmp(Nm, '0')
                NoFlag = NoFlag + 1;
            else
                Parts = strsplit(Nm, ',');
                for Ip = 1:numel(Parts)
                    K = strtrim(Parts{Ip});
                    if isempty(K); continue; end
                    if isKey(FlagCounts, K)
                        FlagCounts(K) = FlagCounts(K) + 1;
                    else
                        FlagCounts(K) = 1;
                    end
                end
            end
        end
        Keys = FlagCounts.keys();
        Vals = zeros(1, numel(Keys));
        for Ik = 1:numel(Keys)
            Vals(Ik) = FlagCounts(Keys{Ik});
        end
        if NoFlag > 0
            Keys = [Keys, {'(none)'}];
            Vals = [Vals, NoFlag];
        end
        [Vals, Order] = sort(Vals, 'descend');
        Keys = Keys(Order);
        if ~isempty(Vals)
            bar(Vals, 'FaceColor', [0.8 0.4 0.3]);
            set(gca, 'XTick', 1:numel(Keys), 'XTickLabel', Keys, ...
                'XTickLabelRotation', 45, 'TickLabelInterpreter', 'none');
            ylabel('Number of outliers');
            title('Flag frequency (bitwise-OR per source)');
            box on; grid on;
        end

        Result.NepochsHist = AllNep;
        Result.FlagCounts  = containers.Map(Keys, num2cell(Vals));
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
