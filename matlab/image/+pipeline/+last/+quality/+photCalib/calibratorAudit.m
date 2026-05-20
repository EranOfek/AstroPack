function Result = calibratorAudit(AI, PC, Args)
    % Compare used vs non-used calibrators across LAST and Gaia metrics
    % Description: For one calibrated visit (1x24 AstroImage + 1x24
    %              PhotCalibTrans), pools every calibrator from
    %              PC(k).SourceData.Table, augments each row with
    %              LAST-side and Gaia-side neighbour / quality metrics,
    %              and tests whether the sub-population with Used=false
    %              differs systematically from Used=true.
    %
    %              Tested hypotheses:
    %                (1) NU calibrators are more often blended / binary —
    %                    proxies: LAST neighbour count within Radius,
    %                    Gaia neighbour count within Radius, Gaia
    %                    astrometric_excess_noise, reduced
    %                    astrometric_chi2_al, parallax S/N.
    %                (2) NU calibrators have systematic distributional
    %                    bias — colour (BP-RP), brightness (G), spatial
    %                    distribution per crop, residuals, MagErr.
    %
    % Input  : - AI 1xNcrop AstroImage array (one visit, calibrated;
    %                CatData.Catalog must include FLAGS).
    %          - PC 1xNcrop PhotCalibTrans array paired with AI; each
    %                must have SourceData.Table populated with the Used
    %                column (i.e. post-calibrate).
    %          * ...,key,val,...
    %            'Radius'      - neighbour search radius [arcsec].
    %                            Default 2.
    %            'GaiaCatName' - catsHTM catalog. Default 'GAIADR3'.
    %            'OutDir'      - directory for saved figures and
    %                            Result.mat. Default '~/results_calib_audit'.
    %            'Plot','SaveFig','Verbose' - logical defaults true.
    % Output : - Result struct with:
    %            .Pool    - long table (all crops concatenated) with
    %                       original SourceData columns plus LAST / Gaia
    %                       augmentation columns and per-row Crop index.
    %            .Counts  - per-crop NUsed, NNU, NU_Rate.
    %            .Stats   - struct of per-metric two-sample stats
    %                       (median U/NU, mean U/NU, std U/NU, KS_p,
    %                        MW_p, CliffsDelta).
    %            .Args    - echo.
    % Author : D. Kovaleva (May 2026)
    % Example: R = pipeline.last.quality.photCalib.calibratorAudit( ...
    %              AI, PC, 'Radius', 2, 'OutDir', '~/results_calib_audit');

    arguments
        AI (1,:)
        PC (1,:)
        Args.Radius      (1,1) double {mustBePositive} = 2
        Args.GaiaCatName (1,:) char                    = 'GAIADR3'
        Args.OutDir      {mustBeText}                  = '~/results_calib_audit'
        Args.Plot        logical                       = true
        Args.SaveFig     logical                       = true
        Args.Verbose     logical                       = true
    end

    Ncrop = numel(PC);
    if numel(AI) ~= Ncrop
        error('calibratorAudit:Mismatch', ...
            'AI (%d) and PC (%d) must have the same length.', numel(AI), Ncrop);
    end

    OutDir = char(Args.OutDir);
    if (Args.SaveFig || Args.Plot) && ~exist(OutDir, 'dir')
        mkdir(OutDir);
    end

    if Args.Verbose
        fprintf('calibratorAudit: %d crops, R=%.1f arcsec\n', Ncrop, Args.Radius);
    end

    ArcSecPerRad = (180 / pi) * 3600;

    % ============================================================
    % Phase 1: Pool SourceData tables across crops
    % ============================================================
    Cells = cell(1, Ncrop);
    for K = 1:Ncrop
        if isempty(PC(K).SourceData) || isempty(PC(K).TransModel); continue; end
        T = PC(K).SourceData.Table;
        if ~ismember('Used', T.Properties.VariableNames); continue; end
        T.Crop = repmat(K, height(T), 1);
        Cells{K} = T;
    end
    Cells = Cells(~cellfun(@isempty, Cells));
    if isempty(Cells)
        error('calibratorAudit:NoCalib', ...
            'No PCs with populated SourceData / Used column.');
    end
    Pool = unifyAndConcat(Cells);
    Pool.Used = logical(Pool.Used);
    Npool = height(Pool);

    if Args.Verbose
        fprintf('  pooled %d calibrators (%d Used, %d NonUsed)\n', ...
            Npool, sum(Pool.Used), sum(~Pool.Used));
    end

    % ============================================================
    % Phase 2: LAST neighbour metrics + own FLAGS
    % ============================================================
    Pool.LastN_within        = nan(Npool, 1);
    Pool.LastNearestDist_as  = nan(Npool, 1);
    Pool.LastNearestDeltaMag = nan(Npool, 1);
    Pool.LastFlags           = nan(Npool, 1);
    Pool.LastSrcIdx          = nan(Npool, 1);

    for K = 1:Ncrop
        Mask = (Pool.Crop == K);
        if ~any(Mask); continue; end

        Cat = AI(K).CatData;
        AllRA  = getColTry(Cat, {'RA','ALPHA_J2000','Alpha'});
        AllDec = getColTry(Cat, {'Dec','DELTA_J2000','Delta'});
        AllX   = getColTry(Cat, {'X1','X','XPEAK','X_IMAGE'});
        AllY   = getColTry(Cat, {'Y1','Y','YPEAK','Y_IMAGE'});
        AllMag = getColTry(Cat, {'MAG_PSF','MAG_APER_3'});
        AllFlg = getColTry(Cat, {'FLAGS'});

        if isempty(AllRA) || isempty(AllDec)
            if Args.Verbose
                fprintf('  crop %d: AI catalog missing RA/Dec, skipping\n', K);
            end
            continue;
        end

        % Pool RA/Dec are in degrees (per SourceData convention) — convert
        % to radians for celestial.coo.sphere_dist_fast.
        CalRA  = Pool.RA(Mask)  * pi/180;
        CalDec = Pool.Dec(Mask) * pi/180;
        % AI catalog RA/Dec — convert to rad if AstroCatalog units say deg.
        AllRA  = toRad(AllRA,  Cat, {'RA','ALPHA_J2000','Alpha'});
        AllDec = toRad(AllDec, Cat, {'Dec','DELTA_J2000','Delta'});
        CalX = []; CalY = [];
        if ismember('X', Pool.Properties.VariableNames); CalX = Pool.X(Mask); end
        if ismember('Y', Pool.Properties.VariableNames); CalY = Pool.Y(Mask); end

        PoolIdx = find(Mask);
        Ncal = numel(CalRA);

        for J = 1:Ncal
            % Self-index in AI catalog by (X,Y) — same source in the same
            % catalog, tolerant to floating-point copy noise (within 1 px).
            SrcI = NaN;
            if ~isempty(CalX) && ~isempty(CalY) && ~isempty(AllX) && ~isempty(AllY)
                D2 = (AllX - CalX(J)).^2 + (AllY - CalY(J)).^2;
                [Mn, Im] = min(D2);
                if Mn < 1   % px^2 — within 1 px of the calibrator's recorded XY
                    SrcI = Im;
                end
            end

            DistRad = celestial.coo.sphere_dist_fast( ...
                CalRA(J), CalDec(J), AllRA(:), AllDec(:));
            DistAs = DistRad * ArcSecPerRad;
            if ~isnan(SrcI); DistAs(SrcI) = Inf; end

            Pool.LastN_within(PoolIdx(J)) = sum(DistAs <= Args.Radius);

            [NearD, NearI] = min(DistAs);
            if isfinite(NearD)
                Pool.LastNearestDist_as(PoolIdx(J)) = NearD;
                if ~isnan(SrcI) && ~isempty(AllMag)
                    Pool.LastNearestDeltaMag(PoolIdx(J)) = ...
                        AllMag(NearI) - AllMag(SrcI);
                end
            end
            if ~isnan(SrcI) && ~isempty(AllFlg)
                Pool.LastFlags(PoolIdx(J)) = AllFlg(SrcI);
            end
            Pool.LastSrcIdx(PoolIdx(J)) = SrcI;
        end
    end

    % ============================================================
    % Phase 3: Gaia neighbour and astrometric metrics via match_catsHTM
    % ============================================================
    Pool.Gaia_N_within        = nan(Npool, 1);
    Pool.Gaia_NearestDist_as  = nan(Npool, 1);
    Pool.Gaia_G               = nan(Npool, 1);
    Pool.Gaia_BPRP            = nan(Npool, 1);
    Pool.Gaia_PlxSN           = nan(Npool, 1);
    Pool.Gaia_NonSingleStar   = nan(Npool, 1);
    Pool.Gaia_BPRPExcessFactor= nan(Npool, 1);

    for K = 1:Ncrop
        Mask = (Pool.Crop == K);
        if ~any(Mask); continue; end
        PoolIdx = find(Mask);

        Sub = AstroCatalog;
        Sub.Catalog  = [Pool.RA(Mask), Pool.Dec(Mask)];
        Sub.ColNames = {'RA', 'Dec'};
        Sub.ColUnits = {'deg', 'deg'};   % SourceData stores deg

        try
            [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Sub, ...
                Args.GaiaCatName, ...
                'Radius',      Args.Radius, ...
                'RadiusUnits', 'arcsec');
        catch ME
            if Args.Verbose
                fprintf('  crop %d: Gaia match failed (%s)\n', K, ME.message);
            end
            continue;
        end

        NbrCount = ResInd.Obj2_NmatchObj1;
        NearIdx  = ResInd.Obj2_IndInObj1;
        NearDist = ResInd.Obj2_Dist;   % radians

        Glist     = getColTry(CatH, {'phot_g_mean_mag','Mag_G','MagG'});
        BPlist    = getColTry(CatH, {'phot_bp_mean_mag','Mag_BP','MagBP'});
        RPlist    = getColTry(CatH, {'phot_rp_mean_mag','Mag_RP','MagRP'});
        BPRPlist  = getColTry(CatH, {'bp_rp'});
        PlxList   = getColTry(CatH, {'Plx','parallax'});
        ErrPlxList= getColTry(CatH, {'ErrPlx','parallax_error'});
        NSSlist   = getColTry(CatH, {'non_single_star'});
        BPRPexcList = getColTry(CatH, {'phot_bp_rp_excess_factor'});

        for J = 1:numel(PoolIdx)
            P = PoolIdx(J);
            if J <= numel(NbrCount); Pool.Gaia_N_within(P) = NbrCount(J); end
            if J > numel(NearIdx) || isnan(NearIdx(J)); continue; end
            Ni = NearIdx(J);
            Pool.Gaia_NearestDist_as(P) = NearDist(J) * ArcSecPerRad;
            Pool.Gaia_G(P)        = pickAt(Glist, Ni);
            % Prefer the catalog's own bp_rp; fall back to BP-RP arithmetic.
            BPRPv = pickAt(BPRPlist, Ni);
            if isnan(BPRPv)
                BPRPv = pickAt(BPlist, Ni) - pickAt(RPlist, Ni);
            end
            Pool.Gaia_BPRP(P)               = BPRPv;
            Pool.Gaia_NonSingleStar(P)      = pickAt(NSSlist, Ni);
            Pool.Gaia_BPRPExcessFactor(P)   = pickAt(BPRPexcList, Ni);
            Pl = pickAt(PlxList, Ni); Pe = pickAt(ErrPlxList, Ni);
            if isfinite(Pl) && isfinite(Pe) && Pe > 0
                Pool.Gaia_PlxSN(P) = Pl / Pe;
            end
        end
    end

    % Derived: boolean "any binarity bit set" from Gaia_NonSingleStar bitmask.
    % NaN propagates (i.e. unmatched calibrators stay NaN, not 0).
    Pool.Gaia_IsBinaryFlagged = nan(Npool, 1);
    Fin = isfinite(Pool.Gaia_NonSingleStar);
    Pool.Gaia_IsBinaryFlagged(Fin) = double(Pool.Gaia_NonSingleStar(Fin) > 0);

    % ============================================================
    % Phase 4: stats — per-numeric-column two-sample tests
    % ============================================================
    NumericCols = { ...
        'LastN_within','LastNearestDist_as','LastNearestDeltaMag','LastFlags', ...
        'Gaia_N_within','Gaia_NearestDist_as','Gaia_G','Gaia_BPRP', ...
        'Gaia_PlxSN','Gaia_NonSingleStar','Gaia_IsBinaryFlagged', ...
        'Gaia_BPRPExcessFactor', ...
        'MagErr','MatchDistance','RA','Dec'};

    % Columns to plot on log-X (and bin in log space).
    LogXCols = {'LastNearestDist_as','Gaia_PlxSN'};

    Stats = struct();
    UMask = Pool.Used;
    NMask = ~Pool.Used;
    for I = 1:numel(NumericCols)
        C = NumericCols{I};
        if ~ismember(C, Pool.Properties.VariableNames); continue; end
        Vu = Pool.(C)(UMask); Vu = Vu(isfinite(Vu));
        Vn = Pool.(C)(NMask); Vn = Vn(isfinite(Vn));
        S.N_used = numel(Vu); S.N_nu = numel(Vn);
        if S.N_used < 1 && S.N_nu < 1; continue; end
        S.MedUsed  = safeMedian(Vu);
        S.MedNU    = safeMedian(Vn);
        S.MeanUsed = safeMean(Vu);
        S.MeanNU   = safeMean(Vn);
        S.StdUsed  = safeStd(Vu);
        S.StdNU    = safeStd(Vn);
        S.KS_p = NaN; S.MW_p = NaN; S.CliffsDelta = NaN;
        if S.N_used >= 5 && S.N_nu >= 5
            try [~, S.KS_p] = kstest2(Vu, Vn); catch; end
            try S.MW_p = ranksum(Vu, Vn); catch; end
            S.CliffsDelta = cliffsDelta(Vu, Vn);
        end
        Stats.(C) = S;
    end

    % ============================================================
    % Per-crop counts
    % ============================================================
    NUsed = accumarray(Pool.Crop, double(Pool.Used),  [Ncrop, 1]);
    NNU   = accumarray(Pool.Crop, double(~Pool.Used), [Ncrop, 1]);
    Counts = table((1:Ncrop).', NUsed, NNU, ...
        'VariableNames', {'Crop','NUsed','NNU'});
    Counts.NU_Rate = Counts.NNU ./ max(Counts.NUsed + Counts.NNU, 1);

    Result.Pool   = Pool;
    Result.Counts = Counts;
    Result.Stats  = Stats;
    Result.Args   = Args;

    if Args.Verbose
        printStats(Stats);
    end

    if Args.SaveFig
        try
            save(fullfile(OutDir, 'calibratorAudit.mat'), 'Result', '-v7.3');
        catch ME
            warning('calibratorAudit:SaveFailed', ...
                'Result.mat save failed: %s', ME.message);
        end
    end

    % ============================================================
    % Phase 5: plots
    % ============================================================
    if Args.Plot
        plotDistributions(Pool, Stats, Args, OutDir, LogXCols);
        plotNURate(Counts, Args, OutDir);
        plotCMD(Pool, Args, OutDir);
    end
end

% =========================================================================
function v = toRad(v, Cat, names)
    % Convert v to radians if the catalog's unit for one of the matching
    % column names is 'deg'. Leaves v untouched otherwise.
    if isempty(v) || isempty(Cat); return; end
    if isempty(Cat.ColUnits) || isempty(Cat.ColNames); return; end
    for I = 1:numel(names)
        idx = find(strcmp(Cat.ColNames, names{I}), 1);
        if ~isempty(idx) && idx <= numel(Cat.ColUnits)
            if strcmpi(Cat.ColUnits{idx}, 'deg')
                v = v * pi/180;
            end
            return;
        end
    end
end

% =========================================================================
function v = getColTry(Cat, names)
    v = [];
    if isempty(Cat); return; end
    for I = 1:numel(names)
        try
            v = Cat.getCol(names{I});
            if ~isempty(v); return; end
        catch
        end
    end
end

% =========================================================================
function v = pickAt(vec, idx)
    if isempty(vec) || isnan(idx) || idx < 1 || idx > numel(vec)
        v = NaN;
    else
        v = vec(idx);
    end
end

% =========================================================================
function T = unifyAndConcat(Cells)
    % Concatenate cell of tables that may differ in columns by intersecting
    % schemas (keeps only columns present in every table).
    Vars = Cells{1}.Properties.VariableNames;
    for I = 2:numel(Cells)
        Vars = intersect(Vars, Cells{I}.Properties.VariableNames, 'stable');
    end
    for I = 1:numel(Cells)
        Cells{I} = Cells{I}(:, Vars);
    end
    T = vertcat(Cells{:});
end

% =========================================================================
function m = safeMedian(v); if isempty(v); m = NaN; else; m = median(v); end; end
function m = safeMean(v);   if isempty(v); m = NaN; else; m = mean(v);   end; end
function s = safeStd(v);    if numel(v) < 2; s = NaN; else; s = std(v);  end; end

% =========================================================================
function d = cliffsDelta(a, b)
    a = a(:); b = b(:);
    if isempty(a) || isempty(b); d = NaN; return; end
    Combined = [a; b];
    R = tiedrank(Combined);
    Ra = sum(R(1:numel(a)));
    n_a = numel(a); n_b = numel(b);
    U = Ra - n_a * (n_a + 1) / 2;
    d = 2 * U / (n_a * n_b) - 1;
end

% =========================================================================
function printStats(Stats)
    Names = fieldnames(Stats);
    fprintf('\n%-22s %6s %6s %12s %12s %10s %10s %8s\n', ...
        'Metric','N_U','N_NU','Med_U','Med_NU','KS_p','MW_p','Cliffs');
    fprintf('%s\n', repmat('-', 1, 92));
    for I = 1:numel(Names)
        S = Stats.(Names{I});
        fprintf('%-22s %6d %6d %12.4g %12.4g %10.3g %10.3g %+8.3f\n', ...
            Names{I}, S.N_used, S.N_nu, S.MedUsed, S.MedNU, ...
            S.KS_p, S.MW_p, S.CliffsDelta);
    end
end

% =========================================================================
function plotDistributions(Pool, Stats, Args, OutDir, LogXCols)
    KeepPlotCols = { ...
        'LastNearestDist_as','LastNearestDeltaMag', ...
        'Gaia_G','Gaia_BPRP','Gaia_BPRPExcessFactor', ...
        'MagErr','MatchDistance'};
    Names = fieldnames(Stats);
    Names = Names(ismember(Names, KeepPlotCols));
    if isempty(Names); return; end
    Nm = numel(Names);
    Ncols = ceil(sqrt(Nm));
    Nrows = ceil(Nm / Ncols);
    F = figure('Name', 'calibratorAudit: distributions', ...
        'Position', [60 60 1500 950]);
    UMask = Pool.Used; NMask = ~Pool.Used;
    for I = 1:Nm
        Ax = subplot(Nrows, Ncols, I); hold on;
        C = Names{I};
        IsLog = ismember(C, LogXCols);
        Vu = Pool.(C)(UMask); Vu = Vu(isfinite(Vu));
        Vn = Pool.(C)(NMask); Vn = Vn(isfinite(Vn));
        if IsLog
            Vu = Vu(Vu > 0); Vn = Vn(Vn > 0);
        end
        if isempty(Vu) && isempty(Vn)
            title(C, 'Interpreter', 'none'); continue;
        end
        Combined = [Vu; Vn];
        if numel(unique(Combined)) < 2
            title(C, 'Interpreter', 'none'); continue;
        end
        if IsLog
            Edges = logspace(log10(min(Combined)), log10(max(Combined)), 30);
        else
            Edges = linspace(min(Combined), max(Combined), 30);
        end
        if ~isempty(Vn)
            histogram(Vn, Edges, 'FaceColor', [0.85 0.85 0.85], ...
                'EdgeColor', 'k', 'LineWidth', 2.0, ...
                'Normalization', 'probability', ...
                'DisplayName', sprintf('NU (N=%d)', numel(Vn)));
        end
        if ~isempty(Vu)
            histogram(Vu, Edges, 'FaceColor', [0.40 0.40 0.40], ...
                'FaceAlpha', 0.85, 'EdgeColor', [0.25 0.25 0.25], ...
                'LineWidth', 0.5, ...
                'Normalization', 'probability', ...
                'DisplayName', sprintf('U (N=%d)', numel(Vu)));
        end
        if IsLog; set(Ax, 'XScale', 'log'); end
        ylabel('Fraction', 'Interpreter', 'none');
        title(sprintf('%s  KS p=%.2g', C, Stats.(C).KS_p), ...
            'Interpreter', 'none');
        legend('Location', 'best', 'Interpreter', 'none');
        grid on; box on;
    end
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'distributions.fig'));
        saveas(F,  fullfile(OutDir, 'distributions.png'));
    end
end

% =========================================================================
function plotNURate(Counts, Args, OutDir)
    F = figure('Name', 'calibratorAudit: NU rate per crop', ...
        'Position', [60 60 720 480]);
    bar(Counts.Crop, Counts.NU_Rate, 'FaceColor', [0.40 0.40 0.40]);
    xlabel('Crop'); ylabel('NU fraction');
    title('Calibrator NonUsed fraction per crop');
    grid on; box on;
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'nu_rate.fig'));
        saveas(F,  fullfile(OutDir, 'nu_rate.png'));
    end
end

% =========================================================================
function plotCMD(Pool, Args, OutDir)
    Names = Pool.Properties.VariableNames;
    if ~ismember('Gaia_G', Names) || ~ismember('Gaia_BPRP', Names); return; end
    F = figure('Name', 'calibratorAudit: Gaia CMD', ...
        'Position', [60 60 700 600]);
    hold on;
    UMask = Pool.Used; NMask = ~Pool.Used;
    scatter(Pool.Gaia_BPRP(UMask), Pool.Gaia_G(UMask), 16, ...
        [0.25 0.25 0.25], 'filled', ...
        'MarkerFaceAlpha', 0.6, 'DisplayName', 'Used');
    scatter(Pool.Gaia_BPRP(NMask), Pool.Gaia_G(NMask), 18, ...
        [0.85 0.20 0.20], 'filled', ...
        'MarkerFaceAlpha', 0.7, 'DisplayName', 'NonUsed');
    set(gca, 'YDir', 'reverse');
    xlabel('BP - RP', 'Interpreter', 'none');
    ylabel('G', 'Interpreter', 'none');
    title('Gaia CMD: Used vs NonUsed');
    legend('Location', 'best');
    grid on; box on;
    if Args.SaveFig
        savefig(F, fullfile(OutDir, 'cmd.fig'));
        saveas(F,  fullfile(OutDir, 'cmd.png'));
    end
end
