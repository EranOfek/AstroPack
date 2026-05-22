function Result = plotAuditDiff(AuStrict, AuLoose, Args)
    % Compare two calibratorAudit results: highlight strict-only rejects
    % Description: Given two audit structs from
    %              pipeline.last.quality.photCalib.calibratorAudit
    %              run on the SAME AstroImage but with different
    %              calibration strictness, splits the (shared) Pool into
    %              three cohorts:
    %                BothU         - Used in both methods (always-kept)
    %                OnlyStrictRej - NU in strict, U in loose (the
    %                                marginal cohort the strict method
    %                                throws out)
    %                BothRej       - NU in both methods
    %              and overlays their normalised distributions for the
    %              same metrics calibratorAudit plots, plus per-metric
    %              KS / Mann-Whitney / Cliffs-delta of the marginal
    %              cohort against the always-kept reference.
    %
    %              Both Pool tables must be aligned row-by-row (same
    %              SourceData ordering, same length).
    %
    % Input  : - AuStrict struct returned by calibratorAudit (strict run).
    %          - AuLoose  struct returned by calibratorAudit (loose run).
    %          * ...,key,val,...
    %            'OutDir'   - directory for saved figure / .mat.
    %                         Default '~/results_calib_audit'.
    %            'SaveFig'  - logical, default true.
    %            'Verbose'  - logical, default true.
    % Output : - Result struct with:
    %            .Masks   - .BothU, .OnlyStrictRej, .BothRej (logical
    %                       vectors of Pool height).
    %            .Counts  - [NBothU NOnlyStrictRej NBothRej].
    %            .Stats   - struct of per-metric two-sample stats
    %                       (OnlyStrictRej vs BothU): KS_p, MW_p,
    %                       CliffsDelta, MedOnlyStrict, MedBothU.
    %            .Args    - echo.
    % Author : D. Kovaleva (May 2026)
    % Example: Au_S = pipeline.last.quality.photCalib.calibratorAudit(AI, PC_strict);
    %          Au_L = pipeline.last.quality.photCalib.calibratorAudit(AI, PC_loose);
    %          R = pipeline.last.quality.photCalib.plotAuditDiff(Au_S, Au_L);

    arguments
        AuStrict struct
        AuLoose  struct
        Args.OutDir  {mustBeText} = '~/results_calib_audit'
        Args.SaveFig logical      = true
        Args.Verbose logical      = true
        Args.Cohorts cell         = {'AlwaysKept', 'AlwaysRejected', 'DiffRejected'}
        % Cohort vocabulary (any subset, in the order to be drawn):
        %   'AlwaysKept'     - Used in both methods                  (BothU)
        %   'AlwaysRejected' - NU in both methods                    (BothRej)
        %   'DiffRejected'   - NU in strict, U in loose              (OnlyStrictRej)
        %   'RejectedStrict' - any NU under strict                   (BothRej | OnlyStrictRej)
        %   'RejectedLoose'  - any NU under loose                    (BothRej | OnlyLooseRej)
    end

    if ~isfield(AuStrict, 'Pool') || ~isfield(AuLoose, 'Pool')
        error('plotAuditDiff:NoPool', 'Both inputs must have a .Pool table.');
    end
    if height(AuStrict.Pool) ~= height(AuLoose.Pool)
        error('plotAuditDiff:Mismatch', ...
            'Pool heights differ: strict=%d loose=%d', ...
            height(AuStrict.Pool), height(AuLoose.Pool));
    end

    OutDir = char(Args.OutDir);
    if Args.SaveFig && ~exist(OutDir, 'dir'); mkdir(OutDir); end

    % --- Cohort masks --------------------------------------------------
    Us = logical(AuStrict.Pool.Used);
    Ul = logical(AuLoose.Pool.Used);
    BothU         =  Us &  Ul;
    OnlyStrictRej = ~Us &  Ul;
    BothRej       = ~Us & ~Ul;
    OnlyLooseRej  =  Us & ~Ul;   % rare: loose stricter on this row

    Result.Masks  = struct('BothU', BothU, ...
                           'OnlyStrictRej', OnlyStrictRej, ...
                           'BothRej', BothRej, ...
                           'OnlyLooseRej', OnlyLooseRej);
    Result.Counts = [sum(BothU), sum(OnlyStrictRej), sum(BothRej), sum(OnlyLooseRej)];

    if Args.Verbose
        fprintf(['plotAuditDiff: BothU=%d  OnlyStrictRej=%d  ', ...
                 'BothRej=%d  OnlyLooseRej=%d  (Pool=%d)\n'], ...
            Result.Counts(1), Result.Counts(2), Result.Counts(3), ...
            Result.Counts(4), height(AuStrict.Pool));
    end

    % Same plot config as calibratorAudit
    KeepPlotCols = { ...
        'LastNearestDist_as','LastNearestDeltaMag', ...
        'Gaia_G','Gaia_BPRP','Gaia_BPRPExcessFactor', ...
        'MagErr','MatchDistance'};
    LogXCols = {'LastNearestDist_as','Gaia_PlxSN'};

    % Use AuStrict.Pool as the data source — both Pools share the
    % numeric augmentation columns; only the .Used flag differs.
    Pool = AuStrict.Pool;

    % --- Per-metric stats: OnlyStrictRej vs BothU ----------------------
    Stats = struct();
    for I = 1:numel(KeepPlotCols)
        C = KeepPlotCols{I};
        if ~ismember(C, Pool.Properties.VariableNames); continue; end
        Va = Pool.(C)(BothU);
        Vb = Pool.(C)(OnlyStrictRej);
        T  = twoSampleStats(Vb, Va);   % positive CliffsDelta = OnlyStrictRej > BothU
        Stats.(C) = struct( ...
            'N_BothU',         T.N_B, ...
            'N_OnlyStrictRej', T.N_A, ...
            'MedBothU',         T.Med_B, ...
            'MedOnlyStrictRej', T.Med_A, ...
            'KS_p',             T.KS_p, ...
            'MW_p',             T.MW_p, ...
            'CliffsDelta',      T.CliffsDelta);
    end
    Result.Stats = Stats;
    Result.Args  = Args;

    if Args.Verbose
        printStats(Stats);
    end

    if Args.SaveFig
        try
            save(fullfile(OutDir, 'plotAuditDiff.mat'), 'Result', '-v7.3');
        catch ME
            warning('plotAuditDiff:SaveFailed', ...
                'Result.mat save failed: %s', ME.message);
        end
    end

    plotDiffGrid(Pool, Result, KeepPlotCols, LogXCols, Args, OutDir);
end

% =========================================================================
function plotDiffGrid(Pool, Result, Names, LogXCols, Args, OutDir)
    Names = Names(ismember(Names, Pool.Properties.VariableNames));
    Names = Names(ismember(Names, fieldnames(Result.Stats)));
    if isempty(Names); return; end
    Nm = numel(Names);
    Ncols = ceil(sqrt(Nm));
    Nrows = ceil(Nm / Ncols);

    % Build cohort plot config from Args.Cohorts (preserves listed order).
    Cohorts = buildCohorts(Args.Cohorts, Result.Masks);
    if isempty(Cohorts)
        warning('plotAuditDiff:NoCohorts', ...
            'No valid cohort names supplied; nothing to plot.');
        return;
    end
    FigName = sprintf('plotAuditDiff: %s', strjoin(Args.Cohorts, ' + '));
    OutBase = ['audit_diff_' strjoin(lower(Args.Cohorts), '_')];

    F = figure('Name', FigName, 'Position', [60 60 1500 950]);
    for I = 1:Nm
        Ax = subplot(Nrows, Ncols, I); hold on;
        C = Names{I};
        IsLog = ismember(C, LogXCols);

        % Pool finite (and positive on log) values across cohorts to set edges
        AllFin = false(height(Pool), 1);
        for J = 1:numel(Cohorts); AllFin = AllFin | Cohorts(J).M; end
        Vall = Pool.(C)(AllFin); Vall = Vall(isfinite(Vall));
        if IsLog; Vall = Vall(Vall > 0); end
        if numel(unique(Vall)) < 2
            title(C, 'Interpreter', 'none'); continue;
        end
        if IsLog
            Edges = logspace(log10(min(Vall)), log10(max(Vall)), 30);
        else
            Edges = linspace(min(Vall), max(Vall), 30);
        end

        for J = 1:numel(Cohorts)
            V = Pool.(C)(Cohorts(J).M); V = V(isfinite(V));
            if IsLog; V = V(V > 0); end
            if isempty(V); continue; end
            histogram(V, Edges, 'FaceColor', Cohorts(J).Face, ...
                'FaceAlpha', Cohorts(J).Alpha, ...
                'EdgeColor', Cohorts(J).Edge, ...
                'LineWidth', Cohorts(J).LW, ...
                'Normalization', 'probability', ...
                'DisplayName', sprintf('%s (N=%d)', Cohorts(J).Label, numel(V)));
        end
        if IsLog; set(Ax, 'XScale', 'log'); end
        ylabel('Fraction', 'Interpreter', 'none');
        title(C, 'Interpreter', 'none');
        legend('Location', 'best', 'Interpreter', 'none');
        grid on; box on;
    end
    if Args.SaveFig
        savefig(F, fullfile(OutDir, [OutBase '.fig']));
        saveas(F,  fullfile(OutDir, [OutBase '.png']));
    end
end

% =========================================================================
function C = buildCohorts(Names, Masks)
    % Map cohort names to (mask, label, face, alpha, edge, linewidth).
    Tab = struct();
    Tab.AlwaysKept = struct('M', Masks.BothU, ...
        'Label', 'AlwaysKept (BothU)', ...
        'Face',  [0.40 0.40 0.40], 'Alpha', 0.8, ...
        'Edge',  [0.25 0.25 0.25], 'LW', 0.5);
    Tab.AlwaysRejected = struct('M', Masks.BothRej, ...
        'Label', 'AlwaysRejected (BothRej)', ...
        'Face',  [0.85 0.85 0.85], 'Alpha', 1.0, ...
        'Edge',  'k', 'LW', 1.6);
    Tab.DiffRejected = struct('M', Masks.OnlyStrictRej, ...
        'Label', 'DiffRejected (S \\ R)', ...
        'Face',  [0.85 0.20 0.20], 'Alpha', 0.45, ...
        'Edge',  [0.55 0.10 0.10], 'LW', 0.8);
    Tab.RejectedStrict = struct('M', Masks.BothRej | Masks.OnlyStrictRej, ...
        'Label', 'RejectedStrict (S)', ...
        'Face',  [0.55 0.55 0.55], 'Alpha', 0.7, ...
        'Edge',  [0.25 0.25 0.25], 'LW', 0.5);
    Tab.RejectedLoose  = struct('M', Masks.BothRej | Masks.OnlyLooseRej, ...
        'Label', 'RejectedLoose (R)', ...
        'Face',  [0.92 0.92 0.92], 'Alpha', 1.0, ...
        'Edge',  'k', 'LW', 1.0);

    Allowed = fieldnames(Tab);
    C = struct('M', {}, 'Label', {}, 'Face', {}, 'Alpha', {}, 'Edge', {}, 'LW', {});
    for I = 1:numel(Names)
        N = Names{I};
        if ~ismember(N, Allowed)
            warning('plotAuditDiff:UnknownCohort', ...
                'Unknown cohort "%s" — allowed: %s', ...
                N, strjoin(Allowed, ', '));
            continue;
        end
        C(end+1) = Tab.(N); %#ok<AGROW>
    end
end

% =========================================================================
function m = safeMedian(v); if isempty(v); m = NaN; else; m = median(v); end; end

% =========================================================================
function d = cliffsDelta(a, b)
    a = a(:); b = b(:);
    if isempty(a) || isempty(b); d = NaN; return; end
    R = tiedrank([a; b]);
    Ra = sum(R(1:numel(a)));
    n_a = numel(a); n_b = numel(b);
    U = Ra - n_a * (n_a + 1) / 2;
    d = 2 * U / (n_a * n_b) - 1;
end

% =========================================================================
function printStats(Stats)
    Names = fieldnames(Stats);
    fprintf('\n%-22s %8s %8s %12s %12s %10s %10s %8s\n', ...
        'Metric','N_BothU','N_OSR','Med_BothU','Med_OSR','KS_p','MW_p','Cliffs');
    fprintf('%s\n', repmat('-', 1, 96));
    for I = 1:numel(Names)
        S = Stats.(Names{I});
        fprintf('%-22s %8d %8d %12.4g %12.4g %10.3g %10.3g %+8.3f\n', ...
            Names{I}, S.N_BothU, S.N_OnlyStrictRej, ...
            S.MedBothU, S.MedOnlyStrictRej, ...
            S.KS_p, S.MW_p, S.CliffsDelta);
    end
end
