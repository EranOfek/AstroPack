function Result = calibratorRejectFraction(Au, Args)
    % Fraction of rejected calibrators inside high-rejection-rate windows
    % Description: Sorts calibrators by one named characteristic
    %              (e.g. 'LastNearestDist_as'), slides a fixed-size
    %              window over the sorted vector, and computes the local
    %              rejection rate NU/WindowSize per window position.
    %              Windows whose rate exceeds Threshold define the
    %              "bad scope" range(s) of the characteristic; the
    %              function reports the fraction of all rejected
    %              calibrators whose value falls inside any bad-scope
    %              range.
    %
    %              Default WindowStep=1 gives a fully sliding (smooth)
    %              rate curve; setting WindowStep=WindowSize gives
    %              disjoint windows equivalent to fixed-N quantile bins.
    %              Only full-size windows are emitted (the last window
    %              starts at sample N-WindowSize+1).
    %
    %              Threshold modes:
    %                'mean' (default) - the population rejection rate
    %                                    computed directly from the
    %                                    Var-finite raw data:
    %                                    total_NU / total_calibrators.
    %                                    Bins are not used for this
    %                                    number; they are used only to
    %                                    identify where the local rate
    %                                    exceeds the threshold.
    %                'quantile'       - Q-th percentile of the rate
    %                                    vector (Q given by QuantileLevel,
    %                                    default 0.84). Each window is
    %                                    one data point of the same size,
    %                                    so the percentile is unweighted.
    %                numeric in (0,1) - fixed cutoff, e.g. 0.5 for
    %                                    "more than half rejected".
    %
    % Input  : - Au struct returned by
    %            pipeline.last.quality.photCalib.calibratorAudit.
    %          * ...,key,val,...
    %            'Var'          - column in Au.Pool to use. Required.
    %            'WindowSize'   - calibrators per window. Default 10.
    %            'WindowStep'   - step between window starts. Default 1
    %                             (fully sliding); equal to WindowSize
    %                             gives disjoint windows.
    %            'Threshold'    - 'mean' (default = population rate
    %                             NU/Total) | 'quantile' | numeric in
    %                             (0,1).
    %            'QuantileLevel'- quantile level when Threshold =
    %                             'quantile'. Default 0.84.
    %            'OutDir'       - directory for figure / .mat. Default  F1 = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                   Au, 'Var', 'LastNearestDist_as');
    %                             '~/results_calib_audit'.
    %            'Plot','SaveFig','Verbose' - logical defaults true.
    % Output : - Result struct with:
    %            .Curve     - table per window: WindowStart, WindowEnd,
    %                          VarLow, VarHigh, VarCenter, NUsed, NNU,
    %                          NTotal (=WindowSize), Rate.
    %            .BadWindowIdx - row indices into .Curve marking bad
    %                          scope.
    %            .BadScope  - Kx2 numeric matrix of merged contiguous
    %                          [lo hi] ranges in Var units.
    %            .InBadMask - logical vector of length height(Au.Pool):
    %                          true where the row's Var falls in any
    %                          BadScope range; false for non-finite Var
    %                          rows. Combine across multiple runs with
    %                          & or | to get joint masks.
    %            .FractionInBadScope - scalar f in [0,1].
    %            .NUTotal   - count of finite NU rows in Var.
    %            .NUInBad   - count of NU rows whose Var falls in any
    %                          bad-scope range.
    %            .Threshold - resolved numeric threshold actually used.
    %            .ThresholdMode - echo of the user input ('mean' /
    %                          'quantile' / numeric).
    %            .Var,.WindowSize,.WindowStep - echoes.
    %            .Args      - full echo.
    % Author : D. Kovaleva (May 2026)
    % Example: % Default sliding 10-calibrator window, population-rate threshold:
    %          F = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                  Au, 'Var', 'LastNearestDist_as');
    %
    %          % Disjoint 25-calibrator windows (quantile-bin equivalent):
    %          F = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                  Au, 'Var', 'Gaia_BPRP', ...
    %                  'WindowSize', 25, 'WindowStep', 25);
    %
    %          % 84% quantile of the per-window rate distribution:
    %          F = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                  Au, 'Var', 'LastNearestDist_as', ...
    %                  'Threshold', 'quantile');
    %
    %          % Combine bad-scope masks across several variables.
    %          % InBadMask is aligned with Au.Pool rows, so & / | work
    %          % directly. Non-finite Var rows are false in each mask.
    %          F1 = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                   Au, 'Var', 'LastNearestDist_as');
    %          F2 = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                   Au, 'Var', 'MagErr');
    %          F3 = pipeline.last.quality.photCalib.calibratorRejectFraction( ...
    %                   Au, 'Var', 'Gaia_BPRP');
    %          NU      = ~Au.Pool.Used;
    %          NUTotal = nnz(NU);
    %          M_AND   = F1.InBadMask & F2.InBadMask & F3.InBadMask;  % bad in ALL
    %          M_OR    = F1.InBadMask | F2.InBadMask | F3.InBadMask;  % bad in ANY
    %          fprintf('NU in bad scope of ALL: %d/%d (%.3f)\n', ...
    %                  nnz(NU & M_AND), NUTotal, nnz(NU & M_AND)/NUTotal);
    %          fprintf('NU in bad scope of ANY: %d/%d (%.3f)\n', ...
    %                  nnz(NU & M_OR ), NUTotal, nnz(NU & M_OR )/NUTotal);
    %
    %          % "Bad in at least 2 of 3" via integer count of mask hits:
    %          K     = double(F1.InBadMask) + double(F2.InBadMask) + double(F3.InBadMask);
    %          M_2of3 = K >= 2;
    %          fprintf('NU bad in >=2 of 3 params: %d/%d\n', ...
    %                  nnz(NU & M_2of3), NUTotal);

    arguments
        Au struct
        Args.Var           (1,:) char
        Args.WindowSize    (1,1) double {mustBePositive, mustBeInteger} = 50
        Args.WindowStep    (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.Threshold     = 'mean'
        Args.QuantileLevel (1,1) double {mustBeInRange(Args.QuantileLevel, 0, 1)} = 0.84
        Args.OutDir        {mustBeText} = '~/results_calib_audit'
        Args.Plot          logical = true
        Args.SaveFig       logical = true
        Args.Verbose       logical = true
    end

    if ~isfield(Au, 'Pool') || ~istable(Au.Pool)
        error('calibratorRejectFraction:NoPool', 'Au.Pool table is missing.');
    end
    if ~ismember(Args.Var, Au.Pool.Properties.VariableNames)
        error('calibratorRejectFraction:UnknownVar', ...
            'Pool has no column "%s". Available: %s', ...
            Args.Var, strjoin(Au.Pool.Properties.VariableNames, ', '));
    end

    % --- Pull and clean ---
    V  = Au.Pool.(Args.Var);
    U  = logical(Au.Pool.Used);
    Fin = isfinite(V);
    V = V(Fin);
    U = U(Fin);
    NU = ~U;

    if numel(V) < Args.WindowSize
        error('calibratorRejectFraction:Empty', ...
            'Only %d finite Var=%s values; need at least WindowSize=%d.', ...
            numel(V), Args.Var, Args.WindowSize);
    end

    % --- Sort by Var, slide window ---
    [Vs, Idx] = sort(V);
    Us  = U(Idx);
    NUs = ~Us;

    Starts = (1:Args.WindowStep:(numel(Vs) - Args.WindowSize + 1)).';
    Ends   = Starts + Args.WindowSize - 1;
    Nw     = numel(Starts);

    NUsed  = zeros(Nw, 1);
    NNU    = zeros(Nw, 1);
    VarLow    = zeros(Nw, 1);
    VarHigh   = zeros(Nw, 1);
    VarCenter = zeros(Nw, 1);
    for I = 1:Nw
        S = Starts(I); E = Ends(I);
        NUsed(I)  = sum(Us(S:E));
        NNU(I)    = sum(NUs(S:E));
        VarLow(I)    = Vs(S);
        VarHigh(I)   = Vs(E);
        VarCenter(I) = median(Vs(S:E));
    end
    NTotal = NUsed + NNU;
    Rate   = NNU ./ NTotal;

    Curve = table(Starts, Ends, VarLow, VarHigh, VarCenter, ...
        NUsed, NNU, NTotal, Rate, ...
        'VariableNames', {'WindowStart','WindowEnd','VarLow','VarHigh', ...
                          'VarCenter','NUsed','NNU','NTotal','Rate'});

    % --- Resolve threshold ---
    if ischar(Args.Threshold) || isstring(Args.Threshold)
        switch lower(char(Args.Threshold))
            case 'mean'
                Threshold = sum(NU) / max(numel(V), 1);
            case 'quantile'
                Threshold = quantile(Rate, Args.QuantileLevel);
            otherwise
                error('calibratorRejectFraction:BadThreshold', ...
                    'Threshold must be ''mean'', ''quantile'', or a number in (0,1).');
        end
    else
        Threshold = double(Args.Threshold);
        if ~(isscalar(Threshold) && Threshold > 0 && Threshold < 1)
            error('calibratorRejectFraction:BadThreshold', ...
                'Numeric Threshold must be a scalar in (0,1).');
        end
    end

    % --- Bad-scope: windows above threshold ---
    %   Each window's "responsibility strip" is centred on its VarCenter
    %   and reaches halfway to each neighbour's VarCenter. This keeps
    %   the bad-scope range visually aligned with where the rate curve
    %   sits above the threshold, instead of overshooting via the much
    %   wider [VarLow, VarHigh] span of the underlying calibrators.
    BadWindowIdx = find(Rate > Threshold);
    [WLo, WHi]   = windowStrips(VarCenter);
    BadScope     = mergeRanges(WLo(BadWindowIdx), WHi(BadWindowIdx));

    % --- Per-row "in bad scope" mask, aligned with Au.Pool height ---
    %   Non-finite Var rows are false (excluded from the analysis).
    InBadFull = false(height(Au.Pool), 1);
    if ~isempty(BadScope)
        InBadFin = false(numel(V), 1);
        for K = 1:size(BadScope, 1)
            InBadFin = InBadFin | (V >= BadScope(K, 1) & V <= BadScope(K, 2));
        end
        FinIdx = find(Fin);
        InBadFull(FinIdx(InBadFin)) = true;
    end

    % --- Fraction of NU in bad scope ---
    NUFull  = ~logical(Au.Pool.Used);
    NUInBad = sum(NUFull & InBadFull);
    NUTotal = sum(NU);
    if NUTotal > 0
        FractionInBadScope = NUInBad / NUTotal;
    else
        FractionInBadScope = NaN;
    end

    Result.Curve              = Curve;
    Result.BadWindowIdx       = BadWindowIdx;
    Result.BadScope           = BadScope;
    Result.InBadMask          = InBadFull;     % logical, length = height(Au.Pool)
    Result.FractionInBadScope = FractionInBadScope;
    Result.NUTotal            = NUTotal;
    Result.NUInBad            = NUInBad;
    Result.Threshold          = Threshold;
    Result.ThresholdMode      = Args.Threshold;
    Result.Var                = Args.Var;
    Result.WindowSize         = Args.WindowSize;
    Result.WindowStep         = Args.WindowStep;
    Result.Args               = Args;

    if Args.Verbose
        fprintf('calibratorRejectFraction(%s, WindowSize=%d, Step=%d):\n', ...
            Args.Var, Args.WindowSize, Args.WindowStep);
        fprintf('  windows = %d   total finite Var = %d\n', Nw, numel(V));
        fprintf('  threshold mode = %s  -> %.4f\n', ...
            char(string(Args.Threshold)), Threshold);
        fprintf('  bad windows (rate > %.4f): %d', Threshold, numel(BadWindowIdx));
        if ~isempty(BadScope)
            Strs = arrayfun(@(k) sprintf('[%.4g, %.4g]', ...
                BadScope(k,1), BadScope(k,2)), 1:size(BadScope,1), ...
                'UniformOutput', false);
            fprintf('  -> %s', strjoin(Strs, ' U '));
        end
        fprintf('\n  NU total = %d  NU in bad scope = %d  fraction = %.3f\n', ...
            NUTotal, NUInBad, FractionInBadScope);
    end

    if Args.Plot
        plotRateCurve(Curve, BadWindowIdx, Args, Result);
    end
    if Args.SaveFig
        try
            OutDir = char(Args.OutDir);
            if ~exist(OutDir, 'dir'); mkdir(OutDir); end
            File = fullfile(OutDir, sprintf('reject_rate_%s.mat', Args.Var));
            save(File, 'Result', '-v7.3');
        catch ME
            warning('calibratorRejectFraction:SaveFailed', ...
                'Save failed: %s', ME.message);
        end
    end
end

% =========================================================================
function [Lo, Hi] = windowStrips(VarCenter)
    % Per-window "responsibility strip" around VarCenter: half-step to
    % each neighbour. Boundary windows extend to the edge VarCenter.
    VC = VarCenter(:);
    N  = numel(VC);
    Lo = nan(N, 1); Hi = nan(N, 1);
    if N == 0; return; end
    if N == 1
        Lo(1) = VC(1); Hi(1) = VC(1); return;
    end
    Mid = (VC(1:end-1) + VC(2:end)) / 2;
    Lo(1)     = VC(1);
    Lo(2:end) = Mid;
    Hi(1:end-1) = Mid;
    Hi(end)     = VC(end);
end

% =========================================================================
function R = mergeRanges(Lo, Hi)
    % Merge [Lo Hi] pairs that touch or overlap (sorted ascending) into a
    % minimal Kx2 set of contiguous ranges.
    if isempty(Lo); R = zeros(0, 2); return; end
    [Lo, Idx] = sort(Lo); Hi = Hi(Idx);
    R = [Lo(1), Hi(1)];
    for I = 2:numel(Lo)
        if Lo(I) <= R(end, 2)
            R(end, 2) = max(R(end, 2), Hi(I));
        else
            R(end+1, :) = [Lo(I), Hi(I)]; %#ok<AGROW>
        end
    end
end

% =========================================================================
function plotRateCurve(Curve, BadWindowIdx, Args, Result)
    F = figure('Name', sprintf('rejection rate vs %s', Args.Var), ...
        'Position', [80 80 850 520]);
    hold on;
    % Bad-window shading: thin strip centred on each VarCenter (half-step
    % to each neighbour), so the shading aligns with where the curve
    % sits above the threshold.
    [WLo, WHi] = windowStrips(Curve.VarCenter);
    YL = [0 1];
    for K = 1:numel(BadWindowIdx)
        I = BadWindowIdx(K);
        XL = [WLo(I), WHi(I)];
        patch([XL fliplr(XL)], [YL(1) YL(1) YL(2) YL(2)], ...
            [0.95 0.85 0.85], 'EdgeColor', 'none', 'FaceAlpha', 0.5, ...
            'HandleVisibility', 'off');
    end
    plot(Curve.VarCenter, Curve.Rate, '-', ...
        'Color', [0.10 0.10 0.10], 'LineWidth', 1.2, ...
        'DisplayName', sprintf('rate (NU/%d)', Args.WindowSize));
    yline(Result.Threshold, '--', ...
        sprintf('threshold (%s) = %.3f', ...
            char(string(Result.ThresholdMode)), Result.Threshold), ...
        'Color', [0.55 0.10 0.10], 'LineWidth', 1.0, ...
        'LabelHorizontalAlignment', 'left', ...
        'HandleVisibility', 'off');
    ylim([0 1]);
    xlabel(Args.Var, 'Interpreter', 'none');
    ylabel('Rejection rate', 'Interpreter', 'none');
    title(sprintf(['Rejection rate vs %s   (window=%d step=%d, ', ...
        'bad windows=%d, fraction of NU in bad scope = %.3f)'], ...
        Args.Var, Args.WindowSize, Args.WindowStep, ...
        numel(BadWindowIdx), Result.FractionInBadScope), ...
        'Interpreter', 'none');
    legend('Location', 'best', 'Interpreter', 'none');
    grid on; box on;
    if Args.SaveFig
        OutDir = char(Args.OutDir);
        if ~exist(OutDir, 'dir'); mkdir(OutDir); end
        Base = fullfile(OutDir, sprintf('reject_rate_%s', Args.Var));
        savefig(F, [Base '.fig']);
        saveas(F,  [Base '.png']);
    end
end
