%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_findPlans.m
% Author      : Chen Tishler
% Created     : 10/06/2026
% Updated     : 22/06/2026
% Description : Debug for LcsHelper_v4_findPlans — bidirectional LCS plan
%               scanner centered on a given start date.
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_findPlans()
%==========================================================================

function debug_LcsHelper_v4_findPlans()
    % Run basic, limited-radius, and empty-result findPlans smoke tests.

    fprintf('========== DEBUG LcsHelper_v4_findPlans ==========\n');

    % --- Ensure ASTROPACK_DATA_PATH is set ---
    debug_ensureDataPath();

    % --- Sub-tests ---
    debug_LcsHelper_v4_findPlans_basic();
    debug_LcsHelper_v4_findPlans_limitedRadius();
    debug_LcsHelper_v4_findPlans_emptyResult();

    fprintf('========== DEBUG LcsHelper_v4_findPlans DONE ==========\n');
end

% -------------------------------------------------------------------------

function debug_LcsHelper_v4_findPlans_basic()
    % Standard call: find 2 plans before and 2 plans on/after 2029-01-05.

    fprintf('\n--- debug_LcsHelper_v4_findPlans_basic ---\n');

    Plans = ultrasat.planner.LcsHelper_v4_findPlans(datetime('2029-01-05'), 2);

    if ~istable(Plans)
        error('debug_LcsHelper_v4_findPlans_basic: expected a table result');
    end

    % --- Verify expected output schema ---
    expectedCols = {'plan_start_date', 'offset_days', 'status', ...
        'num_observations', 'nA', 'nB', 'nC', 'nD', 'variant_used', 'detail'};
    for I = 1:numel(expectedCols)
        if ~ismember(expectedCols{I}, Plans.Properties.VariableNames)
            error('debug_LcsHelper_v4_findPlans_basic: missing column "%s"', expectedCols{I});
        end
    end

    fprintf('Returned %d plan(s):\n', height(Plans));
    disp(Plans(:, {'plan_start_date', 'offset_days', 'status', 'num_observations', 'variant_used'}));

    if height(Plans) > 0
        % --- Feasible plans should be sorted and have positive counts ---
        FeasibleMask = strcmp(Plans.status, 'FEASIBLE');
        nBefore = sum(Plans.offset_days(FeasibleMask) < 0);
        nAfter  = sum(Plans.offset_days(FeasibleMask) >= 0);
        fprintf('Feasible: %d before, %d on/after start date\n', nBefore, nAfter);

        if any(FeasibleMask)
            if any(Plans.num_observations(FeasibleMask) <= 0)
                error('debug_LcsHelper_v4_findPlans_basic: feasible plan has zero observations');
            end
            if any(Plans.variant_used(FeasibleMask) <= 0)
                error('debug_LcsHelper_v4_findPlans_basic: feasible plan has variant_used=0');
            end
        end

        if ~issorted(Plans.offset_days)
            error('debug_LcsHelper_v4_findPlans_basic: result table is not sorted by offset_days');
        end
    end

    fprintf('debug_LcsHelper_v4_findPlans_basic: OK\n');
end

% -------------------------------------------------------------------------

function debug_LcsHelper_v4_findPlans_limitedRadius()
    % Small radius — may find fewer than NumPlans per direction.

    fprintf('\n--- debug_LcsHelper_v4_findPlans_limitedRadius ---\n');

    Plans = ultrasat.planner.LcsHelper_v4_findPlans(datetime('2029-01-05'), 1, 'MaxRadius', 3);

    if ~istable(Plans)
        error('debug_LcsHelper_v4_findPlans_limitedRadius: expected a table result');
    end

    % --- All feasible offsets must stay within MaxRadius ---
    FeasibleMask = strcmp(Plans.status, 'FEASIBLE');
    nBefore = sum(Plans.offset_days(FeasibleMask) < 0);
    nAfter  = sum(Plans.offset_days(FeasibleMask) >= 0);

    if any(abs(Plans.offset_days(FeasibleMask)) > 3)
        error('debug_LcsHelper_v4_findPlans_limitedRadius: result contains offset beyond MaxRadius=3');
    end

    fprintf('Returned %d plan(s) within ±3 days (%d before, %d after)\n', ...
        sum(FeasibleMask), nBefore, nAfter);
    fprintf('debug_LcsHelper_v4_findPlans_limitedRadius: OK\n');
end

% -------------------------------------------------------------------------

function debug_LcsHelper_v4_findPlans_emptyResult()
    % MaxRadius=0: only tries StartDate itself; validates empty-table schema.

    fprintf('\n--- debug_LcsHelper_v4_findPlans_emptyResult ---\n');

    Plans = ultrasat.planner.LcsHelper_v4_findPlans(datetime('2029-01-05'), 2, ...
        'MaxRadius', 0, 'Verbose', false);

    if ~istable(Plans)
        error('debug_LcsHelper_v4_findPlans_emptyResult: expected a table result');
    end

    % --- Schema must be present even when no feasible plans are found ---
    expectedCols = {'plan_start_date', 'offset_days', 'status', ...
        'num_observations', 'nA', 'nB', 'nC', 'nD', 'variant_used', 'detail'};
    for I = 1:numel(expectedCols)
        if ~ismember(expectedCols{I}, Plans.Properties.VariableNames)
            error('debug_LcsHelper_v4_findPlans_emptyResult: missing column "%s"', expectedCols{I});
        end
    end

    fprintf('With MaxRadius=0: returned %d plan(s) (only offset=0 tried)\n', height(Plans));
    fprintf('debug_LcsHelper_v4_findPlans_emptyResult: OK\n');
end

% -------------------------------------------------------------------------

function debug_ensureDataPath()
    % Set ASTROPACK_DATA_PATH to a local fallback when unset.

    if ~isempty(getenv('ASTROPACK_DATA_PATH'))
        return;
    end
    fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
    if ispc
        setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
    else
        setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
    end
end
