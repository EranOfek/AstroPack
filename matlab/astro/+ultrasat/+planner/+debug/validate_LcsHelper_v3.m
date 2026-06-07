%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.validate_LcsHelper_v3.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 07/06/2026
% Description : Validate LcsHelper_v3 schedule output against formal LCS
%               rules (Sets A/B/C/D, 45-day windows, slot budget, etc.).
% Run by      : ultrasat.planner.debug.validate_LcsHelper_v3()
%==========================================================================

function validate_LcsHelper_v3()

    fprintf('========== VALIDATE LcsHelper_v3 ==========\n');

    % ---- Build the planner and run the full pipeline --------------------
    Obj = local_build_helper();

    if isempty(Obj.Schedule) || height(Obj.Schedule) == 0
        error('validate_LcsHelper_v3: Schedule is empty (pipeline failed)');
    end

    fprintf('Input CSV : %s\n', local_gridFile());
    fprintf('StartDate : %s\n', datestr(Obj.StartDate));
    fprintf('SetC_start_ind : %d\n', Obj.SetC_start_ind);
    fprintf('Schedule rows  : %d\n\n', height(Obj.Schedule));

    % ---- Run validation sections ----------------------------------------
    TotalFail = 0;
    TotalPass = 0;

    CheckList = {
        @() local_check_field_counts(Obj)
        @() local_check_setA(Obj)
        @() local_check_setB(Obj)
        @() local_check_setC(Obj)
        @() local_check_setD(Obj)
        @() local_check_slot_budget(Obj)
        @() local_check_window_bounds(Obj)
        @() local_check_no_duplicates(Obj)
        @() local_check_daily_schedule(Obj)
    };

    for k = 1:numel(CheckList)
        [nFail, nPass] = CheckList{k}();
        TotalFail = TotalFail + nFail;
        TotalPass = TotalPass + nPass;
    end

    % ---- Summary --------------------------------------------------------
    fprintf('\n========== SUMMARY ==========\n');
    fprintf('%d checks passed, %d failed\n', TotalPass, TotalFail);

    if TotalFail > 0
        error('validate_LcsHelper_v3: %d validation check(s) failed', TotalFail);
    end

    fprintf('validate_LcsHelper_v3: ALL CHECKS PASSED\n');
end


% =========================================================================
% BUILD HELPER
% =========================================================================

function Obj = local_build_helper()
    % Construct LcsHelper_v3 and run prep + full schedule (including SetD).
    CsvFile = local_gridFile();

    Obj = ultrasat.planner.LcsHelper_v3( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', CsvFile, ...
        'Verbose', false, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    local_assert_pipeline_complete(Obj);
end


function local_assert_pipeline_complete(Obj)
    % Fail fast when categorize_then_schedule did not finish successfully.
    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);

    PipelineOk = (nA == Obj.SetAnumel) && ...
                 (nB == 3 * Obj.SetBnumel) && ...
                 (nC == Obj.SetCnumel) && ...
                 ~isempty(Obj.Daily_schedule);

    if PipelineOk
        return
    end

    error(['validate_LcsHelper_v3: LcsHelper_v3 did not produce a complete schedule.\n' ...
           '  SetA: %d/%d, SetB rows: %d/%d, SetC: %d/%d, Daily_schedule empty: %d\n' ...
           '  Check LcsHelper_v3 warnings above (e.g. categorize_then_schedule failure).'], ...
        nA, Obj.SetAnumel, nB, 3 * Obj.SetBnumel, nC, Obj.SetCnumel, isempty(Obj.Daily_schedule));
end


function GridFile = local_gridFile()
    % Resolve the bundled LCS grid CSV relative to this script (no env var).
    ThisDir  = fileparts(mfilename('fullpath'));
    GridFile = fullfile(ThisDir, '..', 'data', 'LCS_nonoverlapping_grid_surveys.csv');
    if ~isfile(GridFile)
        error('validate_LcsHelper_v3: grid file not found: %s', GridFile);
    end
end


% =========================================================================
% PASS / FAIL HELPERS
% =========================================================================

function [nFail, nPass] = local_pass(CheckName)
    fprintf('  [PASS] %s\n', CheckName);
    nFail = 0;
    nPass = 1;
end


function [nFail, nPass] = local_fail(CheckName, Msg)
    fprintf('  [FAIL] %s: %s\n', CheckName, Msg);
    nFail = 1;
    nPass = 0;
end


function [nFail, nPass] = local_run_checks(SectionName, CheckFns)
    % Run a list of check functions and aggregate pass/fail counts.
    fprintf('\n--- %s ---\n', SectionName);
    nFail = 0;
    nPass = 0;
    for k = 1:numel(CheckFns)
        [f, p] = CheckFns{k}();
        nFail = nFail + f;
        nPass = nPass + p;
    end
end


% =========================================================================
% FIELD COUNTS
% =========================================================================

function [nFail, nPass] = local_check_field_counts(Obj)
    % Verify the expected number of placed fields per set.
    % SetB produces 3 schedule rows per field (1x B_45 + 2x B_90).
    nFail = 0;
    nPass = 0;

    MaskA = strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0;
    MaskB = ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0;
    MaskC = strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0;
    MaskD = strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0;

    nA = sum(MaskA);
    nB = sum(MaskB);
    nC = sum(MaskC);
    nD = sum(MaskD);

    CheckFns = {
        @() local_assert_equal('SetA field count', nA, Obj.SetAnumel)
        @() local_assert_equal('SetB row count (3 per field)', nB, 3 * Obj.SetBnumel)
        @() local_assert_equal('SetC field count', nC, Obj.SetCnumel)
        @() local_assert_max('SetD field count', nD, Obj.SetDnumel)
    };

    [nFail, nPass] = local_run_checks('Field counts', CheckFns);
end


% =========================================================================
% SET A  (48 fields, 45-day windows, 6 groups x 8 slots)
% =========================================================================

function [nFail, nPass] = local_check_setA(Obj)
    % SetA: daily-cadence 45-day blocks in 6 parallel groups of 8 slots.
    % Fields moved by SetD pre-clean use group >= 7 and Full_windows alignment.
    nFail = 0;
    nPass = 0;

    SchedA = Obj.Schedule(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;

    CheckFns = {
        @() local_assert_unique('SetA unique field IDs', SchedA.Field)
        @() local_assert_all('SetA window length = Min_window (45d)', ...
            SchedA.end - SchedA.start + 1 == L)
    };

    % Original groups 1..6: each group should hold exactly 8 placed fields.
    CheckFns{end+1} = @() local_check_setA_group_counts(SchedA, Obj.SetA_Nwindows); %#ok<AGROW>

    % Moved SetA rows (group >= 7): ind must be a Full_windows index and
    % start/end must match that window (SetD bump moves use this convention).
    MovedMask = SchedA.group >= 7;
    if any(MovedMask)
        Moved = SchedA(MovedMask, :);
        CheckFns{end+1} = @() local_assert_all( ...
            'SetA moved rows: start matches Full_windows.start(ind)', ...
            Moved.start == Obj.Full_windows.start(Moved.ind)'); %#ok<AGROW>
        CheckFns{end+1} = @() local_assert_all( ...
            'SetA moved rows: end matches Full_windows.end(ind)', ...
            Moved.end == Obj.Full_windows.end(Moved.ind)'); %#ok<AGROW>
    end

    [nFail, nPass] = local_run_checks('Set A (45-day, groups 1-6)', CheckFns);
end


function [nFail, nPass] = local_check_setA_group_counts(SchedA, SetANwindows)
    % Each original SetA group (1..6) must contain exactly 8 placed fields.
    nFail = 0;
    nPass = 0;
    for G = 1:SetANwindows
        nInGroup = sum(SchedA.group == G);
        [f, p] = local_assert_equal( ...
            sprintf('SetA group %d has 8 fields', G), nInGroup, 8);
        nFail = nFail + f;
        nPass = nPass + p;
    end
end


% =========================================================================
% SET B  (16 fields x 3 rows: 1 B_45 + 2 B_90, 135-day super-pattern)
% =========================================================================

function [nFail, nPass] = local_check_setB(Obj)
    % Each SetB field occupies three Full_windows indices spanning 135 days:
    % one high-cadence block (B_45) and two low-cadence blocks (B_90).
    nFail = 0;
    nPass = 0;

    SchedB = Obj.Schedule(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & ...
        Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;
    Ninds = height(Obj.Full_windows);

    CheckFns = {
        @() local_assert_all('SetB row window length = 45d', ...
            SchedB.end - SchedB.start + 1 == L)
        @() local_assert_all('SetB rows align with Full_windows boundaries', ...
            ismember(SchedB.start, Obj.Full_windows.start) & ...
            ismember(SchedB.end, Obj.Full_windows.end))
    };

    % Per-field structure: exactly 1 B_45 + 2 B_90, three distinct fw_inds.
    FieldsB = unique(SchedB.Field);
    CheckFns{end+1} = @() local_assert_equal( ...
        'SetB unique field count', numel(FieldsB), Obj.SetBnumel); %#ok<AGROW>
    CheckFns{end+1} = @() local_check_setB_all_fields(SchedB, L, Ninds); %#ok<AGROW>

    [nFail, nPass] = local_run_checks('Set B (B_45 + 2x B_90, 135d pattern)', CheckFns);
end


function [nFail, nPass] = local_check_setB_all_fields(SchedB, L, Ninds)
    % Per-field SetB checks (loop kept in a function to avoid closure issues).
    nFail = 0;
    nPass = 0;
    FieldsB = unique(SchedB.Field);

    for F = FieldsB(:)'
        Rows = SchedB(SchedB.Field == F, :);
        nB45 = sum(strcmp(Rows.category, 'B_45'));
        nB90 = sum(strcmp(Rows.category, 'B_90'));
        FwInds = local_setB_fwInds(Rows);
        SpanDays = max(Rows.start) - min(Rows.start) + L;

        [f, p] = local_assert_equal(sprintf('SetB field %d: 1 B_45 row', F), nB45, 1);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = local_assert_equal(sprintf('SetB field %d: 2 B_90 rows', F), nB90, 2);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = local_assert_equal( ...
            sprintf('SetB field %d: 3 distinct Full_windows inds', F), ...
            numel(unique(FwInds)), 3);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = local_assert_all( ...
            sprintf('SetB field %d: fw_inds in 1..%d', F, Ninds), ...
            FwInds >= 1 & FwInds <= Ninds);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = local_assert_equal( ...
            sprintf('SetB field %d: 135-day span', F), SpanDays, 3 * L);
        nFail = nFail + f; nPass = nPass + p;
    end
end


function FwInds = local_setB_fwInds(Rows)
    % Extract Full_windows index from B_45/B_90 group encoding (100+k / 200+k).
    FwInds = zeros(height(Rows), 1);
    for R = 1:height(Rows)
        if strcmp(Rows.category{R}, 'B_45')
            FwInds(R) = Rows.group(R) - 100;
        else
            FwInds(R) = Rows.group(R) - 200;
        end
    end
end


% =========================================================================
% SET C  (16 fields, 2 super-windows of 135 days, groups 11/12)
% =========================================================================

function [nFail, nPass] = local_check_setC(Obj)
    % SetC: 8 fields per 135-day super-window; observed every 4 days.
    % Super-windows start at Full_windows.start(SetC_start_ind) and +135d.
    nFail = 0;
    nPass = 0;

    SchedC = Obj.Schedule(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0, :);
    Lsuper = 3 * Obj.Min_window;
    Sci = Obj.SetC_start_ind;

    ExpectedS1 = Obj.Full_windows.start(Sci);
    ExpectedS2 = ExpectedS1 + Lsuper;

    CheckFns = {
        @() local_assert_unique('SetC unique field IDs', SchedC.Field)
        @() local_assert_all('SetC window length = 135d', ...
            SchedC.end - SchedC.start + 1 == Lsuper)
        @() local_assert_all('SetC start days are super-window anchors', ...
            ismember(SchedC.start, [ExpectedS1; ExpectedS2]))
        @() local_assert_equal('SetC group 11 field count', sum(SchedC.group == 11), 8)
        @() local_assert_equal('SetC group 12 field count', sum(SchedC.group == 12), 8)
        @() local_assert_all('SetC groups are 11 or 12 only', ...
            ismember(SchedC.group, [11, 12]))
        @() local_assert_all('SetC ind in 1..8 (cadence slot)', ...
            SchedC.ind >= 1 & SchedC.ind <= 8)
    };

    [nFail, nPass] = local_run_checks('Set C (2 x 135-day super-windows)', CheckFns);
end


% =========================================================================
% SET D  (up to 4 fields, 45-day windows, group 301..304)
% =========================================================================

function [nFail, nPass] = local_check_setD(Obj)
    % SetD: optional high-priority fields placed into open Full_windows slots.
    nFail = 0;
    nPass = 0;

    SchedD = Obj.Schedule(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;

    CheckFns = {
        @() local_assert_max('SetD placed field count', height(SchedD), Obj.SetDnumel)
    };

    if height(SchedD) > 0
        CheckFns{end+1} = @() local_assert_unique('SetD unique field IDs', SchedD.Field); %#ok<AGROW>
        CheckFns{end+1} = @() local_assert_all('SetD window length = 45d', ...
            SchedD.end - SchedD.start + 1 == L); %#ok<AGROW>
        CheckFns{end+1} = @() local_assert_all('SetD group encoding 301..304', ...
            SchedD.group >= 301 & SchedD.group <= 300 + Obj.SetDnumel); %#ok<AGROW>
        CheckFns{end+1} = @() local_assert_all('SetD ind matches Full_windows row', ...
            SchedD.start == Obj.Full_windows.start(SchedD.ind)' & ...
            SchedD.end == Obj.Full_windows.end(SchedD.ind)'); %#ok<AGROW>
        % group = 300 + slot_in_setD (1..4); ind = Full_windows index (not slot).
        CheckFns{end+1} = @() local_assert_unique( ...
            'SetD unique group slots (301..304)', SchedD.group); %#ok<AGROW>
    end

    [nFail, nPass] = local_run_checks('Set D (up to 4 fields, 45d)', CheckFns);
end


% =========================================================================
% SLOT BUDGET  (filled(k) <= 11, n4 divisible by 4)
% =========================================================================

function [nFail, nPass] = local_check_slot_budget(Obj)
    % Replicate LcsHelper_v3 slot-budget convention:
    %   filled(k) = nA(k) + nB45(k) + n4(k)/4  <= Daily_LCS_slots (11)
    %   n4(k) = nB90(k) + nC(k) must be divisible by 4.
    % SetD rows are excluded (placed into open slots after A/B/C balance).
    nFail = 0;
    nPass = 0;

    [~, ~, ~, ~, n4, Filled] = local_compute_slot_occupancy( ...
        Obj.Schedule, Obj.Full_windows);

    CheckFns = {
        @() local_check_slot_budget_all_inds(n4, Filled, Obj.Daily_LCS_slots)
        @() local_pass(sprintf('Slot occupancy summary: filled = [%s]', ...
            num2str(Filled, '%d ')))
    };

    [nFail, nPass] = local_run_checks('Slot budget (11 slots per Full_windows ind)', CheckFns);
end


function [nFail, nPass] = local_check_slot_budget_all_inds(n4, Filled, DailyLcsSlots)
    % Per-ind slot budget checks (loop kept in a function to avoid closure issues).
    nFail = 0;
    nPass = 0;
    Ninds = numel(Filled);

    for K = 1:Ninds
        [f, p] = local_assert_divisible( ...
            sprintf('Slot budget ind %d: n4 divisible by 4', K), n4(K), 4);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = local_assert_max( ...
            sprintf('Slot budget ind %d: filled <= Daily_LCS_slots', K), ...
            Filled(K), DailyLcsSlots);
        nFail = nFail + f; nPass = nPass + p;
    end
end


function [nA, nB45, nB90, nC, n4, Filled] = local_compute_slot_occupancy(Schedule, Full_windows)
    % Mirror local_compute_slot_occupancy in LcsHelper_v3.m (SetD excluded).
    Ninds = height(Full_windows);
    nA   = zeros(1, Ninds);
    nB45 = zeros(1, Ninds);
    nB90 = zeros(1, Ninds);
    nC   = zeros(1, Ninds);

    for R = 1:height(Schedule)
        Cat = Schedule.category{R};
        Ind = Schedule.ind(R);
        Grp = Schedule.group(R);

        if strcmp(Cat, 'A')
            if Ind >= 1 && Ind <= Ninds
                nA(Ind) = nA(Ind) + 1;
            end
        elseif strcmp(Cat, 'B_45')
            FwInd = Grp - 100;
            if FwInd >= 1 && FwInd <= Ninds
                nB45(FwInd) = nB45(FwInd) + 1;
            end
        elseif strcmp(Cat, 'B_90')
            FwInd = Grp - 200;
            if FwInd >= 1 && FwInd <= Ninds
                nB90(FwInd) = nB90(FwInd) + 1;
            end
        elseif strcmp(Cat, 'C')
            StartInd = find(Full_windows.start == Schedule.start(R), 1);
            if isempty(StartInd)
                continue
            end
            for Kk = StartInd:min(StartInd + 2, Ninds)
                nC(Kk) = nC(Kk) + 1;
            end
        end
        % SetD: intentionally ignored (same as LcsHelper_v3)
    end

    n4 = nB90 + nC;
    Filled = nA + nB45 + n4 / 4;
end


% =========================================================================
% WINDOW BOUNDS
% =========================================================================

function [nFail, nPass] = local_check_window_bounds(Obj)
    % All schedule rows must lie within the planning horizon.
    nFail = 0;
    nPass = 0;

    Placed = Obj.Schedule(Obj.Schedule.Field > 0, :);

    CheckFns = {
        @() local_assert_min('All rows: start >= First_day', ...
            min(Placed.start), Obj.First_day)
        @() local_assert_max('All rows: end <= Last_day', ...
            max(Placed.end), Obj.Last_day)
    };

    [nFail, nPass] = local_run_checks('Window bounds [First_day, Last_day]', CheckFns);
end


% =========================================================================
% NO CROSS-SET DUPLICATES
% =========================================================================

function [nFail, nPass] = local_check_no_duplicates(Obj)
    % Each field ID may appear in only one set (A, B, C, or D).
    nFail = 0;
    nPass = 0;

    FieldsA = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0));
    FieldsB = unique(Obj.Schedule.Field(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0));
    FieldsC = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0));
    FieldsD = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0));

    CheckFns = {
        @() local_assert_disjoint('SetA vs SetB field IDs', FieldsA, FieldsB)
        @() local_assert_disjoint('SetA vs SetC field IDs', FieldsA, FieldsC)
        @() local_assert_disjoint('SetA vs SetD field IDs', FieldsA, FieldsD)
        @() local_assert_disjoint('SetB vs SetC field IDs', FieldsB, FieldsC)
        @() local_assert_disjoint('SetB vs SetD field IDs', FieldsB, FieldsD)
        @() local_assert_disjoint('SetC vs SetD field IDs', FieldsC, FieldsD)
    };

    [nFail, nPass] = local_run_checks('No cross-set field duplicates', CheckFns);
end


% =========================================================================
% DAILY SCHEDULE
% =========================================================================

function [nFail, nPass] = local_check_daily_schedule(Obj)
    % calcDailySchedule must produce a non-empty day x slot matrix.
    nFail = 0;
    nPass = 0;

    ExpectedRows = Obj.Last_day - Obj.First_day + 1;
    ExpectedCols = Obj.Daily_LCS_slots;

    CheckFns = {
        @() local_assert_true('Daily_schedule is not empty', ~isempty(Obj.Daily_schedule))
        @() local_assert_equal('Daily_schedule row count', ...
            size(Obj.Daily_schedule, 1), ExpectedRows)
        @() local_assert_equal('Daily_schedule column count (slots)', ...
            size(Obj.Daily_schedule, 2), ExpectedCols)
    };

    if ~isempty(Obj.Daily_schedule)
        nObserved = sum(~isnan(Obj.Daily_schedule(:)));
        CheckFns{end+1} = @() local_assert_true( ...
            'Daily_schedule contains observations', nObserved > 0); %#ok<AGROW>
    end

    [nFail, nPass] = local_run_checks('Daily schedule matrix', CheckFns);
end


% =========================================================================
% GENERIC ASSERTIONS
% =========================================================================

function [nFail, nPass] = local_assert_equal(CheckName, Actual, Expected)
    if isequal(Actual, Expected)
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('expected %s, got %s', local_fmt(Expected), local_fmt(Actual)));
    end
end


function [nFail, nPass] = local_assert_max(CheckName, Actual, Limit)
    if Actual <= Limit
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('expected <= %s, got %s', local_fmt(Limit), local_fmt(Actual)));
    end
end


function [nFail, nPass] = local_assert_min(CheckName, Actual, Limit)
    if Actual >= Limit
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('expected >= %s, got %s', local_fmt(Limit), local_fmt(Actual)));
    end
end


function [nFail, nPass] = local_assert_true(CheckName, Condition)
    if Condition
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, 'condition is false');
    end
end


function [nFail, nPass] = local_assert_all(CheckName, Mask)
    if all(Mask(:))
        [nFail, nPass] = local_pass(CheckName);
    else
        nBad = sum(~Mask(:));
        [nFail, nPass] = local_fail(CheckName, sprintf('%d row(s) failed', nBad));
    end
end


function [nFail, nPass] = local_assert_unique(CheckName, Values)
    Values = Values(:);
    if numel(unique(Values)) == numel(Values)
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('found %d values, %d unique', numel(Values), numel(unique(Values))));
    end
end


function [nFail, nPass] = local_assert_disjoint(CheckName, A, B)
    Overlap = intersect(A(:), B(:));
    if isempty(Overlap)
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('overlap: %s', mat2str(Overlap(:)')));
    end
end


function [nFail, nPass] = local_assert_divisible(CheckName, Value, Divisor)
    if mod(Value, Divisor) == 0
        [nFail, nPass] = local_pass(CheckName);
    else
        [nFail, nPass] = local_fail(CheckName, ...
            sprintf('value %d is not divisible by %d', Value, Divisor));
    end
end


function S = local_fmt(X)
    if isscalar(X)
        S = num2str(X);
    else
        S = mat2str(X);
    end
end
