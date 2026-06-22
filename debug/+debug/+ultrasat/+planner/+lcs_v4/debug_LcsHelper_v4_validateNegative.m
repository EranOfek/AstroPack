%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_validateNegative.m
% Author      : Chen Tishler
% Created     : 10/06/2026
% Description : Adversarial / negative tests for LcsHelper_v4_validate.
%               Builds one valid plan (Jan 5, 2029), then deliberately
%               corrupts specific aspects of Schedule or Daily_schedule in
%               memory.  Each sub-test asserts the validator returns nFail>0.
%               All 10 sub-tests run to completion regardless of failures.
%               Final summary shows how many corruptions were caught.
%
%               Email review issues confirmed already fixed in v4 validate:
%                 - n4 label  → printed as nCadence4
%                 - moved-group check  → assert_unique on group/ind slots
%                 - extinction ranking  → warn_long_field_extinction_ranking
%                 - SetD ranking  → warn_setD_ranking
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_validateNegative()
%==========================================================================

function debug_LcsHelper_v4_validateNegative()

    fprintf('\n========== DEBUG LcsHelper_v4_validateNegative ==========\n');

    % ---- Build one valid plan (shared by all sub-tests) ------------------
    fprintf('Building valid plan (2029-01-05)...\n');
    ThisDir = fileparts(mfilename('fullpath'));
    RepoRoot = getenv('ASTROPACK_PATH');
    if isempty(RepoRoot)
        error('ASTROPACK_PATH is not set');
    end
    CsvFile = fullfile(RepoRoot, 'matlab', 'astro', '+ultrasat', '+planner', 'data', 'LCS_fields.csv');

    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate',           datetime(2029, 1, 5), ...
        'AllSkyTable',         CsvFile, ...
        'Verbose',             false, ...
        'prep_before_schedule', true, ...
        'build_the_schedule',   true);

    if isempty(Obj) || isempty(Obj.Schedule)
        fprintf('[ERROR] Failed to build valid plan. Aborting adversarial tests.\n');
        return
    end
    fprintf('  done  (Schedule: %d rows, variant: %d)\n\n', height(Obj.Schedule), Obj.Variant_used);

    % ---- Baseline: valid plan must already pass all checks ---------------
    R0 = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    if R0.nFail > 0
        fprintf('[ERROR] Baseline plan already fails validation (%d failures).\n', R0.nFail);
        fprintf('        Adversarial tests require a valid starting plan.\n');
        return
    end
    fprintf('[OK] Baseline: valid plan passes all checks (0 failures).\n\n');

    % ---- Adversarial sub-tests -------------------------------------------
    TestNames = { ...
        'setA_missing_rows', ...
        'setA_duplicate_field', ...
        'setA_wrong_window_length', ...
        'setB_wrong_span', ...
        'setC_wrong_group', ...
        'cross_set_duplicate', ...
        'slot_budget_overflow', ...
        'slot_budget_indivisible', ...
        'window_out_of_bounds', ...
        'daily_schedule_mismatch' ...
    };

    TestFns = { ...
        @() adv_setA_missing_rows(Obj), ...
        @() adv_setA_duplicate_field(Obj), ...
        @() adv_setA_wrong_window_length(Obj), ...
        @() adv_setB_wrong_span(Obj), ...
        @() adv_setC_wrong_group(Obj), ...
        @() adv_cross_set_duplicate(Obj), ...
        @() adv_slot_budget_overflow(Obj), ...
        @() adv_slot_budget_indivisible(Obj), ...
        @() adv_window_out_of_bounds(Obj), ...
        @() adv_daily_schedule_mismatch(Obj) ...
    };

    NTests     = numel(TestNames);
    nCaught    = 0;

    for T = 1:NTests
        Name = TestNames{T};
        fprintf('[NEG %2d/%d] %-35s ... ', T, NTests, Name);
        nFail = TestFns{T}();
        if nFail > 0
            fprintf('caught (nFail=%d)  [OK]\n', nFail);
            nCaught = nCaught + 1;
        else
            fprintf('MISSED (nFail=0)  [FAIL]\n');
        end
    end

    fprintf('\n========== RESULT: %d/%d adversarial tests passed ==========\n', nCaught, NTests);
    if nCaught < NTests
        fprintf('WARNING: %d corruption(s) not caught — validator may miss those cases.\n', NTests - nCaught);
    else
        fprintf('Validator correctly catches all tested corruption types.\n');
    end
end


% =========================================================================
% ADVERSARIAL TEST FUNCTIONS
% Each function: saves original → corrupts → validates → restores → returns nFail
% Tables and matrices are value types in MATLAB, so Orig = Obj.X is a copy.
% =========================================================================


% Test 1: Remove all SetA rows.
%   check_pipeline_complete: nA = 0 ≠ 48  → nFail > 0
function nFail = adv_setA_missing_rows(Obj)
    Orig = Obj.Schedule;
    Obj.Schedule = Obj.Schedule(~strcmp(Obj.Schedule.category, 'A'), :);
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 2: Copy first SetA field ID into second SetA row (duplicate).
%   check_setA: assert_unique on SetA field IDs  → nFail > 0
function nFail = adv_setA_duplicate_field(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxA = find(strcmp(S.category, 'A') & S.Field > 0);
    if numel(IdxA) >= 2
        S.Field(IdxA(2)) = S.Field(IdxA(1));
    end
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 3: Extend one SetA row to a 100-day window (should be 45).
%   check_setA: assert_all window length = 45  → nFail > 0
function nFail = adv_setA_wrong_window_length(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxA = find(strcmp(S.category, 'A') & S.Field > 0, 1);
    S.end(IdxA) = S.start(IdxA) + 99;
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 4: Shift one B_45 row 46 days earlier (breaks 135-day span for that field).
%   check_setB: 135-day span assertion  → nFail > 0
%   Note: window length is preserved (end-start+1 still=45), but start no
%   longer aligns with Full_windows boundaries and 135-day span fails.
function nFail = adv_setB_wrong_span(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxB45 = find(strcmp(S.category, 'B_45') & S.Field > 0, 1);
    S.start(IdxB45) = S.start(IdxB45) - 46;
    S.end(IdxB45)   = S.end(IdxB45)   - 46;
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 5: Set one SetC row's group to 99 (not in any variant's C_blocks).
%   check_setC: assert_all groups match v4 variant blocks  → nFail > 0
function nFail = adv_setC_wrong_group(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxC = find(strcmp(S.category, 'C') & S.Field > 0, 1);
    S.group(IdxC) = 99;
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 6: Copy a SetA field ID into a SetC row (same field in two sets).
%   check_no_duplicates: assert_disjoint(SetA, SetC)  → nFail > 0
function nFail = adv_cross_set_duplicate(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxA = find(strcmp(S.category, 'A') & S.Field > 0, 1);
    IdxC = find(strcmp(S.category, 'C') & S.Field > 0, 1);
    S.Field(IdxC) = S.Field(IdxA);
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 7: Remap all SetA ind=2 rows to ind=1 (total rows unchanged = 48).
%   nA(1) jumps from ~5 to ~12; filledABC(1) > 11
%   check_slot_budget: assert_max filledABC <= 11  → nFail > 0
function nFail = adv_slot_budget_overflow(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    MaskA2 = strcmp(S.category, 'A') & S.Field > 0 & S.ind == 2;
    S.ind(MaskA2) = 1;
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 8: Remap one B_90 row from group>201 to group=201 (fw_ind=1).
%   nB90(1): 0→1; nCadence4(1) = 1; mod(1,4)≠0
%   check_slot_budget: assert_divisible nCadence4 by 4  → nFail > 0
function nFail = adv_slot_budget_indivisible(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxB90 = find(strcmp(S.category, 'B_90') & S.Field > 0 & S.group > 201, 1);
    if ~isempty(IdxB90)
        S.group(IdxB90) = 201;
    end
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 9: Move one row's start 5 days before First_day.
%   check_window_bounds: assert_min start >= First_day  → nFail > 0
function nFail = adv_window_out_of_bounds(Obj)
    Orig = Obj.Schedule;
    S = Obj.Schedule;
    IdxPlaced = find(S.Field > 0, 1);
    S.start(IdxPlaced) = Obj.First_day - 5;
    Obj.Schedule = S;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Schedule = Orig;
end


% Test 10: Replace one non-NaN Daily_schedule cell with a fake field ID.
%   check_daily_schedule: mismatch between Schedule and Daily_schedule  → nFail > 0
function nFail = adv_daily_schedule_mismatch(Obj)
    OrigDS = Obj.Daily_schedule;
    DS = Obj.Daily_schedule;
    [r, c] = find(~isnan(DS), 1);
    if ~isempty(r)
        DS(r, c) = 99999;
    end
    Obj.Daily_schedule = DS;
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, 'Verbose', false, 'DumpCsv', false);
    nFail = R.nFail;
    Obj.Daily_schedule = OrigDS;
end
