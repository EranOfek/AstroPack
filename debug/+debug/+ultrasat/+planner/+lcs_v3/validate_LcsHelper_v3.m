%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v3/validate_LcsHelper_v3.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 15/06/2026
% Description : Validate LcsHelper_v3 schedule output against formal LCS
%               rules (Sets A/B/C/D, 45-day windows, slot budget, etc.).
%               Runs all checks to completion; prints PASS/FAIL for each
%               rule without stopping on the first failure.
%
% Run by      : debug.ultrasat.planner.lcs_v3.validate_LcsHelper_v3()
%==========================================================================
%
% VALIDATION TEST ORDER
% ---------------------
% All sections run to completion; failures do not stop later checks.
%
% 0. BUILD (build_helper)
%    Construct LcsHelper_v3 with StartDate January 5, 2029, bundled
%    LCS grid CSV, prep_before_schedule + build_the_schedule (full A/B/C/D).
%    On exception or empty result, validation exits with a single failure.
%
% 1. PIPELINE COMPLETENESS (check_pipeline_complete)
%    - Schedule table is non-empty
%    - SetA placed field count == SetAnumel (48)
%    - SetB schedule row count == 3 * SetBnumel (48 rows = 16 fields x 3)
%    - SetC placed field count == SetCnumel (16)
%    - Daily_schedule matrix was built (non-empty)
%
% 2. FIELD COUNTS (check_field_counts)
%    - SetA field count == SetAnumel
%    - SetB row count == 3 * SetBnumel
%    - SetC field count == SetCnumel
%    - SetD field count <= SetDnumel (max 4)
%
% 3. SET A (check_setA)
%    48 fields in 6 groups x 8 slots; 45-day (Min_window) windows.
%    - All SetA field IDs are unique
%    - Every row spans exactly Min_window days (end - start + 1 == 45)
%    - Groups 1..6: each group has exactly 8 placed fields
%    - Moved rows (group >= 7, SetD pre-clean bumps):
%        start/end match Full_windows.start(ind) / Full_windows.end(ind)
%
% 4. SET B (check_setB)
%    16 fields x 3 rows (1x B_45 + 2x B_90) over a 135-day super-pattern.
%    - All rows span exactly 45 days
%    - start/end align with Full_windows boundaries
%    - Exactly SetBnumel (16) unique field IDs
%    Per field:
%    - Exactly 1 B_45 row and 2 B_90 rows
%    - 3 distinct Full_windows indices (group 100+k / 200+k encoding)
%    - fw_inds within 1..Ninds
%    - Total span from earliest to latest start is 135 days (3 * Min_window)
%
% 5. SET C (check_setC)
%    16 fields in two 135-day super-windows (8 fields each); 4-day cadence.
%    - All SetC field IDs are unique
%    - Every row spans exactly 135 days (3 * Min_window)
%    - start is one of the two super-window anchors:
%        Full_windows.start(SetC_start_ind) and +135 days
%    - Group 11 has 8 fields; group 12 has 8 fields
%    - Groups are 11 or 12 only
%    - ind in 1..8 (cadence slot within super-window)
%
% 6. SET D (check_setD)
%    Up to 4 optional high-priority fields in open Full_windows slots.
%    - Placed field count <= SetDnumel (4)
%    When any SetD rows exist:
%    - Field IDs are unique
%    - Window length is 45 days
%    - group encoding is 301..304 (300 + slot 1..4)
%    - start/end match Full_windows row at ind
%    - group slots 301..304 are unique (no duplicate slots)
%
% 7. SLOT BUDGET (check_slot_budget)
%    Window-index occupancy mirrors LcsHelper_v3 filled(k) convention;
%    SetD rows are excluded from occupancy (placed into open slack).
%      filledABC(k) = nA(k) + nB45(k) + nCadence4(k)/4  <= Daily_LCS_slots (11)
%      nCadence4(k) = nB90(k) + nC(k)
%    Per Full_windows index k:
%    - nCadence4(k) is divisible by 4
%    - filledABC(k) <= 11
%    - Informational summary of per-ind occupancy vectors
%
% 8. WINDOW BOUNDS (check_window_bounds)
%    All placed schedule rows (Field > 0) lie within the planning horizon.
%    - min(start) >= First_day
%    - max(end) <= Last_day
%
% 9. NO CROSS-SET DUPLICATES (check_no_duplicates)
%    Each field ID may appear in only one set.
%    - SetA vs SetB field IDs are disjoint
%    - SetA vs SetC disjoint
%    - SetA vs SetD disjoint
%    - SetB vs SetC disjoint
%    - SetB vs SetD disjoint
%    - SetC vs SetD disjoint
%
% 10. DAILY SCHEDULE (check_daily_schedule)
%     calcDailySchedule output: day x slot matrix of field IDs.
%     - Daily_schedule is non-empty
%     - Row count == Last_day - First_day + 1
%     - Column count == Daily_LCS_slots (11)
%     - At least one non-NaN observation entry
%
%==========================================================================

function validate_LcsHelper_v3()

    log_enter('validate_LcsHelper_v3');

    % ---- Build the planner and run the full pipeline --------------------
    Obj = build_helper();

    if isempty(Obj)
        fprintf('\n========== SUMMARY ==========\n');
        fprintf('0 checks passed, 1 failed\n');
        fprintf('validate_LcsHelper_v3: VALIDATION FAILED (planner not built)\n');
        log_exit('validate_LcsHelper_v3', 1, 0);
        return
    end

    % ---- Context banner (helps compare runs) ------------------------------
    fprintf('\n--- Run context ---\n');
    fprintf('  Input CSV      : %s\n', gridFile());
    fprintf('  StartDate      : %s  (expected %s)\n', ...
        datestr(Obj.StartDate), datestr(validationStartDate()));
    fprintf('  SetC_start_ind : %d\n', Obj.SetC_start_ind);
    fprintf('  Schedule rows  : %d\n', height(Obj.Schedule));
    if isempty(Obj.Daily_schedule)
        fprintf('  Daily_schedule : (empty)\n');
    else
        fprintf('  Daily_schedule : %s\n', mat2str(size(Obj.Daily_schedule)));
    end

    % ---- Run every validation section (never stop early) ----------------
    TotalFail = 0;
    TotalPass = 0;

    CheckList = {
        @() check_pipeline_complete(Obj)
        @() check_field_counts(Obj)
        @() check_setA(Obj)
        @() check_setB(Obj)
        @() check_setC(Obj)
        @() check_setD(Obj)
        @() check_slot_budget(Obj)
        @() check_window_bounds(Obj)
        @() check_no_duplicates(Obj)
        @() check_daily_schedule(Obj)
    };

    for k = 1:numel(CheckList)
        [nFail, nPass] = CheckList{k}();
        TotalFail = TotalFail + nFail;
        TotalPass = TotalPass + nPass;
    end

    % ---- Final summary (informational only; no error() thrown) ----------
    fprintf('\n========== SUMMARY ==========\n');
    TotalWarn = check_warnings(Obj);
    fprintf('%d checks passed, %d failed, %d warnings\n', TotalPass, TotalFail, TotalWarn);

    if TotalFail > 0
        fprintf('validate_LcsHelper_v3: VALIDATION FAILED\n');
    else
        fprintf('validate_LcsHelper_v3: ALL CHECKS PASSED\n');
    end

    % ---- Dump results to CSV for inspection ---------------------------------
    dump_results(Obj);

    log_exit('validate_LcsHelper_v3', TotalFail, TotalPass);
end


% =========================================================================
% LOGGING HELPERS
% =========================================================================

function log_enter(FuncName)
    % Print a consistent "enter function" marker for traceability.
    fprintf('\n>> ENTER %s\n', FuncName);
end


function log_exit(FuncName, nFail, nPass)
    % Print a consistent "exit function" marker, optionally with tallies.
    if nargin >= 3
        fprintf('<< EXIT  %s  (%d passed, %d failed)\n', FuncName, nPass, nFail);
    else
        fprintf('<< EXIT  %s\n', FuncName);
    end
end


% =========================================================================
% BUILD HELPER
% =========================================================================

function Obj = build_helper()
    % Construct LcsHelper_v3 and run the full two-phase pipeline.
    %
    % Phase 1 (prep_before_schedule): build Full_windows, categorize fields.
    % Phase 2 (build_the_schedule): place Sets A/B/C/D and calcDailySchedule.
    %
    % Uses a fixed StartDate (January 5, 2029) and the bundled grid CSV
    % resolved by gridFile(). Returns [] on exception so the caller can
    % print a clean failure summary without re-throwing.
    log_enter('build_helper');

    Obj = [];
    try
        CsvFile = gridFile();
        PlanStart = validationStartDate();
        fprintf('  CSV file   : %s\n', CsvFile);
        fprintf('  StartDate  : %s (January 5, 2029)\n', datestr(PlanStart));
        fprintf('  Running LcsHelper_v3 pipeline (prep + schedule + SetD)...\n');

        Obj = ultrasat.planner.LcsHelper_v3( ...
            'StartDate', PlanStart, ...
            'AllSkyTable', CsvFile, ...
            'Verbose', false, ...
            'prep_before_schedule', true, ...
            'build_the_schedule', true);

        fprintf('  Pipeline finished.\n');
        fprintf('  Schedule rows     : %d\n', height(Obj.Schedule));
        fprintf('  SetC_start_ind    : %d\n', Obj.SetC_start_ind);
        if isempty(Obj.Daily_schedule)
            fprintf('  Daily_schedule    : (empty — categorize_then_schedule may have failed)\n');
        else
            fprintf('  Daily_schedule    : %s\n', mat2str(size(Obj.Daily_schedule)));
        end
    catch ME
        fprintf('  [FAIL] LcsHelper_v3 build raised an exception:\n');
        fprintf('         %s\n', ME.message);
        if ~isempty(ME.stack)
            fprintf('         at %s (line %d)\n', ME.stack(1).name, ME.stack(1).line);
        end
    end

    log_exit('build_helper');
end


function GridFile = gridFile()
    % Resolve the bundled LCS grid CSV path via ASTROPACK_PATH.
    RepoRoot = getenv('ASTROPACK_PATH');
    if isempty(RepoRoot)
        error('ASTROPACK_PATH is not set');
    end
    GridFile = fullfile(RepoRoot, 'matlab', 'astro', '+ultrasat', '+planner', 'data', 'LCS_fields.csv');
    if ~isfile(GridFile)
        warning('validate_LcsHelper_v3:gridFileNotFound', ...
            'Grid file not found: %s', GridFile);
    end
end


function StartDate = validationStartDate()
    % Fixed plan start date for this validation suite: January 5, 2029.
    StartDate = datetime(2029, 1, 5);
end


% =========================================================================
% RESULTS DUMP (CSV)
% =========================================================================

function dump_results(Obj)
    % Write LcsHelper_v3 schedule, daily schedule, and Full_windows to CSV
    % files under debug/+debug/+ultrasat/+planner/+lcs_v3/output/validation/ for offline inspection.
    log_enter('dump_results');

    if isempty(Obj)
        fprintf('  [SKIP] Obj is empty – nothing to dump.\n');
        log_exit('dump_results');
        return
    end

    ThisDir   = fileparts(mfilename('fullpath'));
    OutputDir = fullfile(ThisDir, 'output', 'validation');
    if ~exist(OutputDir, 'dir')
        mkdir(OutputDir);
        fprintf('  Created output dir: %s\n', OutputDir);
    end

    % --- Schedule table ------------------------------------------------------
    dump_schedule(Obj, OutputDir);

    % --- Full_windows table --------------------------------------------------
    dump_full_windows(Obj, OutputDir);

    % --- Daily_schedule matrix -----------------------------------------------
    dump_daily_schedule(Obj, OutputDir);

    fprintf('  All CSV files written to: %s\n', OutputDir);
    log_exit('dump_results');
end


function dump_schedule(Obj, OutputDir)
    % Write Obj.Schedule to schedule.csv, sorted by category then field.
    CsvPath = fullfile(OutputDir, 'schedule.csv');
    if isempty(Obj.Schedule)
        fprintf('  [SKIP] Schedule is empty – skipping schedule.csv\n');
        return
    end

    T = Obj.Schedule;

    % Add human-readable date columns derived from numeric day offsets.
    Origin = Obj.StartDate;   % datetime
    if isdatetime(Origin)
        T.start_date = cellstr(datestr(Origin + days(T.start - 1), 'yyyy-mm-dd'));
        T.end_date   = cellstr(datestr(Origin + days(T.end   - 1), 'yyyy-mm-dd'));
    end

    writetable(T, CsvPath);
    fprintf('  Written: schedule.csv  (%d rows)\n', height(T));
end


function dump_full_windows(Obj, OutputDir)
    % Write Obj.Full_windows to full_windows.csv.
    CsvPath = fullfile(OutputDir, 'full_windows.csv');
    if isempty(Obj.Full_windows)
        fprintf('  [SKIP] Full_windows is empty – skipping full_windows.csv\n');
        return
    end

    T = Obj.Full_windows;

    Origin = Obj.StartDate;
    if isdatetime(Origin)
        T.start_date = cellstr(datestr(Origin + days(T.start - 1), 'yyyy-mm-dd'));
        T.end_date   = cellstr(datestr(Origin + days(T.end   - 1), 'yyyy-mm-dd'));
    end

    writetable(T, CsvPath);
    fprintf('  Written: full_windows.csv  (%d rows)\n', height(T));
end


function dump_daily_schedule(Obj, OutputDir)
    % Write Obj.Daily_schedule (day x slot matrix) to daily_schedule.csv.
    % Rows = calendar days; columns = slot_1 .. slot_N; values = field IDs (NaN = empty).
    CsvPath = fullfile(OutputDir, 'daily_schedule.csv');
    if isempty(Obj.Daily_schedule)
        fprintf('  [SKIP] Daily_schedule is empty – skipping daily_schedule.csv\n');
        return
    end

    M    = Obj.Daily_schedule;
    Ndays = size(M, 1);
    Nslots = size(M, 2);

    % Build column names: day, date, slot_1..slot_N
    SlotNames = arrayfun(@(s) sprintf('slot_%d', s), 1:Nslots, 'UniformOutput', false);

    Origin   = Obj.StartDate;
    DayNums  = (Obj.First_day : Obj.First_day + Ndays - 1)';
    if isdatetime(Origin)
        DateStrs = cellstr(datestr(Origin + days(DayNums - 1), 'yyyy-mm-dd'));
    else
        DateStrs = num2cell(DayNums);
    end

    T = array2table(M, 'VariableNames', SlotNames);
    T = [table(DayNums, DateStrs, 'VariableNames', {'day', 'date'}), T];

    writetable(T, CsvPath);
    fprintf('  Written: daily_schedule.csv  (%d days x %d slots)\n', Ndays, Nslots);
end


% =========================================================================
% WARNING-ONLY CHECKS
% =========================================================================

function nWarn = check_warnings(Obj)
    log_enter('check_warnings');
    nWarn = 0;
    nWarn = nWarn + warn_long_field_extinction_ranking(Obj);
    nWarn = nWarn + warn_setD_ranking(Obj);
    fprintf('  Warning checks: %d warning(s)\n', nWarn);
    log_exit('check_warnings');
end


function nWarn = warn_long_field_extinction_ranking(Obj)
    nWarn = 0;
    if isempty(Obj) || isempty(Obj.SetB_fields) || isempty(Obj.SetC_fields)
        nWarn = warning_check('Long-field extinction ranking', false, ...
            'SetB/SetC field tables are missing');
        return
    end

    LongA = table();
    if ~isempty(Obj.SetA_fields) && ismember('max_window', Obj.SetA_fields.Properties.VariableNames)
        LongA = Obj.SetA_fields(Obj.SetA_fields.max_window >= Obj.Max_window_cut, :);
    end

    MaxB = max(Obj.SetB_fields.Av_ext);
    MinC = min(Obj.SetC_fields.Av_ext);
    if MaxB > MinC
        nWarn = nWarn + warning_check('Long-field extinction ranking: SetB before SetC', false, ...
            sprintf('max SetB Av_ext %.4g > min SetC Av_ext %.4g', MaxB, MinC));
    else
        nWarn = nWarn + warning_check('Long-field extinction ranking: SetB before SetC', true, ...
            sprintf('max SetB Av_ext %.4g <= min SetC Av_ext %.4g', MaxB, MinC));
    end

    if ~isempty(LongA)
        MaxC = max(Obj.SetC_fields.Av_ext);
        MinLongA = min(LongA.Av_ext);
        if MaxC > MinLongA
            nWarn = nWarn + warning_check('Long-field extinction ranking: SetC before long SetA', false, ...
                sprintf('max SetC Av_ext %.4g > min long SetA Av_ext %.4g', MaxC, MinLongA));
        else
            nWarn = nWarn + warning_check('Long-field extinction ranking: SetC before long SetA', true, ...
                sprintf('max SetC Av_ext %.4g <= min long SetA Av_ext %.4g', MaxC, MinLongA));
        end
    else
        nWarn = nWarn + warning_check('Long-field extinction ranking: long SetA present', false, ...
            'no long SetA rows found to compare');
    end
end


function nWarn = warn_setD_ranking(Obj)
    nWarn = 0;
    SchedD = Obj.Schedule(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0, :);
    if isempty(SchedD)
        nWarn = nWarn + warning_check('SetD ranking', true, 'no SetD rows placed');
        return
    end
    if isempty(Obj.SetD_ranked_fields) || ~ismember('Field', Obj.SetD_ranked_fields.Properties.VariableNames)
        nWarn = nWarn + warning_check('SetD ranking', false, 'SetD_ranked_fields is missing');
        return
    end

    SchedD = sortrows(SchedD, 'group');
    RankFields = Obj.SetD_ranked_fields.Field(:)';
    RankPos = nan(1, height(SchedD));
    for I = 1:height(SchedD)
        Pos = find(RankFields == SchedD.Field(I), 1);
        if ~isempty(Pos)
            RankPos(I) = Pos;
        end
    end

    if any(isnan(RankPos))
        nWarn = nWarn + warning_check('SetD ranking: placed fields in rank list', false, ...
            sprintf('placed=%s rank=%s', mat2str(SchedD.Field'), mat2str(RankFields)));
    else
        nWarn = nWarn + warning_check('SetD ranking: placed fields in rank list', true, ...
            sprintf('rank positions=%s', mat2str(RankPos)));
    end

    if all(~isnan(RankPos)) && any(diff(RankPos) < 0)
        nWarn = nWarn + warning_check('SetD ranking: selected order follows rank order', false, ...
            sprintf('rank positions=%s', mat2str(RankPos)));
    else
        nWarn = nWarn + warning_check('SetD ranking: selected order follows rank order', true, ...
            sprintf('rank positions=%s', mat2str(RankPos)));
    end

    if all(~isnan(RankPos)) && max(RankPos) > height(SchedD)
        nWarn = nWarn + warning_check('SetD ranking: earlier ranked fields skipped', false, ...
            sprintf('selected rank positions=%s', mat2str(RankPos)));
    else
        nWarn = nWarn + warning_check('SetD ranking: earlier ranked fields skipped', true, ...
            sprintf('selected rank positions=%s', mat2str(RankPos)));
    end
end


function nWarn = warning_check(CheckName, IsOk, Msg)
    if IsOk
        fprintf('  [WARN-OK] %s: %s\n', CheckName, Msg);
        nWarn = 0;
    else
        fprintf('  [WARN] %s: %s\n', CheckName, Msg);
        nWarn = 1;
    end
end


% =========================================================================
% PASS / FAIL HELPERS
% =========================================================================

function [nFail, nPass] = pass_check(CheckName)
    % Record a single passing check and return [nFail=0, nPass=1].
    fprintf('  [PASS] %s\n', CheckName);
    nFail = 0;
    nPass = 1;
end


function [nFail, nPass] = fail_check(CheckName, Msg)
    % Record a single failing check without stopping execution.
    %
    % Returns [nFail=1, nPass=0] so the caller can aggregate tallies
    % across all checks in a section.
    fprintf('  [FAIL] %s: %s\n', CheckName, Msg);
    nFail = 1;
    nPass = 0;
end


function [nFail, nPass] = run_checks(SectionName, CheckFns)
    % Run a list of zero-argument check lambdas and aggregate pass/fail counts.
    %
    % CheckFns is a cell array of function handles, each returning [nFail, nPass].
    % Prints a section header/footer via log_enter/log_exit and never throws.
    log_enter(SectionName);
    nFail = 0;
    nPass = 0;
    for k = 1:numel(CheckFns)
        [f, p] = CheckFns{k}();
        nFail = nFail + f;
        nPass = nPass + p;
    end
    log_exit(SectionName, nFail, nPass);
end


% =========================================================================
% PIPELINE COMPLETENESS  (did categorize_then_schedule succeed?)
% =========================================================================

function [nFail, nPass] = check_pipeline_complete(Obj)
    % Verify that LcsHelper_v3 produced a full A/B/C schedule and daily matrix.
    % A partial schedule (e.g. after shuffle exhaustion) still allows downstream
    % checks to run so we can see all rule violations at once.
    log_enter('check_pipeline_complete');

    nFail = 0;
    nPass = 0;

    if isempty(Obj.Schedule)
        [f, p] = fail_check('Pipeline: Schedule table exists', 'Schedule is empty');
        nFail = nFail + f; nPass = nPass + p;
        log_exit('check_pipeline_complete', nFail, nPass);
        return
    end

    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);

    fprintf('  Pipeline counts: A=%d/%d, B rows=%d/%d, C=%d/%d, D=%d (max %d)\n', ...
        nA, Obj.SetAnumel, nB, 3 * Obj.SetBnumel, nC, Obj.SetCnumel, nD, Obj.SetDnumel);

    CheckFns = {
        @() assert_equal('Pipeline: StartDate is January 5, 2029', ...
            dateshift(Obj.StartDate, 'start', 'day'), validationStartDate())
        @() assert_equal('Pipeline: SetA field count', nA, Obj.SetAnumel)
        @() assert_equal('Pipeline: SetB row count (3 per field)', nB, 3 * Obj.SetBnumel)
        @() assert_equal('Pipeline: SetC field count', nC, Obj.SetCnumel)
        @() assert_true('Pipeline: Daily_schedule built', ~isempty(Obj.Daily_schedule))
    };

    for k = 1:numel(CheckFns)
        [f, p] = CheckFns{k}();
        nFail = nFail + f;
        nPass = nPass + p;
    end

    log_exit('check_pipeline_complete', nFail, nPass);
end


% =========================================================================
% FIELD COUNTS
% =========================================================================

function [nFail, nPass] = check_field_counts(Obj)
    % Verify the expected number of placed fields per set.
    % SetB produces 3 schedule rows per field (1x B_45 + 2x B_90).
    log_enter('check_field_counts');

    MaskA = strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0;
    MaskB = ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0;
    MaskC = strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0;
    MaskD = strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0;

    nA = sum(MaskA);
    nB = sum(MaskB);
    nC = sum(MaskC);
    nD = sum(MaskD);

    fprintf('  Counts: A=%d, B rows=%d, C=%d, D=%d\n', nA, nB, nC, nD);

    CheckFns = {
        @() assert_equal('SetA field count', nA, Obj.SetAnumel)
        @() assert_equal('SetB row count (3 per field)', nB, 3 * Obj.SetBnumel)
        @() assert_equal('SetC field count', nC, Obj.SetCnumel)
        @() assert_max('SetD field count', nD, Obj.SetDnumel)
    };

    [nFail, nPass] = run_checks('Field counts', CheckFns);
    log_exit('check_field_counts', nFail, nPass);
end


% =========================================================================
% SET A  (48 fields, 45-day windows, 6 groups x 8 slots)
% =========================================================================

function [nFail, nPass] = check_setA(Obj)
    % SetA: daily-cadence 45-day blocks in 6 parallel groups of 8 slots.
    % Fields moved by SetD pre-clean use group >= 7 and Full_windows alignment.
    log_enter('check_setA');

    SchedA = Obj.Schedule(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;

    fprintf('  SetA placed fields: %d\n', height(SchedA));
    if ~isempty(SchedA)
        fprintf('  SetA groups present: %s\n', mat2str(unique(SchedA.group)'));
    end

    CheckFns = {
        @() assert_unique('SetA unique field IDs', SchedA.Field)
        @() assert_all('SetA window length = Min_window (45d)', ...
            SchedA.end - SchedA.start + 1 == L)
    };

    % Original groups 1..6: each group should hold exactly 8 placed fields.
    CheckFns{end+1} = @() check_setA_group_counts(SchedA, Obj.SetA_Nwindows); %#ok<AGROW>

    % Moved SetA rows (group >= 7): ind must be a Full_windows index and
    % start/end must match that window (SetD bump moves use this convention).
    MovedMask = SchedA.group >= 7;
    if any(MovedMask)
        Moved = SchedA(MovedMask, :);
        fprintf('  SetA moved rows (group>=7): %d\n', height(Moved));
        CheckFns{end+1} = @() assert_all( ...
            'SetA moved rows: start matches Full_windows.start(ind)', ...
            Moved.start == Obj.Full_windows.start(Moved.ind)); %#ok<AGROW>
        CheckFns{end+1} = @() assert_all( ...
            'SetA moved rows: end matches Full_windows.end(ind)', ...
            Moved.end == Obj.Full_windows.end(Moved.ind)); %#ok<AGROW>
    else
        fprintf('  SetA moved rows (group>=7): none\n');
    end

    [nFail, nPass] = run_checks('Set A (45-day, groups 1-6)', CheckFns);
    log_exit('check_setA', nFail, nPass);
end


function [nFail, nPass] = check_setA_group_counts(SchedA, SetANwindows)
    % Each original SetA group (1..6) must contain at most 8 placed fields.
    % A group may have fewer than 8 if schedule_SetD_v3 (Case B) moved one of
    % its fields to a different Full_windows slot; those moved fields appear
    % with group >= 7. The total of all SetA rows always sums to 48.
    log_enter('check_setA_group_counts');
    nFail = 0;
    nPass = 0;

    % Groups 1..SetANwindows are the original parallel SetA lanes.
    % Fields bumped by SetD (Case B) leave their original group and reappear
    % with group >= 7, so a group may legitimately have fewer than 8 fields.
    for G = 1:SetANwindows
        nInGroup = sum(SchedA.group == G);
        fprintf('  Group %d: %d fields\n', G, nInGroup);
        [f, p] = assert_max( ...
            sprintf('SetA group %d has <= 8 fields', G), nInGroup, 8);
        nFail = nFail + f;
        nPass = nPass + p;
    end
    MovedGroups = unique(SchedA.group(SchedA.group > SetANwindows));
    if isempty(MovedGroups)
        fprintf('  Moved groups: none\n');
    else
        for G = MovedGroups(:)'
            nInGroup = sum(SchedA.group == G);
            fprintf('  Moved group %d: %d fields\n', G, nInGroup);
        end
        [f, p] = assert_unique('SetA moved rows have unique group/ind slots', ...
            SchedA.group(SchedA.group > SetANwindows) * 1000 + SchedA.ind(SchedA.group > SetANwindows));
        nFail = nFail + f;
        nPass = nPass + p;
    end
    [f, p] = assert_equal('SetA group accounting includes all rows', ...
        sum(ismember(SchedA.group, [1:SetANwindows, MovedGroups(:)'])), height(SchedA));
    nFail = nFail + f;
    nPass = nPass + p;
    log_exit('check_setA_group_counts', nFail, nPass);
end


% =========================================================================
% SET B  (16 fields x 3 rows: 1 B_45 + 2 B_90, 135-day super-pattern)
% =========================================================================

function [nFail, nPass] = check_setB(Obj)
    % Each SetB field occupies three Full_windows indices spanning 135 days:
    % one high-cadence block (B_45) and two low-cadence blocks (B_90).
    log_enter('check_setB');

    SchedB = Obj.Schedule(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & ...
        Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;
    Ninds = height(Obj.Full_windows);

    fprintf('  SetB schedule rows: %d\n', height(SchedB));
    fprintf('  SetB unique fields : %d\n', numel(unique(SchedB.Field)));

    CheckFns = {
        @() assert_all('SetB row window length = 45d', ...
            SchedB.end - SchedB.start + 1 == L)
        @() assert_all('SetB rows align with Full_windows boundaries', ...
            ismember(SchedB.start, Obj.Full_windows.start) & ...
            ismember(SchedB.end, Obj.Full_windows.end))
    };

    FieldsB = unique(SchedB.Field);
    CheckFns{end+1} = @() assert_equal( ...
        'SetB unique field count', numel(FieldsB), Obj.SetBnumel); %#ok<AGROW>
    CheckFns{end+1} = @() check_setB_all_fields(SchedB, L, Ninds); %#ok<AGROW>

    [nFail, nPass] = run_checks('Set B (B_45 + 2x B_90, 135d pattern)', CheckFns);
    log_exit('check_setB', nFail, nPass);
end


function [nFail, nPass] = check_setB_all_fields(SchedB, L, Ninds)
    % Per-field SetB checks: each of the 16 fields must have exactly
    % 1 B_45 row, 2 B_90 rows, 3 distinct Full_windows indices, and a
    % 135-day total span.
    %
    % Implemented as a separate function (not an inline loop) because MATLAB
    % closures inside a for-loop capture the loop variable by reference,
    % causing all lambdas to see the last value of F.
    log_enter('check_setB_all_fields');
    nFail = 0;
    nPass = 0;
    FieldsB = unique(SchedB.Field);

    for F = FieldsB(:)'
        Rows = SchedB(SchedB.Field == F, :);
        nB45 = sum(strcmp(Rows.category, 'B_45'));
        nB90 = sum(strcmp(Rows.category, 'B_90'));
        FwInds = setB_fwInds(Rows);

        % 135-day span: earliest start to latest start, plus one window length.
        % The three rows occupy three consecutive 45-day Full_windows blocks.
        SpanDays = max(Rows.start) - min(Rows.start) + L;

        [f, p] = assert_equal(sprintf('SetB field %d: 1 B_45 row', F), nB45, 1);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = assert_equal(sprintf('SetB field %d: 2 B_90 rows', F), nB90, 2);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = assert_equal( ...
            sprintf('SetB field %d: 3 distinct Full_windows inds', F), ...
            numel(unique(FwInds)), 3);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = assert_all( ...
            sprintf('SetB field %d: fw_inds in 1..%d', F, Ninds), ...
            FwInds >= 1 & FwInds <= Ninds);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = assert_equal( ...
            sprintf('SetB field %d: 135-day span', F), SpanDays, 3 * L);
        nFail = nFail + f; nPass = nPass + p;
    end

    log_exit('check_setB_all_fields', nFail, nPass);
end


function FwInds = setB_fwInds(Rows)
    % Extract Full_windows index from B_45/B_90 group encoding.
    %
    % LcsHelper_v3 encodes the Full_windows index in the group column:
    %   B_45 rows: group = 100 + fw_ind
    %   B_90 rows: group = 200 + fw_ind
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

function [nFail, nPass] = check_setC(Obj)
    % Validate SetC placement rules.
    %
    % SetC places 16 fields across two 135-day super-windows (8 fields each).
    % Each field is observed every 4 days within its super-window.
    % Super-windows start at Full_windows.start(SetC_start_ind) and +135d.
    % group 11 = first super-window; group 12 = second super-window.
    % ind 1..8 = cadence slot within the super-window.
    log_enter('check_setC');

    % Filter to placed SetC rows only (Field > 0)
    SchedC = Obj.Schedule(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0, :);

    % SetC observations span a full 135-day super-window (3 x 45-day Min_window)
    Lsuper = 3 * Obj.Min_window;

    % SetC_start_ind points to the Full_windows row whose start day anchors
    % the first 135-day super-window; the second begins exactly Lsuper days later
    Sci = Obj.SetC_start_ind;
    ExpectedS1 = Obj.Full_windows.start(Sci);
    ExpectedS2 = ExpectedS1 + Lsuper;    % second super-window starts immediately after

    fprintf('  SetC_start_ind=%d, super-window starts: %d, %d\n', Sci, ExpectedS1, ExpectedS2);
    fprintf('  SetC placed fields: %d\n', height(SchedC));

    % --- Checks ----------------------------------------------------
    % group 11 = first super-window; group 12 = second super-window
    % ind 1..8 = cadence slot within the super-window (4-day step)
    CheckFns = {
        @() assert_unique('SetC unique field IDs', SchedC.Field)
        @() assert_all('SetC window length = 135d', ...
            SchedC.end - SchedC.start + 1 == Lsuper)
        @() assert_all('SetC start days are super-window anchors', ...
            ismember(SchedC.start, [ExpectedS1; ExpectedS2]))
        @() assert_equal('SetC group 11 field count', sum(SchedC.group == 11), 8)
        @() assert_equal('SetC group 12 field count', sum(SchedC.group == 12), 8)
        @() assert_all('SetC groups are 11 or 12 only', ...
            ismember(SchedC.group, [11, 12]))
        @() assert_all('SetC ind in 1..8 (cadence slot)', ...
            SchedC.ind >= 1 & SchedC.ind <= 8)
    };

    [nFail, nPass] = run_checks('Set C (2 x 135-day super-windows)', CheckFns);
    log_exit('check_setC', nFail, nPass);
end


% =========================================================================
% SET D  (up to 4 fields, 45-day windows, group 301..304)
% =========================================================================

function [nFail, nPass] = check_setD(Obj)
    % SetD: optional high-priority fields placed into open Full_windows slots.
    log_enter('check_setD');

    SchedD = Obj.Schedule(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0, :);
    L = Obj.Min_window;

    fprintf('  SetD placed fields: %d (max %d)\n', height(SchedD), Obj.SetDnumel);
    if ~isempty(SchedD)
        fprintf('  SetD field IDs  : %s\n', mat2str(SchedD.Field'));
        fprintf('  SetD groups     : %s\n', mat2str(SchedD.group'));
    end

    CheckFns = {
        @() assert_max('SetD placed field count', height(SchedD), Obj.SetDnumel)
    };

    if height(SchedD) > 0
        CheckFns{end+1} = @() assert_unique('SetD unique field IDs', SchedD.Field); %#ok<AGROW>
        CheckFns{end+1} = @() assert_all('SetD window length = 45d', ...
            SchedD.end - SchedD.start + 1 == L); %#ok<AGROW>
        CheckFns{end+1} = @() assert_all('SetD group encoding 301..304', ...
            SchedD.group >= 301 & SchedD.group <= 300 + Obj.SetDnumel); %#ok<AGROW>
        CheckFns{end+1} = @() assert_all('SetD ind matches Full_windows row', ...
            SchedD.start == Obj.Full_windows.start(SchedD.ind) & ...
            SchedD.end == Obj.Full_windows.end(SchedD.ind)); %#ok<AGROW>
        % group = 300 + slot_in_setD (1..4); ind = Full_windows index (not slot).
        CheckFns{end+1} = @() assert_unique( ...
            'SetD unique group slots (301..304)', SchedD.group); %#ok<AGROW>
    end

    [nFail, nPass] = run_checks('Set D (up to 4 fields, 45d)', CheckFns);
    log_exit('check_setD', nFail, nPass);
end


% =========================================================================
% SLOT BUDGET  (filled(k) <= 11, 4-day cadence pool divisible by 4)
% =========================================================================

function [nFail, nPass] = check_slot_budget(Obj)
    % Replicate LcsHelper_v3 slot-budget convention:
    %   filledABC(k) = nA(k) + nB45(k) + nCadence4(k)/4  <= Daily_LCS_slots (11)
    %   nCadence4(k) = nB90(k) + nC(k) must be divisible by 4.
    % SetD rows are counted separately because they are placed into open slack
    % after A/B/C balancing.
    log_enter('check_slot_budget');

    [nA, nB45, nB90, nC, nD, nCadence4, FilledABC, FilledWithD] = compute_slot_occupancy( ...
        Obj.Schedule, Obj.Full_windows);

    fprintf('  Per-ind slot occupancy (ind 1..%d):\n', numel(FilledABC));
    fprintf('    nA         = [%s]\n', num2str(nA, '%d '));
    fprintf('    nB45       = [%s]\n', num2str(nB45, '%d '));
    fprintf('    nB90       = [%s]\n', num2str(nB90, '%d '));
    fprintf('    nC         = [%s]\n', num2str(nC, '%d '));
    fprintf('    nCadence4  = [%s]  (B_90 + C)\n', num2str(nCadence4, '%d '));
    fprintf('    filledABC  = [%s]  (limit %d before SetD)\n', num2str(FilledABC, '%.2f '), Obj.Daily_LCS_slots);
    fprintf('    nD         = [%s]\n', num2str(nD, '%d '));
    fprintf('    filled+D   = [%s]  (informational final use)\n', num2str(FilledWithD, '%.2f '));

    CheckFns = {
        @() check_slot_budget_all_inds(nCadence4, FilledABC, Obj.Daily_LCS_slots)
        @() check_final_slot_usage(FilledWithD, Obj.Daily_LCS_slots)
        @() pass_check(sprintf('Slot occupancy summary: filledABC = [%s], nD = [%s]', ...
            num2str(FilledABC, '%d '), num2str(nD, '%d ')))
    };

    [nFail, nPass] = run_checks('Slot budget (11 slots per Full_windows ind)', CheckFns);
    log_exit('check_slot_budget', nFail, nPass);
end


function [nFail, nPass] = check_final_slot_usage(FilledWithD, DailyLcsSlots)
    log_enter('check_final_slot_usage');
    nFail = 0;
    nPass = 0;
    for K = 1:numel(FilledWithD)
        [f, p] = assert_max( ...
            sprintf('Final slot use ind %d: filledABC + nD <= Daily_LCS_slots', K), ...
            FilledWithD(K), DailyLcsSlots);
        nFail = nFail + f;
        nPass = nPass + p;
    end
    log_exit('check_final_slot_usage', nFail, nPass);
end


function [nFail, nPass] = check_slot_budget_all_inds(nCadence4, FilledABC, DailyLcsSlots)
    % Per-ind slot budget checks (loop in a function to avoid closure issues).
    log_enter('check_slot_budget_all_inds');
    nFail = 0;
    nPass = 0;
    Ninds = numel(FilledABC);

    % K is a Full_windows index (1..Ninds). Two independent constraints:
    %   1) nCadence4(K) must be divisible by 4 (B_90 + C rows consume slots
    %      in groups of 4 via the nCadence4/4 term in the filled formula).
    %   2) filledABC(K) must not exceed the daily LCS slot limit (11).
    for K = 1:Ninds
        [f, p] = assert_divisible( ...
            sprintf('Slot budget ind %d: nCadence4 divisible by 4', K), nCadence4(K), 4);
        nFail = nFail + f; nPass = nPass + p;
        [f, p] = assert_max( ...
            sprintf('Slot budget ind %d: filledABC <= Daily_LCS_slots', K), ...
            FilledABC(K), DailyLcsSlots);
        nFail = nFail + f; nPass = nPass + p;
    end

    log_exit('check_slot_budget_all_inds', nFail, nPass);
end


function [nA, nB45, nB90, nC, nD, nCadence4, FilledABC, FilledWithD] = compute_slot_occupancy(Schedule, Full_windows)
    % Mirror the slot-occupancy computation in LcsHelper_v3.m.
    %
    % For each Full_windows index k, counts how many schedule rows occupy
    % that window and computes the effective slot load:
    %   filledABC(k) = nA(k) + nB45(k) + nCadence4(k)/4
    % where nCadence4(k) = nB90(k) + nC(k).
    %
    % SetD rows are intentionally excluded: they are placed into open slack
    % after A/B/C balance and do not contribute to the filled budget.
    Ninds = height(Full_windows);
    nA   = zeros(1, Ninds);
    nB45 = zeros(1, Ninds);
    nB90 = zeros(1, Ninds);
    nC   = zeros(1, Ninds);
    nD   = zeros(1, Ninds);

    for R = 1:height(Schedule)
        Cat = Schedule.category{R};
        Ind = Schedule.ind(R);
        Grp = Schedule.group(R);

        if strcmp(Cat, 'A')
            % SetA: ind column is the Full_windows index directly
            if Ind >= 1 && Ind <= Ninds
                nA(Ind) = nA(Ind) + 1;
            end
        elseif strcmp(Cat, 'B_45')
            % B_45: group = 100 + fw_ind  =>  decode fw_ind from group
            FwInd = Grp - 100;
            if FwInd >= 1 && FwInd <= Ninds
                nB45(FwInd) = nB45(FwInd) + 1;
            end
        elseif strcmp(Cat, 'B_90')
            % B_90: group = 200 + fw_ind  =>  decode fw_ind from group
            FwInd = Grp - 200;
            if FwInd >= 1 && FwInd <= Ninds
                nB90(FwInd) = nB90(FwInd) + 1;
            end
        elseif strcmp(Cat, 'C')
            % SetC spans three consecutive Full_windows indices starting at
            % the window whose start day matches this row's start day.
            StartInd = find(Full_windows.start == Schedule.start(R), 1);
            if isempty(StartInd)
                continue
            end
            for Kk = StartInd:min(StartInd + 2, Ninds)
                nC(Kk) = nC(Kk) + 1;
            end
        elseif strcmp(Cat, 'D')
            if Ind >= 1 && Ind <= Ninds
                nD(Ind) = nD(Ind) + 1;
            end
        end
    end

    % nCadence4 counts low-cadence rows (B_90 + C); each group of 4 consumes 1 slot.
    nCadence4 = nB90 + nC;
    FilledABC = nA + nB45 + nCadence4 / 4;
    FilledWithD = FilledABC + nD;
end


% =========================================================================
% WINDOW BOUNDS
% =========================================================================

function [nFail, nPass] = check_window_bounds(Obj)
    % All schedule rows must lie within the planning horizon [First_day, Last_day].
    log_enter('check_window_bounds');

    Placed = Obj.Schedule(Obj.Schedule.Field > 0, :);
    fprintf('  Horizon: day %d .. %d\n', Obj.First_day, Obj.Last_day);
    fprintf('  Placed schedule rows: %d\n', height(Placed));

    if isempty(Placed)
        [nFail, nPass] = run_checks('Window bounds [First_day, Last_day]', {
            @() fail_check('Window bounds', 'no placed schedule rows to check')
        });
        log_exit('check_window_bounds', nFail, nPass);
        return
    end

    fprintf('  start range: %d .. %d\n', min(Placed.start), max(Placed.start));
    fprintf('  end range  : %d .. %d\n', min(Placed.end), max(Placed.end));

    CheckFns = {
        @() assert_min('All rows: start >= First_day', ...
            min(Placed.start), Obj.First_day)
        @() assert_max('All rows: end <= Last_day', ...
            max(Placed.end), Obj.Last_day)
    };

    [nFail, nPass] = run_checks('Window bounds [First_day, Last_day]', CheckFns);
    log_exit('check_window_bounds', nFail, nPass);
end


% =========================================================================
% NO CROSS-SET DUPLICATES
% =========================================================================

function [nFail, nPass] = check_no_duplicates(Obj)
    % Each field ID may appear in only one set (A, B, C, or D).
    log_enter('check_no_duplicates');

    FieldsA = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0));
    FieldsB = unique(Obj.Schedule.Field(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0));
    FieldsC = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0));
    FieldsD = unique(Obj.Schedule.Field(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0));

    fprintf('  Unique fields: A=%d, B=%d, C=%d, D=%d\n', ...
        numel(FieldsA), numel(FieldsB), numel(FieldsC), numel(FieldsD));

    CheckFns = {
        @() assert_disjoint('SetA vs SetB field IDs', FieldsA, FieldsB)
        @() assert_disjoint('SetA vs SetC field IDs', FieldsA, FieldsC)
        @() assert_disjoint('SetA vs SetD field IDs', FieldsA, FieldsD)
        @() assert_disjoint('SetB vs SetC field IDs', FieldsB, FieldsC)
        @() assert_disjoint('SetB vs SetD field IDs', FieldsB, FieldsD)
        @() assert_disjoint('SetC vs SetD field IDs', FieldsC, FieldsD)
    };

    [nFail, nPass] = run_checks('No cross-set field duplicates', CheckFns);
    log_exit('check_no_duplicates', nFail, nPass);
end


% =========================================================================
% DAILY SCHEDULE
% =========================================================================

function [nFail, nPass] = check_daily_schedule(Obj)
    % calcDailySchedule must produce a non-empty day x slot matrix.
    log_enter('check_daily_schedule');

    ExpectedRows = Obj.Last_day - Obj.First_day + 1;
    ExpectedCols = Obj.Daily_LCS_slots;

    if isempty(Obj.Daily_schedule)
        fprintf('  Daily_schedule: (empty)\n');
    else
        fprintf('  Daily_schedule size: %s (expected [%d %d])\n', ...
            mat2str(size(Obj.Daily_schedule)), ExpectedRows, ExpectedCols);
        fprintf('  Observed field-days: %d\n', sum(~isnan(Obj.Daily_schedule(:))));
    end

    CheckFns = {
        @() assert_true('Daily_schedule is not empty', ~isempty(Obj.Daily_schedule))
        @() assert_equal('Daily_schedule row count', ...
            size(Obj.Daily_schedule, 1), ExpectedRows)
        @() assert_equal('Daily_schedule column count (slots)', ...
            size(Obj.Daily_schedule, 2), ExpectedCols)
    };

    if ~isempty(Obj.Daily_schedule)
        nObserved = sum(~isnan(Obj.Daily_schedule(:)));
        CheckFns{end+1} = @() assert_true( ...
            'Daily_schedule contains observations', nObserved > 0); %#ok<AGROW>
        CheckFns{end+1} = @() check_daily_schedule_matches_schedule(Obj); %#ok<AGROW>
    end

    [nFail, nPass] = run_checks('Daily schedule matrix', CheckFns);
    log_exit('check_daily_schedule', nFail, nPass);
end


function [nFail, nPass] = check_daily_schedule_matches_schedule(Obj)
    [nBadDays, FirstBad] = daily_schedule_mismatch_count(Obj);
    if nBadDays == 0
        [nFail, nPass] = pass_check('Daily_schedule matches Schedule rows and cadence');
    else
        [nFail, nPass] = fail_check('Daily_schedule matches Schedule rows and cadence', ...
            sprintf('%d day(s) mismatch; first bad day=%d', nBadDays, FirstBad));
    end
end


function [nBadDays, FirstBad] = daily_schedule_mismatch_count(Obj)
    Ndays = size(Obj.Daily_schedule, 1);
    Expected = cell(Ndays, 1);
    for D = 1:Ndays
        Expected{D} = [];
    end

    for R = 1:height(Obj.Schedule)
        F = Obj.Schedule.Field(R);
        if F <= 0
            continue
        end
        Cat = Obj.Schedule.category{R};
        for CurrD = Obj.Schedule.start(R):Obj.Schedule.end(R)
            if CurrD < Obj.First_day || CurrD > Obj.Last_day
                continue
            end
            if (strcmp(Cat, 'C') || strcmp(Cat, 'B_90')) && ...
                    mod((CurrD - Obj.Schedule.start(R) + 1), 4) ~= mod(Obj.Schedule.ind(R), 4)
                continue
            end
            Row = CurrD - Obj.First_day + 1;
            Expected{Row}(end+1) = F; %#ok<AGROW>
        end
    end

    nBadDays = 0;
    FirstBad = NaN;
    for D = 1:Ndays
        Actual = Obj.Daily_schedule(D, :);
        Actual = Actual(~isnan(Actual));
        if ~isequal(sort(Actual(:)'), sort(Expected{D}(:)'))
            nBadDays = nBadDays + 1;
            if isnan(FirstBad)
                FirstBad = Obj.First_day + D - 1;
            end
        end
    end
end


% =========================================================================
% GENERIC ASSERTIONS  (never throw; always return pass/fail counts)
% =========================================================================

function [nFail, nPass] = assert_equal(CheckName, Actual, Expected)
    if isequal(Actual, Expected)
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('expected %s, got %s', fmt(Expected), fmt(Actual)));
    end
end


function [nFail, nPass] = assert_max(CheckName, Actual, Limit)
    if Actual <= Limit
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('expected <= %s, got %s', fmt(Limit), fmt(Actual)));
    end
end


function [nFail, nPass] = assert_min(CheckName, Actual, Limit)
    if Actual >= Limit
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('expected >= %s, got %s', fmt(Limit), fmt(Actual)));
    end
end


function [nFail, nPass] = assert_true(CheckName, Condition)
    if Condition
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, 'condition is false');
    end
end


function [nFail, nPass] = assert_all(CheckName, Mask)
    if isempty(Mask)
        [nFail, nPass] = fail_check(CheckName, 'no data to check');
        return
    end
    if all(Mask(:))
        [nFail, nPass] = pass_check(CheckName);
    else
        nBad = sum(~Mask(:));
        [nFail, nPass] = fail_check(CheckName, sprintf('%d row(s) failed', nBad));
    end
end


function [nFail, nPass] = assert_unique(CheckName, Values)
    Values = Values(:);
    if isempty(Values)
        [nFail, nPass] = fail_check(CheckName, 'no values to check');
        return
    end
    if numel(unique(Values)) == numel(Values)
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('found %d values, %d unique', numel(Values), numel(unique(Values))));
    end
end


function [nFail, nPass] = assert_disjoint(CheckName, A, B)
    Overlap = intersect(A(:), B(:));
    if isempty(Overlap)
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('overlap: %s', mat2str(Overlap(:)')));
    end
end


function [nFail, nPass] = assert_divisible(CheckName, Value, Divisor)
    if mod(Value, Divisor) == 0
        [nFail, nPass] = pass_check(CheckName);
    else
        [nFail, nPass] = fail_check(CheckName, ...
            sprintf('value %d is not divisible by %d', Value, Divisor));
    end
end


function S = fmt(X)
    if isscalar(X)
        S = num2str(X);
    else
        S = mat2str(X);
    end
end
