%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_validate.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 21/06/2026
% Description : Debug driver for ultrasat.planner.LcsHelper_v4_validate.
%               Asserts captured Report is returned, non-empty, and that a
%               valid baseline plan has warnings separated from hard failures.
%               Ends with optional standalone smoke (build + CSV dump).
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_validate()
%==========================================================================

function debug_LcsHelper_v4_validate()

    fprintf('\n========== DEBUG LcsHelper_v4_validate ==========\n');

    nOk = 0;
    nFail = 0;

    Tests = {
        'captureReport',        @debug_validate_captureReport
        'warningsOnlyBaseline', @debug_validate_warningsOnlyBaseline
        'standaloneSmoke',      @debug_validate_standaloneSmoke
    };

    for k = 1:size(Tests, 1)
        TestName = Tests{k, 1};
        TestFn = Tests{k, 2};
        fprintf('\n--- %s ---\n', TestName);
        try
            TestFn();
            nOk = nOk + 1;
            fprintf('[OK] %s\n', TestName);
        catch ME
            nFail = nFail + 1;
            fprintf(2, '[FAIL] %s: %s\n', TestName, ME.message);
            if ~isempty(ME.stack)
                fprintf(2, '       at %s (line %d)\n', ME.stack(1).name, ME.stack(1).line);
            end
        end
    end

    fprintf('\n========== DEBUG LcsHelper_v4_validate DONE ==========\n');
    fprintf('Tests: %d OK, %d failed (of %d)\n', nOk, nFail, size(Tests, 1));
    if nFail > 0
        error('debug_LcsHelper_v4_validate: %d test(s) failed', nFail);
    end
end


function debug_validate_captureReport()
    % Assert Capture returns non-empty Report and scalar counts.

    Obj = debug_buildValidPlan();
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, ...
        'Verbose', false, 'DumpCsv', false, ...
        'Capture', true, 'PrintToConsole', false);

    assert(isa(R, 'ultrasat.planner.LcsValidationResult'), ...
        'Result must be LcsValidationResult');
    assert(ischar(R.Report) && ~isempty(R.Report), 'Report must be non-empty char');
    assert(contains(R.Report, 'SUMMARY'), 'Report must contain SUMMARY section');
    assert(isscalar(R.nFail) && isscalar(R.nPass) && isscalar(R.nWarn), ...
        'nFail, nPass, nWarn must be scalar');
    assert(ischar(R.FailReport) && ischar(R.WarnReport), ...
        'FailReport and WarnReport must be char');

    Counts = debug_countReportMarkers(R.Report);
    fprintf('  nFail=%d nPass=%d nWarn=%d reportLen=%d\n', ...
        R.nFail, R.nPass, R.nWarn, numel(R.Report));
    fprintf('  parsed failLines=%d warnLines=%d\n', ...
        Counts.nFailLines, Counts.nWarnLines);
end


function debug_validate_warningsOnlyBaseline()
    % Valid Jan 5 plan: hard checks pass; warnings may be present but no [FAIL] lines.

    Obj = debug_buildValidPlan();
    R = ultrasat.planner.LcsHelper_v4_validate(Obj, ...
        'Verbose', false, 'DumpCsv', false, ...
        'Capture', true, 'PrintToConsole', false);

    assert(R.nFail == 0, 'Baseline plan must pass hard checks (nFail==0), got %d', R.nFail);
    assert(R.nPass > 0, 'Baseline plan must have passing checks');
    assert(R.passed(), 'Baseline plan must pass (R.passed() == true)');

    Counts = debug_countReportMarkers(R.Report);
    assert(Counts.nFailLines == 0, ...
        'Report must contain no [FAIL] lines for baseline, got %d', Counts.nFailLines);

    if R.nWarn > 0
        assert(R.hasWarnings(), 'R.hasWarnings() must be true when nWarn>0');
        assert(Counts.nWarnLines > 0, ...
            'nWarn=%d but report has no [WARN] lines (excluding [WARN-OK])', R.nWarn);
        assert(contains(R.Report, 'VALIDATION PASSED WITH WARNINGS'), ...
            'Report banner must say PASSED WITH WARNINGS when nWarn>0');
        assert(~isempty(R.WarnReport), 'WarnReport must be non-empty when nWarn>0');
        assert(strcmp(R.Status, 'passed_with_warnings'), ...
            'Status must be passed_with_warnings when nWarn>0');
    else
        assert(contains(R.Report, 'ALL CHECKS PASSED'), ...
            'Report banner must say ALL CHECKS PASSED when nWarn==0');
        assert(isempty(R.WarnReport), 'WarnReport must be empty when nWarn==0');
        assert(strcmp(R.Status, 'passed'), 'Status must be passed when nWarn==0');
    end

    assert(isempty(R.FailReport), 'FailReport must be empty when nFail==0');

    fprintf('  baseline: nFail=%d nWarn=%d failLines=%d warnLines=%d\n', ...
        R.nFail, R.nWarn, Counts.nFailLines, Counts.nWarnLines);
end


function debug_validate_standaloneSmoke()
    % Full standalone validation (build Jan 5 2029 + CSV dump) for manual inspection.

    fprintf('  Running standalone LcsHelper_v4_validate() ...\n');
    R = ultrasat.planner.LcsHelper_v4_validate();
    fprintf('  standalone: nFail=%d nPass=%d nWarn=%d status=%s\n', ...
        R.nFail, R.nPass, R.nWarn, R.Status);
end


function Obj = debug_buildValidPlan()
    % Build one valid LcsHelper_v4 plan (Jan 5 2029) shared by capture tests.

    RepoRoot = getenv('ASTROPACK_PATH');
    if isempty(RepoRoot)
        error('ASTROPACK_PATH is not set');
    end
    CsvFile = fullfile(RepoRoot, 'matlab', 'astro', '+ultrasat', '+planner', 'data', 'LCS_fields.csv');
    if ~isfile(CsvFile)
        error('Grid file not found: %s', CsvFile);
    end

    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate', datetime(2029, 1, 5), ...
        'AllSkyTable', CsvFile, ...
        'Verbose', false, ...
        'validate_after_schedule', false, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    if isempty(Obj) || isempty(Obj.Schedule)
        error('Failed to build valid plan for validation tests');
    end
end


function Counts = debug_countReportMarkers(Report)
    % Count [FAIL], [WARN] (excluding [WARN-OK]), and [PASS] lines in captured report.

    Counts.nFailLines = 0;
    Counts.nWarnLines = 0;
    Counts.nPassLines = 0;
    if isempty(Report)
        return
    end

    Lines = splitlines(string(Report));
    Trimmed = strtrim(Lines);
    Counts.nFailLines = sum(startsWith(Trimmed, '[FAIL]'));
    Counts.nWarnLines = sum(startsWith(Trimmed, '[WARN]') & ~startsWith(Trimmed, '[WARN-OK]'));
    Counts.nPassLines = sum(startsWith(Trimmed, '[PASS]'));
end
