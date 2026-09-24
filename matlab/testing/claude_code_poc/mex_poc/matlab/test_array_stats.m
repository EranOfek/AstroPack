% test_array_stats.m
% Simple pass/fail test suite for array_stats_wrapper / array_stats MEX.
% No Toolboxes required.
%
% NOTE: In MATLAB scripts, all local function definitions must appear AFTER
% all script-level statements — that is why the helper functions are at the
% bottom of this file.

fprintf('\n==============================\n');
fprintf(' array_stats TEST SUITE\n');
fprintf('==============================\n\n');

passed = 0;
total  = 0;

% =========================================================================
% TEST 1: Normal 1D vector
% =========================================================================
total = total + 1;
ok = run_test('Normal 1D vector [1 2 3 4 5]', @() t1(), false);
passed = passed + ok;

% =========================================================================
% TEST 2: Normal 2D matrix (magic square)
% =========================================================================
total = total + 1;
ok = run_test('Normal 2D matrix magic(4)', @() t2(), false);
passed = passed + ok;

% =========================================================================
% TEST 3: Single element
% =========================================================================
total = total + 1;
ok = run_test('Single element [42.0]', @() t3(), false);
passed = passed + ok;

% =========================================================================
% TEST 4: Large random array — verify against MATLAB built-ins
% =========================================================================
total = total + 1;
ok = run_test('Large random array (10000 elements)', @() t4(), false);
passed = passed + ok;

% =========================================================================
% TEST 5: Error case — wrong input type (integer array)
% =========================================================================
total = total + 1;
ok = run_test('Error: wrong input type (int32 array)', @() t5(), true);
passed = passed + ok;

% =========================================================================
% TEST 6: Error case — empty array
% =========================================================================
total = total + 1;
ok = run_test('Error: empty array []', @() t6(), true);
passed = passed + ok;

% =========================================================================
% TEST 7: Edge case — array containing NaN values
% =========================================================================
total = total + 1;
ok = run_test('Edge case: array with NaN [1 2 NaN 4 5]', @() t7(), false);
passed = passed + ok;

% =========================================================================
% Summary
% =========================================================================
fprintf('==============================\n');
fprintf(' RESULTS: %d/%d tests passed\n', passed, total);
fprintf('==============================\n\n');

if passed == total
    fprintf('ALL TESTS PASSED\n\n');
else
    fprintf('SOME TESTS FAILED — see details above\n\n');
end

% =========================================================================
% LOCAL FUNCTIONS — must appear after all script-level statements in MATLAB
% =========================================================================

% -------------------------------------------------------------------------
% run_test: run a test, catch errors, report PASS/FAIL
%   name         — string label
%   fn           — function handle (no args)
%   expect_error — true if the test is supposed to throw
% -------------------------------------------------------------------------
function ok = run_test(name, fn, expect_error)
    fprintf('Test: %s\n', name);
    try
        fn();
        if expect_error
            fprintf('  FAIL — expected an error but none was thrown.\n\n');
            ok = false;
        else
            fprintf('  PASS\n\n');
            ok = true;
        end
    catch ME
        if expect_error
            fprintf('  PASS (caught expected error: %s)\n\n', ME.identifier);
            ok = true;
        else
            fprintf('  FAIL — unexpected error: %s\n         %s\n\n', ...
                    ME.identifier, ME.message);
            ok = false;
        end
    end
end

% -------------------------------------------------------------------------
function t1()
    A = [1.0 2.0 3.0 4.0 5.0];
    s = array_stats_wrapper(A);
    exp_mean = mean(A);
    exp_std  = std(A);
    exp_min  = min(A);
    exp_max  = max(A);
    exp_cnt  = numel(A);
    tol = 1e-10;
    assert(abs(s.mean    - exp_mean) < tol, 'mean mismatch:    got %.6f expected %.6f', s.mean,    exp_mean);
    assert(abs(s.std_dev - exp_std)  < tol, 'std_dev mismatch: got %.6f expected %.6f', s.std_dev, exp_std);
    assert(abs(s.min_val - exp_min)  < tol, 'min mismatch:     got %.6f expected %.6f', s.min_val, exp_min);
    assert(abs(s.max_val - exp_max)  < tol, 'max mismatch:     got %.6f expected %.6f', s.max_val, exp_max);
    assert(s.element_count == exp_cnt,       'count mismatch:   got %d expected %d',     s.element_count, exp_cnt);
    fprintf('  mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
            s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
end

% -------------------------------------------------------------------------
function t2()
    A = magic(4);
    s = array_stats_wrapper(A);
    flat = A(:);
    exp_mean = mean(flat);
    exp_std  = std(flat);
    exp_min  = min(flat);
    exp_max  = max(flat);
    exp_cnt  = numel(flat);
    tol = 1e-10;
    assert(abs(s.mean    - exp_mean) < tol, 'mean mismatch');
    assert(abs(s.std_dev - exp_std)  < tol, 'std_dev mismatch');
    assert(abs(s.min_val - exp_min)  < tol, 'min mismatch');
    assert(abs(s.max_val - exp_max)  < tol, 'max mismatch');
    assert(s.element_count == exp_cnt,       'count mismatch');
    fprintf('  mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
            s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
end

% -------------------------------------------------------------------------
function t3()
    A = 42.0;
    s = array_stats_wrapper(A);
    tol = 1e-10;
    assert(abs(s.mean    - 42.0) < tol, 'mean should be 42');
    assert(abs(s.std_dev - 0.0)  < tol, 'std_dev of single element should be 0');
    assert(abs(s.min_val - 42.0) < tol, 'min should be 42');
    assert(abs(s.max_val - 42.0) < tol, 'max should be 42');
    assert(s.element_count == 1,          'count should be 1');
    fprintf('  mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
            s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
end

% -------------------------------------------------------------------------
function t4()
    rng(42);
    A = randn(1, 10000);
    s = array_stats_wrapper(A);
    exp_mean = mean(A);
    exp_std  = std(A);
    exp_min  = min(A);
    exp_max  = max(A);
    tol = 1e-10;
    assert(abs(s.mean    - exp_mean) < tol, 'mean mismatch (large)');
    assert(abs(s.std_dev - exp_std)  < tol, 'std_dev mismatch (large)');
    assert(abs(s.min_val - exp_min)  < tol, 'min mismatch (large)');
    assert(abs(s.max_val - exp_max)  < tol, 'max mismatch (large)');
    assert(s.element_count == 10000,        'count mismatch (large)');
    fprintf('  mean=%.6f  std=%.6f  min=%.6f  max=%.6f  n=%d\n', ...
            s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
end

% -------------------------------------------------------------------------
function t5()
    array_stats_wrapper(int32([1 2 3]));
end

% -------------------------------------------------------------------------
function t6()
    array_stats_wrapper([]);
end

% -------------------------------------------------------------------------
function t7()
    A = [1.0 2.0 NaN 4.0 5.0];
    s = array_stats_wrapper(A);
    valid    = A(~isnan(A));
    exp_mean = mean(valid);
    exp_std  = std(valid);
    exp_min  = min(valid);
    exp_max  = max(valid);
    exp_cnt  = numel(A);    % total count including NaN
    tol = 1e-10;
    fprintf('  NaN-aware stats: mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
            s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
    fprintf('  Expected:        mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
            exp_mean, exp_std, exp_min, exp_max, exp_cnt);
    assert(abs(s.mean    - exp_mean) < tol, 'NaN test mean mismatch');
    assert(abs(s.std_dev - exp_std)  < tol, 'NaN test std mismatch');
    assert(abs(s.min_val - exp_min)  < tol, 'NaN test min mismatch');
    assert(abs(s.max_val - exp_max)  < tol, 'NaN test max mismatch');
    assert(s.element_count == exp_cnt,       'NaN test count mismatch');
end
