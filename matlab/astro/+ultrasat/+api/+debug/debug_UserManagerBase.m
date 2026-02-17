function debug_UserManagerBase()
    %DEBUG_USERMANAGERBASE Test script for the UserManagerBase class helpers.
    %   This script tests the utility methods (matchMask, MatchParams, MergeParams)
    %   inherited by user manager classes. It creates a dummy child class to
    %   gain access to these protected methods for testing.

    fprintf('============================================\n');
    fprintf('Starting debug script for UserManagerBase class\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('============================================\n\n');

    % --- Setup ---
    % Since UserManagerBase is a base class, we create a simple child
    % class instance to test its protected methods.
    try
        testManager = ultrasat.api.UserManagerBase();
        fprintf('  [SUCCESS] DummyManager instance created.\n');
    catch ME
        fprintf('  [FAIL] Could not create DummyManager instance: %s\n', ME.message);
        return;
    end

    % --- Test Suite ---

    % Test 1: matchMask
    fprintf('\n--- Testing matchMask method ---\n');
    test_matchMask(testManager, 'data.csv', '*.csv', true);
    test_matchMask(testManager, 'data.log', '*.csv', false);
    test_matchMask(testManager, 'image_01.jpg', 'image_??.jpg', true);
    test_matchMask(testManager, 'image_abc.jpg', 'image_??.jpg', false);
    test_matchMask(testManager, 'MissionControl.PlanList.History.Open', 'MissionControl.PlanList.*', true);
    test_matchMask(testManager, 'ObservationPlanner.HCS.Open', 'ObservationPlanner.*', true);

    % Test 2: MergeParams
    fprintf('\n--- Testing MergeParams method ---\n');
    base = struct('user', 'chen', 'mode', 'read');
    override = struct('mode', 'write', 'level', 5);
    expectedMerge = struct('user', 'chen', 'mode', 'write', 'level', 5);

    merged = testManager.MergeParams_public(base, override);

    if isequal(merged, expectedMerge)
        fprintf('  [SUCCESS] MergeParams correctly merged structs.\n');
    else
        fprintf('  [FAIL] MergeParams did not produce the expected result.\n');
        disp('Expected:'); disp(expectedMerge);
        disp('Got:'); disp(merged);
    end

    % Test 3: MatchParams
    fprintf('\n--- Testing MatchParams method ---\n');
    required = struct('status', {{'approved', 'pending'}}, 'priority', {{'high'}});

    % Test case 1: Successful match
    effective_success = struct('status', 'pending', 'priority', 'high', 'extra_field', 'abc');
    test_MatchParams(testManager, required, effective_success, true);

    % Test case 2: Failed match (wrong value)
    effective_fail_value = struct('status', 'rejected', 'priority', 'high');
    test_MatchParams(testManager, required, effective_fail_value, false);

    % Test case 3: Failed match (missing key)
    effective_fail_key = struct('status', 'approved');
    test_MatchParams(testManager, required, effective_fail_key, false);

    fprintf('\n============================================\n');
    fprintf('Debug script for UserManagerBase finished.\n');
    fprintf('============================================\n');
end

% --- Helper functions for testing ---

function test_matchMask(manager, str, mask, expected)
    result = manager.matchMask_public(str, mask);
    if result == expected
        fprintf('  [SUCCESS] matchMask("%s", "%s") -> %s\n', str, mask, string(result));
    else
        fprintf('  [FAIL] matchMask("%s", "%s") -> %s (Expected: %s)\n', str, mask, string(result), string(expected));
    end
end


function test_MatchParams(manager, required, effective, expected)
    result = manager.MatchParams_public(required, effective);
    if result == expected
        fprintf('  [SUCCESS] MatchParams test case passed as expected (%s).\n', string(expected));
    else
        fprintf('  [FAIL] MatchParams test case failed. Expected %s but got %s.\n', string(expected), string(result));
    end
end

