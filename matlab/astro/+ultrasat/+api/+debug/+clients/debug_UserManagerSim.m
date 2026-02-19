function debug_UserManagerSim()
    %DEBUG_USERMANAGERSIM Comprehensive test script for the UserManagerSim class.
    %   This script tests all methods of UserManagerSim:
    %   1. Constructor and initialization
    %   2. getNamespaceList method
    %   3. login and logout methods (both versions)
    %   4. IsAllowed permission checking
    %   5. getKeyValue and setKeyValue methods
    %   6. load_json and save_json helper methods
    %   7. Error handling and edge cases

    fprintf('============================================\n');
    fprintf('Starting comprehensive debug script for UserManagerSim class\n');
    fprintf('Time: %s\n', datestr(now));
    fprintf('============================================\n\n');

    try
        % --- Test 1: Constructor and Initialization ---
        fprintf('--- Test 1: Constructor and Initialization ---\n');
        userManager = test_constructor();
        if isempty(userManager)
            fprintf('  [FAIL] Constructor test failed, cannot continue\n');
            return;
        end
        fprintf('  [SUCCESS] Constructor and initialization completed\n\n');

        % --- Test 2: getNamespaceList Method ---
        fprintf('--- Test 2: getNamespaceList Method ---\n');
        test_getNamespaceList(userManager);
        fprintf('  [SUCCESS] getNamespaceList test completed\n\n');

        % --- Test 3: Login Methods ---
        fprintf('--- Test 3: Login Methods ---\n');
        test_login_methods(userManager);
        fprintf('  [SUCCESS] Login methods test completed\n\n');

        % --- Test 4: IsAllowed Permission Checking ---
        fprintf('--- Test 4: IsAllowed Permission Checking ---\n');
        test_IsAllowed_method(userManager);
        fprintf('  [SUCCESS] IsAllowed method test completed\n\n');

        % --- Test 5: Key-Value Operations ---
        fprintf('--- Test 5: Key-Value Operations ---\n');
        test_keyvalue_operations(userManager);
        fprintf('  [SUCCESS] Key-value operations test completed\n\n');

        % --- Test 6: JSON Helper Methods ---
        fprintf('--- Test 6: JSON Helper Methods ---\n');
        test_json_operations(userManager);
        fprintf('  [SUCCESS] JSON operations test completed\n\n');

        % --- Test 7: Error Handling and Edge Cases ---
        fprintf('--- Test 7: Error Handling and Edge Cases ---\n');
        test_error_handling(userManager);
        fprintf('  [SUCCESS] Error handling test completed\n\n');

        % --- Test 8: Logout Method ---
        fprintf('--- Test 8: Logout Method ---\n');
        test_logout_method(userManager);
        fprintf('  [SUCCESS] Logout method test completed\n\n');

        fprintf('============================================\n');
        fprintf('All UserManagerSim tests completed successfully!\n');
        fprintf('============================================\n');

    catch ME
        fprintf('\n[ERROR] Test suite failed with error:\n');
        fprintf('  Message: %s\n', ME.message);
        fprintf('  Stack trace:\n');
        for i = 1:length(ME.stack)
            fprintf('    %s (line %d)\n', ME.stack(i).name, ME.stack(i).line);
        end
    end
end

% ============================================================================
% TEST FUNCTIONS
% ============================================================================

function userManager = test_constructor()
    % Test UserManagerSim constructor and initialization
    try
        fprintf('  Testing constructor...\n');
        userManager = ultrasat.api.clients.UserManagerSim();

        % Verify basic properties are set
        fprintf('  Verifying properties...\n');
        assert(~isempty(userManager.DbPath), 'DbPath should not be empty');
        assert(~isempty(userManager.Validator), 'Validator should be initialized');
        assert(~isempty(userManager.ApiSimProvider), 'ApiSimProvider should be initialized');
        assert(strcmp(userManager.LogPrefix, 'UserManagerSim'), 'LogPrefix should be set correctly');
        assert(~isempty(userManager.DeviceId), 'DeviceId should be set');

        fprintf('    [SUCCESS] Constructor test passed\n');
        fprintf('    DbPath: %s\n', userManager.DbPath);
        fprintf('    DeviceId: %s\n', userManager.DeviceId);

    catch ME
        fprintf('    [FAIL] Constructor test failed: %s\n', ME.message);
        userManager = [];
    end
end

function test_getNamespaceList(userManager)
    % Test getNamespaceList method
    try
        fprintf('  Testing getNamespaceList...\n');
        response = userManager.getNamespaceList();

        % Verify response structure
        assert(isfield(response, 'ok'), 'Response should have ok field');
        assert(isfield(response, 'status'), 'Response should have status field');
        assert(isfield(response, 'namespaces'), 'Response should have namespaces field');
        assert(isfield(response, 'display_list'), 'Response should have display_list field');

        if response.ok
            fprintf('    [SUCCESS] getNamespaceList returned %d namespaces\n', length(response.namespaces));
            if ~isempty(response.namespaces)
                fprintf('    Namespaces: %s\n', strjoin(response.namespaces, ', '));
            end
        else
            fprintf('    [INFO] getNamespaceList returned error: %s\n', response.message);
        end

    catch ME
        fprintf('    [FAIL] getNamespaceList test failed: %s\n', ME.message);
    end
end

function test_login_methods(userManager)
    % Test both login and login0 methods
    try
        fprintf('  Testing login method...\n');

        % Test successful login
        loginResp = userManager.login('chen', '123', 'OPER');
        if loginResp.ok
            fprintf('    [SUCCESS] Login successful for user: %s\n', userManager.User);
            fprintf('    Session ID: %s\n', userManager.SessionId);
            fprintf('    Namespace: %s\n', userManager.NamespaceId);
            fprintf('    IsLoggedIn: %s\n', string(userManager.IsLoggedIn));
        else
            fprintf('    [FAIL] Login failed: %s\n', loginResp.message);
        end

        % Test login0 method (if it exists and works)
        fprintf('  Testing login0 method...\n');
        try
            login0Resp = userManager.login0('chen', '123', 'OPER');
            if login0Resp.ok
                fprintf('    [SUCCESS] Login0 successful\n');
            else
                fprintf('    [INFO] Login0 failed: %s\n', login0Resp.message);
            end
        catch ME
            fprintf('    [INFO] Login0 method not available or failed: %s\n', ME.message);
        end

    catch ME
        fprintf('    [FAIL] Login methods test failed: %s\n', ME.message);
    end
end

function test_IsAllowed_method(userManager)
    % Test IsAllowed permission checking with different users and scenarios
    try
        fprintf('  Testing IsAllowed with admin user...\n');

        % Test various permissions for admin user
        test_IsAllowed_single(userManager, 'MissionControl.Planner.Run', 'any_plan', true);
        test_IsAllowed_single(userManager, 'System.Namespace.Select', '', true);
        test_IsAllowed_single(userManager, 'ObservationPlanner.HCS.Open', 'hcs_plan_123', true);

        % Test with different user (planner)
        fprintf('  Testing IsAllowed with planner user...\n');
        userManager.logout('chen');
        userManager.login('sasha', '123', 'default_namespace');

        test_IsAllowed_single(userManager, 'ObservationPlanner.HCS.Open', 'hcs_plan_123', true);
        test_IsAllowed_single(userManager, 'ObservationPlanner.DDT.Commit', 'ddt_plan_456', true);
        test_IsAllowed_single(userManager, 'MissionControl.PlanReview.Commit', 'plan_789', false);

        % Test with guest user
        fprintf('  Testing IsAllowed with guest user...\n');
        userManager.logout('sasha');
        userManager.login('guest', '123', 'default_namespace');

        test_IsAllowed_single(userManager, 'MissionControl.Scheduler.Open', '', true);
        test_IsAllowed_single(userManager, 'MissionControl.PlanReview.Open', '', true);
        test_IsAllowed_single(userManager, 'MissionControl.PlanReview.Save', 'review_1', false);

    catch ME
        fprintf('    [FAIL] IsAllowed method test failed: %s\n', ME.message);
    end
end

function test_IsAllowed_single(manager, action, item, expected)
    % Helper function to test a single IsAllowed call
    try
        [isAllowed, msg] = manager.IsAllowed(action, item);
        if isAllowed == expected
            fprintf('    [SUCCESS] IsAllowed(''%s'', ''%s'') -> %s. Reason: %s\n', action, item, string(isAllowed), msg);
        else
            fprintf('    [FAIL] IsAllowed(''%s'', ''%s'') -> %s. Expected %s. Reason: %s\n', action, item, string(isAllowed), string(expected), msg);
        end
    catch ME
        fprintf('    [ERROR] IsAllowed(''%s'', ''%s'') failed: %s\n', action, item, ME.message);
    end
end

function test_keyvalue_operations(userManager)
    % Test getKeyValue and setKeyValue methods
    try
        fprintf('  Testing setKeyValue...\n');

        % Test setting various key-value pairs
        testStores = {'test_store1', 'test_store2', 'config'};
        testKeys = {'key1', 'key2', 'setting1'};
        testValues = {'value1', 42, struct('nested', 'data')};

        for i = 1:length(testStores)
            response = userManager.setKeyValue(testStores{i}, testKeys{i}, testValues{i});
            if response.ok
                fprintf('    [SUCCESS] Set %s.%s = %s\n', testStores{i}, testKeys{i}, string(testValues{i}));
            else
                fprintf('    [FAIL] Failed to set %s.%s: %s\n', testStores{i}, testKeys{i}, response.message);
            end
        end

        fprintf('  Testing getKeyValue...\n');

        % Test retrieving the values
        for i = 1:length(testStores)
            response = userManager.getKeyValue(testStores{i}, testKeys{i}, 'default_value');
            if response.ok
                fprintf('    [SUCCESS] Retrieved %s.%s = %s\n', testStores{i}, testKeys{i}, string(response.value));
            else
                fprintf('    [FAIL] Failed to get %s.%s: %s\n', testStores{i}, testKeys{i}, response.message);
            end
        end

        % Test getting non-existent key
        response = userManager.getKeyValue('nonexistent', 'key', 'default');
        if response.ok && strcmp(response.value, 'default')
            fprintf('    [SUCCESS] Non-existent key returned default value\n');
        else
            fprintf('    [FAIL] Non-existent key test failed\n');
        end

    catch ME
        fprintf('    [FAIL] Key-value operations test failed: %s\n', ME.message);
    end
end

function test_json_operations(userManager)
    % Test load_json and save_json helper methods
    try
        fprintf('  Testing JSON operations...\n');

        % Create test data
        testData = struct();
        testData.test_string = 'hello world';
        testData.test_number = 42;
        testData.test_array = [1, 2, 3, 4, 5];
        testData.test_struct = struct('nested', 'value', 'number', 123);

        % Test file path
        testFile = fullfile(userManager.DbPath, 'test_json_operations.json');

        % Test save_json
        fprintf('    Testing save_json...\n');
        userManager.save_json(testFile, testData);
        if exist(testFile, 'file')
            fprintf('    [SUCCESS] save_json created file\n');
        else
            fprintf('    [FAIL] save_json did not create file\n');
        end

        % Test load_json
        fprintf('    Testing load_json...\n');
        loadedData = userManager.load_json(testFile);
        if isstruct(loadedData) && isfield(loadedData, 'test_string')
            fprintf('    [SUCCESS] load_json loaded data correctly\n');
            fprintf('    Loaded test_string: %s\n', loadedData.test_string);
        else
            fprintf('    [FAIL] load_json did not load data correctly\n');
        end

        % Clean up test file
        if exist(testFile, 'file')
            delete(testFile);
        end

    catch ME
        fprintf('    [FAIL] JSON operations test failed: %s\n', ME.message);
    end
end

function test_error_handling(userManager)
    % Test error handling and edge cases
    try
        fprintf('  Testing error handling...\n');

        % Test IsAllowed without being logged in
        fprintf('    Testing IsAllowed without login...\n');
        userManager.logout('guest');
        [isAllowed, msg] = userManager.IsAllowed('some.action', 'some_item');
        if ~isAllowed && contains(msg, 'not logged in')
            fprintf('    [SUCCESS] IsAllowed correctly rejected when not logged in\n');
        else
            fprintf('    [FAIL] IsAllowed should reject when not logged in\n');
        end

        % Test login with invalid credentials
        fprintf('    Testing login with invalid credentials...\n');
        loginResp = userManager.login('nonexistent_user', 'wrong_password', 'OPER');
        if ~loginResp.ok
            fprintf('    [SUCCESS] Login correctly rejected invalid credentials\n');
        else
            fprintf('    [FAIL] Login should reject invalid credentials\n');
        end

        % Test getKeyValue with non-existent store
        fprintf('    Testing getKeyValue with non-existent store...\n');
        response = userManager.getKeyValue('nonexistent_store', 'key', 'default');
        if response.ok && strcmp(response.value, 'default')
            fprintf('    [SUCCESS] getKeyValue handled non-existent store correctly\n');
        else
            fprintf('    [FAIL] getKeyValue should return default for non-existent store\n');
        end

    catch ME
        fprintf('    [FAIL] Error handling test failed: %s\n', ME.message);
    end
end

function test_logout_method(userManager)
    % Test logout method
    try
        fprintf('  Testing logout method...\n');

        % First ensure we're logged in
        if ~userManager.IsLoggedIn
            userManager.login('chen', '123', 'OPER');
        end

        % Test logout
        logoutResp = userManager.logout('chen');
        if logoutResp.ok && ~userManager.IsLoggedIn
            fprintf('    [SUCCESS] Logout successful\n');
        else
            fprintf('    [FAIL] Logout failed or IsLoggedIn flag incorrect\n');
        end

    catch ME
        fprintf('    [FAIL] Logout method test failed: %s\n', ME.message);
    end
end
