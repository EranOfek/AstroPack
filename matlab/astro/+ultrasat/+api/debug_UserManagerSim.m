function debug_UserManagerSim()
    %DEBUG_USERMANAGERSIM Test script for the UserManagerSim class.
    %   This script performs an integration test by:
    %   1. Setting up a temporary, simulated database directory.
    %   2. Writing the users.json, roles.json, and permissions.json files.
    %   3. Instantiating UserManagerSim to use this temporary database.
    %   4. Running login, permission checks (IsAllowed), and logout tests.
    %   5. Cleaning up the temporary directory.
    
    fprintf('============================================\n');
    fprintf('Starting debug script for UserManagerSim class\n');
    fprintf('Time: %s (Israel Daylight Time)\n', datestr(now));
    fprintf('============================================\n\n');

    % --- Setup: Create a temporary database environment ---
    simDbPath = fullfile(tempdir, 'UserManagerSim_TestDB');
    cleanupObj = onCleanup(@() cleanup(simDbPath)); % Ensure cleanup runs on exit/error
    
    fprintf('--- Step 1: Setting up simulated database at: %s ---\n', simDbPath);
    if isfolder(simDbPath); rmdir(simDbPath, 's'); end
    mkdir(fullfile(simDbPath, 'users'));
    
    % Write the necessary JSON files to the temp directory
    createFiles = false;
    if createFiles
        createJsonFile(fullfile(simDbPath, 'users', 'users.json'), getUsersJsonContent());
        createJsonFile(fullfile(simDbPath, 'users', 'roles.json'), getRolesJsonContent());
        createJsonFile(fullfile(simDbPath, 'users', 'permissions.json'), getPermissionsJsonContent());
        fprintf('  [SUCCESS] Simulated database files created.\n\n');        
    end

    % --- Test 1: Instantiation and Login ---
    fprintf('--- Step 2: Instantiating UserManagerSim and logging in ---\n');
    try
        % We pass the temp path to the constructor's 'SubUrl' which is
        % interpreted as a local path by ApiSimProvider.
        userManager = ultrasat.api.UserManagerSim('SubUrl', simDbPath);
    catch ME
        fprintf('  [FAIL] Could not instantiate UserManagerSim: %s\n', ME.message);
        return;
    end
    
    % Test successful login
    fprintf('Attempting login for user "chen"...\n');
    loginResp = userManager.login('chen', '123', 'default_namespace');
    if loginResp.ok
        fprintf('  [SUCCESS] Login successful for user: %s\n', userManager.User);
    else
        fprintf('  [FAIL] Login failed: %s\n', loginResp.message);
        return; % Cannot proceed if login fails
    end
    
    % --- Test 2: Permission Checks (IsAllowed) ---
    fprintf('\n--- Step 3: Testing permissions for user "chen" (role: admin) ---\n');
    test_IsAllowed(userManager, 'MissionControl.Planner.Run', 'any_plan', true);
    test_IsAllowed(userManager, 'System.Namespace.Select', '', true);
    
    % --- Test 3: Switch user to "sasha" (role: planner) ---
    fprintf('\n--- Step 4: Testing permissions for user "sasha" (role: planner) ---\n');
    userManager.logout('chen');
    userManager.login('sasha', '123', 'default_namespace');
    fprintf('Logged in as user: %s\n', userManager.User);
    
    test_IsAllowed(userManager, 'ObservationPlanner.HCS.Open', 'hcs_plan_123', true);
    test_IsAllowed(userManager, 'ObservationPlanner.DDT.Commit', 'ddt_plan_456', true);
    test_IsAllowed(userManager, 'MissionControl.PlanReview.Commit', 'plan_789', false); % Planners cannot commit reviews

    % --- Test 4: Switch user to "guest" ---
    fprintf('\n--- Step 5: Testing permissions for user "guest" ---\n');
    userManager.logout('sasha');
    userManager.login('guest', '123', 'default_namespace');
    fprintf('Logged in as user: %s\n', userManager.User);
    
    test_IsAllowed(userManager, 'MissionControl.Scheduler.Open', '', true);
    test_IsAllowed(userManager, 'MissionControl.PlanReview.Open', '', true); % Guest can open (read-only)
    test_IsAllowed(userManager, 'MissionControl.PlanReview.Save', 'review_1', false); % Guest cannot save
    
    % --- Test 5: Logout ---
    fprintf('\n--- Step 6: Testing logout ---\n');
    logoutResp = userManager.logout('guest');
    if logoutResp.ok && ~userManager.IsLoggedIn
        fprintf('  [SUCCESS] Logout successful.\n');
    else
        fprintf('  [FAIL] Logout failed or IsLoggedIn flag is incorrect.\n');
    end

    fprintf('\n============================================\n');
    fprintf('Debug script for UserManagerSim finished.\n');
    fprintf('============================================\n');
end

% --- Helper functions for testing ---

function test_IsAllowed(manager, action, item, expected)
    [isAllowed, msg] = manager.IsAllowed(action, item);
    if isAllowed == expected
        fprintf('  [SUCCESS] IsAllowed(''%s'', ''%s'') -> %s. Reason: %s\n', action, item, string(isAllowed), msg);
    else
        fprintf('  [FAIL] IsAllowed(''%s'', ''%s'') -> %s. Expected %s. Reason: %s\n', action, item, string(isAllowed), string(expected), msg);
    end
end


function createJsonFile(filePath, content)
    fid = fopen(filePath, 'w');
    if fid == -1; error('Could not create test file: %s', filePath); end
    fwrite(fid, content, 'char');
    fclose(fid);
end


function cleanup(folderPath)
    fprintf('\n--- Cleaning up temporary directory ---\n');
    if isfolder(folderPath)
        try
            rmdir(folderPath, 's');
            fprintf('  [SUCCESS] Deleted: %s\n', folderPath);
        catch ME
            fprintf('  [FAIL] Could not delete directory: %s. Error: %s\n', folderPath, ME.message);
        end
    end
end


% --- Functions to provide JSON content ---
% (Embedding the JSON content makes the test script self-contained)

function jsonStr = getUsersJsonContent()
    jsonStr = '{ "users": { "admin": { "display_name": "Administrator", "password": "123", "roles": ["admin"], "is_active": true }, "chen": { "display_name": "Chen Tishler", "password": "123", "roles": ["admin"], "is_active": true }, "sasha": { "display_name": "Sasha", "password": "123", "roles": ["planner"], "is_active": true }, "guest": { "display_name": "Guest", "password": "123", "roles": ["guest"], "is_active": true } } }';
end

function jsonStr = getRolesJsonContent()
    jsonStr = '{ "roles": { "admin": { "display_name": "Administrator", "permissions": ["*"] }, "planner": { "display_name": "Observation Planner", "permissions": [ "ObservationPlanner.*", "MissionControl.Planner.*", "MissionControl.Scheduler.*" ] }, "guest": { "display_name": "Guest User", "permissions": [ "MissionControl.Scheduler.Open", "MissionControl.PlanList.History.Open", "MissionControl.PlanReview.Open" ] } } }';
end

function jsonStr = getPermissionsJsonContent()
    jsonStr = fileread('permissions.json'); % It's easier to read the large permissions file
end
