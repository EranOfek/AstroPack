%==========================================================================
% ULTRASAT
%
% File:   debug_MissionClient.m
% Author: Chen Tishler
% Updated: 24/03/2025
%
%==========================================================================

function debug_MissionClient()
    % Main debug function for MissionApiClient (FastAPI plans_manager).
    % Tests getPlansList, loadPlan, savePlan. Other methods are stubbed (Not supported).

    fprintf('========== DEBUG MISSION API CLIENT (plans_manager) ==========\n');
    fprintf('Set SOC_API_BASE (e.g. http://localhost:8321), SOC_API_KEY if required.\n');
    fprintf('===============================================================\n\n');

    debugGetApprovedTargets();  % Stub: Not supported
    debugPlansList();
    debugLoadSavePlan();
    debugValidateSubmitPlan();  % Stub: Not supported
    debugPlanStatus();          % Stub: Not supported
    debugDeletePlan();          % Stub: Not supported
    debugExposure();            % Stub: Not supported

    fprintf('\n========== DEBUG COMPLETED ==========\n');
end

function client = createTestClient()
    % Helper function to create a MissionApiClient for FastAPI plans_manager testing.
    % Set SOC_API_BASE, SOC_API_KEY, or pass defaults (localhost:8321).

    apiUrl = getenv('SOC_API_BASE');
    if isempty(apiUrl)
        apiUrl = 'http://localhost:8321';
    end
    apiKey = getenv('SOC_API_KEY');
    namespace = 'OPER';
    client = ultrasat.api.MissionApiClient('ApiUrl', apiUrl, 'Namespace', namespace, 'ApiKey', apiKey);
    fprintf('Created MissionApiClient: ApiUrl=%s, Namespace=%s\n', client.ApiUrl, client.Client.Namespace);
end

function debugLogin()
    % Test the login functionality

    fprintf('\n===== DEBUG LOGIN =====\n');
    client = createTestClient();

    % Test valid login
    fprintf('Testing valid login...\n');
    response = client.login('test_user', 'test_password');
    fprintf('Login response:\n');
    disp(response);

    % Test invalid login
    fprintf('\nTesting invalid login...\n');
    response = client.login('invalid_user', 'wrong_password');
    fprintf('Invalid login response:\n');
    disp(response);
end

function debugLogout()
    % Test the logout functionality

    fprintf('\n===== DEBUG LOGOUT =====\n');
    client = createTestClient();

    % First login to get a valid session
    client.login('test_user', 'test_password');

    % Then test logout
    fprintf('Testing logout...\n');
    response = client.logout('test_user');
    fprintf('Logout response:\n');
    disp(response);
end

function debugKeyValue()
    % Test the key-value store functionality

    fprintf('\n===== DEBUG KEY-VALUE STORE =====\n');
    client = createTestClient();

    % Test setting a key-value pair
    fprintf('Testing setKeyValue...\n');
    store_name = 'debug_store';
    key_name = 'debug_key';
    value = ['Debug value set at ', datestr(now)];

    response = client.setKeyValue(store_name, key_name, value);
    fprintf('setKeyValue response:\n');
    disp(response);

    % Test getting the key-value pair
    fprintf('\nTesting getKeyValue...\n');
    response = client.getKeyValue(store_name, key_name, 'Default value');
    fprintf('getKeyValue response:\n');
    disp(response);
    fprintf('Retrieved value: %s\n', response.value);

    % Test getting a non-existent key
    fprintf('\nTesting getKeyValue with non-existent key...\n');
    response = client.getKeyValue(store_name, 'nonexistent_key', 'Default value');
    fprintf('getKeyValue response for non-existent key:\n');
    disp(response);
    fprintf('Default value returned: %s\n', response.value);
end

function debugGetApprovedTargets()
    % Test the getApprovedTargets functionality

    fprintf('\n===== DEBUG GET APPROVED TARGETS =====\n');
    client = createTestClient();

    % Prepare function parameters
    fprintf('Testing getApprovedTargets...\n');
    start_time = datetime(2024, 1, 1, 0, 0, 0);
    end_time = datetime(2025, 12, 31, 0, 0, 0);

    % Call the API function
    response = client.getApprovedTargets(start_time, end_time);
    fprintf('getApprovedTargets response:\n');
    disp(response);

    % Check if targets were returned
    if isfield(response, 'targets') && ~isempty(response.targets)
        fprintf('Found %d targets\n', numel(response.targets));

        % Display first target details
        fprintf('\nFirst target details:\n');
        disp(response.targets(1));

        % Save results for further analysis
        save(fullfile(tempdir, 'api_response.mat'), 'response');
        target1 = response.targets(1);
        save(fullfile(tempdir, 'api_target1.mat'), 'target1');

        % Convert array of structs to Table
        try
            TargetsTable = struct2table(response.targets);
            fprintf('\nTargets as table:\n');
            disp(TargetsTable);

            % Convert Table back to array of struct
            TargetsArray = table2struct(TargetsTable);
            fprintf('\nTable converted back to struct array - first entry:\n');
            disp(TargetsArray(1));
        catch ME
            fprintf('Error converting targets to table: %s\n', ME.message);
        end
    else
        fprintf('No targets found or invalid response\n');
    end
end

function debugPlansList()
    % Test the getPlansList functionality

    fprintf('\n===== DEBUG GET PLANS LIST =====\n');
    client = createTestClient();

    % Define time range for the past 10 years
    start_time = datetime('now') - years(5);
    end_time = datetime('now') + years(5);

    % Call the API function without filters
    fprintf('Testing getPlansList with no filters...\n');
    response = client.getPlansList();
    fprintf('getPlansList response:\n');
    disp(response);

    if isfield(response, 'plans') && ~isempty(response.plans)
        fprintf('Found %d plans\n', numel(response.plans));

        % Display first plan details
        fprintf('\nFirst plan details:\n');
        disp(response.plans(1));
    else
        fprintf('No plans found or invalid response\n');
    end

    % Call the API function with time filters
    fprintf('\nTesting getPlansList with time filters...\n');
    response = client.getPlansList(start_time, end_time);
    fprintf('getPlansList with time filters response:\n');
    disp(response);

    % Call the API function with time filters and title search
    fprintf('\nTesting getPlansList with time filters and title search...\n');
    response = client.getPlansList(start_time, end_time, 'Test');
    fprintf('getPlansList with time filters and title search response:\n');
    disp(response);
end

function debugLoadSavePlan()
    % Test the loadPlan and savePlan functionality

    fprintf('\n===== DEBUG LOAD/SAVE PLAN =====\n');
    client = createTestClient();

    % First get a list of plans to find one to load
    plans_response = client.getPlansList();

    if ~isfield(plans_response, 'plans') || isempty(plans_response.plans)
        fprintf('No plans found to load\n');
        return;
    end

    % Get the first plan's PK
    plan_pk = plans_response.plans(1).pk;

    % Test loading a plan
    fprintf('Testing loadPlan with pk=%d...\n', plan_pk);
    response = client.loadPlan(plan_pk);
    fprintf('loadPlan response:\n');
    disp(response);

    if response.ok
        fprintf('Plan loaded successfully\n');
        fprintf('Plan data:\n');
        disp(client.PlanData);

        % Test saving a plan
        fprintf('\nTesting savePlan...\n');
        % Make a small change to the plan
        client.PlanData.title = [client.PlanData.title, ' (Updated)'];
        save_response = client.savePlan();
        fprintf('savePlan response:\n');
        disp(save_response);

        if save_response.ok
            fprintf('Plan saved successfully\n');
        else
            fprintf('Failed to save plan\n');
        end
    else
        fprintf('Failed to load plan\n');
    end
end

function debugValidateSubmitPlan()
    % Test the validatePlan and submitPlan functionality

    fprintf('\n===== DEBUG VALIDATE/SUBMIT PLAN =====\n');
    client = createTestClient();

    % First load a plan to work with
    plans_response = client.getPlansList();

    if ~isfield(plans_response, 'plans') || isempty(plans_response.plans)
        fprintf('No plans found to validate/submit\n');
        return;
    end

    % Get the first plan's PK
    plan_pk = plans_response.plans(1).pk;
    client.loadPlan(plan_pk);

    if isempty(client.PlanData)
        fprintf('Failed to load plan for validation\n');
        return;
    end

    % Get targets for validation/submission
    if isfield(client.PlanData, 'targets') && ~isempty(client.PlanData.targets)
        targets = client.PlanData.targets;

        % Test plan validation
        fprintf('Testing validatePlan...\n');
        validate_response = client.validatePlan(targets);
        fprintf('validatePlan response:\n');
        disp(validate_response);

        if validate_response.ok
            fprintf('Plan validated successfully\n');

            % Test plan submission (commented out to prevent actual submission)
            fprintf('\nTesting submitPlan...\n');
            fprintf('(Actual submission commented out in the code to prevent side effects)\n');

            % Uncomment this line to actually submit the plan
            % submit_response = client.submitPlan(targets);
            % fprintf('submitPlan response:\n');
            % disp(submit_response);
        else
            fprintf('Plan validation failed\n');
        end
    else
        fprintf('No targets found in the plan\n');
    end
end

function debugPlanStatus()
    % Test the getPlanStatus functionality

    fprintf('\n===== DEBUG GET PLAN STATUS =====\n');
    client = createTestClient();

    % First get a list of plans to find one to check
    plans_response = client.getPlansList();

    if ~isfield(plans_response, 'plans') || isempty(plans_response.plans)
        fprintf('No plans found to check status\n');
        return;
    end

    % Get the first plan's PK
    plan_pk = plans_response.plans(1).pk;

    % Test getting plan status
    fprintf('Testing getPlanStatus with pk=%d...\n', plan_pk);
    response = client.getPlanStatus(plan_pk);
    fprintf('getPlanStatus response:\n');
    disp(response);

    if response.ok && isfield(response, 'data')
        fprintf('Plan status: %s\n', response.data.status);

        % Display history if available
        if isfield(response.data, 'history') && ~isempty(response.data.history)
            fprintf('\nPlan history:\n');
            disp(response.data.history);
        end

        % Display metadata if available
        if isfield(response.data, 'metadata') && ~isempty(response.data.metadata)
            fprintf('\nPlan metadata:\n');
            disp(response.data.metadata);
        end
    else
        fprintf('Failed to get plan status\n');
    end
end

function debugDeletePlan()
    % Test the deletePlan functionality

    fprintf('\n===== DEBUG DELETE PLAN =====\n');
    client = createTestClient();

    % First get a list of plans to find one to delete
    plans_response = client.getPlansList();

    if ~isfield(plans_response, 'plans') || isempty(plans_response.plans)
        fprintf('No plans found to delete\n');
        return;
    end

    % Get the last plan's PK (less likely to be important)
    plan_pk = plans_response.plans(end).pk;

    % Test deleting a plan (commented out to prevent actual deletion)
    fprintf('Testing deletePlan with pk=%d...\n', plan_pk);
    fprintf('(Actual deletion commented out in the code to prevent data loss)\n');

    % Uncomment this line to actually delete the plan
    % response = client.deletePlan(plan_pk);
    % fprintf('deletePlan response:\n');
    % disp(response);

    % Instead, display what would happen
    fprintf('If executed, this would delete plan with pk=%d\n', plan_pk);
end

function debugExposure()
    % Test the getExposure functionality

    fprintf('\n===== DEBUG GET EXPOSURE =====\n');
    client = createTestClient();

    % Define parameters for the exposure query
    table_name = 'exposure_data';
    healpix_indices = [1, 2, 3, 4];
    start_timestamp = datetime('now') - days(30);
    end_timestamp = datetime('now');
    select_all = false;

    % Test getExposure
    fprintf('Testing getExposure...\n');
    response = client.getExposure(table_name, healpix_indices, start_timestamp, end_timestamp, select_all);
    fprintf('getExposure response:\n');
    disp(response);

    if isfield(response, 'data') && ~isempty(response.data)
        fprintf('Found %d exposure records\n', numel(response.data));

        % Display first record
        fprintf('\nFirst exposure record:\n');
        disp(response.data(1));
    else
        fprintf('No exposure data found or invalid response\n');
    end

    % Test with select_all=true
    fprintf('\nTesting getExposure with select_all=true...\n');
    response = client.getExposure(table_name, healpix_indices, start_timestamp, end_timestamp, true);
    fprintf('getExposure response:\n');
    disp(response);

    if isfield(response, 'data') && ~isempty(response.data)
        fprintf('Found %d exposure records\n', numel(response.data));
    else
        fprintf('No exposure data found or invalid response\n');
    end
end

