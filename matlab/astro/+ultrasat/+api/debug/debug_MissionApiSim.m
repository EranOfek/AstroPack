% Run with: ultrasat.api.debug_MissionClientSim()
% https://chatgpt.com/c/67b1bc9e-869c-8012-b527-debac46e0d95

function debug_MissionApiSim()
    % debug_MissionClientSim - Main function to test MissionClientSim functionalities.
    %
    % Tests the login and logout functionality of the ultrasat.api.MissionClientSim class.
    
    %clc;
    fprintf('=== Testing ultrasat.api.MissionClientSim ===\n');
    
    % Set up test environment
    %DbPath = fullfile(pwd, 'DbPath'); % Use current directory for testing
    DbPath = fullfile(getenv('SOC_PATH'), 'sim', 'backend');
    if ~exist(DbPath, 'dir')
        %mkdir(DbPath);
    end

    % Create a sample users.json file
    % createSampleUsersFile(DbPath);

    % Initialize MissionClientSim object
    clientSim = ultrasat.api.MissionClientSim();   % 'DbPath', DbPath);  


    debugLogin(clientSim, 'chen', '123');
    

    debugGetPlansList(clientSim, [], [], []);  
    
    return;

    %debugSavePlan(clientSim);
    debugLoadPlan(clientSim, 2);
    return;

    % Test login
    fprintf('\n--- Testing login ---\n');
    debugLogin(clientSim, 'yossi', '123');          % Valid login
    debugLogin(clientSim, 'eran', 'wrong_pass');    % Invalid login

    % Test logout
    fprintf('\n--- Testing logout ---\n');
    debugLogout(clientSim, 'yossi');        % Valid logout
    debugLogout(clientSim, 'unknown_user'); % Invalid logout

    fprintf('\n--- Debugging getKeyValue ---\n');
    debugGetKeyValue(clientSim, 'Settings', 'Theme', 'Light');
    debugGetKeyValue(clientSim, 'Settings', 'NonExistentKey', 'Default');

    fprintf('\n--- Debugging setKeyValue ---\n');
    debugSetKeyValue(clientSim, 'Settings', 'Theme', 'Dark');
    debugSetKeyValue(clientSim, 'Settings', 'FontSize', 12);

    fprintf('\n--- Debugging getApprovedTargets ---\n');
    debugGetApprovedTargets(clientSim, datetime('2028-01-01 00:00:00'), datetime('2028-01-01 02:30:00'));    

    fprintf('\n--- Debugging getExposure ---\n');
    debugGetExposure(clientSim, 'sky_exposure', [101, 103], datetime('2028-01-01 00:00:00'), datetime('2028-01-01 04:00:00'), false);

    fprintf('\n--- Debugging savePlan ---\n');
    %debugSavePlan(clientSim);
    
    fprintf('\n--- Debugging getPlansList ---\n');
    debugGetPlansList(clientSim, [], [], 'first');  
    debugGetPlansList(clientSim, [], [], 'second');  
    debugGetPlansList(clientSim, [], [], 'plan');  


    debugGetPlansList(clientSim, [], [], []);  % Retrieve all plans
    debugGetPlansList(clientSim, '2025-03-01T00:00:00.000000Z', '2025-03-05T00:00:00.000000Z', []);  % Retrieve plans from a specific time range
    debugGetPlansList(clientSim, [], [], 'science');  % Retrieve plans with "science" in the title
    debugGetPlansList(clientSim, '2025-03-01T00:00:00.000000Z', '2025-03-05T00:00:00.000000Z', 'mission');  % Retrieve plans with both time range and title filter

    fprintf('\n--- Debugging loadPlan ---\n');
    debugLoadPlan(clientSim, 15);

    fprintf('\n--- Debugging deletePlan ---\n');
    debugDeletePlan(clientSim, 2);    

    fprintf('\n--- Debugging getPlanStatus ---\n');
    debugGetPlanStatus(clientSim, 1);

    fprintf('\n--- Debugging submit ---\n');
    debugSubmit(clientSim);

    fprintf('\n--- Debugging validate ---\n');
    debugValidate(clientSim);    

end
    
% =========================================================================

% =========================================================================

function debugLogin(clientSim, UserName, Password)
    % Tests the login functionality with provided username and password.
    fprintf('Attempting login for user: %s\n', UserName);
    response = clientSim.login(UserName, Password, 'MyNamespace');
    if response.ok
        fprintf('Login successful: %s\n', jsonencode(response));
    else
        fprintf('Login failed: %s\n', jsonencode(response));
    end
end


function debugLogout(clientSim, UserName)
    % Tests the logout functionality for the given username.
    fprintf('Attempting logout for user: %s\n', UserName);
    response = clientSim.logout(UserName);
    if response.ok
        fprintf('Logout successful: %s\n', jsonencode(response));
    else
        fprintf('Logout failed: %s\n', jsonencode(response));
    end
end


function createSampleUsersFile(DbPath)
    % Creates a sample users.json file in the specified DbPath.
    users = [
        struct('UserName', 'admin_user', 'Password', 'admin_pass', 'Role', 'Admin'), ...
        struct('UserName', 'planner_user', 'Password', 'planner_pass', 'Role', 'Planner'), ...
        struct('UserName', 'viewer_user', 'Password', 'viewer_pass', 'Role', 'Viewer')
    ];
    filePath = fullfile(DbPath, 'users.json');
    fid = fopen(filePath, 'w');
    fwrite(fid, jsonencode(users), 'char');
    fclose(fid);
    fprintf('Created sample users.json in %s\n', DbPath);
end

% =========================================================================

function debugGetKeyValue(clientSim, Store, Key, Default)
    fprintf('Getting value for Store: %s, Key: %s\n', Store, Key);
    response = clientSim.getKeyValue(Store, Key, Default);
    if response.ok
        fprintf('Value retrieved: %s\n', jsonencode(response.value));
    else
        fprintf('Failed to retrieve value.\n');
    end
end


function debugSetKeyValue(clientSim, Store, Key, Value)
    fprintf('Setting value for Store: %s, Key: %s to %s\n', Store, Key, jsonencode(Value));
    response = clientSim.setKeyValue(Store, Key, Value);
    if response.ok
        fprintf('Value set successfully.\n');
    else
        fprintf('Failed to set value.\n');
    end
end

% =========================================================================

function debugGetApprovedTargets(clientSim, start_time, end_time)
    fprintf('Getting approved targets from %s to %s\n', datestr(start_time), datestr(end_time));
    response = clientSim.getApprovedTargets(start_time, end_time);
    if response.ok
        fprintf('Approved targets retrieved:\n');
        for i = 1:numel(response.targets)
            disp(response.targets(i));
        end
    else
        fprintf('Failed to retrieve approved targets.\n');
    end
end


function createSampleTargetsFile(DbPath)
    % Creates a sample approved_targets.json file in the specified DbPath.
    targets = [
        struct('pk', 1, 'target_id', 'TGT001', 'ra', 11.0, 'decl', 12.0, 'roll', 0.0, ...
               'start_time', '2028-01-01T00:00:00.000000Z', 'end_time', '2028-01-01T00:15:00.000000Z', ...
               'exposure', 300, 'image_count', 3, 'total_seconds', 900), ...
        struct('pk', 2, 'target_id', 'TGT002', 'ra', 21.0, 'decl', 22.0, 'roll', 0.0, ...
               'start_time', '2028-01-01T01:00:00.000000Z', 'end_time', '2028-01-01T01:15:00.000000Z', ...
               'exposure', 300, 'image_count', 3, 'total_seconds', 900), ...
        struct('pk', 3, 'target_id', 'TGT003', 'ra', 31.0, 'decl', 32.0, 'roll', 0.0, ...
               'start_time', '2028-01-01T02:00:00.000000Z', 'end_time', '2028-01-01T02:15:00.000000Z', ...
               'exposure', 300, 'image_count', 3, 'total_seconds', 900), ...
        struct('pk', 4, 'target_id', 'TGT004', 'ra', 41.0, 'decl', 42.0, 'roll', 0.0, ...
               'start_time', '2028-01-01T03:00:00.000000Z', 'end_time', '2028-01-01T03:15:00.000000Z', ...
               'exposure', 300, 'image_count', 3, 'total_seconds', 900), ...
        struct('pk', 5, 'target_id', 'TGT005', 'ra', 51.0, 'decl', 52.0, 'roll', 0.0, ...
               'start_time', '2028-01-01T04:00:00.000000Z', 'end_time', '2028-01-01T04:15:00.000000Z', ...
               'exposure', 300, 'image_count', 3, 'total_seconds', 900)
    ];
    filePath = fullfile(DbPath, 'approved_targets.json');
    fid = fopen(filePath, 'w');
    fwrite(fid, jsonencode(targets), 'char');
    fclose(fid);
    fprintf('Created sample approved_targets.json in %s\n', DbPath);
end

% =========================================================================

function debugGetExposure(clientSim, table_name, healpix_indices, start_timestamp, end_timestamp, select_all)
    fprintf('Getting exposure data from table: %s\n', table_name);
    response = clientSim.getExposure(table_name, healpix_indices, start_timestamp, end_timestamp, select_all);
    if response.ok
        fprintf('Exposure data retrieved:\n');
        disp(struct2table(response.data));
    else
        fprintf('Failed to retrieve exposure data.\n');
    end
end


function createSampleExposureFile(DbPath)
    % Creates a sample sky_exposure.json file in DbPath.
    exposures = [
        struct('healpix_index', 101, 'num_exposures', 5, 'total_duration', 1200, ...
               'timestamps', {'2028-01-01T00:30:00.000000Z', '2028-01-01T01:00:00.000000Z'}), ...
        struct('healpix_index', 102, 'num_exposures', 3, 'total_duration', 900, ...
               'timestamps', {'2028-01-01T02:00:00.000000Z'}), ...
        struct('healpix_index', 103, 'num_exposures', 4, 'total_duration', 1500, ...
               'timestamps', {'2028-01-01T03:00:00.000000Z', '2028-01-01T03:30:00.000000Z'})
    ];
    filePath = fullfile(DbPath, 'sky_exposure.json');
    fid = fopen(filePath, 'w');
    fwrite(fid, jsonencode(exposures), 'char');
    fclose(fid);
    fprintf('Created sample sky_exposure.json in %s\n', DbPath);
end

% =========================================================================

function savePlanFiles(folder, planData)
    % Save JSON and MAT files for a given plan.
    jsonFile = fullfile(folder, sprintf('%d.json', planData.pk));
    matFile = fullfile(folder, sprintf('%d.mat', planData.pk));

    fid = fopen(jsonFile, 'w');
    fwrite(fid, jsonencode(rmfield(planData, 'matlab_mat')), 'char');
    fclose(fid);

    matlab_mat = planData.matlab_mat;
    save(matFile, 'matlab_mat');
end


function debugGetPlansList(clientSim, start_timestamp, end_timestamp, title_subtext)
    fprintf('Getting list of plans...\n');
    response = clientSim.getPlansList(start_timestamp, end_timestamp, title_subtext);
    if response.ok
        fprintf('Plans list retrieved successfully:\n');
        disp(struct2table(response.plans));
    else
        fprintf('Failed to retrieve plans list.\n');
    end
end


function debugLoadPlan(clientSim, pk)
    fprintf('Loading plan with pk=%d...\n', pk);
    response = clientSim.loadPlan(pk);
    if response.ok
        fprintf('Plan loaded successfully:\n');
        clientSim.PlanData.display();  % Display the loaded PlanData object
    else
        fprintf('Failed to load plan with pk=%d.\n', pk);
    end
end


function debugSavePlan(clientSim)
    fprintf('Saving new plan...\n');

    BaseDataDir = '~/matlab/data/ULTRASAT/';
    if ispc
        BaseDataDir =  'C:/AstroPack/Data/ULTRASAT/';
    end

    % Create a new PlanData instance
    newPlan = ultrasat.api.PlanData();
    newPlan.id = '20250105163045123';
    newPlan.created_by = 'new_user';
    newPlan.planner = ultrasat.planner.uplanner('AstPlanner','YS','Type','HCS', 'BaseDataDir', BaseDataDir);  % Instance of your UPlanner class
    newPlan.create_time = datetime('2025-01-05T16:30:45.123Z', 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z');
    newPlan.update_time = datetime('2025-01-06T16:30:45.123Z', 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS''Z');
    newPlan.status = 'pending';

    build = true;
    if build
        upHCS = newPlan.planner;
        HCS_fields = table({'S1','N2','N3'}',[67,215,254]',[-59,60,64]','VariableNames',{'Name','RA','Dec'},'RowNames',{'S1','N2','N3'}');
        upHCS.StartTime = 'now';
        upHCS.EndTime = upHCS.StartTime+calmonths(6)-days(1);
        upHCS.addUniqTargets(HCS_fields.RA('S1'),HCS_fields.Dec('S1'),'Name',HCS_fields.Name('S1'));
        upHCS.buildHCS;    
    end

    % Set the client's PlanData and save
    clientSim.PlanData = newPlan;
    response = clientSim.savePlan();

    if response.ok
        fprintf('Plan saved successfully: %s, pk: %d\n', response.message, clientSim.PlanData.pk);
    else
        fprintf('Failed to save plan.\n');
    end
end


function debugDeletePlan(clientSim, pk)
    fprintf('Deleting plan with pk=%d...\n', pk);
    response = clientSim.deletePlan(pk);
    if response.ok
        fprintf('Plan deleted successfully: %s\n', response.message);
    else
        fprintf('Failed to delete plan with pk=%d.\n', pk);
    end
end


function createSamplePlansFiles(DbPath)
    % Creates sample JSON and MAT plan files in DbPath.
    plansFolder = fullfile(DbPath, 'plans');
    if ~exist(plansFolder, 'dir')
        mkdir(plansFolder);
    end

    % Sample plan data
    planData1 = struct('pk', 1, 'id', '20250105143045123', 'created_by', 'admin_user', ...
        'plan_info', struct('details', 'Plan 1 details'), 'targets', struct('target_list', 'TGT001'), ...
        'matlab_mat', rand(3), 'create_time', '2025-01-05T14:30:45.123Z', ...
        'update_time', '2025-01-06T14:30:45.123Z', 'status', 'draft', 'metadata', struct(), ...
        'history', struct(), 'deleted', false);

    planData2 = struct('pk', 2, 'id', '20250105153045123', 'created_by', 'planner_user', ...
        'plan_info', struct('details', 'Plan 2 details'), 'targets', struct('target_list', 'TGT002'), ...
        'matlab_mat', rand(2), 'create_time', '2025-01-05T15:30:45.123Z', ...
        'update_time', '2025-01-06T15:30:45.123Z', 'status', 'submitted', 'metadata', struct(), ...
        'history', struct(), 'deleted', false);

    % Save plan 1
    savePlanFiles(plansFolder, planData1);
    % Save plan 2
    savePlanFiles(plansFolder, planData2);
end


% =========================================================================

function debugGetPlanStatus(clientSim, pk)
    fprintf('Getting plan status for pk=%d...\n', pk);
    response = clientSim.getPlanStatus(pk);
    if response.ok
        fprintf('Plan status retrieved successfully:\n');
        disp(response.data);
    else
        fprintf('Failed to retrieve plan status.\n');
    end
end

% =========================================================================

function debugSubmit(clientSim)
    fprintf('Submitting plan...\n');
    Plan = createSamplePlan();

    % Ensure clientSim.PlanData is already loaded or created
    if isempty(clientSim.PlanData)
        fprintf('No plan loaded. Please load a plan before submitting.\n');
        return;
    end

    response = clientSim.submitPlan(Plan);  % Updated method name
    if response.ok
        fprintf('Plan submitted successfully: %s\n', response.message);
    else
        fprintf('Failed to submit plan.\n');
    end
end


function debugValidate(clientSim)
    fprintf('Validating plan...\n');
    Plan = createSamplePlan();

    if isempty(clientSim.PlanData)
        fprintf('No plan loaded. Please load a plan before validating.\n');
        return;
    end

    response = clientSim.validatePlan(Plan);  % Updated method name
    if response.ok
        fprintf('Validation completed successfully:\n');
        disp(response.task);
    else
        fprintf('Validation failed.\n');
    end
end


function Plan = createSamplePlan()
    Plan(1) = struct(...
        'title', 'Target1', ...
        'ra', 11.0, ...
        'decl', 12.0, ...
        'roll', 0.0, ...
        'start_time', '2028-01-01T00:00:00.000Z', ...
        'end_time', '2028-01-01T00:30:00.000Z', ...
        'exposure', 300, ...
        'image_count', 3, ...
        'total_seconds', 900, ...
        'tiles', '1,2,3' ...
    );

    Plan(2) = struct(...
        'title', 'Target2', ...
        'ra', 21.0, ...
        'decl', 22.0, ...
        'roll', 0.0, ...
        'start_time', '2028-01-01T01:00:00.000Z', ...
        'end_time', '2028-01-01T01:30:00.000Z', ...
        'exposure', 300, ...
        'image_count', 3, ...
        'total_seconds', 900, ...
        'tiles', '4,5,6' ...
    );
end

% =========================================================================

