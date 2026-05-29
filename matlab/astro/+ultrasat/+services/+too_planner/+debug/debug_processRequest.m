%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+too_planner/+debug/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 17/05/2026
% Description : Debug function for too_planner processRequest (flat API).
%               Uses planner +debug/input_data LVC fixture and working_dir/ for outputs.
%               Requires SOC_PATH and ASTROPACK_DATA_PATH (Windows).
% Run by      : ultrasat.services.too_planner.debug.debug_processRequest()
%==========================================================================

function debug_processRequest()

    % Get SOC_PATH environment variable
    socPath = getenv('SOC_PATH');
    if isempty(socPath)
        fprintf('SOC_PATH not set. Setting fallback for local testing...\n');
        if ispc
            setenv('SOC_PATH', 'c:\soc');
        else
            setenv('SOC_PATH', '~/soc');
        end
    end

    % Get ASTROPACK_DATA_PATH environment variable
    dataPath = getenv('ASTROPACK_DATA_PATH');
    if isempty(dataPath)
        fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
        if ispc
            setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
        else
            setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
        end
    end

    % Run tests
    debug_processRequestHealth();
    debug_processRequestTooPlanner();
end


function debug_processRequestHealth()
    % Run health test
    
    fprintf('\n=== TOO Planner: Health Test ===\n');
    try
        % Create input struct
        Input = struct('action', 'health');

        % Call processRequest        
        Output = ultrasat.services.too_planner.processRequest(Input);

        % Display output
        fprintf('status  : %s\n', Output.status);
        fprintf('message : %s\n', Output.message);

        if ~strcmp(Output.status, 'ok')
            error('Health check returned non-ok status: %s', Output.status);
        end
        fprintf('Test PASSED\n');
    catch ex
        fprintf('Exception: %s\n', ex.message);
        for s = 1:length(ex.stack)
            fprintf('  at %s (line %d)\n', ex.stack(s).name, ex.stack(s).line);
        end
    end
    fprintf('=== TEST COMPLETE ===\n\n');
end


function debug_processRequestTooPlanner()
    % Run TOO Planner: Plan Test (flat API)
    fprintf('\n=== TOO Planner: Plan Test (flat API) ===\n');
    try
        % Prepare working directory and output folder
        baseDir = fileparts(mfilename('fullpath'));
        workDir = fullfile(baseDir, 'working_dir');
        output_folder = fullfile(workDir, 'output');
        if ~isfolder(workDir)
            mkdir(workDir);
        end
        if ~isfolder(output_folder)
            mkdir(output_folder);
        end

        % Get LVC CSV file, using same csv file as in TooPlannerRunner test
        csv_filename = debug_getLvcCsvPath();

        % Create input struct
        Input = struct( ...
            'action', 'too_planner', ...
            'planner_name', 'AK', ...
            'csv_filename', csv_filename, ...
            'output_folder', output_folder, ...
            'plans', struct( ...
                'label', 'fast_4', ...
                'TOOMaxTargets', 4, ...
                'TOOMinCoveredProb', 0.3, ...
                'TOOWindowDurationHours', 3, ...
                'Verbosity', 0, ...
                'DrawMaps', 0 ...
            ), ...
            'timeout', 300 ...
        );

        fprintf('Input (flat):\n');
        disp(Input);

        % Simulate JsonFileIpc: persist request JSON and set IPC path metadata
        ipcJsonFile = fullfile(workDir, 'too_planner_request.json');
        jsonText = jsonencode(Input, 'PrettyPrint', true);
        fid = fopen(ipcJsonFile, 'wt');
        fwrite(fid, jsonText, 'char');
        fclose(fid);
        Input.IpcInputJsonFilename = ipcJsonFile;

        fprintf('\nCalling processRequest...\n');

        % Call processRequest
        Output = ultrasat.services.too_planner.processRequest(Input);

        % Display output
        fprintf('status                : %s\n', Output.status);
        fprintf('message               : %s\n', Output.message);
        fprintf('summary_file          : %s\n', Output.summary_file);
        fprintf('total_plans_attempted : %d\n', Output.total_plans_attempted);
        fprintf('total_plans_succeeded : %d\n', Output.total_plans_succeeded);
        fprintf('total_plans_failed    : %d\n', Output.total_plans_failed);

        % Display plan results
        if isfield(Output, 'plans') && ~isempty(Output.plans)
            fprintf('\n=== Plan Results (%d items) ===\n', numel(Output.plans));
            for i = 1:numel(Output.plans)
                fprintf('Plan %d: run_id=%s, status=%s, exposures=%d\n', ...
                    i, Output.plans(i).run_id, Output.plans(i).status, ...
                    Output.plans(i).exposures_scheduled);
            end
        end

        if ~strcmp(Output.status, 'ok')
            error('processRequest returned non-ok status: %s', Output.status);
        end
        if Output.total_plans_succeeded < 1
            error('processRequest: expected at least one successful plan, got %d', ...
                Output.total_plans_succeeded);
        end
        preservedJson = fullfile(output_folder, 'too_planner_request.json');
        if ~isfile(preservedJson)
            error('processRequest: expected preserved input JSON: %s', preservedJson);
        end
        fprintf('Test PASSED\n');
    catch ex
        fprintf('Exception in debug_processRequestTooPlanner: %s\n', ex.message);
        for s = 1:length(ex.stack)
            fprintf('  at %s (line %d)\n', ex.stack(s).name, ex.stack(s).line);
        end
    end
    fprintf('=== TEST COMPLETE ===\n\n');
end


function csvPath = debug_getLvcCsvPath()
    % Get LVC CSV file path, using same csv file as in TooPlannerRunner test
    
    baseDir = fileparts(mfilename('fullpath'));
    plannerDebugDir = fullfile(baseDir, '..', '..', '..', '+planner', '+debug');
    csvPath = fullfile(plannerDebugDir, 'input_data', 'lvc_2024_04_01_00_40_58_000000.csv');
    if ~isfile(csvPath)
        error(['debug_getLvcCsvPath: fixture not found: %s\n' ...
            'Copy lvc_2024_04_01_00_40_58_000000.csv into +planner/+debug/input_data/.'], csvPath);
    end
end
