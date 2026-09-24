%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_TooPlannerRunner.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 17/05/2026
% Description : Debug function for TooPlannerRunner.
%               Uses input_data/ fixtures and working_dir/ for generated files.
%               Requires SOC_PATH and ASTROPACK_DATA_PATH (Windows).
%
% Run by      : debug.ultrasat.planner.debug_TooPlannerRunner()
%==========================================================================

function debug_TooPlannerRunner()
    % Exercise TooPlannerRunner runFromJson success, failure, and invalid-config paths.

    fprintf('========== DEBUG TOO PLANNER RUNNER ==========\n');

    % --- Step 1: Ensure SOC_PATH (required by Loggable base class) ---
    socPath = getenv('SOC_PATH');

    if isempty(socPath)
        fprintf('SOC_PATH not set. Setting fallback c:\\soc for local testing...\n');
        if ispc
            setenv('SOC_PATH', 'c:\soc');
        else
            setenv('SOC_PATH', '~/soc');
        end
    end

    % --- Step 2: Ensure ASTROPACK_DATA_PATH (required by buildTOO) ---
    dataPath = getenv('ASTROPACK_DATA_PATH');
    if isempty(dataPath)
        fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
        if ispc
            setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
        else
            setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
        end
    end

    % --- Step 3: Run success/failure/invalid-config scenarios ---
    debug_runFromJson_expectFailure();
    debug_runFromJson_expectSuccess();
    debug_runFromJsonInvalidConfig();

    fprintf('========== DEBUG TOO PLANNER RUNNER DONE ==========\n');
end


function debug_runFromJson_expectFailure()
    % Expect zero plans scheduled when probability map CSV is too short.

    fprintf('\n--- debug_runFromJson_expectFailure ---\n');

    csvFile = debug_createTooShortCsv();
    debug_runFromJson(csvFile, false);
end


function debug_runFromJson_expectSuccess()
    % Expect at least one plan scheduled with full LVK probability map fixture.

    fprintf('\n--- debug_runFromJson_expectSuccess ---\n');

    csvFile = debug_getLvcCsvPath();
    debug_runFromJson(csvFile, true);
end


function debug_runFromJson(csvFilename, expectSuccess)
    % Write JSON config, run TooPlannerRunner, and assert summary outcome.

    fprintf('\n--- debug_runFromJson (expectSuccess=%d) ---\n', expectSuccess);

    baseDir = fileparts(mfilename('fullpath'));
    workDir = fullfile(baseDir, 'working_dir');
    outputFolder = fullfile(workDir, 'output');
    if ~isfolder(workDir)
        mkdir(workDir);
    end
    if ~isfolder(outputFolder)
        mkdir(outputFolder);
    end

    % Create plan configuration struct
    planCfg = struct( ...
        'label', 'fast_4', ...
        'TOOMaxTargets', 4, ...
        'TOOMinCoveredProb', 0.3, ...
        'TOOWindowDurationHours', 3, ...
        'Verbosity', 0, ...
        'DrawMaps', 0 ...
    );

    % Create JSON config struct
    cfg = struct( ...
        'planner_name', 'AK', ...
        'csv_filename', char(csvFilename), ...
        'output_folder', outputFolder, ...
        'plans', planCfg ...
    );

    % Create JSON filename
    if expectSuccess
        jsonTag = 'success';
    else
        jsonTag = 'failure';
    end

    % Create JSON filename based on expectSuccess parameter
    jsonFilename = fullfile(workDir, ['too_debug_run_' jsonTag '.json']);

    % Write JSON string to file
    jsonStr = jsonencode(cfg, "PrettyPrint", true);
    fidJson = fopen(jsonFilename, 'w');
    if fidJson < 0
        error('debug_runFromJson: cannot write %s', jsonFilename);
    end
    fwrite(fidJson, jsonStr, 'char');
    fclose(fidJson);

    % Create and run a TooPlannerRunner object
    runner = ultrasat.planner.TooPlannerRunner();
    summaryFileName = runner.runFromJson(jsonFilename);

    % Assert the result of the test
    debug_assertRunFromJsonResult(summaryFileName, expectSuccess);
end


function debug_runFromJsonInvalidConfig()
    % Expect empty summary when required JSON fields are missing.

    fprintf('\n--- debug_runFromJsonInvalidConfig ---\n');

    try
        baseDir = fileparts(mfilename('fullpath'));
        workDir = fullfile(baseDir, 'working_dir');
        outputFolder = fullfile(workDir, 'output');
        if ~isfolder(workDir)
            mkdir(workDir);
        end

        cfg = struct( ...
            'output_folder', outputFolder, ...
            'plans', struct('label', 'test', 'TOOMaxTargets', 2) ...
        );

        % Write JSON string to file
        jsonFilename = fullfile(workDir, 'too_debug_invalid.json');
        jsonStr = jsonencode(cfg, "PrettyPrint", true);
        fidJson = fopen(jsonFilename, 'w');
        fwrite(fidJson, jsonStr, 'char');
        fclose(fidJson);

        % Create and run a TooPlannerRunner object
        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(jsonFilename);

        if isempty(summaryFileName)
            fprintf('Error path exercised: runFromJson returned empty as expected\n');
        else
            error('debug_runFromJsonInvalidConfig: expected empty summary for invalid config');
        end
    catch ME
        if strcmp(ME.identifier, 'MATLAB:error')
            rethrow(ME);
        end
        fprintf('debug_runFromJsonInvalidConfig: %s\n', ME.message);
    end
end


function debug_assertRunFromJsonResult(summaryFileName, expectSuccess)
    % Assert summary file exists and plan counts match expected outcome.

    if isempty(summaryFileName) || ~isfile(summaryFileName)
        error('debug_runFromJson: summary file missing (expectSuccess=%d)', expectSuccess);
    end

    % Read summary file
    summary = jsondecode(fileread(summaryFileName));
    nSucceeded = summary.total_plans_succeeded;
    fprintf('summary: %s (succeeded=%d, failed=%d)\n', summaryFileName, nSucceeded, summary.total_plans_failed);

    if expectSuccess
        if nSucceeded < 1
            error('debug_runFromJson: expected at least one successful plan, got %d', nSucceeded);
        end
        if ~isfield(summary, 'plans') || isempty(summary.plans)
            error('debug_runFromJson: expected non-empty plans in summary');
        end

        % Get first plan from summary
        firstPlan = debug_firstSummaryPlan(summary.plans);

        % Assert first plan json_file exists
        if ~isfield(firstPlan, 'json_file') || ~isfile(firstPlan.json_file)
            error('debug_runFromJson: first plan json_file missing: %s', debug_safeField(firstPlan, 'json_file', ''));
        end

        fprintf('Plan success path OK: %s\n', firstPlan.json_file);
    else
        if nSucceeded ~= 0
            error('debug_runFromJson: expected zero successful plans, got %d', nSucceeded);
        end
        fprintf('Plan failure path OK (no plans scheduled)\n');
    end
end


function csvPath = debug_createTooShortCsv()
    % Write minimal CSV with one skymap row (insufficient for buildTOO).

    baseDir = fileparts(mfilename('fullpath'));
    workDir = fullfile(baseDir, 'working_dir');
    if ~isfolder(workDir)
        mkdir(workDir);
    end

    csvPath = fullfile(workDir, 'too_debug_short.csv');
    fid = fopen(csvPath, 'w');
    if fid < 0
        error('debug_createTooShortCsv: cannot write %s', csvPath);
    end
    fprintf(fid, 'UNIQ,PROBDENSITY,RA,DEC\n');
    fprintf(fid, '1040,2.207529977052296e-08,56.25,12.02\n');
    fclose(fid);
end


function csvPath = debug_getLvcCsvPath()
    % Resolve full LVK probability map fixture from input_data/.

    baseDir = fileparts(mfilename('fullpath'));
    csvPath = fullfile(baseDir, 'input_data', 'lvc_2024_04_01_00_40_58_000000.csv');
    if ~isfile(csvPath)
        error(['debug_getLvcCsvPath: fixture not found: %s\n' ...
            'Copy lvc_2024_04_01_00_40_58_000000.csv into debug/+debug/+ultrasat/+planner/input_data/ ' ...
            '(same file as ASTROPACK_DATA_PATH/ULTRASAT/).'], csvPath);
    end
end


function plan = debug_firstSummaryPlan(plans)
    % Extract first plan entry from cell or struct array summary.

    if iscell(plans)
        plan = plans{1};
    elseif isstruct(plans)
        plan = plans(1);
    else
        error('debug_firstSummaryPlan: unexpected plans type: %s', class(plans));
    end
end


function val = debug_safeField(s, fieldName, defaultVal)
    % Return struct field value or default when missing or empty.

    if isfield(s, fieldName) && ~isempty(s.(fieldName))
        val = s.(fieldName);
    else
        val = defaultVal;
    end
end
