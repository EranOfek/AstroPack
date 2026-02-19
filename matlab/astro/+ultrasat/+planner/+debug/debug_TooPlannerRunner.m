%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.debug_TooPlannerRunner.m
% Author      : Chen Tishler
% Created     : 19/02/2026
% Updated     : 19/02/2026
% Description : Debug function for TooPlannerRunner.
%               Creates sample CSV + JSON in debug folder, runs runFromJson,
%               verifies output. Requires SOC_PATH.
% Run by:     ultrasat.planner.debug.debug_TooPlannerRunner()
%==========================================================================

function debug_TooPlannerRunner()
    fprintf('========== DEBUG TOO PLANNER RUNNER ==========\n');

    % SOC_PATH required by Loggable (TooPlannerRunner base class)
    socPath = getenv('SOC_PATH');
    if isempty(socPath)
        fprintf('SOC_PATH not set. Setting fallback c:\\soc for local testing...\n');
        if ispc
            setenv('SOC_PATH', 'c:\soc');
        else
            setenv('SOC_PATH', '~/soc');
        end
    end

    debug_runFromJson();
    debug_runFromJsonInvalidConfig();

    fprintf('========== DEBUG TOO PLANNER RUNNER DONE ==========\n');
end


function debug_runFromJson()
    fprintf('\n--- debug_runFromJson ---\n');
    try
        baseDir = fileparts(mfilename('fullpath'));

        % 1. Write sample CSV (UNIQ, PROBDENSITY, RA, DEC)
        csvFilename = fullfile(baseDir, 'too_debug.csv');
        fid = fopen(csvFilename, 'w');
        fprintf(fid, 'UNIQ,PROBDENSITY,RA,DEC\n');
        fprintf(fid, '1040,2.207529977052296e-08,56.24999999999999,12.024699180565822\n');
        fprintf(fid, '1041,1.0761150121646524e-09,59.0625,14.477512185929925\n');
        fprintf(fid, '1044,5.385870698225297e-11,61.87499999999999,16.957763300004142\n');
        fprintf(fid, '1045,3.237669037930435e-12,64.6875,19.47122063449069\n');
        fclose(fid);

        % 2. Create output folder
        outputFolder = fullfile(baseDir, 'output');
        if ~isfolder(outputFolder)
            mkdir(outputFolder);
        end

        % 3. Build JSON config
        planCfg = struct( ...
            'label', 'fast_4', ...
            'TOOMaxTargets', 4, ...
            'TOOMinCoveredProb', 0.3, ...
            'TOOWindowDurationHours', 3, ...
            'Verbosity', 0, ...
            'DrawMaps', 0 ...
        );
        cfg = struct( ...
            'planner_name', 'AK', ...
            'csv_filename', csvFilename, ...
            'output_folder', outputFolder, ...
            'plans', planCfg ...
        );

        jsonFilename = fullfile(baseDir, 'too_debug.json');
        jsonStr = jsonencode(cfg, "PrettyPrint", true);
        fidJson = fopen(jsonFilename, 'w');
        fwrite(fidJson, jsonStr, 'char');
        fclose(fidJson);

        % 4. Run TooPlannerRunner
        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(jsonFilename);

        % 5. Verify output
        if ~isempty(summaryFileName)
            fprintf('summaryFileName: %s\n', summaryFileName);
            if isfile(summaryFileName)
                fprintf('summary.json exists: OK\n');
            else
                fprintf('summary.json not found\n');
            end
        else
            fprintf('runFromJson returned empty (check logs for errors)\n');
        end
    catch ME
        fprintf('debug_runFromJson failed: %s\n', ME.message);
        for s = 1:length(ME.stack)
            fprintf('  at %s (line %d)\n', ME.stack(s).name, ME.stack(s).line);
        end
    end
end


function debug_runFromJsonInvalidConfig()
    fprintf('\n--- debug_runFromJsonInvalidConfig ---\n');
    try
        baseDir = fileparts(mfilename('fullpath'));

        % Create JSON with missing csv_filename (invalid config)
        cfg = struct( ...
            'output_folder', fullfile(baseDir, 'output'), ...
            'plans', struct('label', 'test', 'TOOMaxTargets', 2) ...
        );
        jsonFilename = fullfile(baseDir, 'too_debug_invalid.json');
        jsonStr = jsonencode(cfg, "PrettyPrint", true);
        fidJson = fopen(jsonFilename, 'w');
        fwrite(fidJson, jsonStr, 'char');
        fclose(fidJson);

        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(jsonFilename);

        if isempty(summaryFileName)
            fprintf('Error path exercised: runFromJson returned empty as expected\n');
        else
            fprintf('Unexpected: runFromJson returned non-empty for invalid config\n');
        end
    catch ME
        fprintf('debug_runFromJsonInvalidConfig: %s (expected for invalid config)\n', ME.message);
    end
end
