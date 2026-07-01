%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_Too.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Description : Step-by-step debug for TOO (Target of Opportunity) plans.
%               Code paths mirror uplanner.unitTest TOO block.
%               Uses debug/+debug/+ultrasat/+planner/input_data/ LVK probability map fixture.
%
% Run by      : debug.ultrasat.planner.debug_Too()
%==========================================================================

function debug_Too()
    % Step-by-step TOO planner smoke tests mirroring uplanner.unitTest.

    fprintf('========== DEBUG TOO PLANNER ==========\n');

    % --- Step 1: Ensure data path ---
    debug_ensureDataPath();

    % --- Step 2: Minimal explicit-target build ---
    debug_Too_minimal();

    % --- Step 3: Build from probability map (moderate coverage) ---
    debug_Too_withProbMap();

    % --- Step 4: Build from probability map (high coverage) ---
    debug_Too_highCoverage();

    fprintf('========== DEBUG TOO PLANNER DONE ==========\n');
end


function debug_Too_minimal()
    % Minimal TOO from explicit RA/Dec/Name (from unitTest).

    fprintf('\n--- debug_Too_minimal ---\n');

    fields = debug_sampleFieldsTable();
    upTOO = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'TOO');
    upTOO.buildTOO('RA', fields.RA, 'Dec', fields.Dec, 'Name', fields.Name);

    fprintf('%d exposures scheduled\n', height(upTOO.Plan));
    fprintf('debug_Too_minimal: OK\n');
end


function debug_Too_withProbMap()
    % TOO from local probability map: low target count, moderate coverage.

    fprintf('\n--- debug_Too_withProbMap ---\n');

    csvFile = debug_getLvcCsvPath();
    if isempty(csvFile)
        return;
    end

    upTOO = ultrasat.planner.uplanner('AstPlanner', 'AK', 'Type', 'TOO');
    upTOO.TOOMaxTargets = 4;
    upTOO.TOOMinCoveredProb = 0.3;
    upTOO.TOOWindowDuration = hours(3);
    upTOO.TOOAlertProbMap = readtable(csvFile);

    fprintf('Max targets: %d, min covered prob: %.2f, window: %s\n', ...
        upTOO.TOOMaxTargets, upTOO.TOOMinCoveredProb, char(upTOO.TOOWindowDuration));

    upTOO.buildTOO('Verbosity', 0, 'DrawMaps', 0);

    fprintf('%d exposures scheduled\n', height(upTOO.Plan));
    fprintf('debug_Too_withProbMap: OK\n');
end


function debug_Too_highCoverage()
    % TOO from same map with high coverage settings (from unitTest).

    fprintf('\n--- debug_Too_highCoverage ---\n');

    csvFile = debug_getLvcCsvPath();
    if isempty(csvFile)
        return;
    end

    upTOO = ultrasat.planner.uplanner('AstPlanner', 'AK', 'Type', 'TOO');
    upTOO.TOOMaxTargets = 100;
    upTOO.TOOMinCoveredProb = 0.9;
    upTOO.TOOWindowDuration = hours(5);
    upTOO.TOOAlertProbMap = readtable(csvFile);

    fprintf('Max targets: %d, min covered prob: %.2f, window: %s\n', ...
        upTOO.TOOMaxTargets, upTOO.TOOMinCoveredProb, char(upTOO.TOOWindowDuration));

    upTOO.buildTOO('Verbosity', 0, 'DrawMaps', 0);

    fprintf('%d exposures scheduled\n', height(upTOO.Plan));
    fprintf('debug_Too_highCoverage: OK\n');
end


function T = debug_sampleFieldsTable()
    % Shared 3-field sample table used across planner debug scripts.

    T = table({'S1', 'N2', 'N3'}', [67, 215, 254]', [-59, 60, 64]', ...
        'VariableNames', {'Name', 'RA', 'Dec'}, 'RowNames', {'S1', 'N2', 'N3'});
end


function csvPath = debug_getLvcCsvPath()
    % Resolve LVK probability map fixture from input_data/ beside this script.

    baseDir = fileparts(mfilename('fullpath'));
    csvPath = fullfile(baseDir, 'input_data', 'lvc_2024_04_01_00_40_58_000000.csv');
    if isfile(csvPath)
        return;
    end
    fprintf('WARNING: fixture not found: %s\n', csvPath);
    fprintf('Copy lvc_2024_04_01_00_40_58_000000.csv into debug/+debug/+ultrasat/+planner/input_data/.\n');
    csvPath = '';
end


function debug_ensureDataPath()
    % Set ASTROPACK_DATA_PATH fallback when unset.

    if ~isempty(getenv('ASTROPACK_DATA_PATH'))
        return;
    end
    fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
    if ispc
        setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
    else
        setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
    end
end
