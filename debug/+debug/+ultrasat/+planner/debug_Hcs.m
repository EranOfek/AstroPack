%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_Hcs.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Description : Step-by-step debug for HCS (High Cadence Survey) plans.
%               Code paths mirror uplanner.unitTest HCS block.
%
% Run by      : debug.ultrasat.planner.debug_Hcs()
%==========================================================================

function debug_Hcs()
    % Step-by-step HCS planner smoke tests mirroring uplanner.unitTest.

    fprintf('========== DEBUG HCS PLANNER ==========\n');

    % --- Step 1: Ensure data path ---
    debug_ensureDataPath();

    % --- Step 2: Basic single-target build ---
    debug_Hcs_basic();

    % --- Step 3: Inspect plan properties ---
    debug_Hcs_inspect();

    % --- Step 4: Extended window exposure scaling ---
    debug_Hcs_customExptime();

    fprintf('========== DEBUG HCS PLANNER DONE ==========\n');
end


function debug_Hcs_basic()
    % Minimal HCS build: single target, 6-month window (from unitTest).

    fprintf('\n--- debug_Hcs_basic ---\n');

    HCS_fields = debug_sampleFieldsTable();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'HCS', ...
        'StartTime', 'now', 'EndTime', datetime('now') + calmonths(6) - days(1));
    upHCS.addUniqTargets(HCS_fields.RA('S1'), HCS_fields.Dec('S1'), 'Name', HCS_fields.Name('S1'));
    upHCS.buildHCS;

    fprintf('HCS plan rows: %d\n', height(upHCS.Plan));
    fprintf('debug_Hcs_basic: OK\n');
end


function debug_Hcs_inspect()
    % Build HCS and print key plan properties for interactive inspection.

    fprintf('\n--- debug_Hcs_inspect ---\n');

    HCS_fields = debug_sampleFieldsTable();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'HCS', ...
        'StartTime', 'now', 'EndTime', datetime('now') + calmonths(6) - days(1));
    upHCS.addUniqTargets(HCS_fields.RA('S1'), HCS_fields.Dec('S1'), 'Name', HCS_fields.Name('S1'));
    upHCS.buildHCS;

    fprintf('Type:      %s\n', upHCS.Type);
    fprintf('StartTime: %s\n', char(upHCS.StartTime));
    fprintf('EndTime:   %s\n', char(upHCS.EndTime));
    fprintf('Exptime:   %s\n', char(upHCS.Exptime));
    fprintf('Tiles:     %s\n', char(upHCS.Tiles));
    fprintf('Plan rows: %d\n', height(upHCS.Plan));

    if height(upHCS.Plan) > 0
        fprintf('Plan columns: %s\n', strjoin(upHCS.Plan.Properties.VariableNames, ', '));
        if ismember('Nexposures', upHCS.Plan.Properties.VariableNames)
            fprintf('Nexposures (first row): %g\n', upHCS.Plan.Nexposures(1));
        end
    end

    fprintf('debug_Hcs_inspect: OK\n');
end


function debug_Hcs_customExptime()
    % Extended 12-month window to verify exposure count scaling.

    fprintf('\n--- debug_Hcs_customExptime ---\n');

    HCS_fields = debug_sampleFieldsTable();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'HCS', ...
        'StartTime', 'now', 'EndTime', datetime('now') + calmonths(12) - days(1));
    upHCS.addUniqTargets(HCS_fields.RA('S1'), HCS_fields.Dec('S1'), 'Name', HCS_fields.Name('S1'));
    upHCS.buildHCS;

    spanDays = days(upHCS.EndTime - upHCS.StartTime);
    fprintf('Plan span: %.1f days, plan rows: %d\n', spanDays, height(upHCS.Plan));

    if height(upHCS.Plan) > 0 && ismember('Nexposures', upHCS.Plan.Properties.VariableNames)
        fprintf('Nexposures (first row): %g\n', upHCS.Plan.Nexposures(1));
    end

    fprintf('debug_Hcs_customExptime: OK\n');
end


function T = debug_sampleFieldsTable()
    % Shared 3-field sample table used across planner debug scripts.

    T = table({'S1', 'N2', 'N3'}', [67, 215, 254]', [-59, 60, 64]', ...
        'VariableNames', {'Name', 'RA', 'Dec'}, 'RowNames', {'S1', 'N2', 'N3'});
end


function debug_ensureDataPath()
    % Set ASTROPACK_DATA_PATH fallback when unset (required by uplanner BaseDataDir).

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
