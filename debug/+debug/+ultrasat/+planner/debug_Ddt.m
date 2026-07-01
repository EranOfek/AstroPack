%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_Ddt.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Description : Step-by-step debug for DDT (Director Discretionary Time) plans.
%               Code paths mirror uplanner.unitTest DDT block.
%
% Run by      : debug.ultrasat.planner.debug_Ddt()
%==========================================================================

function debug_Ddt()
    % Step-by-step DDT planner smoke tests mirroring uplanner.unitTest.

    fprintf('========== DEBUG DDT PLANNER ==========\n');

    % --- Step 1: Ensure data path ---
    debug_ensureDataPath();

    % --- Step 2: Basic single-group build ---
    debug_Ddt_basic();

    % --- Step 3: Multiple groups on different start times ---
    debug_Ddt_multipleGroups();

    % --- Step 4: Inspect plan summary ---
    debug_Ddt_inspect();

    fprintf('========== DEBUG DDT PLANNER DONE ==========\n');
end


function debug_Ddt_basic()
    % Single DDT group with two targets (from unitTest).

    fprintf('\n--- debug_Ddt_basic ---\n');

    fields = debug_sampleFieldsTable();
    upDDT = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'DDT');
    upDDT.addUniqTargets(fields.RA, fields.Dec, 'Name', num2cell(fields.Name));
    upDDT.addDDT2Plan([1, 2], datetime('now', 'TimeZone', 'UTC'));

    fprintf('DDT plan rows: %d\n', height(upDDT.Plan));
    fprintf('debug_Ddt_basic: OK\n');
end


function debug_Ddt_multipleGroups()
    % Two groups on different start times (from unitTest).

    fprintf('\n--- debug_Ddt_multipleGroups ---\n');

    fields = debug_sampleFieldsTable();
    upDDT = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'DDT');
    upDDT.addUniqTargets(fields.RA, fields.Dec, 'Name', num2cell(fields.Name));
    upDDT.addDDT2Plan([1, 2], datetime('now', 'TimeZone', 'UTC'));
    upDDT.addDDT2Plan([3, 2], datetime('tomorrow', 'TimeZone', 'UTC'));

    fprintf('DDT plan rows after 2 groups: %d\n', height(upDDT.Plan));

    if height(upDDT.Plan) > 0 && ismember('Group', upDDT.Plan.Properties.VariableNames)
        groups = unique(upDDT.Plan.Group);
        fprintf('Group IDs: %s\n', mat2str(groups'));
    end

    fprintf('debug_Ddt_multipleGroups: OK\n');
end


function debug_Ddt_inspect()
    % Build two groups and print plan summary.

    fprintf('\n--- debug_Ddt_inspect ---\n');

    fields = debug_sampleFieldsTable();
    upDDT = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'DDT');
    upDDT.addUniqTargets(fields.RA, fields.Dec, 'Name', num2cell(fields.Name));
    upDDT.addDDT2Plan([1, 2], datetime('now', 'TimeZone', 'UTC'));
    upDDT.addDDT2Plan([3, 2], datetime('tomorrow', 'TimeZone', 'UTC'));

    fprintf('Type:       %s\n', upDDT.Type);
    fprintf('UniqTarg:   %d targets\n', height(upDDT.UniqTarg));
    fprintf('Plan rows:  %d\n', height(upDDT.Plan));

    if height(upDDT.Plan) > 0
        fprintf('Plan columns: %s\n', strjoin(upDDT.Plan.Properties.VariableNames, ', '));
        if ismember('Group', upDDT.Plan.Properties.VariableNames)
            for g = unique(upDDT.Plan.Group)'
                n = sum(upDDT.Plan.Group == g);
                fprintf('  Group %g: %d rows\n', g, n);
            end
        end
    end

    fprintf('debug_Ddt_inspect: OK\n');
end


function T = debug_sampleFieldsTable()
    % Shared 3-field sample table used across planner debug scripts.

    T = table({'S1', 'N2', 'N3'}', [67, 215, 254]', [-59, 60, 64]', ...
        'VariableNames', {'Name', 'RA', 'Dec'}, 'RowNames', {'S1', 'N2', 'N3'});
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
