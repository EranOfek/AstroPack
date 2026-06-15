%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.debug_Lcs.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Updated     : 31/05/2026
% Description : Step-by-step debug for LCS (Low Cadence Survey) plans.
%               Code paths mirror uplanner.unitTest LCS block.
%               HCS must be built before retrieveApproved (LCS depends on HCS).
% Run by      : ultrasat.planner.debug.debug_Lcs()
%==========================================================================

function debug_Lcs()

    fprintf('========== DEBUG LCS PLANNER ==========\n');

    debug_ensureDataPath();

    debug_Lcs_buildLCS1();
    debug_Lcs_buildLCS_legacy();
    debug_Lcs_retrieveApproved();
    debug_Lcs_editAndDelete();

    fprintf('========== DEBUG LCS PLANNER DONE ==========\n');
end


function debug_Lcs_buildLCS1()
    % New LCS (2026) via LcsHelper and surveys grid CSV

    fprintf('\n--- debug_Lcs_buildLCS1 ---\n');

    upLCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'LCS');
    upLCS.StartTime = '2029-02-01 00:00:00';
    upLCS.EndTime = upLCS.StartTime + caldays(420);
    upLCS.DailyWindowStartTime = duration('00:00:00');

    gridFile = fullfile(upLCS.BaseDataDir, 'LCS_fields.csv');
    if ~isfile(gridFile)
        error('debug_Lcs_buildLCS1: grid file not found: %s', gridFile);
    end

    LCS_grid = readtable(gridFile);
    upLCS.addUniqTargets(LCS_grid.RA, LCS_grid.Dec, 'Name', num2cell(LCS_grid.Field));
    upLCS.buildLCS1;

    fprintf('LCS plan rows (buildLCS1): %d\n', height(upLCS.Plan));
    fprintf('debug_Lcs_buildLCS1: OK\n');
end


function debug_Lcs_buildLCS_legacy()
    % Legacy buildLCS with visibility filter and TargetList

    fprintf('\n--- debug_Lcs_buildLCS_legacy ---\n');

    upLCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'LCS');
    upLCS.StartTime = 'now';
    upLCS.EndTime = upLCS.StartTime + caldays(45);
    upLCS.DailyWindowStartTime = duration('03:00:00');

    gridFile = fullfile(upLCS.BaseDataDir, 'LCS_nonoverlapping_grid.csv');
    if ~isfile(gridFile)
        error('debug_Lcs_buildLCS_legacy: grid file not found: %s', gridFile);
    end

    LCS_grid = readtable(gridFile);
    F = LCS_grid.V45 == 1 & LCS_grid.A_U_1 == 1;
    upLCS.addUniqTargets(LCS_grid.RA(F), LCS_grid.Dec(F), 'Name', num2cell(LCS_grid.Field(F)));

    upLCS.updateTargetVisibility('WindowStartTime', upLCS.StartTime, ...
        'WindowEndTime', upLCS.EndTime);
    F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits, 1));

    fprintf('Visible targets: %d of %d\n', numel(F2), height(upLCS.UniqTarg));

    upLCS.buildLCS('TargetList', F2);

    fprintf('LCS plan rows (buildLCS legacy): %d\n', height(upLCS.Plan));
    fprintf('debug_Lcs_buildLCS_legacy: OK\n');
end


function debug_Lcs_retrieveApproved()
    % HCS -> LCS handoff via retrieveMissionApprovedPlan (unitTest)

    fprintf('\n--- debug_Lcs_retrieveApproved ---\n');

    % Build minimal HCS plan first (same as unitTest dependency)
    fields = debug_sampleFieldsTable();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'HCS', ...
        'StartTime', 'now', 'EndTime', datetime('now') + calmonths(6) - days(1));
    upHCS.addUniqTargets(fields.RA('S1'), fields.Dec('S1'), 'Name', fields.Name('S1'));
    upHCS.buildHCS;
    fprintf('HCS plan rows for handoff: %d\n', height(upHCS.Plan));

    upLCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'LCS');
    upLCS.StartTime = 'now';
    upLCS.EndTime = upLCS.StartTime + caldays(45);
    upLCS.DailyWindowStartTime = duration('03:00:00');

    gridFile = fullfile(upLCS.BaseDataDir, 'LCS_nonoverlapping_grid.csv');
    LCS_grid = readtable(gridFile);
    F = LCS_grid.V45 == 1 & LCS_grid.A_U_1 == 1;
    upLCS.addUniqTargets(LCS_grid.RA(F), LCS_grid.Dec(F), 'Name', num2cell(LCS_grid.Field(F)));
    upLCS.updateTargetVisibility('WindowStartTime', upLCS.StartTime, ...
        'WindowEndTime', upLCS.EndTime);
    F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits, 1));

    % From HCS Plan table
    upLCS.retrieveMissionApprovedPlan('inputPlan', upHCS.Plan);
    fprintf('retrieveMissionApprovedPlan from HCS.Plan: OK\n');

    % From api_response.mat struct (if present)
    apiFile = fullfile(upLCS.BaseDataDir, 'api_response.mat');
    if isfile(apiFile)
        S = load(apiFile);
        if isfield(S, 'response')
            try
                upLCS.retrieveMissionApprovedPlan('inputPlan', S.response);
                fprintf('retrieveMissionApprovedPlan from api_response.mat: OK\n');
            catch ME
                fprintf('WARNING: api_response.mat struct test failed: %s\n', ME.message);
            end
        else
            fprintf('WARNING: api_response.mat has no ''response'' variable; skipping struct test\n');
        end
    else
        fprintf('WARNING: api_response.mat not found; skipping struct test\n');
    end

    upLCS.buildLCS('TargetList', F2);
    fprintf('LCS plan rows after approved handoff: %d\n', height(upLCS.Plan));
    fprintf('debug_Lcs_retrieveApproved: OK\n');
end


function debug_Lcs_editAndDelete()
    % Edit/delete plan rows and unique targets (unitTest LCS edit block)

    fprintf('\n--- debug_Lcs_editAndDelete ---\n');

    upLCS = debug_buildLcsForEdit();

    upLCS.adjustGroupStartTime;
    fprintf('adjustGroupStartTime: OK\n');

    checkStatus = upLCS.planSelfConsistencyCheck;
    if ~checkStatus
        error('debug_Lcs_editAndDelete: planSelfConsistencyCheck failed');
    end
    fprintf('planSelfConsistencyCheck: OK\n');

    upLCS.editUniqTarg(4, 'Name', "bla");
    upLCS.editUniqTarg(4, 'RA', 100);

    upLCS.editPlanRow(1);
    upLCS.editPlanRow(1, 'Tiles', "124");
    upLCS.editPlanRow(1, 'updateRowsProp', true);
    upLCS.editPlanRow(1, 'Nexposures', 2);
    upLCS.editPlanRow(1, 'ExpTime', seconds(250));
    upLCS.editPlanRow(10, 'ExpTime', seconds(250));
    upLCS.editPlanRow(5, 'ExpTime', seconds(250));

    upLCS.delPlanRow(10);
    upLCS.delPlanRow(3);
    upLCS.delPlanRow(1);

    upLCS.delUniqTarg(5, 'abort_if_in_plan', false);

    fprintf('Plan rows after edits/deletes: %d\n', height(upLCS.Plan));
    fprintf('debug_Lcs_editAndDelete: OK\n');
end


function upLCS = debug_buildLcsForEdit()
    % Build LCS plan with enough rows for edit/delete exercises

    fields = debug_sampleFieldsTable();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'HCS', ...
        'StartTime', 'now', 'EndTime', datetime('now') + calmonths(6) - days(1));
    upHCS.addUniqTargets(fields.RA('S1'), fields.Dec('S1'), 'Name', fields.Name('S1'));
    upHCS.buildHCS;

    upLCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'LCS');
    upLCS.StartTime = 'now';
    upLCS.EndTime = upLCS.StartTime + caldays(45);
    upLCS.DailyWindowStartTime = duration('03:00:00');

    LCS_grid = readtable(fullfile(upLCS.BaseDataDir, 'LCS_nonoverlapping_grid.csv'));
    F = LCS_grid.V45 == 1 & LCS_grid.A_U_1 == 1;
    upLCS.addUniqTargets(LCS_grid.RA(F), LCS_grid.Dec(F), 'Name', num2cell(LCS_grid.Field(F)));
    upLCS.updateTargetVisibility('WindowStartTime', upLCS.StartTime, ...
        'WindowEndTime', upLCS.EndTime);
    F2 = find(all(upLCS.Vis.SunLimits & upLCS.Vis.EarthLimits & upLCS.Vis.MoonLimits, 1));

    upLCS.retrieveMissionApprovedPlan('inputPlan', upHCS.Plan);
    upLCS.buildLCS('TargetList', F2);
end


function T = debug_sampleFieldsTable()
    T = table({'S1', 'N2', 'N3'}', [67, 215, 254]', [-59, 60, 64]', ...
        'VariableNames', {'Name', 'RA', 'Dec'}, 'RowNames', {'S1', 'N2', 'N3'});
end


function debug_ensureDataPath()
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
