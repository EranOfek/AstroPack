%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_PlansManagerSaveUpdatePlanWithHistoryAndMetadata.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Debug save-then-update flow via PlansManagerClient.savePlan.
%               Tests with metadata and history: Scenario A (empty but present)
%               and Scenario B (populated with data).
%==========================================================================

function debug_PlansManagerSaveUpdatePlanWithHistoryAndMetadata()
    fprintf('========== DEBUG SAVE/UPDATE PLAN (metadata & history) ==========\n');

    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    debug_saveThenUpdatePlan_empty(client);
    debug_saveThenUpdatePlan_withData(client);

    fprintf('========== DEBUG SAVE/UPDATE PLAN DONE ==========\n');
end


function [PlanData, upHCS] = debug_createPlannerPlanData()
    % Create uplanner HCS + PlanData (RA=215, Dec=60, 1 Jan 2028 - 31 Jul 2028).
    BaseDataDir = getBaseDataDir();
    PlanData = ultrasat.api.models.PlanData();
    StartTime = datetime(2028, 1, 1, 'TimeZone', 'UTC');
    EndTime = datetime(2028, 7, 31, 'TimeZone', 'UTC');
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'debug_user', 'Type', 'HCS', ...
        'StartTime', StartTime, 'EndTime', EndTime, ...
        'BaseDataDir', BaseDataDir);
    upHCS.addUniqTargets(215, 60, 'Name', 'debug_target');
    upHCS.buildHCS('HCS_UniqTarg', 1);
    PlanData.planner = upHCS;
    ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);
end


function debug_saveThenUpdatePlan_empty(client)
    % Scenario A: metadata and history empty but present (newMetadata + struct()).
    fprintf('\n--- debug_saveThenUpdatePlan_empty (metadata & history empty, present) ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();
        planStruct = PlanData.toStruct();
        % Keep metadata and history - PlanData defaults: newMetadata(), history=struct()
        debug_runSaveThenUpdate(client, planStruct, 'empty');
    catch ME
        fprintf('debug_saveThenUpdatePlan_empty failed: %s\n', ME.message);
    end
end


function debug_saveThenUpdatePlan_withData(client)
    % Scenario B: metadata and history populated with data.
    fprintf('\n--- debug_saveThenUpdatePlan_withData (metadata & history with data) ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();
        PlanData.setStatus('ValidationStatus', 'OK', struct('ShortStatus', 'Valid'));
        PlanData.addHistory('Debug test entry');
        planStruct = PlanData.toStruct();
        % Keep metadata and history populated
        debug_runSaveThenUpdate(client, planStruct, 'withData');
    catch ME
        fprintf('debug_saveThenUpdatePlan_withData failed: %s\n', ME.message);
    end
end


function debug_runSaveThenUpdate(client, planStruct, scenarioName)
    % Shared save-then-update flow. planStruct already has metadata/history as needed.
    try
        % Step 1: Save (insert)
        resp = client.savePlan(planStruct);
        fprintf('Step 1 savePlan [%s]: ok=%d, status=%s\n', scenarioName, resp.ok, debug_getStatus(resp));
        if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
            fprintf('Save failed, cannot run update\n');
            return;
        end
        savedPk = resp.data;
        fprintf('Saved pk=%d, %d targets\n', savedPk, numel(planStruct.targets));

        % Step 2: Modify plan data
        planStruct.pk = savedPk;
        planStruct.title = ['updated_debug_plan_' scenarioName];
        planStruct.status = 'draft';

        % Modify targets array
        if ~isempty(planStruct.targets)
            t = planStruct.targets;
            if iscell(t)
                t{1}.name = ['updated_debug_target_' scenarioName];
                if isfield(t{1}, 'exposure'), t{1}.exposure = 600; end
                planStruct.targets = t;
            else
                t(1).name = ['updated_debug_target_' scenarioName];
                if isfield(t(1), 'exposure'), t(1).exposure = 600; end
                planStruct.targets = t;
            end
        end

        % Step 3: Update (save again with pk)
        resp2 = client.savePlan(planStruct);
        fprintf('Step 2 savePlan (update) [%s]: ok=%d, status=%s\n', scenarioName, resp2.ok, debug_getStatus(resp2));
        if resp2.ok && isfield(resp2, 'data') && ~isempty(resp2.data)
            updatedPk = resp2.data;
            fprintf('Updated pk=%d\n', updatedPk);
            if updatedPk ~= savedPk
                fprintf('WARNING: update returned different pk (%d vs %d)\n', updatedPk, savedPk);
            end

            % Round-trip: getPlan and verify metadata/history returned
            debug_roundTripCheck(client, updatedPk, scenarioName);
        else
            fprintf('Update failed\n');
        end
    catch ME
        fprintf('debug_runSaveThenUpdate [%s] failed: %s\n', scenarioName, ME.message);
    end
end


function debug_roundTripCheck(client, planPk, scenarioName)
    % Fetch plan and verify metadata and history fields are present.
    fprintf('Round-trip check [%s]: getPlan pk=%d ...\n', scenarioName, planPk);
    try
        resp = client.getPlan(planPk);
        if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
            fprintf('  getPlan failed\n');
            return;
        end
        plan = resp.data;
        hasMeta = isfield(plan, 'metadata') && ~isempty(plan.metadata);
        hasHist = isfield(plan, 'history');
        fprintf('  metadata present: %d, history present: %d\n', hasMeta, hasHist);
        if hasMeta
            fprintf('  metadata fields: %s\n', strjoin(fieldnames(plan.metadata), ', '));
        end
        if hasHist && isstruct(plan.history)
            n = numel(plan.history);
            fprintf('  history entries: %d\n', n);
        end
    catch ME
        fprintf('  round-trip check failed: %s\n', ME.message);
    end
end


function s = debug_getStatus(response)
    if isfield(response, 'status')
        s = response.status;
    else
        s = '';
    end
end


function BaseDataDir = getBaseDataDir()
    if ispc
        BaseDataDir = fullfile(getenv('ASTROPACK_DATA_PATH'), 'ULTRASAT');
    else
        BaseDataDir = '~/matlab/data/ULTRASAT/';
    end
end
