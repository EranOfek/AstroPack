%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_PlansManagerSaveUpdatePlan.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Debug save-then-update flow via PlansManagerClient.savePlan.
%               Saves a plan, modifies data (title, targets), then saves again as update.
%==========================================================================

function debug_PlansManagerSaveUpdatePlan()
    fprintf('========== DEBUG SAVE/UPDATE PLAN ==========\n');

    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    debug_saveThenUpdatePlan(client);

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


function debug_saveThenUpdatePlan(client)
    % Save plan, then modify and update.
    fprintf('\n--- debug_saveThenUpdatePlan ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();

        % Step 1: Save (insert)
        planStruct = PlanData.toStruct();

        resp = client.savePlan(planStruct);
        fprintf('Step 1 savePlan: ok=%d, status=%s\n', resp.ok, debug_getStatus(resp));
        if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
            fprintf('Save failed, cannot run update\n');
            return;
        end
        savedPk = resp.data;
        fprintf('Saved pk=%d, %d targets\n', savedPk, numel(planStruct.targets));

        % Step 2: Modify plan data
        planStruct.pk = savedPk;
        planStruct.title = 'updated_debug_plan';
        planStruct.status = 'draft';

        % Modify targets array
        if ~isempty(planStruct.targets)
            t = planStruct.targets;
            if iscell(t)
                t{1}.name = 'updated_debug_target';
                if isfield(t{1}, 'exposure'), t{1}.exposure = 600; end
                planStruct.targets = t;
            else
                t(1).name = 'updated_debug_target';
                if isfield(t(1), 'exposure'), t(1).exposure = 600; end
                planStruct.targets = t;
            end
        end

        % Step 3: Update (save again with pk)
        resp2 = client.savePlan(planStruct);
        fprintf('Step 2 savePlan (update): ok=%d, status=%s\n', resp2.ok, debug_getStatus(resp2));
        if resp2.ok && isfield(resp2, 'data') && ~isempty(resp2.data)
            updatedPk = resp2.data;
            fprintf('Updated pk=%d\n', updatedPk);
            if updatedPk ~= savedPk
                fprintf('WARNING: update returned different pk (%d vs %d)\n', updatedPk, savedPk);
            end
        else
            fprintf('Update failed\n');
        end
    catch ME
        fprintf('debug_saveThenUpdatePlan failed: %s\n', ME.message);
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
