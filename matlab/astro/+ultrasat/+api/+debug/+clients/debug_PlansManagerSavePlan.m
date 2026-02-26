%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_PlansManagerSavePlan.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Debug save PlanData via PlansManagerClient.savePlan.
%               Tests with HCS plan (0 targets) and with HCS plan (1 target). No matlab mat.
%==========================================================================

function debug_PlansManagerSavePlan()
    fprintf('========== DEBUG SAVE PLAN ==========\n');

    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    debug_saveHcsPlanNoTargets(client);
    debug_saveHcsPlanOneTarget(client);
    debug_saveHcsPlanTwoTargets(client);

    fprintf('========== DEBUG SAVE PLAN DONE ==========\n');
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


function debug_saveHcsPlanNoTargets(client)
    % Create HCS plan with zero targets and call client.savePlan.
    fprintf('\n--- debug_saveHcsPlanNoTargets ---\n');
    try
        BaseDataDir = getBaseDataDir();
        PlanData = ultrasat.api.models.PlanData();
        StartTime = datetime(2028, 1, 1, 'TimeZone', 'UTC');
        EndTime = datetime(2028, 7, 31, 'TimeZone', 'UTC');
        upHCS = ultrasat.planner.uplanner('AstPlanner', 'debug_user', 'Type', 'HCS', ...
            'StartTime', StartTime, 'EndTime', EndTime, ...
            'BaseDataDir', BaseDataDir);
        PlanData.planner = upHCS;
        ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);

        PlanData.setStatus('BuildStatus', 'MyTestBuildStatus');

        planStruct = PlanData.toStruct();
        planStruct = rmfield(planStruct, 'planner');
        planStruct = rmfield(planStruct, 'history');
        %planStruct = rmfield(planStruct, 'metadata');

        response = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if response.ok && isfield(response, 'data') && ~isempty(response.data)
            fprintf('Saved pk=%d, 0 targets\n', response.data);
        else
            fprintf('Save failed or no pk returned\n');
        end
    catch ME
        fprintf('debug_saveHcsPlanNoTargets failed: %s\n', ME.message);
    end
end


function debug_saveHcsPlanOneTarget(client)
    % Create HCS plan with one target and call client.savePlan.
    fprintf('\n--- debug_saveHcsPlanOneTarget ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();
        planStruct = PlanData.toStruct();
        planStruct = rmfield(planStruct, 'planner');
        planStruct = rmfield(planStruct, 'history');
        %planStruct = rmfield(planStruct, 'metadata');

        response = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if response.ok && isfield(response, 'data') && ~isempty(response.data)
            fprintf('Saved pk=%d, %d targets\n', response.data, numel(PlanData.targets));
        else
            fprintf('Save failed or no pk returned\n');
        end
    catch ME
        fprintf('debug_saveHcsPlanOneTarget failed: %s\n', ME.message);
    end
end


function debug_saveHcsPlanTwoTargets(client)
    % Create HCS plan with one target and call client.savePlan.
    fprintf('\n--- debug_saveHcsPlanOneTarget ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();

        % For testing just duplicate the first target
        PlanData.targets(2) = PlanData.targets(1);

        planStruct = PlanData.toStruct();
        planStruct = rmfield(planStruct, 'planner');
        planStruct = rmfield(planStruct, 'history');
        %planStruct = rmfield(planStruct, 'metadata');

        response = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if response.ok && isfield(response, 'data') && ~isempty(response.data)
            fprintf('Saved pk=%d, %d targets\n', response.data, numel(PlanData.targets));
        else
            fprintf('Save failed or no pk returned\n');
        end
    catch ME
        fprintf('debug_saveHcsPlanOneTarget failed: %s\n', ME.message);
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
