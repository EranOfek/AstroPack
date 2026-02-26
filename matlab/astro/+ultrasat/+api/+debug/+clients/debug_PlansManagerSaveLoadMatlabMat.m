%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_PlansManagerSaveLoadMatlabMat.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Debug save/load of matlab mat via PlansManagerClient.
%               Creates minimal HCS uplanner (no targets), saves plan+mat, loads mat, verifies round-trip.
%==========================================================================

function debug_PlansManagerSaveLoadMatlabMat()
    fprintf('========== DEBUG SAVE/LOAD MATLAB MAT ==========\n');
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    [PlanData, upHCS] = debug_createHcsNoTargets();
    savedPk = debug_savePlanAndMat(client, PlanData);
    if ~isempty(savedPk)
        debug_loadAndVerifyMat(client, savedPk, upHCS);
    end
    fprintf('========== DEBUG SAVE/LOAD MATLAB MAT DONE ==========\n');
end


function [PlanData, upHCS] = debug_createHcsNoTargets()
    % Create uplanner HCS with no targets (no addUniqTargets, no buildHCS).
    MainModule = ultrasat.planner.guiutils.MainModule();
    BaseDataDir = MainModule.BaseDataDir;
    PlanData = ultrasat.api.models.PlanData();
    StartTime = datetime(2028, 1, 1, 'TimeZone', 'UTC');
    EndTime = datetime(2028, 7, 31, 'TimeZone', 'UTC');
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'debug_user', 'Type', 'HCS', ...
        'StartTime', StartTime, 'EndTime', EndTime, 'BaseDataDir', BaseDataDir);
    PlanData.planner = upHCS;
    ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);
end


function savedPk = debug_savePlanAndMat(client, PlanData)
    planStruct = PlanData.toStruct();
    resp = client.savePlan(planStruct);
    if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
        fprintf('savePlan failed\n');
        savedPk = [];
        return;
    end
    savedPk = resp.data;
    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(PlanData.planner, 'planner');
    matResp = client.saveMatlabMat(savedPk, base64Str);
    fprintf('savePlan pk=%d, saveMatlabMat ok=%d\n', savedPk, matResp.ok);
end


function debug_loadAndVerifyMat(client, pk, originalPlanner)
    resp = client.getMatlabMat(pk);
    if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
        fprintf('getMatlabMat failed\n');
        return;
    end
    loaded = ultrasat.api.utils.MatBase64Utils.base64ToMat(resp.data, 'planner');
    if isempty(loaded)
        fprintf('base64ToMat failed\n');
        return;
    end
    % Verify round-trip
    ok = strcmp(loaded.Type, originalPlanner.Type) && ...
         strcmp(loaded.AstPlanner, originalPlanner.AstPlanner) && ...
         isequal(loaded.StartTime, originalPlanner.StartTime) && ...
         isequal(loaded.EndTime, originalPlanner.EndTime);
    if ok
        fprintf('Round-trip verify: PASS\n');
    else
        fprintf('Round-trip verify: FAIL\n');
    end
end
