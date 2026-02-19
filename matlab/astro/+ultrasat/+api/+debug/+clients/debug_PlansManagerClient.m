%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_PlansManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 18/02/2026
% Description : Debug function for PlansManagerClient.
%               Uses ClientFactory for baseUrl. Namespace 'dev'.
%               Tests all client functions via debug_* helpers.
%==========================================================================

function debug_PlansManagerClient()
    fprintf('========== DEBUG PLANS MANAGER CLIENT ==========\n');

    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    pk = debug_getPlansList(client);
    debug_getPlan(client, pk);
    savedPk = debug_savePlan(client);
    testPk = [];
    if ~isempty(savedPk)
        testPk = savedPk;
    elseif ~isempty(pk)
        testPk = pk;
    end
    debug_getMatlabMat(client, testPk);
    debug_saveMatlabMat(client, testPk);
    debug_plannerWorkflow(client);

    fprintf('========== DEBUG PLANS MANAGER CLIENT DONE ==========\n');
end


function [PlanData, upHCS] = debug_createPlannerPlanData()
    % Create real uplanner HCS + PlanData (RA=215, Dec=60, 1 Jan 2028 - 31 Jul 2028). No external files.
    MainModule = ultrasat.planner.guiutils.MainModule();
    BaseDataDir = MainModule.BaseDataDir;
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


function debug_plannerWorkflow(client)
    % Simulate real planner HCS workflow: uplanner, single target (RA=215, Dec=60),
    % build HCS, PlanData, save via PlansClient. No external files.
    fprintf('\n--- debug_plannerWorkflow ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();
        planStruct = PlanData.toStruct();
        planStruct = rmfield(planStruct, 'planner');
        response = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        if response.ok && isfield(response, 'data') && ~isempty(response.data)
            fprintf('Planner workflow: saved pk=%d, %d targets\n', response.data, numel(PlanData.targets));
        end
    catch ME
        fprintf('debug_plannerWorkflow failed: %s\n', ME.message);
    end
end


function pk = debug_getPlansList(client)
    fprintf('\n--- debug_getPlansList ---\n');
    response = client.getPlansList([], [], [], []);
    fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
    pk = [];
    if response.ok && isfield(response, 'plans') && ~isempty(response.plans)
        fprintf('Plans count: %d\n', numel(response.plans));
        p1 = response.plans(1);
        if iscell(response.plans), p1 = response.plans{1}; end
        pk = p1.pk;
    else
        fprintf('Plans count: 0 or failed\n');
    end
end


function debug_getPlan(client, pk)
    fprintf('\n--- debug_getPlan ---\n');
    if isempty(pk)
        fprintf('Skipping (no pk available)\n');
        return;
    end
    response = client.getPlan(pk);
    fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
    if response.ok && isfield(response, 'data') && ~isempty(response.data)
        d = response.data;
        fprintf('data: pk=%s title=%s\n', num2str(debug_getField(d, 'pk', [])), debug_getField(d, 'title', ''));
    end
end


function savedPk = debug_savePlan(client)
    fprintf('\n--- debug_savePlan ---\n');
    try
        [PlanData, ~] = debug_createPlannerPlanData();
        planStruct = PlanData.toStruct();
        planStruct = rmfield(planStruct, 'planner');
        response = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
        savedPk = [];
        if response.ok && isfield(response, 'data') && ~isempty(response.data)
            savedPk = response.data;
            fprintf('Saved pk=%d\n', savedPk);
            % Save uplanner .mat (like planner/guiutils MissionApiSim.savePlan)
            planner = PlanData.planner;
            tmpFile = [tempname, '.mat'];
            save(tmpFile, 'planner', '-v7');
            fid = fopen(tmpFile, 'rb');
            bytes = fread(fid, inf, 'uint8');
            fclose(fid);
            delete(tmpFile);
            base64Str = matlab.net.base64encode(bytes');
            matResp = client.saveMatlabMat(savedPk, base64Str);
            if matResp.ok
                fprintf('Saved uplanner mat for pk=%d\n', savedPk);
            else
                fprintf('saveMatlabMat failed: %s\n', debug_getStatus(matResp));
            end
        else
            fprintf('Save failed or no pk returned\n');
        end
    catch ME
        fprintf('debug_savePlan failed: %s\n', ME.message);
        savedPk = [];
    end
end


function debug_getMatlabMat(client, pk)
    fprintf('\n--- debug_getMatlabMat ---\n');
    if isempty(pk)
        fprintf('Skipping (no pk available)\n');
        return;
    end
    response = client.getMatlabMat(pk);
    fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
    if response.ok && isfield(response, 'data') && ~isempty(response.data)
        fprintf('data length: %d (base64)\n', numel(response.data));
    else
        fprintf('data: empty\n');
    end
end


function debug_saveMatlabMat(client, pk)
    fprintf('\n--- debug_saveMatlabMat ---\n');
    if isempty(pk)
        fprintf('Skipping (no pk available)\n');
        return;
    end
    matlab_mat = rand(2, 2);
    tmpFile = [tempname, '.mat'];
    save(tmpFile, 'matlab_mat', '-v7');
    fid = fopen(tmpFile, 'r');
    bytes = fread(fid, inf, 'uint8');
    fclose(fid);
    delete(tmpFile);
    base64Str = matlab.net.base64encode(bytes');
    response = client.saveMatlabMat(pk, base64Str);
    fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
    if response.ok
        verifyResp = client.getMatlabMat(pk);
        if verifyResp.ok && isfield(verifyResp, 'data') && ~isempty(verifyResp.data)
            fprintf('Round-trip verify: getMatlabMat returned %d chars\n', numel(verifyResp.data));
        end
    end
end


function s = debug_getStatus(response)
    if isfield(response, 'status')
        s = response.status;
    else
        s = '';
    end
end


function v = debug_getField(s, fld, default)
    if isfield(s, fld)
        v = s.(fld);
    else
        v = default;
    end
end
