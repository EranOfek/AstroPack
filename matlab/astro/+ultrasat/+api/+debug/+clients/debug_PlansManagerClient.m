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

    debug_saveMatlabMat(client, 2);
    return;

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
            base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(PlanData.planner, 'planner');
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
    testObj = uint8(randi(255, 1024, 1024));
    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(testObj, 'matlab_mat');
    response = client.saveMatlabMat(pk, base64Str);
    fprintf('ok=%d, status=%s\n', response.ok, debug_getStatus(response));
    if response.ok
        verifyResp = client.getMatlabMat(pk);
        if verifyResp.ok && isfield(verifyResp, 'data') && ~isempty(verifyResp.data)
            loaded = ultrasat.api.utils.MatBase64Utils.base64ToMat(verifyResp.data, 'matlab_mat');
            fprintf('Round-trip verify: getMatlabMat returned %d chars, loaded %dx%d matrix\n', ...
                numel(verifyResp.data), size(loaded, 1), size(loaded, 2));

            % Compare testObj and loaded for verification
            if isequal(testObj, loaded)
                fprintf('Verification passed: testObj and loaded are identical.\n');
            else
                diffNorm = norm(double(testObj(:)) - double(loaded(:)));
                fprintf('Verification failed: testObj and loaded differ. Norm of difference: %.6g\n', diffNorm);
                % Optionally: print where they differ for debugging
                [row, col] = find(testObj ~= loaded, 1);
                if ~isempty(row)
                    fprintf('First mismatch at row %d, col %d: testObj=%d, loaded=%d\n', row, col, testObj(row, col), loaded(row, col));
                end
            end
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
