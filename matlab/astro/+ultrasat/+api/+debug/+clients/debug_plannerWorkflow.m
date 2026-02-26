%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.api.debug.clients.debug_plannerWorkflow.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Full round-trip debug: create plan (no targets) -> save ->
%               add 1 target -> build -> save -> add 2nd target -> save ->
%               get list -> load -> compare to original. Saves mat at each step.
%==========================================================================

function debug_plannerWorkflow(client)
    % Optional client arg - if not provided, create one.
    if nargin < 1 || isempty(client)
        factory = ultrasat.api.clients.ClientFactory();
        baseUrl = factory.getServiceBaseUrl('plans_manager');
        client = ultrasat.api.clients.PlansManagerClient(baseUrl);
        client.Namespace = 'dev';
    end

    fprintf('========== DEBUG PLANNER WORKFLOW (FULL ROUND-TRIP) ==========\n');

    try
        [savedPk, originalPlanStruct, originalPlanner] = runWorkflow(client);
        if isempty(savedPk)
            fprintf('Workflow failed at save step\n');
            return;
        end

        % Get list, load, compare
        fprintf('\n--- getPlansList ---\n');
        listResp = client.getPlansList([], [], [], []);
        fprintf('getPlansList ok=%d, plans count=%d\n', listResp.ok, ...
            iif(isfield(listResp, 'plans') && ~isempty(listResp.plans), numel(listResp.plans), 0));

        fprintf('\n--- load and compare ---\n');
        planResp = client.getPlan(savedPk);
        if ~planResp.ok || ~isfield(planResp, 'data') || isempty(planResp.data)
            fprintf('getPlan failed\n');
            return;
        end
        loadedPlan = planResp.data;

        matResp = client.getMatlabMat(savedPk);
        loadedPlanner = [];
        if matResp.ok && isfield(matResp, 'data') && ~isempty(matResp.data)
            loadedPlanner = ultrasat.api.utils.MatBase64Utils.base64ToMat(matResp.data, 'planner');
        end

        comparePlans(originalPlanStruct, loadedPlan, originalPlanner, loadedPlanner);

    catch ME
        fprintf('debug_plannerWorkflow failed: %s\n', ME.message);
    end

    fprintf('========== DEBUG PLANNER WORKFLOW DONE ==========\n');
end


function [savedPk, planStruct, upHCS] = runWorkflow(client)
    savedPk = [];
    planStruct = [];
    upHCS = [];

    BaseDataDir = getBaseDataDir();
    StartTime = datetime(2028, 1, 1, 'TimeZone', 'UTC');
    EndTime = datetime(2028, 7, 31, 'TimeZone', 'UTC');

    % Step 1: Create HCS with NO targets, save plan + mat
    fprintf('\n--- Step 1: Create plan (0 targets), save ---\n');
    PlanData = ultrasat.api.models.PlanData();
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'debug_user', 'Type', 'HCS', ...
        'StartTime', StartTime, 'EndTime', EndTime, 'BaseDataDir', BaseDataDir);
    PlanData.planner = upHCS;
    ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);

    planStruct = PlanData.toStruct();
    planStruct = rmfield(planStruct, 'planner');
    planStruct = rmfield(planStruct, 'history');
    planStruct = rmfield(planStruct, 'metadata');

    resp = client.savePlan(planStruct);
    if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
        fprintf('Step 1 savePlan failed\n');
        return;
    end
    savedPk = resp.data;

    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(upHCS, 'planner');
    matResp = client.saveMatlabMat(savedPk, base64Str);
    fprintf('Saved pk=%d, 0 targets, saveMatlabMat ok=%d\n', savedPk, matResp.ok);

    % Step 2: Add one target, build HCS, save
    fprintf('\n--- Step 2: Add 1 target, build, save ---\n');
    upHCS.addUniqTargets(215, 60, 'Name', 'target1');
    upHCS.buildHCS('HCS_UniqTarg', 1);
    PlanData.planner = upHCS;
    ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);

    planStruct = PlanData.toStruct();
    planStruct = rmfield(planStruct, 'planner');
    planStruct = rmfield(planStruct, 'history');
    planStruct = rmfield(planStruct, 'metadata');
    planStruct.pk = savedPk;

    resp = client.savePlan(planStruct);
    if ~resp.ok
        fprintf('Step 2 savePlan failed\n');
        return;
    end

    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(upHCS, 'planner');
    matResp = client.saveMatlabMat(savedPk, base64Str);
    fprintf('Updated pk=%d, %d targets, saveMatlabMat ok=%d\n', savedPk, numel(planStruct.targets), matResp.ok);

    % Step 3: Add another target (to targets array), save
    fprintf('\n--- Step 3: Add 2nd target, save ---\n');
    t = planStruct.targets;
    if iscell(t)
        t2 = t{1};
        t2.name = 'target2';
        t2.ra = 200;
        t2.decl = 50;
        t{2} = t2;
    else
        t2 = t(1);
        t2.name = 'target2';
        t2.ra = 200;
        t2.decl = 50;
        t(2) = t2;
    end
    planStruct.targets = t;

    resp = client.savePlan(planStruct);
    if ~resp.ok
        fprintf('Step 3 savePlan failed\n');
        return;
    end

    base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(upHCS, 'planner');
    matResp = client.saveMatlabMat(savedPk, base64Str);
    fprintf('Updated pk=%d, %d targets, saveMatlabMat ok=%d\n', savedPk, numel(t), matResp.ok);
end


function comparePlans(original, loaded, originalPlanner, loadedPlanner)
    % Compare plan fields
    planOk = true;
    if ~isequal(debug_getField(original, 'plan_type', ''), debug_getField(loaded, 'plan_type', ''))
        fprintf('MISMATCH plan_type: %s vs %s\n', debug_getField(original, 'plan_type', ''), debug_getField(loaded, 'plan_type', ''));
        planOk = false;
    end
    nOrig = numelTargets(original);
    nLoad = numelTargets(loaded);
    if nOrig ~= nLoad
        fprintf('MISMATCH target count: %d vs %d\n', nOrig, nLoad);
        planOk = false;
    else
        fprintf('Plan compare: plan_type match, %d targets match\n', nOrig);
    end
    if planOk
        fprintf('Plan compare: PASS\n');
    else
        fprintf('Plan compare: FAIL\n');
    end

    % Compare planner (mat) if loaded
    if ~isempty(loadedPlanner)
        matOk = strcmp(loadedPlanner.Type, originalPlanner.Type) && ...
            strcmp(loadedPlanner.AstPlanner, originalPlanner.AstPlanner) && ...
            isequal(loadedPlanner.StartTime, originalPlanner.StartTime) && ...
            isequal(loadedPlanner.EndTime, originalPlanner.EndTime);
        if matOk
            fprintf('Mat (planner) compare: PASS\n');
        else
            fprintf('Mat (planner) compare: FAIL\n');
        end
    else
        fprintf('Mat (planner) compare: SKIP (no mat loaded)\n');
    end
end


function n = numelTargets(s)
    if ~isfield(s, 'targets') || isempty(s.targets)
        n = 0;
    elseif iscell(s.targets)
        n = numel(s.targets);
    else
        n = numel(s.targets);
    end
end


function v = debug_getField(s, fld, default)
    if isfield(s, fld)
        v = s.(fld);
    else
        v = default;
    end
end


function v = iif(cond, a, b)
    if cond
        v = a;
    else
        v = b;
    end
end


function BaseDataDir = getBaseDataDir()
    try
        MainModule = ultrasat.planner.guiutils.MainModule();
        BaseDataDir = MainModule.BaseDataDir;
    catch
        if ispc
            BaseDataDir = fullfile(getenv('ASTROPACK_DATA_PATH'), 'ULTRASAT');
        else
            BaseDataDir = '~/matlab/data/ULTRASAT/';
        end
    end
end
