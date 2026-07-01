%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+clients/debug_PlansManagerClient.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 05/05/2026
% Description : Debug PlansManagerClient methods one by one (getPlansList,
%               getPlan, savePlan, getMatlabMat, saveMatlabMat).
%
% Run by      : debug.ultrasat.api.clients.debug_PlansManagerClient()
%==========================================================================

function debug_PlansManagerClient()
    % Exercise PlansManagerClient CRUD methods in sequence.

    fprintf('========== DEBUG PLANS MANAGER CLIENT ==========\n');

    % Create a factory and get the base URL for the plans manager service
    factory = ultrasat.api.clients.ClientFactory();
    baseUrl = factory.getServiceBaseUrl('plans_manager', 'nginx');
    client = ultrasat.api.clients.PlansManagerClient(baseUrl);
    client.Namespace = 'dev';

    % 1. getPlansList
    debug_getPlansList(client);

    % 2. savePlan (creates a plan so we have a pk for the rest)
    savedPk = debug_savePlan(client);

    % 3. getPlan
    pk = iif(~isempty(savedPk), savedPk, debug_getFirstPkFromList(client));
    debug_getPlan(client, pk);

    % 4. saveMatlabMat
    if ~isempty(pk)
        debug_saveMatlabMat(client, pk);
    end

    % 5. getMatlabMat
    if ~isempty(pk)
        debug_getMatlabMat(client, pk);
    end

    fprintf('========== DEBUG PLANS MANAGER CLIENT DONE ==========\n');
end


function debug_getPlansList(client)
    % Call getPlansList and print count plus first plan pk.

    fprintf('\n--- 1. getPlansList ---\n');
    resp = client.getPlansList([], [], [], []);
    fprintf('ok=%d, status=%s\n', resp.ok, sget(resp, 'status'));
    if resp.ok && isfield(resp, 'plans') && ~isempty(resp.plans)
        fprintf('plans count: %d\n', numel(resp.plans));
        p1 = resp.plans(1);
        if iscell(resp.plans), p1 = resp.plans{1}; end
        fprintf('first plan pk=%s\n', num2str(p1.pk));
    else
        fprintf('plans: empty or failed\n');
    end
end


function pk = debug_getFirstPkFromList(client)
    % Return pk of first plan from getPlansList, or [] when list is empty.

    pk = [];
    resp = client.getPlansList([], [], [], []);
    if resp.ok && isfield(resp, 'plans') && ~isempty(resp.plans)
        p1 = resp.plans(1);
        if iscell(resp.plans), p1 = resp.plans{1}; end
        pk = p1.pk;
    end
end


function debug_getPlan(client, pk)
    % Fetch single plan by pk and print title/target count.

    fprintf('\n--- 3. getPlan ---\n');
    if isempty(pk)
        fprintf('skip (no pk)\n');
        return;
    end
    resp = client.getPlan(pk);
    fprintf('ok=%d, status=%s\n', resp.ok, sget(resp, 'status'));
    if resp.ok && isfield(resp, 'data') && ~isempty(resp.data)
        d = resp.data;
        fprintf('pk=%s, title=%s, targets=%d\n', num2str(fget(d,'pk',[])), ...
            char(fget(d,'title','')), numelTargets(d));
    else
        fprintf('data: empty or failed\n');
    end
end


function savedPk = debug_savePlan(client)
    % Build minimal HCS plan and save via savePlan; return new pk.

    fprintf('\n--- 2. savePlan ---\n');
    try
        [PlanData, ~] = debug_createMinimalPlan();
        planStruct = PlanData.toStruct();


        resp = client.savePlan(planStruct);
        fprintf('ok=%d, status=%s\n', resp.ok, sget(resp, 'status'));
        savedPk = [];
        if resp.ok && isfield(resp, 'data') && ~isempty(resp.data)
            savedPk = resp.data;
            fprintf('saved pk=%d\n', savedPk);
        else
            fprintf('save failed\n');
        end
    catch ME
        fprintf('savePlan failed: %s\n', ME.message);
        savedPk = [];
    end
end


function debug_getMatlabMat(client, pk)
    % Fetch base64 mat blob for pk and print data length.

    fprintf('\n--- 5. getMatlabMat ---\n');
    if isempty(pk)
        fprintf('skip (no pk)\n');
        return;
    end
    resp = client.getMatlabMat(pk);
    fprintf('ok=%d, status=%s\n', resp.ok, sget(resp, 'status'));
    if resp.ok && isfield(resp, 'data') && ~isempty(resp.data)
        fprintf('data length: %d (base64)\n', numel(resp.data));
    else
        fprintf('data: empty\n');
    end
end


function debug_saveMatlabMat(client, pk)
    % Encode minimal planner to base64 and upload via saveMatlabMat.

    fprintf('\n--- 4. saveMatlabMat ---\n');
    if isempty(pk)
        fprintf('skip (no pk)\n');
        return;
    end
    try
        [PlanData, ~] = debug_createMinimalPlan();
        base64Str = ultrasat.api.utils.MatBase64Utils.matToBase64(PlanData.planner, 'planner');

        resp = client.saveMatlabMat(pk, base64Str);
        fprintf('ok=%d, status=%s\n', resp.ok, sget(resp, 'status'));
    catch ME
        fprintf('saveMatlabMat failed: %s\n', ME.message);
    end
end


function [PlanData, upHCS] = debug_createMinimalPlan()
    % HCS with one target (RA=215, Dec=60).
    BaseDataDir = getBaseDataDir();
    PlanData = ultrasat.api.models.PlanData();
    StartTime = datetime(2028, 1, 1, 'TimeZone', 'UTC');
    EndTime = datetime(2028, 7, 31, 'TimeZone', 'UTC');
    upHCS = ultrasat.planner.uplanner('AstPlanner', 'debug_user', 'Type', 'HCS', ...
        'StartTime', StartTime, 'EndTime', EndTime, 'BaseDataDir', BaseDataDir);
    upHCS.addUniqTargets(215, 60, 'Name', 'debug_target');
    upHCS.buildHCS('HCS_UniqTarg', 1);
    PlanData.planner = upHCS;
    ultrasat.api.utils.PlanDataUtils.syncFromPlanner(PlanData, upHCS);
end


function v = sget(s, fld, default)
    % Safe struct field read with default when field is missing.

    if nargin < 3, default = ''; end
    if isfield(s, fld), v = s.(fld); else, v = default; end
end


function v = fget(s, fld, default)
    % Safe struct field read with default when field is missing.

    if nargin < 3, default = ''; end
    if isfield(s, fld), v = s.(fld); else, v = default; end
end


function n = numelTargets(s)
    % Count targets in plan struct (cell or struct array).

    if ~isfield(s, 'targets') || isempty(s.targets)
        n = 0;
    elseif iscell(s.targets)
        n = numel(s.targets);
    else
        n = numel(s.targets);
    end
end


function v = iif(cond, a, b)
    % Inline conditional: return a when cond is true, else b.

    if cond, v = a; else, v = b; end
end


function BaseDataDir = getBaseDataDir()
    % Resolve ULTRASAT data dir via MainModule or ASTROPACK_DATA_PATH fallback.

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
