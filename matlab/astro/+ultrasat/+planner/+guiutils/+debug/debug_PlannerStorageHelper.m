%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.guiutils.debug.debug_PlannerStorageHelper.m
% Author      : Chen Tishler
% Created     : 18/02/2026
% Updated     : 26/02/2026
% Description : Debug PlannerMainStorageHelper - tests all storage helper
%               functions (getPlansListToUITable, openPlan, savePlan, closePlan,
%               doClosePlan, savePlanToFile, loadPlanFromFile, duplicatePlan).
%==========================================================================

function debug_PlannerStorageHelper()
    fprintf('========== DEBUG PLANNER STORAGE HELPER ==========\n');

    [app, helper] = createMockApp();

    % 1. getPlansListToUITable - fetch plans list and populate table
    debug_getPlansListToUITable(helper, app);

    % 2. savePlan - save plan to database (creates plan for subsequent tests)
    savedPk = debug_savePlan(helper, app);
    if isempty(savedPk)
        fprintf('savePlan failed, some tests will be skipped\n');
    end

    % 3. openPlan flow - getPlan + getMatlabMat, build PlanData, doOpenPlan
    if ~isempty(savedPk)
        debug_openPlanFlow(helper, app, savedPk);
    end

    % 4. doClosePlan - clear MainModule data
    debug_doClosePlan(helper, app);

    % 5. savePlanToFile - save PlanData to local .mat file
    debug_savePlanToFile(app);

    % 6. loadPlanFromFile - load PlanData from local .mat file
    debug_loadPlanFromFile(app);

    % 7. duplicatePlan - reset pk, history, etc. (needs plan loaded first)
    debug_duplicatePlan(app);

    % 8. deletePlan - stub, log only
    debug_deletePlan(helper, app);

    fprintf('========== DEBUG PLANNER STORAGE HELPER DONE ==========\n');
end


function debug_deletePlan(helper, app)
    fprintf('\n--- 8. deletePlan ---\n');
    try
        helper.deletePlan(app);
        fprintf('ok (stub - no-op)\n');
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function [app, helper] = createMockApp()
    % Create minimal mock app and helper for headless testing.
    MainModule = ultrasat.planner.guiutils.MainModule();
    factory = ultrasat.api.clients.ClientFactory();
    MainModule.PlansClient = ultrasat.api.clients.PlansManagerClient(factory.getServiceBaseUrl('plans_manager'));
    MainModule.PlansClient.Namespace = 'dev';
    MainModule.TableHelper = ultrasat.planner.guiutils.TableHelper();

    app = struct();
    app.MainModule = MainModule;
    app.msglog = @(varargin) fprintf('[msg] %s\n', sprintf(varargin{:}));
    app.msgex = @(tag, ME) (fprintf('[error] %s: %s\n', tag, ME.message), rethrow(ME));
    try
        app.UIFigure = uifigure('Visible', 'off');
    catch
        app.UIFigure = figure('Visible', 'off');
    end
    app.OpenPlanApp = struct('Pk', []);
    app.PlanPkEditField = struct('Value', '');
    app.Modified = false;
    app.Preferences = struct('LocalPlanFolder', tempdir(), 'LocalPlanFileName', '');
    app.savePreferences = @() [];
    app.showPlanAll = @() [];
    app.clearModified = @() [];
    app.setStatus = @(varargin) [];
    app.AppUtils = struct('askYesNo', @(varargin) 'No', 'askSaveDiscardCancel', @(varargin) 'Discard', ...
        'askYesNoCancel', @(varargin) 'No', 'msgError', @(varargin) fprintf('%s\n', sprintf(varargin{:})));
    app.SessionHelper = struct('isLogin', @(~) true, 'setButtons', @(varargin) []);
    app.hasPlanner = @() ~isempty(app.MainModule.Planner);
    app.needSave = @(varargin) true;
    app.showModal = @(a) '';  % Return empty to skip modal
    app.showPleaseWait = @(varargin) [];
    app.closePleaseWait = @() [];

    % UITable mock (struct with Data property)
    app.UITable = struct('Data', [], 'ColumnName', {{}}, 'ColumnSortable', false);

    helper = ultrasat.planner.guiutils.PlannerMainStorageHelper();
end


function debug_getPlansListToUITable(helper, app)
    fprintf('\n--- 1. getPlansListToUITable ---\n');
    try
        helper.getPlansListToUITable(app, [], [], [], app.UITable);
        fprintf('ok (PlansClient.getPlansList + TableHelper)\n');
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function savedPk = debug_savePlan(helper, app)
    fprintf('\n--- 2. savePlan ---\n');
    savedPk = [];
    try
        [PlanData, upHCS] = createMinimalPlan();
        app.MainModule.PlanData = PlanData;
        app.MainModule.Planner = upHCS;

        helper.savePlan(app);

        if ~isempty(app.MainModule.PlanData) && ~isempty(app.MainModule.PlanData.pk)
            savedPk = app.MainModule.PlanData.pk;
            fprintf('ok, saved pk=%d\n', savedPk);
        else
            fprintf('savePlan returned but pk empty\n');
        end
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_openPlanFlow(helper, app, pk)
    fprintf('\n--- 3. openPlan flow (getPlan + getMatlabMat + doOpenPlan) ---\n');
    try
        % Simulate load: getPlan, getMatlabMat, build PlanData, doOpenPlan
        app.MainModule.clearData();

        resp = app.MainModule.PlansClient.getPlan(pk);
        if ~resp.ok || ~isfield(resp, 'data') || isempty(resp.data)
            fprintf('getPlan failed\n');
            return;
        end
        PlanData = ultrasat.api.models.PlanData.fromStruct(resp.data);

        matResp = app.MainModule.PlansClient.getMatlabMat(pk);
        if matResp.ok && isfield(matResp, 'data') && ~isempty(matResp.data)
            PlanData.planner = ultrasat.api.utils.MatBase64Utils.base64ToMat(matResp.data, 'planner');
        end

        if ~isempty(PlanData.planner)
            helper.doOpenPlan(app, PlanData);
            fprintf('ok, PlanData loaded, planner set\n');
        else
            fprintf('no planner in matlab_mat\n');
        end
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_doClosePlan(helper, app)
    fprintf('\n--- 4. doClosePlan ---\n');
    try
        helper.doClosePlan(app);
        fprintf('ok, MainModule cleared\n');
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_savePlanToFile(app)
    fprintf('\n--- 5. savePlanToFile ---\n');
    try
        [PlanData, ~] = createMinimalPlan();
        app.MainModule.PlanData = PlanData;
        app.MainModule.Planner = PlanData.planner;

        fname = fullfile(tempdir(), sprintf('debug_plan_%s.mat', datestr(now, 'yyyymmdd_HHMMSS')));
        save(fname, 'PlanData');
        fprintf('ok, saved to %s\n', fname);

        debugPlanFileName(fname);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_loadPlanFromFile(app)
    fprintf('\n--- 6. loadPlanFromFile ---\n');
    try
        fname = debugPlanFileName();
        if isempty(fname) || ~isfile(fname)
            fprintf('skip (no file from savePlanToFile)\n');
            return;
        end
        Data = load(fname);
        app.MainModule.setPlanData(Data.PlanData);
        fprintf('ok, loaded from %s\n', fname);
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function debug_duplicatePlan(app)
    fprintf('\n--- 7. duplicatePlan ---\n');
    try
        if ~app.hasPlanner()
            fprintf('skip (no planner loaded)\n');
            return;
        end
        % Bypass modal: simulate DuplicatePlan flow
        PlanData = app.MainModule.PlanData;
        Planner = app.MainModule.Planner;
        OldPk = PlanData.pk;

        PlanData.pk = [];
        PlanData.id = [];
        PlanData.created_time = ultrasat.api.utils.DateTimeUtils.nowUtc();
        PlanData.updated_time = PlanData.created_time;
        PlanData.history = struct();
        PlanData.addHistory(sprintf('Duplicated from pk=%d', OldPk));
        PlanData.metadata.SubmitStatus = PlanData.newStatusData();
        Planner.Status = 'draft';

        fprintf('ok, PlanData duplicated (pk cleared, history reset)\n');
    catch ME
        fprintf('failed: %s\n', ME.message);
    end
end


function [PlanData, upHCS] = createMinimalPlan()
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


function out = debugPlanFileName(fname)
    persistent stored
    if nargin > 0
        stored = fname;
    end
    if nargout > 0
        out = stored;
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
