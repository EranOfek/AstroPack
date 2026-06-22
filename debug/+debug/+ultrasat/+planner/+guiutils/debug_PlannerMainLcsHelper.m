%==========================================================================
% Project     : ULTRASAT Planner
% Filename    : +debug/+ultrasat/+planner/+guiutils/debug_PlannerMainLcsHelper.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Updated     : 17/06/2026
% Description : Debug PlannerMainLcsHelper UITable builders and row selection.
%
% Run by      : debug.ultrasat.planner.guiutils.debug_PlannerMainLcsHelper()
%==========================================================================

function debug_PlannerMainLcsHelper()
    % Exercise LCS helper table population and selection callbacks headlessly.

    fprintf('========== DEBUG PLANNER MAIN LCS HELPER ==========\n');

    Helper = ultrasat.planner.guiutils.PlannerMainLcsHelper();

    % --- Step 1: Build LCS planner fixture ---
    Planner = debug_buildLcsPlanner();
    [App, LcsApp] = createMockLcsApp(Planner);

    % --- Step 2: Populate group summary table ---
    debug_populateGroupSummary(Helper, App, LcsApp);

    % --- Step 3: Single-row group summary selection ---
    debug_groupSummarySingleSelect(Helper, App, LcsApp);

    % --- Step 4: Multi-row group summary union ---
    debug_groupSummaryMultiSelect(Helper, App, LcsApp);

    % --- Step 5: Field selection and observation dates ---
    debug_groupFieldsSelect(Helper, App, LcsApp);

    % --- Step 6: prepareForSave retains LCS_obj ---
    debug_prepareForSaveKeepsLcsObj(Planner);
    debug_loadRoundtripKeepsLcsData(Helper, Planner);

    % --- Cleanup headless UI figure ---
    if isfield(LcsApp, 'UIFigure') && isvalid(LcsApp.UIFigure)
        delete(LcsApp.UIFigure);
    end

    fprintf('========== DEBUG PLANNER MAIN LCS HELPER DONE ==========\n');
end

% -------------------------------------------------------------------------

function Planner = debug_buildLcsPlanner()
    % Build an LCS uplanner from LCS_fields.csv for table fixture data.

    fprintf('\n--- debug_buildLcsPlanner ---\n');

    % Fallback when ASTROPACK_DATA_PATH unset (typical in bare debug sessions).
    if isempty(getenv('ASTROPACK_DATA_PATH'))
        RepoRoot = getenv('ASTROPACK_PATH');
        if isempty(RepoRoot)
            error('ASTROPACK_PATH is not set');
        end
        DataRoot = fullfile(RepoRoot, 'data');
        if isfolder(DataRoot)
            setenv('ASTROPACK_DATA_PATH', DataRoot);
        end
    end

    Planner = ultrasat.planner.uplanner('AstPlanner', 'debug', 'Type', 'LCS');
    Planner.StartTime = datetime(2029, 2, 1);
    Planner.EndTime = Planner.StartTime + caldays(420);
    Planner.DailyWindowStartTime = duration(0, 0, 0);

    GridFile = fullfile(Planner.BaseDataDir, 'LCS_fields.csv');
    if ~isfile(GridFile)
        error('debug_PlannerMainLcsHelper:gridFileNotFound', 'Grid file not found: %s', GridFile);
    end

    Grid = readtable(GridFile);
    Planner.addUniqTargets(Grid.RA, Grid.Dec, 'Name', num2cell(Grid.Field));
    Planner.buildLCS1;

    if isempty(Planner.LCS_obj) || height(Planner.LCS_obj.Schedule) == 0
        error('debug_PlannerMainLcsHelper:buildFailed', 'buildLCS1 did not produce LCS_obj.Schedule');
    end

    fprintf('  Plan rows: %d, Schedule rows: %d\n', height(Planner.Plan), height(Planner.LCS_obj.Schedule));
    fprintf('debug_buildLcsPlanner: OK\n');
end

% -------------------------------------------------------------------------

function [App, LcsApp] = createMockLcsApp(Planner)
    % Create headless App/LcsApp structs with off-screen UITable widgets.

    MainModule = ultrasat.planner.guiutils.MainModule();
    MainModule.Planner = Planner;

    App = struct();
    App.MainModule = MainModule;
    App.msglog = @(varargin) []; % suppress GUI logging during headless run
    App.hasPlanner = @() ~isempty(App.MainModule.Planner);

    Fig = uifigure('Visible', 'off'); % UITable requires a parent figure even when hidden
    LcsApp = struct();
    LcsApp.UIFigure = Fig;
    LcsApp.UITableGroupSummary = uitable(Fig, 'Visible', 'off', 'Position', [1 1 100 50]);
    LcsApp.UITableGroupFields = uitable(Fig, 'Visible', 'off', 'Position', [1 1 100 50]);
    LcsApp.UITableFieldDates = uitable(Fig, 'Visible', 'off', 'Position', [1 1 100 50]);
end

% -------------------------------------------------------------------------

function debug_populateGroupSummary(Helper, App, LcsApp)
    % Verify populateGroupSummary fills UITableGroupSummary with expected columns.

    fprintf('\n--- debug_populateGroupSummary ---\n');

    Helper.populateGroupSummary(App, LcsApp);
    Summary = LcsApp.UITableGroupSummary.Data;

    assert(istable(Summary) && height(Summary) > 0, 'Group summary empty');
    assert(all(ismember(Summary.Properties.VariableNames, ...
        {'Group', 'NumFields', 'StartDate', 'EndDate'})), 'Unexpected columns');

    fprintf('  Groups: %s\n', strjoin(string(Summary.Group), ', '));
    fprintf('debug_populateGroupSummary: OK\n');
end

% -------------------------------------------------------------------------

function debug_groupSummarySingleSelect(Helper, App, LcsApp)
    % Verify single group selection populates UITableGroupFields.

    fprintf('\n--- debug_groupSummarySingleSelect ---\n');

    Summary = LcsApp.UITableGroupSummary.Data;
    TargetRow = min(3, height(Summary));
    TargetGroup = string(Summary.Group(TargetRow));

    Helper.onGroupSummarySelectionChanged(App, LcsApp, [TargetRow, 1]);
    Fields = LcsApp.UITableGroupFields.Data;

    assert(istable(Fields) && height(Fields) > 0, 'Fields table empty after single select');

    fprintf('  Group %s: %d fields\n', TargetGroup, height(Fields));
    fprintf('debug_groupSummarySingleSelect: OK\n');
end

% -------------------------------------------------------------------------

function debug_groupSummaryMultiSelect(Helper, App, LcsApp)
    % Verify multi-select returns stable union of fields across groups.

    fprintf('\n--- debug_groupSummaryMultiSelect ---\n');

    Summary = LcsApp.UITableGroupSummary.Data;
    if height(Summary) < 2
        fprintf('  Skipped (need >= 2 groups)\n');
        return;
    end

    RowA = 1;
    RowB = 2;
    Helper.onGroupSummarySelectionChanged(App, LcsApp, [RowA, 1; RowB, 1]);
    FieldsUnion = LcsApp.UITableGroupFields.Data;

    % Re-select each group alone to build expected union for comparison.
    Helper.onGroupSummarySelectionChanged(App, LcsApp, [RowA, 1]);
    FieldsA = LcsApp.UITableGroupFields.Data;
    Helper.onGroupSummarySelectionChanged(App, LcsApp, [RowB, 1]);
    FieldsB = LcsApp.UITableGroupFields.Data;

    UnionNames = unique([FieldsA.FieldName; FieldsB.FieldName], 'stable');
    assert(height(FieldsUnion) == numel(UnionNames), ...
        'Multi-select union mismatch (UITable row-index bug?)');

    fprintf('  Union fields for rows %d+%d: %d\n', RowA, RowB, height(FieldsUnion));
    fprintf('debug_groupSummaryMultiSelect: OK\n');
end

% -------------------------------------------------------------------------

function debug_groupFieldsSelect(Helper, App, LcsApp)
    % Verify field selection populates UITableFieldDates observation rows.

    fprintf('\n--- debug_groupFieldsSelect ---\n');

    if isempty(LcsApp.UITableGroupFields.Data) || height(LcsApp.UITableGroupFields.Data) == 0
        Helper.onGroupSummarySelectionChanged(App, LcsApp, [1, 1]);
    end

    Fields = LcsApp.UITableGroupFields.Data;
    assert(height(Fields) > 0, 'No fields to select');

    Helper.onGroupFieldsSelectionChanged(App, LcsApp, [1, 1]);
    Dates = LcsApp.UITableFieldDates.Data;

    assert(istable(Dates) && height(Dates) > 0, 'Field dates empty');
    assert(all(ismember(Dates.Properties.VariableNames, {'Index', 'Date', 'StartTime'})), ...
        'Unexpected date columns');

    fprintf('  Field %s: %d observations\n', string(Fields.FieldName(1)), height(Dates));
    fprintf('debug_groupFieldsSelect: OK\n');
end

% -------------------------------------------------------------------------

function debug_prepareForSaveKeepsLcsObj(Planner)
    % Verify cloned planner keeps LCS_obj.Schedule after prepareForSave.

    fprintf('\n--- debug_prepareForSaveKeepsLcsObj ---\n');

    Saved = Planner.clone();
    Saved.prepareForSave();

    assert(~isempty(Saved.LCS_obj), 'LCS_obj stripped by prepareForSave');
    assert(isempty(Saved.Vis), 'Vis should be cleared');
    assert(istable(Saved.LCS_obj.Schedule) && height(Saved.LCS_obj.Schedule) > 0, ...
        'Schedule lost after prepareForSave');

    fprintf('debug_prepareForSaveKeepsLcsObj: OK\n');
end

% -------------------------------------------------------------------------

function debug_loadRoundtripKeepsLcsData(Helper, Planner)
    % Verify byte-stream save/load roundtrip preserves LCS_obj and table population.

    fprintf('\n--- debug_loadRoundtripKeepsLcsData ---\n');

    Saved = Planner.clone();
    Saved.prepareForSave();
    Loaded = getArrayFromByteStream(getByteStreamFromArray(Saved)); % simulates PlansManager mat persistence

    assert(~isempty(Loaded.LCS_obj), 'LCS_obj lost after save/load roundtrip');
    assert(height(Loaded.LCS_obj.Schedule) > 0, 'Schedule empty after roundtrip');

    [App, LcsApp] = createMockLcsApp(Loaded);
    Helper.populateGroupSummary(App, LcsApp);
    Summary = LcsApp.UITableGroupSummary.Data;

    assert(istable(Summary) && height(Summary) > 0, ...
        'Group summary empty after load (LCS_obj not usable)');

    fprintf('  Loaded plan groups: %s\n', strjoin(string(Summary.Group), ', '));
    fprintf('debug_loadRoundtripKeepsLcsData: OK\n');

    if isfield(LcsApp, 'UIFigure') && isvalid(LcsApp.UIFigure)
        delete(LcsApp.UIFigure);
    end
end
