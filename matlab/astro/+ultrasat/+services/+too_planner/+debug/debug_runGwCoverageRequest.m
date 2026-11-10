%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_runGwCoverageRequest.m
% Author      : Chen Tishler
% Created     : 09/11/2026
% Description : Run TooPlannerRunner on GW coverage experiment request.json
%               (Windows: \\socsrv\rwshare_data\... ; Linux: /data/rwshare_data/...).
%               Default: single S250119cv request. Optional ParentFolder walks
%               the tree for too_planner subfolders and runs each request.json.
%               CleanOutput removes files in each too_planner dir except request.json.
%               ClosePlotWindows (default on, batch only): close all figures before each run.
%               ComparePlans (default on): too_planner_plan_compare.csv at ParentFolder (open in Excel).
%               ShowFigures (default false): set true to show coverage/sky figure windows.
%
% Run by      : ultrasat.services.too_planner.debug.debug_runGwCoverageRequest()
%               ultrasat.services.too_planner.debug.debug_runGwCoverageRequest( ...
%                   ParentFolder="\\socsrv\rwshare_data\gw_coverage_experiment_2026_08\2025\01\18\S250118az")
%               ultrasat.services.too_planner.debug.debug_runGwCoverageRequest( ...
%                   ParentFolder="...", CleanOutput=true)
%               ultrasat.services.too_planner.debug.debug_runGwCoverageRequest( ...
%                   ParentFolder="...", ClosePlotWindows=false)
%               ultrasat.services.too_planner.debug.debug_runGwCoverageRequest( ...
%                   ParentFolder="...", ComparePlans=false)
%               ultrasat.services.too_planner.debug.debug_runGwCoverageRequest( ...
%                   ParentFolder="...", ShowFigures=true)
%==========================================================================

function debug_runGwCoverageRequest(Args)
    % Run TooPlannerRunner on GW coverage experiment request(s) on socsrv share.
    %
    % No ParentFolder: fixed S250119cv/01_preliminary/too_planner/request.json.
    % With ParentFolder: every too_planner subfolder under the parent (sorted).

    arguments
        Args.ParentFolder (1,1) string = ""
        Args.CleanOutput (1,1) logical = false
        Args.ClosePlotWindows (1,1) logical = true
        Args.ComparePlans (1,1) logical = true
        Args.ShowFigures (1,1) logical = false
    end

    % Debug - DO NOT REMOVE THIS
    Args.ParentFolder = "\\socsrv\rwshare_data\gw_coverage_experiment_2026_08";  % \2025\01\18\S250118az";
    Args.CleanOutput = true;


    fprintf('\n========== DEBUG GW COVERAGE TOO REQUEST ==========\n');

    ensureDebugEnvironment();

    if strlength(strtrim(Args.ParentFolder)) == 0
        runSingleDefaultRequest(Args.CleanOutput, Args.ComparePlans, Args.ShowFigures);
    else
        runBatchUnderParent(Args.ParentFolder, Args.CleanOutput, Args.ClosePlotWindows, ...
            Args.ComparePlans, Args.ShowFigures);
    end

    fprintf('========== DEBUG GW COVERAGE TOO REQUEST DONE ==========\n\n');
end

% -------------------------------------------------------------------------

function runSingleDefaultRequest(cleanOutput, comparePlans, showFigures)
    % Single default GW coverage request (legacy debug path).

    relRequest = fullfile( ...
        'gw_coverage_experiment_2026_08', '2025', '01', '19', ...
        'S250119cv', '01_preliminary', 'too_planner', 'request.json');
    RequestJson = fullfile(getRwshareRoot(), relRequest);

    if ~isfile(RequestJson)
        error('debug_runGwCoverageRequest:RequestNotFound', ...
            'request.json not found: %s', RequestJson);
    end

    TooPlannerDir = fileparts(RequestJson);
    ReportDir = fileparts(fileparts(TooPlannerDir));

    PlanLabels = discoverPlanLabelsFromRequest(RequestJson);
    if comparePlans
        initCompareCsvReport(ReportDir, PlanLabels);
    end

    Result = processOneRequest(RequestJson, cleanOutput, showFigures);
    printOneRunResult(RequestJson, Result);
    runPlanCompareIfEnabled(comparePlans, ReportDir, Result, TooPlannerDir, PlanLabels);
    if comparePlans
        fprintf('Plan compare report: %s\n', compareCsvReportPath(ReportDir));
    end
end

% -------------------------------------------------------------------------

function runBatchUnderParent(parentFolder, cleanOutput, closePlotWindows, comparePlans, showFigures)
    % Find all too_planner dirs under parentFolder and run each request.json.

    Parent = char(strtrim(parentFolder));
    ReportDir = Parent;
    if ~isfolder(Parent)
        error('debug_runGwCoverageRequest:ParentNotFound', ...
            'Parent folder not found: %s', Parent);
    end

    TooPlannerDirs = findTooPlannerDirs(Parent);
    TooPlannerDirs = sort(TooPlannerDirs);

    fprintf('Parent: %s\n', Parent);
    fprintf('Found %d too_planner folder(s).\n', numel(TooPlannerDirs));

    PlanLabels = discoverPlanLabelsFromRequests(TooPlannerDirs);
    if comparePlans && ~isempty(PlanLabels)
        initCompareCsvReport(ReportDir, PlanLabels);
    elseif comparePlans
        fprintf('Warning: no plan labels from requests; CSV header deferred to first compare.\n');
    end

    Rows = repmat(emptyBatchRow(), 0, 1);

    for K = 1:numel(TooPlannerDirs)
        TooDir = TooPlannerDirs{K};
        RequestJson = fullfile(TooDir, 'request.json');

        Row = emptyBatchRow();
        Row.Index = K;
        Row.TooPlannerDir = TooDir;
        Row.RequestJson = RequestJson;

        if ~isfile(RequestJson)
            fprintf('Warning: skipping (no request.json): %s\n', TooDir);
            Row.Status = "skipped_no_request";
            Rows(end+1, 1) = Row; %#ok<AGROW>
            continue;
        end

        fprintf('\n--- [%d/%d] %s ---\n', K, numel(TooPlannerDirs), TooDir);

        try
            if closePlotWindows
                closeAllPlotWindows();
            end
            Result = processOneRequest(RequestJson, cleanOutput, showFigures);
            Row.FilesRemoved = Result.FilesRemoved;
            Row.SummaryFile = Result.SummaryFile;
            if Result.Ok
                Row.Status = "ok";
                Row.Message = "";
                runPlanCompareIfEnabled(comparePlans, ReportDir, Result, TooDir, PlanLabels);
            else
                Row.Status = "failed";
                Row.Message = Result.Message;
            end
        catch Ex
            Row.Status = "failed";
            Row.Message = Ex.message;
            fprintf('Failed: %s\n', Ex.message);
        end

        Rows(end+1, 1) = Row; %#ok<AGROW>
    end

    printBatchReport(Rows);

    if comparePlans
        fprintf('Plan compare report: %s\n', compareCsvReportPath(ReportDir));
    end
end

% -------------------------------------------------------------------------

function closeAllPlotWindows()
    % Close standard MATLAB figure windows (batch hygiene before next request).

    close all force;
    fprintf('Closed all figure windows before next request.\n');
end

% -------------------------------------------------------------------------

function Result = processOneRequest(requestJson, cleanOutput, showFigures)
    % Read request, remap share paths, run TooPlannerRunner.

    if nargin < 3
        showFigures = false;
    end

    requestJson = char(requestJson);
    TooPlannerDir = fileparts(requestJson);

    fprintf('Source request: %s\n', requestJson);

    FilesRemoved = 0;
    if cleanOutput
        FilesRemoved = cleanTooPlannerFolder(TooPlannerDir);
        fprintf('CleanOutput: removed %d file(s) in %s\n', FilesRemoved, TooPlannerDir);
    end

    try
        Txt = fileread(requestJson);
        Cfg = jsondecode(Txt);
    catch Ex
        error('debug_runGwCoverageRequest:ReadJson', ...
            'Failed reading %s: %s', requestJson, Ex.message);
    end

    Cfg = remapRequestSharePaths(Cfg);

    Cfg.show_figures = logical(showFigures);

    TempRequestJson = fullfile(tempdir, ...
        ['debug_gw_coverage_' tempRequestBasename(TooPlannerDir) '.json']);
    writeJsonFile(TempRequestJson, Cfg);
    fprintf('Temp request (paths remapped for this OS): %s\n', TempRequestJson);

    Runner = ultrasat.planner.TooPlannerRunner();
    SummaryFile = Runner.runFromJson(TempRequestJson);

    Result = struct();
    Result.FilesRemoved = FilesRemoved;
    Result.SummaryFile = SummaryFile;
    Result.Ok = ~isempty(SummaryFile);
    if Result.Ok
        fprintf('summary.json: %s\n', SummaryFile);
        Result.Message = "";
    else
        fprintf('TooPlannerRunner finished with no summary (check logs).\n');
        Result.Message = "TooPlannerRunner returned no summary";
    end
end

% -------------------------------------------------------------------------

function Nremoved = cleanTooPlannerFolder(tooPlannerDir)
    % Delete files in tooPlannerDir except request.json; keep all subfolders.

    Nremoved = 0;
    if ~isfolder(tooPlannerDir)
        fprintf('Warning: cleanTooPlannerFolder: not a folder: %s\n', tooPlannerDir);
        return;
    end

    Listing = dir(tooPlannerDir);
    for K = 1:numel(Listing)
        if Listing(K).isdir
            continue;
        end
        Name = Listing(K).name;
        if strcmp(Name, 'request.json')
            continue;
        end
        Fpath = fullfile(tooPlannerDir, Name);
        try
            delete(Fpath);
            Nremoved = Nremoved + 1;
            fprintf('CleanOutput: deleted %s\n', Fpath);
        catch Ex
            fprintf('Warning: failed to delete %s: %s\n', Fpath, Ex.message);
        end
    end
end

% -------------------------------------------------------------------------

function dirs = findTooPlannerDirs(root)
    % Collect every .../too_planner directory under root (not nested inside another).

    dirs = {};
    if ~isfolder(root)
        return;
    end

    [~, BaseName, ~] = fileparts(root);
    if strcmp(BaseName, 'too_planner')
        dirs{end+1} = root; %#ok<AGROW>
        return;
    end

    Listing = dir(root);
    for K = 1:numel(Listing)
        if ~Listing(K).isdir
            continue;
        end
        Name = Listing(K).name;
        if strcmp(Name, '.') || strcmp(Name, '..')
            continue;
        end
        Sub = fullfile(root, Name);
        if strcmp(Name, 'too_planner')
            dirs{end+1} = Sub; %#ok<AGROW>
        else
            Child = findTooPlannerDirs(Sub);
            dirs = [dirs, Child]; %#ok<AGROW>
        end
    end
end

% -------------------------------------------------------------------------

function base = tempRequestBasename(tooPlannerDir)
    % Short unique basename for temp request JSON (avoids batch collisions).

    Tag = regexprep(tooPlannerDir, '[^a-zA-Z0-9]+', '_');
    Tag = regexprep(Tag, '^_+|_+$', '');
    if strlength(Tag) > 72
        Tag = extractBefore(Tag, 73);
    end
    if strlength(Tag) == 0
        Tag = "gw";
    end
    base = char(Tag + "_" + string(datestr(now, 'yyyymmdd_HHMMSS_FFF')));
end

% -------------------------------------------------------------------------

function Row = emptyBatchRow()
    Row = struct( ...
        'Index', 0, ...
        'TooPlannerDir', "", ...
        'RequestJson', "", ...
        'Status', "", ...
        'FilesRemoved', 0, ...
        'SummaryFile', "", ...
        'Message', "");
end

% -------------------------------------------------------------------------

function printOneRunResult(requestJson, result)
    if result.Ok
        return;
    end
    fprintf('Run failed for %s: %s\n', requestJson, result.Message);
end

% -------------------------------------------------------------------------

function printBatchReport(rows)
    fprintf('\n========== BATCH REPORT ==========\n');
    if isempty(rows)
        fprintf('No too_planner folders found.\n');
        return;
    end

    fprintf('%4s  %-12s  %6s  %s\n', '#', 'status', 'clean', 'too_planner_dir');
    fprintf('%s\n', repmat('-', 1, 80));

    Nok = 0;
    for K = 1:numel(rows)
        R = rows(K);
         CleanStr = sprintf('%d', R.FilesRemoved);
        fprintf('%4d  %-12s  %6s  %s\n', R.Index, char(R.Status), CleanStr, R.TooPlannerDir);
        if R.Status == "ok"
            Nok = Nok + 1;
            if strlength(R.SummaryFile) > 0
                fprintf('      summary: %s\n', R.SummaryFile);
            end
        elseif strlength(R.Message) > 0
            fprintf('      message: %s\n', R.Message);
        end
    end

    fprintf('%s\n', repmat('-', 1, 80));
    fprintf('Total: %d folder(s), %d ok, %d other.\n', numel(rows), Nok, numel(rows) - Nok);
end

% -------------------------------------------------------------------------

function ensureDebugEnvironment()
    % Set SOC_PATH and ASTROPACK_DATA_PATH fallbacks for local / IDE runs.

    if isempty(getenv('SOC_PATH'))
        fprintf('SOC_PATH not set. Setting fallback for local testing...\n');
        if ispc
            setenv('SOC_PATH', 'c:\soc');
        else
            setenv('SOC_PATH', '~/soc');
        end
    end

    if isempty(getenv('ASTROPACK_DATA_PATH'))
        fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
        if ispc
            setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
        else
            setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
        end
    end
end

% -------------------------------------------------------------------------

function root = getRwshareRoot()
    % rwshare root on Windows (UNC) vs Linux (socsrv mount).

    if ispc
        root = '\\socsrv\rwshare_data';
    else
        root = '/data/rwshare_data';
    end
end

% -------------------------------------------------------------------------

function cfg = remapRequestSharePaths(cfg)
    % Remap csv_filename and output_folder to the current OS rwshare root.

    if isfield(cfg, 'csv_filename') && ~isempty(cfg.csv_filename)
        cfg.csv_filename = remapSharePath(cfg.csv_filename);
    end
    if isfield(cfg, 'output_folder') && ~isempty(cfg.output_folder)
        cfg.output_folder = remapSharePath(cfg.output_folder);
    end
end

% -------------------------------------------------------------------------

function out = remapSharePath(pathIn)
    % Map any socsrv/rwshare_data or /data/rwshare_data prefix to getRwshareRoot().

    if isempty(pathIn)
        out = pathIn;
        return;
    end

    p = char(string(pathIn));
    pNorm = strrep(p, '\', '/');

    tok = regexp(pNorm, 'rwshare_data[/\\]?(.*)$', 'tokens', 'once');
    if isempty(tok) || isempty(tok{1})
        out = p;
        return;
    end

    rel = tok{1};
    relParts = strsplit(rel, '/');
    relParts = relParts(~cellfun(@isempty, relParts));
    if isempty(relParts)
        out = getRwshareRoot();
    else
        out = fullfile(getRwshareRoot(), relParts{:});
    end
end

% -------------------------------------------------------------------------

function writeJsonFile(filename, cfg)
    % Write struct as pretty JSON.

    txt = jsonencode(cfg, 'PrettyPrint', true);
    fid = fopen(filename, 'w');
    if fid < 0
        error('debug_runGwCoverageRequest:WriteJson', ...
            'Failed opening %s for write', filename);
    end
    cleaner = onCleanup(@() fclose(fid)); %#ok<NASGU>
    fwrite(fid, txt, 'char');
end

% -------------------------------------------------------------------------
% Plan compare (multi-plan requests: cover_50 vs cover_90, etc.)
% -------------------------------------------------------------------------

function runPlanCompareIfEnabled(comparePlans, reportDir, result, tooPlannerDir, planLabels)
    % Print aligned compare row and append CSV after a successful run.

    if ~comparePlans || ~result.Ok || strlength(string(result.SummaryFile)) == 0
        return;
    end

    if isempty(planLabels)
        planLabels = discoverPlanLabelsFromFolderOutputs(char(result.SummaryFile));
    end

    if ~isfile(compareCsvReportPath(reportDir))
        initCompareCsvReport(reportDir, planLabels);
    end

    Cmp = comparePlansFromSummary(char(result.SummaryFile), tooPlannerDir);
    if Cmp.HasError
        fprintf('Warning: plan compare for %s: %s\n', tooPlannerDir, Cmp.ErrorMessage);
        return;
    end

    printCompareConsoleRow(Cmp.Row, planLabels);
    appendCompareCsvRow(reportDir, formatCompareCsvRow(Cmp.Row, planLabels));
end

% -------------------------------------------------------------------------

function Cmp = comparePlansFromSummary(summaryFile, tooPlannerDir)
    % Build compare row struct from summary.json plan outputs.

    [AlertId, StageName] = parseTooPlannerPathLabels(tooPlannerDir);
    Cmp = struct('Row', [], 'HasError', false, 'ErrorMessage', "");

    if ~isfile(summaryFile)
        Cmp.HasError = true;
        Cmp.ErrorMessage = "no_summary_file";
        return;
    end

    try
        Summary = jsondecode(fileread(summaryFile));
    catch
        Cmp.HasError = true;
        Cmp.ErrorMessage = "bad_summary_json";
        return;
    end

    PlanEntries = summaryPlansList(Summary);
    Nattempted = 0;
    if isfield(Summary, 'total_plans_attempted') && ~isempty(Summary.total_plans_attempted)
        Nattempted = Summary.total_plans_attempted;
    end

    Metrics = repmat(emptyPlanMetrics(), 0, 1);
    Npartial = 0;

    for K = 1:numel(PlanEntries)
        Entry = PlanEntries(K);
        JsonPath = char(planEntryJsonPath(Entry));
        if isempty(JsonPath) || ~isfile(JsonPath)
            Npartial = Npartial + 1;
            continue;
        end
        M = extractPlanMetrics(JsonPath);
        if isempty(M.Label)
            Npartial = Npartial + 1;
            continue;
        end
        Metrics(end+1, 1) = M; %#ok<AGROW>
    end

    Row = struct();
    Row.AlertId = AlertId;
    Row.StageName = StageName;
    Row.PlansOk = numel(Metrics);
    Row.PlansAttempted = Nattempted;
    Row.Partial = Npartial;
    Row.ScheduleIdentical = char(scheduleIdenticalFlag(Metrics));
    Row.MapStatsDiff = char(mapStatsDiffFlag(Metrics));
    Row.Metrics = Metrics;

    Cmp.Row = Row;
end

% -------------------------------------------------------------------------

function Labels = discoverPlanLabelsFromRequests(tooPlannerDirs)
    % Union of plan labels from request.json in each too_planner folder.

    AllLabels = {};
    for K = 1:numel(tooPlannerDirs)
        RequestJson = fullfile(tooPlannerDirs{K}, 'request.json');
        if ~isfile(RequestJson)
            continue;
        end
        Part = discoverPlanLabelsFromRequest(RequestJson);
        AllLabels = [AllLabels, Part]; %#ok<AGROW>
    end
    Labels = unique(AllLabels, 'stable');
    Labels = sort(Labels);
end

% -------------------------------------------------------------------------

function Labels = discoverPlanLabelsFromRequest(requestJson)
    % Read plans[].label from a GW too_planner request.json.

    Labels = {};
    if ~isfile(requestJson)
        return;
    end
    try
        Cfg = jsondecode(fileread(requestJson));
    catch
        return;
    end
    if ~isfield(Cfg, 'plans') || isempty(Cfg.plans)
        return;
    end
    Plans = Cfg.plans;
    N = numel(Plans);
    for K = 1:N
        P = Plans(K);
        if isfield(P, 'label') && ~isempty(P.label)
            Labels{end+1} = char(string(P.label)); %#ok<AGROW>
        else
            Labels{end+1} = sprintf('plan_%d', K); %#ok<AGROW>
        end
    end
end

% -------------------------------------------------------------------------

function Labels = discoverPlanLabelsFromFolderOutputs(summaryFile)
    % Fallback labels from plan JSON metadata when request has no labels.

    Labels = {};
    if ~isfile(summaryFile)
        return;
    end
    try
        Summary = jsondecode(fileread(summaryFile));
    catch
        return;
    end
    Entries = summaryPlansList(Summary);
    for K = 1:numel(Entries)
        JsonPath = char(planEntryJsonPath(Entries(K)));
        if isempty(JsonPath) || ~isfile(JsonPath)
            continue;
        end
        try
            Doc = jsondecode(fileread(JsonPath));
        catch
            continue;
        end
        Labels{end+1} = planJsonLabel(Doc, JsonPath); %#ok<AGROW>
    end
    Labels = unique(Labels, 'stable');
    Labels = sort(Labels);
end

% -------------------------------------------------------------------------

function ReportPath = compareCsvReportPath(reportDir)
    ReportPath = fullfile(reportDir, 'too_planner_plan_compare.csv');
end

% -------------------------------------------------------------------------

function initCompareCsvReport(reportDir, planLabels)
    % Create/overwrite compare CSV with header row for this batch.

    if ~isfolder(reportDir)
        fprintf('Warning: compare report dir not found: %s\n', reportDir);
        return;
    end

    ReportPath = compareCsvReportPath(reportDir);
    if isfile(ReportPath)
        fprintf('Overwriting prior compare CSV: %s\n', ReportPath);
    end

    HeaderLine = formatCompareCsvHeader(planLabels);
    Fid = fopen(ReportPath, 'w');
    if Fid < 0
        fprintf('Warning: failed creating compare CSV: %s\n', ReportPath);
        return;
    end
    Cleaner = onCleanup(@() fclose(Fid)); %#ok<NASGU>
    fwrite(Fid, [HeaderLine newline], 'char');
end

% -------------------------------------------------------------------------

function HeaderLine = formatCompareCsvHeader(planLabels)
    % CSV header: fixed columns plus wide plan metric columns.

    Fixed = {'alert', 'stage', 'plans_ok', 'plans_attempted', 'partial', ...
        'schedule_identical', 'map_stats_diff'};
    PlanCols = planMetricColumnNames(planLabels);
    AllCols = [Fixed, PlanCols];
    HeaderLine = joinCsvFields(AllCols);
end

% -------------------------------------------------------------------------

function ColNames = planMetricColumnNames(planLabels)
    ColNames = cell(1, 0);
    for K = 1:numel(planLabels)
        Base = sanitizeLabelForColumn(planLabels{K});
        ColNames{end+1} = [Base '_out_uniq']; %#ok<AGROW>
        ColNames{end+1} = [Base '_exp']; %#ok<AGROW>
        ColNames{end+1} = [Base '_used']; %#ok<AGROW>
        ColNames{end+1} = [Base '_cov']; %#ok<AGROW>
    end
end

% -------------------------------------------------------------------------

function Base = sanitizeLabelForColumn(label)
    Base = char(string(label));
    Base = regexprep(Base, '[^a-zA-Z0-9_]', '_');
    Base = regexprep(Base, '_+', '_');
    Base = regexprep(Base, '^_+|_+$', '');
    if isempty(Base)
        Base = 'plan';
    end
end

% -------------------------------------------------------------------------

function RowLine = formatCompareCsvRow(Row, planLabels)
    % One CSV data row from compare row struct.

    Fields = { ...
        csvEscapeField(Row.AlertId), ...
        csvEscapeField(Row.StageName), ...
        csvEscapeField(Row.PlansOk), ...
        csvEscapeField(Row.PlansAttempted), ...
        csvEscapeField(Row.Partial), ...
        csvEscapeField(Row.ScheduleIdentical), ...
        csvEscapeField(Row.MapStatsDiff)};

    for K = 1:numel(planLabels)
        M = findPlanMetricsByLabel(Row.Metrics, planLabels{K});
        if isempty(M.Label)
            Fields = [Fields, {'NA', 'NA', 'NA', 'NA'}]; %#ok<AGROW>
        else
            Fields = [Fields, { ...
                csvEscapeField(M.OutUniq), ...
                csvEscapeField(M.Exp), ...
                csvEscapeField(M.Used), ...
                csvEscapeField(M.Cov)}]; %#ok<AGROW>
        end
    end

    RowLine = joinCsvFields(Fields);
end

% -------------------------------------------------------------------------

function appendCompareCsvRow(reportDir, rowLine)
    % Append one data row to the compare CSV.

    if ~isfolder(reportDir)
        return;
    end

    ReportPath = compareCsvReportPath(reportDir);
    if ~isfile(ReportPath)
        fprintf('Warning: compare CSV missing, skipping row append: %s\n', ReportPath);
        return;
    end

    Fid = fopen(ReportPath, 'a');
    if Fid < 0
        fprintf('Warning: failed appending compare CSV: %s\n', ReportPath);
        return;
    end
    Cleaner = onCleanup(@() fclose(Fid)); %#ok<NASGU>
    fwrite(Fid, [rowLine newline], 'char');
end

% -------------------------------------------------------------------------

function printCompareConsoleRow(Row, planLabels)
    % Fixed-width console summary aligned with CSV columns.

    fprintf('PLAN_COMPARE  %-11s  %-16s  %4s  %4s  ok=%d', ...
        Row.AlertId, Row.StageName, Row.ScheduleIdentical, Row.MapStatsDiff, Row.PlansOk);

    for K = 1:numel(planLabels)
        Label = planLabels{K};
        M = findPlanMetricsByLabel(Row.Metrics, Label);
        if isempty(M.Label)
            fprintf(' | %-10s: --', Label);
        else
            fprintf(' | %-10s: u%-2d e%-2d used%-2s cov%-6s', ...
                Label, M.OutUniq, M.Exp, metricsScalarText(M.Used), metricsScalarText(M.Cov));
        end
    end
    fprintf('\n');
end

% -------------------------------------------------------------------------

function M = findPlanMetricsByLabel(metrics, label)
    M = emptyPlanMetrics();
    if isempty(metrics)
        return;
    end
    for K = 1:numel(metrics)
        if strcmp(metrics(K).Label, label)
            M = metrics(K);
            return;
        end
    end
end

% -------------------------------------------------------------------------

function Line = joinCsvFields(fields)
    Line = strjoin(fields, ',');
end

% -------------------------------------------------------------------------

function S = csvEscapeField(value)
    % Format a value for CSV; quote if needed.

    if isstring(value) || ischar(value)
        Text = char(string(value));
    elseif isnan(value)
        Text = 'NA';
    elseif islogical(value)
        Text = char(string(value));
    elseif isnumeric(value) && isscalar(value)
        if abs(value - round(value)) < 1e-9
            Text = sprintf('%d', round(value));
        else
            Text = sprintf('%.6g', value);
        end
    else
        Text = char(string(value));
    end

    if contains(Text, ',') || contains(Text, '"') || contains(Text, newline)
        Text = strrep(Text, '"', '""');
        S = ['"' Text '"'];
    else
        S = Text;
    end
end

% -------------------------------------------------------------------------

function M = extractPlanMetrics(planJsonPath)
    % Load one plan JSON and extract compare metrics + schedule fingerprint.

    M = emptyPlanMetrics();
    M.JsonPath = char(planJsonPath);

    if ~isfile(planJsonPath)
        return;
    end

    try
        Doc = jsondecode(fileread(planJsonPath));
    catch
        return;
    end

    M.Label = planJsonLabel(Doc, planJsonPath);

    if isfield(Doc, 'metadata') && isfield(Doc.metadata, 'result')
        Res = Doc.metadata.result;
        if isfield(Res, 'exposures_scheduled') && ~isempty(Res.exposures_scheduled)
            M.Exp = Res.exposures_scheduled;
        end
        if isfield(Res, 'too_used_targets') && ~isempty(Res.too_used_targets)
            M.Used = Res.too_used_targets;
        end
        if isfield(Res, 'too_covered_prob') && ~isempty(Res.too_covered_prob)
            M.Cov = Res.too_covered_prob;
        end
        if isfield(Res, 'tstart_utc')
            M.Tstart = char(string(Res.tstart_utc));
        end
        if isfield(Res, 'tend_utc')
            M.Tend = char(string(Res.tend_utc));
        end
    end

    Targets = [];
    if isfield(Doc, 'targets') && ~isempty(Doc.targets)
        Targets = Doc.targets;
    end

    if M.Exp == 0 && ~isempty(Targets)
        M.Exp = numel(Targets);
    end

    M.OutUniq = countUniqTargets(Targets);
    M.Fingerprint = buildScheduleFingerprint(Targets);
end

% -------------------------------------------------------------------------

function Label = planJsonLabel(Doc, planJsonPath)
    % Resolve human-readable plan label from JSON metadata.

    Label = "";
    if isfield(Doc, 'metadata')
        Meta = Doc.metadata;
        if isfield(Meta, 'plan_cfg') && isfield(Meta.plan_cfg, 'label') && ~isempty(Meta.plan_cfg.label)
            Label = string(Meta.plan_cfg.label);
        elseif isfield(Meta, 'run_id') && ~isempty(Meta.run_id)
            Label = string(Meta.run_id);
        end
    end
    if strlength(Label) == 0
        [~, Base, ~] = fileparts(planJsonPath);
        Label = string(Base);
    end
    Label = char(Label);
end

% -------------------------------------------------------------------------

function Nuniq = countUniqTargets(targets)
    % Count distinct pointings in the exported targets list.

    Nuniq = 0;
    if isempty(targets)
        return;
    end

    if isfield(targets, 'UniqTargInd')
        Ind = [targets.UniqTargInd];
        Ind = Ind(~isnan(Ind));
        if ~isempty(Ind)
            Nuniq = numel(unique(Ind));
            return;
        end
    end

    if isfield(targets, 'RA') && isfield(targets, 'Dec')
        Ra = round([targets.RA], 4);
        Dec = round([targets.Dec], 4);
        Keys = Ra + 1i * Dec;
        Nuniq = numel(unique(Keys));
    end
end

% -------------------------------------------------------------------------

function Fp = buildScheduleFingerprint(targets)
    % Hashable signature of the scheduled target rows.

    Fp = "";
    if isempty(targets)
        return;
    end

    if ~isfield(targets, 'Tstart_utc')
        return;
    end

    Starts = {targets.Tstart_utc};
    [~, Ord] = sort(Starts);
    Parts = strings(0, 1);
    for K = 1:numel(Ord)
        T = targets(Ord(K));
        Ra = 0;
        Dec = 0;
        Roll = 0;
        Nexp = 0;
        if isfield(T, 'RA')
            Ra = round(T.RA, 4);
        end
        if isfield(T, 'Dec')
            Dec = round(T.Dec, 4);
        end
        if isfield(T, 'Roll')
            Roll = round(T.Roll, 6);
        end
        if isfield(T, 'Nexposures')
            Nexp = T.Nexposures;
        end
        Tstart = "";
        if isfield(T, 'Tstart_utc')
            Tstart = string(T.Tstart_utc);
        end
        Parts(end+1, 1) = sprintf('%.4f,%.4f,%s,%.6f,%d', Ra, Dec, Tstart, Roll, Nexp); %#ok<AGROW>
    end
    Fp = char(strjoin(Parts, ';'));
end

% -------------------------------------------------------------------------

function Flag = scheduleIdenticalFlag(metrics)
    % Y/N/NA for whether all loaded plans share the same schedule fingerprint.

    if numel(metrics) < 2
        Flag = "NA";
        return;
    end

    Fps = strings(numel(metrics), 1);
    for K = 1:numel(metrics)
        Fps(K) = string(metrics(K).Fingerprint);
    end
    Fps = Fps(strlength(strtrim(Fps)) > 0);
    if numel(Fps) < 2
        Flag = "NA";
        return;
    end

    if numel(unique(Fps)) == 1
        Flag = "Y";
    else
        Flag = "N";
    end
end

% -------------------------------------------------------------------------

function Flag = mapStatsDiffFlag(metrics)
    % Y/N/NA whether map metadata differs across plans.

    if numel(metrics) < 2
        Flag = "NA";
        return;
    end

    UsedVals = [metrics.Used];
    CovVals = [metrics.Cov];
    UsedDiff = numel(unique(UsedVals(~isnan(UsedVals)))) > 1;
    CovDiff = numel(unique(round(CovVals(~isnan(CovVals)), 6))) > 1;

    if UsedDiff || CovDiff
        Flag = "Y";
    else
        Flag = "N";
    end
end

% -------------------------------------------------------------------------

function Entries = summaryPlansList(summary)
    % Normalize summary.plans from jsondecode to struct array.

    Entries = repmat(struct('json_file', ''), 0, 1);
    if ~isfield(summary, 'plans') || isempty(summary.plans)
        return;
    end

    Plans = summary.plans;
    if isstruct(Plans)
        Entries = Plans(:);
        return;
    end

    if iscell(Plans)
        for K = 1:numel(Plans)
            if isstruct(Plans{K})
                Entries(end+1, 1) = Plans{K}; %#ok<AGROW>
            end
        end
    end
end

% -------------------------------------------------------------------------

function JsonPath = planEntryJsonPath(entry)
    % Absolute or relative path to plan JSON from summary entry.

    JsonPath = "";
    if isfield(entry, 'json_file') && ~isempty(entry.json_file)
        JsonPath = string(entry.json_file);
    end
end

% -------------------------------------------------------------------------

function [AlertId, StageName] = parseTooPlannerPathLabels(tooPlannerDir)
    % Extract GW alert id and stage folder name from too_planner path.

    StageDir = fileparts(tooPlannerDir);
    [~, StageName, ~] = fileparts(StageDir);
    Tokens = regexp(tooPlannerDir, 'S[A-Za-z0-9]+', 'match');
    if isempty(Tokens)
        AlertId = "?";
    else
        AlertId = Tokens{end};
    end
    StageName = char(StageName);
end

% -------------------------------------------------------------------------

function M = emptyPlanMetrics()
    M = struct( ...
        'Label', '', ...
        'JsonPath', '', ...
        'OutUniq', 0, ...
        'Exp', 0, ...
        'Used', NaN, ...
        'Cov', NaN, ...
        'Tstart', '', ...
        'Tend', '', ...
        'Fingerprint', '');
end

% -------------------------------------------------------------------------

function S = metricsScalarText(Value)
    % Format used/cov for summary line; NaN -> NA.

    if isnan(Value)
        S = 'NA';
    elseif abs(Value - round(Value)) < 1e-9
        S = sprintf('%d', round(Value));
    else
        S = sprintf('%.3f', Value);
    end
end
