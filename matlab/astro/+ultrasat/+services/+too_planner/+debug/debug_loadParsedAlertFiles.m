%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_loadParsedAlertFiles.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Description : Smoke-test parsed_alert.json under GW coverage share via LvkParsedAlert.
%
% Run:
%   ultrasat.services.too_planner.debug.debug_loadParsedAlertFiles()
%==========================================================================

function debug_loadParsedAlertFiles(Args)
    % Find parsed_alert.json via too_planner stage dirs and load each with LvkParsedAlert.

    arguments
        Args.ParentFolder (1,1) string = ""
        Args.MaxFiles (1,1) double = Inf
        Args.StopOnFirstError (1,1) logical = false
    end

    fprintf('\n========== DEBUG LOAD PARSED ALERT JSON ==========\n');

    Parent = resolveParsedAlertParent(Args.ParentFolder);
    fprintf('Root: %s\n', Parent);

    StageDirs = findStageDirsFromPlanner(Parent);
    if isempty(StageDirs)
        error('debug_loadParsedAlertFiles:NoStages', ...
            'No too_planner stages under: %s', Parent);
    end

    Paths = strings(numel(StageDirs), 1);
    for K = 1:numel(StageDirs)
        Paths(K) = string(fullfile(StageDirs{K}, 'parsed_alert.json'));
    end
    Paths = Paths(isfile(Paths));
    Paths = sort(Paths);

    if isempty(Paths)
        error('debug_loadParsedAlertFiles:NoParsedAlerts', ...
            'No parsed_alert.json files under: %s', Parent);
    end

    Ntotal = numel(Paths);
    if ~isinf(Args.MaxFiles) && Args.MaxFiles > 0 && Args.MaxFiles < Ntotal
        Paths = Paths(1:floor(Args.MaxFiles));
        fprintf('Found %d parsed_alert.json; processing first %d (MaxFiles).\n', Ntotal, numel(Paths));
    else
        fprintf('Found %d parsed_alert.json file(s).\n', Ntotal);
    end

    Rows = repmat(emptyParsedLoadRow(), 0, 1);
    ParentChar = char(Parent);

    for K = 1:numel(Paths)
        Fpath = char(Paths(K));
        Row = tryLoadOneParsedAlert(Fpath);
        Rows(end+1, 1) = Row; %#ok<AGROW>

        RelPath = relativePathFromParent(ParentChar, Fpath);
        printParsedLoadLine(K, numel(Paths), Row, RelPath);

        if ~Row.Ok && Args.StopOnFirstError
            error('debug_loadParsedAlertFiles:LoadFailed', ...
                'Failed on %s: %s', Fpath, Row.Message);
        end
    end

    printParsedLoadSummary(Rows);
    ReportPath = writeParsedLoadReport(Parent, Rows);
    if strlength(ReportPath) > 0
        fprintf('Report CSV: %s\n', ReportPath);
    end

    fprintf('========== DEBUG LOAD PARSED ALERT JSON DONE ==========\n\n');
end

% -------------------------------------------------------------------------

function Parent = resolveParsedAlertParent(parentFolder)
    % Default GW experiment root on rwshare.

    if strlength(strtrim(parentFolder)) > 0
        Parent = char(strtrim(parentFolder));
    else
        Parent = fullfile(getRwshareRoot(), 'gw_coverage_experiment_2026_08');
    end

    if ~isfolder(Parent)
        error('debug_loadParsedAlertFiles:ParentNotFound', ...
            'Parent folder not found: %s', Parent);
    end
end


function Root = getRwshareRoot()
    if ispc
        Root = '\\socsrv\rwshare_data';
    else
        Root = '/data/rwshare_data';
    end
end


function StageDirs = findStageDirsFromPlanner(parentFolder)
    % Stage folder = parent of too_planner (same rule as skymap verify).

    Dreq = dir(fullfile(parentFolder, '**', 'too_planner', 'request.json'));
    Set = strings(0, 1);
    for K = 1:numel(Dreq)
        TooDir = string(Dreq(K).folder);
        Set(end+1) = string(fileparts(TooDir)); %#ok<AGROW>
    end
    Set = unique(Set);
    StageDirs = cell(numel(Set), 1);
    for K = 1:numel(Set)
        StageDirs{K} = char(Set(K));
    end
end


function Row = tryLoadOneParsedAlert(fpath)
    % Load via LvkParsedAlert and record filter-facing scalars.

    Row = emptyParsedLoadRow();
    Row.Path = string(fpath);

    D = dir(fpath);
    if isempty(D)
        Row.Ok = false;
        Row.Message = "file not found";
        return;
    end
    Row.Bytes = D(1).bytes;

    T0 = tic;
    try
        Alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.loadFromJsonFile(fpath);
        Row.Seconds = toc(T0);
        Row.Ok = true;
        Row.Message = "";
        Row.AlertId = string(Alert.alert_id);
        Row.SupereventId = string(Alert.superevent_id);
        Row.AlertType = string(Alert.alert_type);
        Row.Schema = detectSchema(fpath);
        if isnan(Alert.prob_bns) && isnan(Alert.far_per_year)
            Row.Message = "warn: prob_bns and far_per_year both NaN";
        end
    catch Ex
        Row.Seconds = toc(T0);
        Row.Ok = false;
        Row.Message = string(Ex.message);
    end
end


function schema = detectSchema(fpath)
    % Label slim vs legacy from JSON top-level keys before full parse.

    schema = "";
    try
        Doc = jsondecode(fileread(fpath));
        if isfield(Doc, 'alert')
            schema = "slim";
        elseif isfield(Doc, 'raw_fields')
            schema = "transitional";
        else
            schema = "legacy_flat";
        end
    catch
        schema = "unknown";
    end
end


function Rel = relativePathFromParent(parent, fpath)
    ParentNorm = strrep(lower(parent), '/', '\');
    FpathNorm = strrep(lower(fpath), '/', '\');
    if startsWith(FpathNorm, ParentNorm)
        Rel = fpath(numel(parent) + 1 : end);
        Rel = regexprep(Rel, '^[\\/]+', '');
    else
        Rel = fpath;
    end
end


function printParsedLoadLine(index, nFiles, row, relPath)
    Status = "OK";
    if ~row.Ok
        Status = "FAIL";
    end
    Mb = row.Bytes / 1e6;
    if row.Ok
        fprintf('[%d/%d] %-4s  %.2f MB  %.3f s  %s  %s  schema=%s\n', ...
            index, nFiles, char(Status), Mb, row.Seconds, ...
            char(row.AlertId), relPath, char(row.Schema));
        if strlength(row.Message) > 0
            fprintf('        %s\n', char(row.Message));
        end
    else
        fprintf('[%d/%d] %-4s  %.2f MB  %.3f s  %s\n', ...
            index, nFiles, char(Status), Mb, row.Seconds, relPath);
        fprintf('        %s\n', char(row.Message));
    end
end


function printParsedLoadSummary(rows)
    fprintf('\n========== PARSED ALERT LOAD SUMMARY ==========\n');
    if isempty(rows)
        fprintf('No files processed.\n');
        return;
    end
    N = numel(rows);
    OkMask = [rows.Ok];
    Nok = sum(OkMask);
    Nfail = N - Nok;
    fprintf('Total: %d  ok: %d  failed: %d\n', N, Nok, Nfail);
    fprintf('%s\n', repmat('-', 1, 60));
end


function ReportPath = writeParsedLoadReport(parent, rows)
    ReportPath = fullfile(parent, 'parsed_alert_load_report.csv');
    T = struct2table(rows);
    try
        writetable(T, ReportPath);
    catch Ex
        fprintf('Warning: could not write report: %s\n', Ex.message);
        ReportPath = "";
    end
end


function Row = emptyParsedLoadRow()
    Row = struct( ...
        'Path', "", ...
        'Ok', false, ...
        'Bytes', 0, ...
        'Seconds', 0, ...
        'AlertId', "", ...
        'SupereventId', "", ...
        'AlertType', "", ...
        'Schema', "", ...
        'Message', "");
end