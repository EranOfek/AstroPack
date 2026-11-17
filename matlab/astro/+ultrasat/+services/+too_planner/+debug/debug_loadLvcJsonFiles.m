%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_loadLvcJsonFiles.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Updated     : 15/11/2026
% Description : Find all lvc_*.json under the GW coverage experiment share tree
%               and verify MATLAB fileread + jsondecode can load each file.
%               Prints per-file progress and a summary report; writes
%               lvc_json_load_report.csv on the experiment share root.
%
% Run by      : ultrasat.services.too_planner.debug.debug_loadLvcJsonFiles()
%               ultrasat.services.too_planner.debug.debug_loadLvcJsonFiles( ...
%                   MaxFiles=5)
%==========================================================================

function debug_loadLvcJsonFiles(Args)
    % debug_loadLvcJsonFiles Walk lvc_*.json files and jsondecode each one.

    arguments
        Args.ParentFolder (1,1) string = ""
        Args.MaxFiles (1,1) double = Inf
        Args.StopOnFirstError (1,1) logical = false
    end

    fprintf('\n========== DEBUG LOAD LVC JSON ==========\n');

    Parent = resolveParentFolder(Args.ParentFolder);
    fprintf('Root: %s\n', Parent);

    fprintf('Listing lvc_*.json (recursive)...\n');
    Listing = dir(fullfile(Parent, '**', 'lvc_*.json'));
    if isempty(Listing)
        error('debug_loadLvcJsonFiles:NoFiles', ...
            'No lvc_*.json files under: %s', Parent);
    end

  % Sort by full path for stable output.
    Paths = strings(numel(Listing), 1);
    for K = 1:numel(Listing)
        Paths(K) = string(fullfile(Listing(K).folder, Listing(K).name));
    end
    Paths = sort(Paths);

    Ntotal = numel(Paths);
    if ~isinf(Args.MaxFiles) && Args.MaxFiles > 0 && Args.MaxFiles < Ntotal
        Paths = Paths(1:floor(Args.MaxFiles));
        fprintf('Found %d file(s); processing first %d (MaxFiles).\n', Ntotal, numel(Paths));
    else
        fprintf('Found %d lvc_*.json file(s).\n', Ntotal);
    end

    Rows = repmat(emptyLoadRow(), 0, 1);
    ParentChar = char(Parent);

    for K = 1:numel(Paths)
        Fpath = char(Paths(K));
        Row = tryLoadOneLvcJson(Fpath);
        Rows(end+1, 1) = Row; %#ok<AGROW>

        RelPath = relativePathFromParent(ParentChar, Fpath);
        printLoadProgressLine(K, numel(Paths), Row, RelPath);

        if ~Row.Ok && Args.StopOnFirstError
            error('debug_loadLvcJsonFiles:LoadFailed', ...
                'Failed on %s: %s', Fpath, Row.Message);
        end
    end

    printLoadSummaryReport(Rows);
    ReportPath = writeLoadReportCsv(Rows, Parent);
    fprintf('Report CSV: %s\n', ReportPath);

    fprintf('========== DEBUG LOAD LVC JSON DONE ==========\n\n');
end

% -------------------------------------------------------------------------

function Parent = resolveParentFolder(parentFolder)
    % resolveParentFolder Default experiment root on the rwshare tree.

    if strlength(strtrim(parentFolder)) > 0
        Parent = char(strtrim(parentFolder));
    else
        Parent = fullfile(getRwshareRoot(), 'gw_coverage_experiment_2026_08');
    end

    if ~isfolder(Parent)
        error('debug_loadLvcJsonFiles:ParentNotFound', ...
            'Parent folder not found: %s', Parent);
    end
end

% -------------------------------------------------------------------------

function Root = getRwshareRoot()
    % getRwshareRoot rwshare root on Windows (UNC) vs Linux (socsrv mount).

    if ispc
        Root = '\\socsrv\rwshare_data';
    else
        Root = '/data/rwshare_data';
    end
end

% -------------------------------------------------------------------------

function Row = tryLoadOneLvcJson(fpath)
    % tryLoadOneLvcJson fileread + jsondecode with timing and sanity fields.

    Row = emptyLoadRow();
    Row.Path = string(fpath);

    D = dir(fpath);
    if isempty(D)
        Row.Ok = false;
        Row.Message = "file not found";
        return;
    end
    Row.Bytes = D(1).bytes;

    T0 = tic; %#ok<TNOW1> built-in tic; toc via builtin (path may shadow toc)
    try
        Txt = fileread(fpath);
        Doc = jsondecode(Txt);
        Row.Seconds = builtin('toc', T0);
        Row.Ok = true;
        Row.Message = "";

        if ~isstruct(Doc)
            Row.Ok = false;
            Row.Message = sprintf('jsondecode returned %s, not struct', class(Doc));
            return;
        end

        Row.NtopFields = numel(fieldnames(Doc));
        Row.AlertType = topLevelStringField(Doc, 'alert_type');
        Row.SupereventId = topLevelStringField(Doc, 'superevent_id');
        Row.TimeCreated = topLevelStringField(Doc, 'time_created');
    catch Ex
        Row.Seconds = builtin('toc', T0);
        Row.Ok = false;
        Row.Message = string(Ex.message);
    end
end

% -------------------------------------------------------------------------

function S = topLevelStringField(doc, name)
    % topLevelStringField Read a scalar top-level field or return empty.

    S = "";
    if ~isfield(doc, name) || isempty(doc.(name))
        return;
    end
    S = string(doc.(name));
end

% -------------------------------------------------------------------------

function Rel = relativePathFromParent(parent, fpath)
    % relativePathFromParent Path under parent for console display.

    ParentNorm = strrep(lower(parent), '/', '\');
    FpathNorm = strrep(lower(fpath), '/', '\');
    if startsWith(FpathNorm, ParentNorm)
        Rel = fpath(numel(parent) + 1 : end);
        Rel = regexprep(Rel, '^[\\/]+', '');
    else
        Rel = fpath;
    end
end

% -------------------------------------------------------------------------

function printLoadProgressLine(index, nFiles, row, relPath)
    % printLoadProgressLine One OK/FAIL line per file.

    Mb = row.Bytes / 1e6;
    Status = "OK";
    if ~row.Ok
        Status = "FAIL";
    end

    Meta = sprintf('%s  %s', char(row.SupereventId), char(row.AlertType));
    if strlength(row.SupereventId) == 0
        Meta = char(row.AlertType);
    end
    if strlength(row.AlertType) == 0 && strlength(row.SupereventId) == 0
        Meta = '';
    end

    if row.Ok
        fprintf('[%d/%d] %-4s  %.2f MB  %.2f s  %s  %s\n', ...
            index, nFiles, char(Status), Mb, row.Seconds, Meta, relPath);
    else
        fprintf('[%d/%d] %-4s  %.2f MB  %.2f s  %s\n', ...
            index, nFiles, char(Status), Mb, row.Seconds, relPath);
        fprintf('        %s\n', char(row.Message));
    end
end

% -------------------------------------------------------------------------

function printLoadSummaryReport(rows)
    % printLoadSummaryReport Totals, failures, missing expected fields.

    fprintf('\n========== LVC JSON LOAD SUMMARY ==========\n');

    if isempty(rows)
        fprintf('No files processed.\n');
        return;
    end

    N = numel(rows);
    OkMask = [rows.Ok];
    Nok = sum(OkMask);
    Nfail = N - Nok;
    TotalMb = sum([rows.Bytes]) / 1e6;
    TotalSec = sum([rows.Seconds]);

    fprintf('Total: %d  ok: %d  failed: %d  size: %.2f MB  time: %.2f s\n', ...
        N, Nok, Nfail, TotalMb, TotalSec);

    if Nfail > 0
        fprintf('\nFailed files:\n');
        for K = 1:N
            if rows(K).Ok
                continue;
            end
            fprintf('  %s\n    %s\n', rows(K).Path, rows(K).Message);
        end
    end

    MissingMask = OkMask & ( ...
        strlength([rows.SupereventId]) == 0 | strlength([rows.AlertType]) == 0);
    Nmissing = sum(MissingMask);
    if Nmissing > 0
        fprintf('\nLoaded but missing alert_type and/or superevent_id: %d file(s)\n', ...
            Nmissing);
        for K = 1:N
            if ~MissingMask(K)
                continue;
            end
            fprintf('  %s\n', rows(K).Path);
        end
    end

    fprintf('%s\n', repmat('-', 1, 60));
end

% -------------------------------------------------------------------------

function ReportPath = writeLoadReportCsv(rows, parentFolder)
    % writeLoadReportCsv Write lvc_json_load_report.csv under the share experiment root.

    ReportPath = fullfile(parentFolder, 'lvc_json_load_report.csv');
    T = table( ...
        [rows.Path]', ...
        [rows.Ok]', ...
        [rows.Bytes]', ...
        [rows.Seconds]', ...
        [rows.AlertType]', ...
        [rows.SupereventId]', ...
        [rows.Message]', ...
        'VariableNames', {'path', 'ok', 'bytes', 'seconds', 'alert_type', ...
        'superevent_id', 'message'});
    try
        writetable(T, ReportPath);
    catch Ex
        fprintf('Warning: could not write report CSV: %s\n', Ex.message);
        ReportPath = '';
    end
end

% -------------------------------------------------------------------------

function Row = emptyLoadRow()
    Row = struct( ...
        'Path', "", ...
        'Ok', false, ...
        'Bytes', 0, ...
        'Seconds', 0, ...
        'AlertType', "", ...
        'SupereventId', "", ...
        'TimeCreated', "", ...
        'NtopFields', 0, ...
        'Message', "");
end