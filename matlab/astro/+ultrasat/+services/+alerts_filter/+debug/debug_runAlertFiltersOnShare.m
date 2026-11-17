%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+alerts_filter/+debug/debug_runAlertFiltersOnShare.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Description : Run lvk filters on all GW share alert stages; write result JSON and summary CSV.
%
% Run:
%   ultrasat.services.alerts_filter.debug.debug_runAlertFiltersOnShare()
%==========================================================================

function debug_runAlertFiltersOnShare(Args)
    % Batch filter_lvk via processRequest for each stage and filter name.

    arguments
        Args.ParentFolder (1,1) string = ""
        Args.Filters (1,:) string = ["simple", "with_criteria"]
        Args.MaxFiles (1,1) double = Inf
        Args.WriteSummary (1,1) logical = true
    end

    fprintf('\n========== DEBUG RUN ALERT FILTERS ON SHARE ==========\n');

    % Prefer Ultrasat AstroPack on path over legacy C:\AstroPack install.
    restoredefaultpath;
    astroRoot = fileparts(fileparts(fileparts(fileparts(fileparts(mfilename('fullpath'))))));
    matlabRoot = fileparts(astroRoot);
    addpath(genpath(matlabRoot));

    Parent = resolveParent(Args.ParentFolder);
    fprintf('Root: %s\n', Parent);

    StageDirs = findAlertStageDirs(Parent);
    if isempty(StageDirs)
        error('debug_runAlertFiltersOnShare:NoStages', 'No alert_filter folders under: %s', Parent);
    end

    if ~isinf(Args.MaxFiles) && Args.MaxFiles > 0 && Args.MaxFiles < numel(StageDirs)
        StageDirs = StageDirs(1:floor(Args.MaxFiles));
        fprintf('Stages: using first %d of %d (MaxFiles).\n', numel(StageDirs), numel(StageDirs));
    else
        fprintf('Stages: %d\n', numel(StageDirs));
    end

    Rows = repmat(emptyFilterRow(), 0, 1);
    ParentChar = char(Parent);

    for K = 1:numel(StageDirs)
        stageDir = StageDirs{K};
        parsedPath = fullfile(stageDir, 'parsed_alert.json');
        filterDir = fullfile(stageDir, 'alert_filter');

        if ~isfile(parsedPath)
            continue;
        end

        for F = 1:numel(Args.Filters)
            filterName = char(Args.Filters(F));
            Row = emptyFilterRow();
            Row.StageRel = string(relativePath(ParentChar, stageDir));
            Row.Filter = string(filterName);

            Input = struct();
            Input.action = 'filter_lvk';
            Input.filter = filterName;
            Input.alert_file = parsedPath;
            Input.output_folder = filterDir;

            if filterName == "with_criteria"
                critPath = fullfile(filterDir, 'criteria_default.json');
                ensureDefaultCriteriaFile(critPath);
                Input.criteria_file = ultrasat.tools.pathForJson(critPath);
            end

            try
                Out = ultrasat.services.alerts_filter.processRequest(Input);
                Row.Status = string(Out.status);
                Row.Message = string(Out.message);
                Row.ResultFile = string(ultrasat.tools.pathForJson( ...
                    safeOutField(Out, 'result_file', '')));
                Row = fillRowFromResult(Row, Out);
            catch Ex
                Row.Status = "error";
                Row.Message = string(Ex.message);
            end

            Rows(end+1, 1) = Row; %#ok<AGROW>
            printFilterRow(K, numel(StageDirs), F, numel(Args.Filters), Row);
        end
    end

    printFilterSummary(Rows, Args.Filters);

    if Args.WriteSummary
        csvPath = fullfile(Parent, 'alert_filter_summary.csv');
        writeFilterSummaryCsv(csvPath, Rows);
        fprintf('Summary CSV: %s\n', csvPath);
    end

    fprintf('========== DEBUG RUN ALERT FILTERS ON SHARE DONE ==========\n\n');
end


function Parent = resolveParent(parentFolder)
    % Default GW experiment root on rwshare.

    if strlength(strtrim(parentFolder)) > 0
        Parent = char(strtrim(parentFolder));
    else
        Parent = fullfile(getRwshareRoot(), 'gw_coverage_experiment_2026_08');
    end

    if ~isfolder(Parent)
        error('debug_runAlertFiltersOnShare:ParentNotFound', 'Parent not found: %s', Parent);
    end
end


function Root = getRwshareRoot()
    if ispc
        Root = '\\socsrv\rwshare_data';
    else
        Root = '/data/rwshare_data';
    end
end


function StageDirs = findAlertStageDirs(parentFolder)
    % Stage folder = parent of alert_filter/request.json (one per alert product).

    D = dir(fullfile(parentFolder, '**', 'alert_filter', 'request.json'));
    Set = strings(0, 1);
    for K = 1:numel(D)
        Set(end+1) = string(fileparts(D(K).folder)); %#ok<AGROW>
    end
    Set = unique(Set);
    StageDirs = cell(numel(Set), 1);
    for K = 1:numel(Set)
        StageDirs{K} = char(Set(K));
    end
end


function ensureDefaultCriteriaFile(critPath)
    % Write default criteria once if missing (with_criteria batch runs).

    if isfile(critPath)
        return;
    end
    critDir = fileparts(critPath);
    if ~isfolder(critDir)
        mkdir(critDir);
    end
    crit = ultrasat.alerts_filters.lvk.models.LvkFilterCriteria();
    crit.score_min = 1.0;
    crit.terrestrial_max = 0.5;
    crit.far_max = 10.0;
    crit.saveToJsonFile(critPath);
end


function Row = fillRowFromResult(Row, Out)
    % Copy score and decision fields from processRequest output.

    Row.IsToo = false;
    Row.Decision = "";
    Row.Priority = "";
    Row.Score = NaN;

    if ~isfield(Out, 'result') || isempty(Out.result)
        return;
    end
    r = Out.result;
    if isfield(r, 'score')
        Row.Score = r.score;
    end
    if isfield(r, 'is_too')
        Row.IsToo = logical(r.is_too);
    end
    if isfield(r, 'decision')
        Row.Decision = string(r.decision);
    end
    if isfield(r, 'priority')
        Row.Priority = string(r.priority);
    end
    if isfield(r, 'class_probs') && isstruct(r.class_probs)
        if isfield(r.class_probs, 'bns')
            Row.ProbBns = r.class_probs.bns;
        end
        if isfield(r.class_probs, 'nsbh')
            Row.ProbNsbh = r.class_probs.nsbh;
        end
    end
    if isfield(r, 'flags') && isstruct(r.flags) && isfield(r.flags, 'has_skymap')
        Row.HasSkymap = logical(r.flags.has_skymap);
    end
end


function Row = emptyFilterRow()
    Row = struct( ...
        'StageRel', "", ...
        'Filter', "", ...
        'Status', "", ...
        'Message', "", ...
        'ResultFile', "", ...
        'IsToo', false, ...
        'Decision', "", ...
        'Priority', "", ...
        'Score', NaN, ...
        'ProbBns', NaN, ...
        'ProbNsbh', NaN, ...
        'HasSkymap', false);
end


function printFilterRow(stageIdx, nStages, filterIdx, nFilters, Row)
    fprintf('[%d/%d filter %d/%d] %s | %s | status=%s is_too=%d score=%.3f\n', ...
        stageIdx, nStages, filterIdx, nFilters, Row.StageRel, Row.Filter, ...
        Row.Status, Row.IsToo, Row.Score);
end


function printFilterSummary(Rows, Filters)
    fprintf('\n--- Summary ---\n');

    nRows = numel(Rows);
    filtersCol = strings(nRows, 1);
    statusCol = strings(nRows, 1);
    isTooCol = false(nRows, 1);
    for K = 1:nRows
        filtersCol(K) = string(Rows(K).Filter);
        statusCol(K) = string(Rows(K).Status);
        isTooCol(K) = logical(Rows(K).IsToo);
    end

    for F = 1:numel(Filters)
        name = Filters(F);
        mask = filtersCol == name;
        nOk = sum(mask & statusCol == "ok");
        nErr = sum(mask & statusCol ~= "ok");
        nToo = sum(mask & isTooCol);
        fprintf('Filter %s: ok=%d error=%d is_too=%d\n', name, nOk, nErr, nToo);
    end
end


function writeFilterSummaryCsv(csvPath, Rows)
    % Write one row per stage x filter for offline analysis.

    fid = fopen(csvPath, 'w');
    if fid < 0
        error('debug_runAlertFiltersOnShare:CsvWrite', 'Cannot write %s', csvPath);
    end
    fprintf(fid, 'stage_rel,filter,status,is_too,decision,priority,score,prob_bns,prob_nsbh,has_skymap,result_file,message\n');
    for K = 1:numel(Rows)
        R = Rows(K);
        fprintf(fid, '%s,%s,%s,%d,%s,%s,%s,%s,%s,%d,%s,%s\n', ...
            csvEscape(R.StageRel), csvEscape(R.Filter), csvEscape(R.Status), R.IsToo, ...
            csvEscape(R.Decision), csvEscape(R.Priority), numOrEmpty(R.Score), ...
            numOrEmpty(R.ProbBns), numOrEmpty(R.ProbNsbh), R.HasSkymap, ...
            csvEscape(R.ResultFile), csvEscape(R.Message));
    end
    fclose(fid);
end


function s = csvEscape(v)
    s = char(string(v));
    s = strrep(s, '"', '""');
    if contains(s, ',') || contains(s, '"')
        s = ['"', s, '"'];
    end
end


function s = numOrEmpty(x)
    if isnumeric(x) && isscalar(x) && isfinite(x)
        s = sprintf('%.6g', x);
    else
        s = '';
    end
end


function rel = relativePath(parent, target)
    parent = char(parent);
    target = char(target);
    if startsWith(lower(target), lower(parent))
        rel = target(numel(parent) + 1 : end);
        rel = strrep(rel, '\', '/');
        if startsWith(rel, '/')
            rel = rel(2:end);
        end
    else
        rel = target;
    end
end


function v = safeOutField(s, name, defaultVal)
    if isfield(s, name) && ~isempty(s.(name))
        v = s.(name);
    else
        v = defaultVal;
    end
end