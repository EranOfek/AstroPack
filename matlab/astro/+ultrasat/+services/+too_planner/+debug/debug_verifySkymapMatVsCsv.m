%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+too_planner/+debug/debug_verifySkymapMatVsCsv.m
% Author      : Chen Tishler
% Created     : 15/11/2026
% Updated     : 15/11/2026
% Description : Compare skymap.mat and skymap.csv under a GW share tree.
%
% Run:
%   ultrasat.services.too_planner.debug.debug_verifySkymapMatVsCsv()
%   ultrasat.services.too_planner.debug.debug_verifySkymapMatVsCsv( ...
%       ParentFolder="\\socsrv\rwshare_data\gw_coverage_experiment_2026_08")
%==========================================================================

function debug_verifySkymapMatVsCsv(Args)
    % Scan ParentFolder for skymap.mat / skymap.csv pairs and compare tables.

    arguments
        Args.ParentFolder (1,1) string = "\\socsrv\rwshare_data\gw_coverage_experiment_2026_08"
        Args.Tol (1,1) double = 1e-12
    end

    if nargin == 0
        Args.ParentFolder = "\\socsrv\rwshare_data\gw_coverage_experiment_2026_08";
        Args.Tol = 1e-12;
    end

    fprintf('\n========== VERIFY SKYMAP MAT vs CSV ==========\n');
    Parent = char(strtrim(Args.ParentFolder));
    if ~isfolder(Parent)
        error('debug_verifySkymapMatVsCsv:ParentNotFound', 'Folder not found: %s', Parent);
    end

    StageDirs = findStageDirsWithMaps(Parent);
    Rows = repmat(emptyVerifyRow(), 0, 1);

    for K = 1:numel(StageDirs)
        StageDir = StageDirs{K};
        Row = verifyOneStage(StageDir, Args.Tol);
        Rows(end+1, 1) = Row; %#ok<AGROW>
    end

    ReportPath = fullfile(Parent, 'skymap_mat_csv_verify.csv');
    writeVerifyReport(ReportPath, Rows);

    Statuses = {Rows.status};
    OkCount = sum(strcmp(Statuses, 'ok'));
    SkipCount = sum(strcmp(Statuses, 'skip'));
    BadMask = ~strcmp(Statuses, 'ok') & ~strcmp(Statuses, 'skip');
    BadRows = Rows(BadMask);
    MismatchCount = numel(BadRows);
    fprintf('Stages: %d; ok: %d; skip: %d; mismatch: %d\n', ...
        numel(Rows), OkCount, SkipCount, MismatchCount);
    fprintf('Report: %s\n', ReportPath);
    for J = 1:min(20, numel(BadRows))
        fprintf('  [%s] %s — %s\n', BadRows(J).status, BadRows(J).stage_dir, BadRows(J).message);
    end
    fprintf('========== VERIFY SKYMAP MAT vs CSV DONE ==========\n\n');
end

% -------------------------------------------------------------------------

function Rows = findStageDirsWithMaps(parentFolder)
    % Return stage folders: parent of each too_planner/request.json under the tree.

    Dreq = dir(fullfile(parentFolder, '**', 'too_planner', 'request.json'));
    Set = strings(0, 1);
    for K = 1:numel(Dreq)
        TooDir = string(Dreq(K).folder);
        StageDir = string(fileparts(TooDir));
        Set(end+1) = StageDir; %#ok<AGROW>
    end
    Set = unique(Set);
    Rows = cell(numel(Set), 1);
    for K = 1:numel(Set)
        Rows{K} = char(Set(K));
    end
end

% -------------------------------------------------------------------------

function Row = verifyOneStage(stageDir, tol)
    % Load CSV and MAT via the same path the planner uses; compare columns.

    Row = emptyVerifyRow();
    Row.stage_dir = stageDir;

    MatPath = fullfile(stageDir, 'skymap.mat');
    CsvPath = fullfile(stageDir, 'skymap.csv');
    HasMat = isfile(MatPath);
    HasCsv = isfile(CsvPath);

    if ~HasMat && ~HasCsv
        Row.status = 'skip';
        Row.message = 'no maps';
        return;
    end

    if HasCsv
        try
            Tcsv = readtable(CsvPath);
            Row.rows_csv = height(Tcsv);
        catch Ex
            Row.status = 'csv_read_error';
            Row.message = Ex.message;
            return;
        end
    end

    if HasMat
        try
            Tmat = ultrasat.tools.loadProbMap(MatPath);
            Row.rows_mat = height(Tmat);
        catch Ex
            Row.status = 'mat_read_error';
            Row.message = Ex.message;
            return;
        end
    end

    if HasCsv && ~HasMat
        Row.status = 'missing_mat';
        Row.message = 'csv only';
        return;
    end
    if HasMat && ~HasCsv
        Row.status = 'missing_csv';
        Row.message = 'mat only';
        return;
    end

    if Row.rows_csv ~= Row.rows_mat
        Row.status = 'row_mismatch';
        Row.message = sprintf('rows csv=%d mat=%d', Row.rows_csv, Row.rows_mat);
        return;
    end

    Req = {'UNIQ', 'PROBDENSITY', 'RA', 'DEC'};
    for R = 1:numel(Req)
        if ~ismember(Req{R}, Tcsv.Properties.VariableNames) || ...
                ~ismember(Req{R}, Tmat.Properties.VariableNames)
            Row.status = 'column_missing';
            Row.message = ['missing ' Req{R}];
            return;
        end
    end

    for R = 1:numel(Req)
        col = Req{R};
        if ~isa(Tmat.(col), 'double')
            Row.status = 'bad_class';
            Row.message = sprintf('MAT %s is %s, expected double (use loadProbMap)', ...
                col, class(Tmat.(col)));
            return;
        end
    end

    Ucsv = int64(Tcsv.UNIQ);
    Umat = int64(Tmat.UNIQ);
    if ~isequal(Ucsv, Umat)
        Row.status = 'uniq_mismatch';
        Row.message = 'UNIQ differs';
        return;
    end

    Dp = max(abs(Tcsv.PROBDENSITY - Tmat.PROBDENSITY));
    Dra = max(abs(Tcsv.RA - Tmat.RA));
    Ddec = max(abs(Tcsv.DEC - Tmat.DEC));
    Row.max_diff_prob = Dp;
    Row.max_diff_ra = Dra;
    Row.max_diff_dec = Ddec;
    Row.sum_prob_csv = sum(Tcsv.PROBDENSITY);
    Row.sum_prob_mat = sum(Tmat.PROBDENSITY);

    if Dp > tol || Dra > tol || Ddec > tol
        Row.status = 'value_mismatch';
        Row.message = sprintf('max diff p=%.3g ra=%.3g dec=%.3g', Dp, Dra, Ddec);
        return;
    end

    if abs(Row.sum_prob_csv - Row.sum_prob_mat) > max(tol, 1e-12 * max(abs(Row.sum_prob_csv), 1))
        Row.status = 'sum_mismatch';
        Row.message = sprintf('sum csv=%.6g mat=%.6g', Row.sum_prob_csv, Row.sum_prob_mat);
        return;
    end

    Row.status = 'ok';
    Row.message = '';
end

% -------------------------------------------------------------------------

function Row = emptyVerifyRow()
    Row = struct( ...
        'stage_dir', '', ...
        'rows_csv', 0, ...
        'rows_mat', 0, ...
        'max_diff_prob', 0, ...
        'max_diff_ra', 0, ...
        'max_diff_dec', 0, ...
        'sum_prob_csv', 0, ...
        'sum_prob_mat', 0, ...
        'status', '', ...
        'message', '');
end

% -------------------------------------------------------------------------

function writeVerifyReport(reportPath, rows)
    % Write verification CSV for Excel.

    Names = {'stage_dir', 'rows_csv', 'rows_mat', 'max_diff_prob', 'max_diff_ra', ...
        'max_diff_dec', 'sum_prob_csv', 'sum_prob_mat', 'status', 'message'};
    T = struct2table(rows);
    T = T(:, Names);
    writetable(T, reportPath);
end