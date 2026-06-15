%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsHelper_v4_validateScanOutputs.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 10/06/2026
% Description : Validate all LCS v4 scan CSV output folders for one year.
%               Reads existing scan outputs — does not rebuild plans.
%               Per-date reports are written inside each yyyy-mm-dd/success
%               folder, and a yearly index is written in the scan root.
%
% Usage:
%   ultrasat.planner.LcsHelper_v4_validateScanOutputs('Year', 2029, 'ScanDir', '<path>')
%   ultrasat.planner.LcsHelper_v4_validateScanOutputs('Year', 2029)
%==========================================================================

function LcsHelper_v4_validateScanOutputs(varargin)

    P = inputParser;
    addParameter(P, 'Year', 2029);
    addParameter(P, 'ScanDir', '');
    parse(P, varargin{:});

    ScanDir = string(P.Results.ScanDir);
    if strlength(ScanDir) == 0
        ThisDir = fileparts(mfilename('fullpath'));
        ScanDir = string(fullfile(ThisDir, 'output', 'scans', num2str(P.Results.Year)));
    end

    fprintf('LcsHelper_v4_validateScanOutputs\n');
    fprintf('ScanDir: %s\n', ScanDir);

    IndexPath = fullfile(ScanDir, 'lcs_plan_index.csv');
    if ~isfile(IndexPath)
        error('Missing scan index: %s', IndexPath);
    end
    Index = readtable(IndexPath, 'TextType', 'string', 'VariableNamingRule', 'preserve');
    Feasible = Index(ismember(upper(string(Index.status)), ["FEASIBLE", "OPTIMAL"]), :);

    Rows = table('Size', [0 7], ...
        'VariableTypes', {'string','logical','double','double','string','string','string'}, ...
        'VariableNames', {'plan_start_date','passed','failed_count','warning_count','status','report_file','detail'});

    for I = 1:height(Feasible)
        PlanDate = string(Feasible.plan_start_date(I));
        SuccessDir = fullfile(ScanDir, PlanDate, 'success');
        fprintf('[%3d/%3d] %s\n', I, height(Feasible), PlanDate);

        [Passed, FailedCount, WarningCount, Lines] = validate_one_folder(SuccessDir);
        ReportPath = fullfile(SuccessDir, 'matlab_validation_report.txt');
        CsvPath = fullfile(SuccessDir, 'matlab_validation_report.csv');
        write_lines(ReportPath, Lines);
        write_report_csv(CsvPath, Lines);

        Detail = "ok";
        if ~Passed
            Detail = first_fail_detail(Lines);
        end
        Rows(end+1, :) = {PlanDate, Passed, FailedCount, WarningCount, string(Feasible.status(I)), string(ReportPath), Detail}; %#ok<AGROW>
    end

    OutPath = fullfile(ScanDir, 'matlab_validation_index.csv');
    writetable(Rows, OutPath);

    fprintf('MATLAB validation index: %s\n', OutPath);
    fprintf('Folders validated: %d\n', height(Rows));
    fprintf('Passed: %d\n', sum(Rows.passed));
    fprintf('Failed: %d\n', sum(~Rows.passed));
end


function [Passed, FailedCount, WarningCount, Lines] = validate_one_folder(Folder)
    Checks = {};
    Warnings = {};

    SchedulePath = fullfile(Folder, 'schedule.csv');
    WindowsPath = fullfile(Folder, 'full_windows.csv');
    DailyPath = fullfile(Folder, 'daily_schedule.csv');

    add_check('Required CSV files exist', all(isfile([SchedulePath, WindowsPath, DailyPath])), Folder);
    if ~isfile(SchedulePath) || ~isfile(WindowsPath) || ~isfile(DailyPath)
        [Passed, FailedCount, WarningCount, Lines] = finish_report(Checks, Warnings);
        return
    end

    Schedule = readtable(SchedulePath, 'TextType', 'string', 'VariableNamingRule', 'preserve');
    Windows = readtable(WindowsPath, 'TextType', 'string', 'VariableNamingRule', 'preserve');
    Daily = readtable(DailyPath, 'TextType', 'string', 'VariableNamingRule', 'preserve');
    RequiredSchedule = ["category","group","ind","start","end","Field"];
    RequiredWindows = ["start","end"];
    RequiredDaily = ["day"];

    add_check('Schedule required columns', all(ismember(RequiredSchedule, string(Schedule.Properties.VariableNames))), '');
    add_check('Full_windows required columns', all(ismember(RequiredWindows, string(Windows.Properties.VariableNames))), '');
    add_check('Daily_schedule required columns', all(ismember(RequiredDaily, string(Daily.Properties.VariableNames))), '');
    if any(~ismember(RequiredSchedule, string(Schedule.Properties.VariableNames))) || ...
            any(~ismember(RequiredWindows, string(Windows.Properties.VariableNames))) || ...
            any(~ismember(RequiredDaily, string(Daily.Properties.VariableNames)))
        [Passed, FailedCount, WarningCount, Lines] = finish_report(Checks, Warnings);
        return
    end

    Placed = Schedule(Schedule.Field > 0, :);
    Cat = string(Placed.category);
    A = Placed(Cat == "A", :);
    B = Placed(ismember(Cat, ["B_45", "B_90"]), :);
    B45 = Placed(Cat == "B_45", :); %#ok<NASGU>
    B90 = Placed(Cat == "B_90", :); %#ok<NASGU>
    C = Placed(Cat == "C", :);
    D = Placed(Cat == "D", :);

    add_check('Schedule has placed rows', height(Placed) > 0, sprintf('placed=%d', height(Placed)));
    add_check('Schedule categories valid', all(ismember(Cat, ["A","B_45","B_90","C","D"])), '');
    add_check('SetA field count', height(A) == 48, sprintf('got=%d', height(A)));
    add_check('SetB row count', height(B) == 48, sprintf('got=%d', height(B)));
    add_check('SetB unique field count', numel(unique(B.Field)) == 16, sprintf('got=%d', numel(unique(B.Field))));
    add_check('SetC field count', height(C) == 16, sprintf('got=%d', height(C)));
    add_check('SetD field count <= 4', height(D) <= 4, sprintf('got=%d', height(D)));

    add_check('SetA unique fields', numel(unique(A.Field)) == height(A), duplicate_detail(A));
    add_check('SetA 45-day windows', all(win_len(A) == 45), bad_window_detail(A, 45));
    add_check('SetA original groups <= 8', all_group_counts_at_most(A, 1:6, 8), group_counts_detail(A, 1:6));
    MovedA = A(A.group > 6, :);
    if ~isempty(MovedA)
        add_check('SetA moved rows unique group/ind slots', unique_pair_count(MovedA.group, MovedA.ind) == height(MovedA), '');
    end

    add_check('SetB rows are 45 days', all(win_len(B) == 45), bad_window_detail(B, 45));
    add_check('SetB rows align with full windows', rows_align_with_windows(B, Windows), '');
    add_check('SetB per field pattern', setb_pattern_ok(B), '');

    add_check('SetC unique fields', numel(unique(C.Field)) == height(C), duplicate_detail(C));
    add_check('SetC rows are 135 days', all(win_len(C) == 135), bad_window_detail(C, 135));
    add_check('SetC groups are v4 block groups', all(C.group >= 11 & C.group <= 16), '');
    add_check('SetC starts align with full windows', all(ismember(C.start, Windows.start)), '');
    add_check('SetC ind range 1..8', all(C.ind >= 1 & C.ind <= 8), '');

    if ~isempty(D)
        add_check('SetD unique fields', numel(unique(D.Field)) == height(D), duplicate_detail(D));
        add_check('SetD rows are 45 days', all(win_len(D) == 45), bad_window_detail(D, 45));
        add_check('SetD group encoding', all(D.group >= 301 & D.group <= 304), '');
        add_check('SetD unique group slots', numel(unique(D.group)) == height(D), '');
    end

    [nCadence4, FilledABC, FilledWithD] = slot_usage(Placed, Windows);
    add_check('Slot budget nCadence4 divisible by 4', all(mod(nCadence4, 4) == 0), mat2str(nCadence4));
    add_check('Slot budget filledABC <= 11', all(FilledABC <= 11), mat2str(FilledABC));
    add_check('Final slot use filledABC+nD <= 11', all(FilledWithD <= 11), mat2str(FilledWithD));

    add_check('Window bounds start', min(Placed.start) >= min(Daily.day), '');
    add_check('Window bounds end', max(Placed.end) <= max(Daily.day), '');
    add_check('No cross-set duplicates', no_cross_set_duplicates(A, B, C, D), '');
    add_check('Daily schedule row count', height(Daily) > 0, sprintf('rows=%d', height(Daily)));
    SlotCols = startsWith(string(Daily.Properties.VariableNames), 'slot_');
    add_check('Daily schedule slot count', sum(SlotCols) == 11, sprintf('slots=%d', sum(SlotCols)));
    [BadDays, FirstBad] = daily_mismatch_count(Placed, Daily);
    add_check('Daily schedule matches schedule rows and cadence', BadDays == 0, sprintf('bad_days=%d first_bad=%s', BadDays, first_bad_text(FirstBad)));

    add_warning('Long-field extinction ranking', false, 'not available from scan CSV output');
    add_warning('SetD ranking selected order', setd_rank_warning_ok(D), '');

    [Passed, FailedCount, WarningCount, Lines] = finish_report(Checks, Warnings);

    function add_check(Name, Ok, Detail)
        Checks(end+1, :) = {string(Name), logical(Ok), string(Detail)}; %#ok<AGROW>
    end

    function add_warning(Name, Ok, Detail)
        Warnings(end+1, :) = {string(Name), logical(Ok), string(Detail)}; %#ok<AGROW>
    end
end


function [Passed, FailedCount, WarningCount, Lines] = finish_report(Checks, Warnings)
    if isempty(Checks)
        FailedCount = 0;
        PassedCount = 0;
    else
        Ok = cell2mat(Checks(:, 2));
        FailedCount = sum(~Ok);
        PassedCount = sum(Ok);
    end
    if isempty(Warnings)
        WarningCount = 0;
    else
        WarningCount = sum(~cell2mat(Warnings(:, 2)));
    end
    Passed = FailedCount == 0;

    Lines = {
        'LcsHelper_v4_validateScanOutputs report'
        sprintf('checks passed: %d', PassedCount)
        sprintf('checks failed: %d', FailedCount)
        sprintf('warnings: %d', WarningCount)
        ''
    };
    for I = 1:size(Checks, 1)
        Status = 'PASS';
        if ~Checks{I, 2}
            Status = 'FAIL';
        end
        Lines{end+1, 1} = sprintf('[%s] %s: %s', Status, Checks{I, 1}, Checks{I, 3}); %#ok<AGROW>
    end
    if ~isempty(Warnings)
        Lines{end+1, 1} = ''; %#ok<AGROW>
        for I = 1:size(Warnings, 1)
            Status = 'WARN-OK';
            if ~Warnings{I, 2}
                Status = 'WARN';
            end
            Lines{end+1, 1} = sprintf('[%s] %s: %s', Status, Warnings{I, 1}, Warnings{I, 3}); %#ok<AGROW>
        end
    end
end


function L = win_len(T)
    L = T.end - T.start + 1;
end


function S = duplicate_detail(T)
    if isempty(T)
        S = "";
        return
    end
    [U, ~, IC] = unique(T.Field);
    Counts = accumarray(IC, 1);
    S = "duplicates=" + mat2str(U(Counts > 1)');
end


function S = bad_window_detail(T, Expected)
    if isempty(T)
        S = "empty";
    else
        S = sprintf('bad_rows=%d', sum(win_len(T) ~= Expected));
    end
end


function Ok = all_group_counts_at_most(T, Groups, Limit)
    Ok = true;
    for G = Groups
        Ok = Ok && sum(T.group == G) <= Limit;
    end
end


function S = group_counts_detail(T, Groups)
    Counts = zeros(1, numel(Groups));
    for I = 1:numel(Groups)
        Counts(I) = sum(T.group == Groups(I));
    end
    S = mat2str(Counts);
end


function N = unique_pair_count(A, B)
    N = height(unique(table(A, B)));
end


function Ok = rows_align_with_windows(T, Windows)
    Ok = true;
    for I = 1:height(T)
        Ok = Ok && any(Windows.start == T.start(I) & Windows.end == T.end(I));
    end
end


function Ok = setb_pattern_ok(B)
    Ok = true;
    Fields = unique(B.Field);
    for I = 1:numel(Fields)
        Rows = B(B.Field == Fields(I), :);
        Cat = string(Rows.category);
        Fw = Rows.group;
        Fw(Cat == "B_45") = Fw(Cat == "B_45") - 100;
        Fw(Cat == "B_90") = Fw(Cat == "B_90") - 200;
        Span = max(Rows.start) - min(Rows.start) + 45;
        Ok = Ok && sum(Cat == "B_45") == 1 && sum(Cat == "B_90") == 2 && ...
            numel(unique(Fw)) == 3 && Span == 135;
    end
end


function [nCadence4, FilledABC, FilledWithD] = slot_usage(Schedule, Windows)
    N = height(Windows);
    nA = zeros(1, N);
    nB45 = zeros(1, N);
    nB90 = zeros(1, N);
    nC = zeros(1, N);
    nD = zeros(1, N);
    Cat = string(Schedule.category);
    for I = 1:height(Schedule)
        if Cat(I) == "A" && Schedule.ind(I) >= 1 && Schedule.ind(I) <= N
            nA(Schedule.ind(I)) = nA(Schedule.ind(I)) + 1;
        elseif Cat(I) == "B_45"
            K = Schedule.group(I) - 100;
            if K >= 1 && K <= N
                nB45(K) = nB45(K) + 1;
            end
        elseif Cat(I) == "B_90"
            K = Schedule.group(I) - 200;
            if K >= 1 && K <= N
                nB90(K) = nB90(K) + 1;
            end
        elseif Cat(I) == "C"
            K = find(Windows.start == Schedule.start(I), 1);
            if ~isempty(K)
                for J = K:min(K+2, N)
                    nC(J) = nC(J) + 1;
                end
            end
        elseif Cat(I) == "D" && Schedule.ind(I) >= 1 && Schedule.ind(I) <= N
            nD(Schedule.ind(I)) = nD(Schedule.ind(I)) + 1;
        end
    end
    nCadence4 = nB90 + nC;
    FilledABC = nA + nB45 + nCadence4 / 4;
    FilledWithD = FilledABC + nD;
end


function Ok = no_cross_set_duplicates(A, B, C, D)
    Sets = {unique(A.Field), unique(B.Field), unique(C.Field), unique(D.Field)};
    Ok = true;
    for I = 1:numel(Sets)
        for J = I+1:numel(Sets)
            Ok = Ok && isempty(intersect(Sets{I}, Sets{J}));
        end
    end
end


function [BadDays, FirstBad] = daily_mismatch_count(Schedule, Daily)
    SlotNames = string(Daily.Properties.VariableNames(startsWith(string(Daily.Properties.VariableNames), 'slot_')));
    FirstDay = min(Daily.day);
    LastDay = max(Daily.day);
    Expected = containers.Map('KeyType', 'double', 'ValueType', 'any');
    for D = FirstDay:LastDay
        Expected(D) = [];
    end
    Cat = string(Schedule.category);
    for I = 1:height(Schedule)
        for D = Schedule.start(I):Schedule.end(I)
            if D < FirstDay || D > LastDay
                continue
            end
            if (Cat(I) == "C" || Cat(I) == "B_90") && mod(D - Schedule.start(I) + 1, 4) ~= mod(Schedule.ind(I), 4)
                continue
            end
            V = Expected(D);
            V(end+1) = Schedule.Field(I); %#ok<AGROW>
            Expected(D) = V;
        end
    end

    BadDays = 0;
    FirstBad = NaN;
    for I = 1:height(Daily)
        D = Daily.day(I);
        Actual = [];
        for S = SlotNames
            V = Daily.(S)(I);
            if ~ismissing(V) && strlength(string(V)) > 0 && string(V) ~= "NaN" && string(V) ~= "nan"
                Actual(end+1) = str2double(string(V)); %#ok<AGROW>
            end
        end
        Exp = Expected(D);
        if ~isequal(sort(Actual), sort(Exp))
            BadDays = BadDays + 1;
            if isnan(FirstBad)
                FirstBad = D;
            end
        end
    end
end


function Text = first_bad_text(FirstBad)
    if isnan(FirstBad)
        Text = 'none';
    else
        Text = num2str(FirstBad);
    end
end


function Ok = setd_rank_warning_ok(D)
    Rank = [79 12 48 28 16 88 55 32 213 26];
    if isempty(D)
        Ok = true;
        return
    end
    D = sortrows(D, 'group');
    Pos = zeros(1, height(D));
    for I = 1:height(D)
        P = find(Rank == D.Field(I), 1);
        if isempty(P)
            Ok = false;
            return
        end
        Pos(I) = P;
    end
    Ok = all(diff(Pos) >= 0) && max(Pos) <= height(D);
end


function write_lines(Path, Lines)
    Fid = fopen(Path, 'w');
    Cleaner = onCleanup(@() fclose(Fid));
    for I = 1:numel(Lines)
        fprintf(Fid, '%s\n', Lines{I});
    end
    clear Cleaner
end


function write_report_csv(Path, Lines)
    Names = strings(0, 1);
    Statuses = strings(0, 1);
    Details = strings(0, 1);
    for I = 1:numel(Lines)
        Line = string(Lines{I});
        if startsWith(Line, "[")
            Tok = regexp(Line, '^\[(.*?)\]\s*(.*?):\s*(.*)$', 'tokens', 'once');
            if ~isempty(Tok)
                Statuses(end+1, 1) = string(Tok{1}); %#ok<AGROW>
                Names(end+1, 1) = string(Tok{2}); %#ok<AGROW>
                Details(end+1, 1) = string(Tok{3}); %#ok<AGROW>
            end
        end
    end
    T = table(Statuses, Names, Details, 'VariableNames', {'status','check','detail'});
    writetable(T, Path);
end


function Detail = first_fail_detail(Lines)
    Detail = "";
    for I = 1:numel(Lines)
        Line = string(Lines{I});
        if startsWith(Line, "[FAIL]")
            Detail = Line;
            return
        end
    end
end
