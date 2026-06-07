%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.scan_Lcs2029_v3_vs_cpsat.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 07/06/2026
% Description : Scan all 2029 LCS start dates with LcsHelper_v3 and CP-SAT,
%               write per-plan outputs, and compare feasibility indices.
%
% Run by:
%   ultrasat.planner.debug.scan_Lcs2029_v3_vs_cpsat()
%
% Useful shorter test:
%   ultrasat.planner.debug.scan_Lcs2029_v3_vs_cpsat( ...
%       'ScanStart', datetime(2029,1,1), ...
%       'ScanEnd', datetime(2029,1,7));
%==========================================================================

function scan_Lcs2029_v3_vs_cpsat(Args)
    arguments
        Args.ScanStart datetime = datetime(2029, 1, 1)
        Args.ScanEnd   datetime = datetime(2029, 12, 31)
        Args.OutputDir char = ''
        Args.GridFile char = ''
        Args.RunV3 logical = true
        Args.RunCpSat logical = true
        Args.PrepareCpSatInputs logical = true
        Args.Compare logical = true
        Args.CpSatTimeLimit double = 30
        Args.ContinueOnError logical = true
    end

    ThisDir = fileparts(mfilename('fullpath'));
    PlannerDir = fullfile(ThisDir, '..');

    if isempty(Args.OutputDir)
        Args.OutputDir = fullfile(ThisDir, 'lcs_2029_scan');
    end
    if isempty(Args.GridFile)
        Args.GridFile = defaultGridFile(PlannerDir);
    end

    ensureDir(Args.OutputDir);

    fprintf('========== LCS 2029 V3 vs CP-SAT scan ==========\n');
    fprintf('Scan range : %s .. %s\n', isoDate(Args.ScanStart), isoDate(Args.ScanEnd));
    fprintf('Output dir : %s\n', Args.OutputDir);
    fprintf('Grid file  : %s\n', Args.GridFile);

    V3Dir = fullfile(Args.OutputDir, 'v3');
    CpDir = fullfile(Args.OutputDir, 'cpsat');
    InputDir = fullfile(Args.OutputDir, 'cpsat_inputs');

    if Args.RunV3
        scanV3(Args.ScanStart, Args.ScanEnd, Args.GridFile, V3Dir, Args.ContinueOnError);
    end

    if Args.RunCpSat
        scanCpSat(Args.ScanStart, Args.ScanEnd, InputDir, CpDir, ...
            Args.PrepareCpSatInputs, Args.CpSatTimeLimit);
    end

    if Args.Compare
        compareScans(V3Dir, CpDir, Args.OutputDir);
    end

    fprintf('========== LCS 2029 V3 vs CP-SAT scan DONE ==========\n');
end


function scanV3(ScanStart, ScanEnd, GridFile, OutputDir, ContinueOnError)
    fprintf('\n--- V3 scan ---\n');
    ensureDir(OutputDir);
    Dates = scanDates(ScanStart, ScanEnd);
    Rows = repmat(emptyIndexRow(), numel(Dates), 1);

    for I = 1:numel(Dates)
        D = Dates(I);
        Stamp = dateStamp(D);
        fprintf('[V3 %3d/%3d] %s\n', I, numel(Dates), isoDate(D));

        try
            Obj = ultrasat.planner.LcsHelper_v3( ...
                'StartDate', D, ...
                'AllSkyTable', GridFile, ...
                'Verbose', false, ...
                'prep_before_schedule', true, ...
                'build_the_schedule', true);

            Summary = summarizeV3(Obj);
            IsFeasible = isV3Feasible(Obj, Summary);

            Rows(I).plan_start_date = isoDate(D);
            Rows(I).status = ternary(IsFeasible, 'FEASIBLE', 'INFEASIBLE');
            Rows(I).num_observations = Summary.num_observations;
            Rows(I).detail = sprintf('A=%d B=%d C=%d D=%d SetC_start_ind=%d', ...
                Summary.nA, Summary.nB_fields, Summary.nC, Summary.nD, Summary.set_c_start_ind);

            if IsFeasible
                DateDir = fullfile(OutputDir, isoDate(D));
                PlanDir = fullfile(DateDir, 'success');
                ensureDir(PlanDir);
                dumpV3Plan(Obj, D, PlanDir);
                Rows(I).plan_file = sprintf('lcs_plan_%s.csv', Stamp);
                Rows(I).plan_dir = fullfile(isoDate(D), 'success');
                copyfile(fullfile(PlanDir, Rows(I).plan_file), ...
                    fullfile(OutputDir, Rows(I).plan_file));
            end
        catch ME
            Rows(I).plan_start_date = isoDate(D);
            Rows(I).status = 'ERROR';
            Rows(I).detail = ME.message;
            if ~ContinueOnError
                rethrow(ME);
            end
        end
    end

    Index = struct2table(Rows, 'AsArray', true);
    writetable(Index, fullfile(OutputDir, 'lcs_plan_index.csv'));
    fprintf('V3 index written: %s\n', fullfile(OutputDir, 'lcs_plan_index.csv'));
end


function scanCpSat(ScanStart, ScanEnd, InputDir, OutputDir, PrepareInputs, TimeLimit)
    fprintf('\n--- CP-SAT scan ---\n');
    ensureDir(OutputDir);
    ensureDir(InputDir);

    PlannerDir = fullfile(fileparts(mfilename('fullpath')), '..');
    SolverDir = fullfile(PlannerDir, 'lcs_solver');
    PythonExe = fullfile(SolverDir, '.venv', 'Scripts', 'python.exe');

    if ~isfile(PythonExe)
        error('scanCpSat: python executable not found: %s', PythonExe);
    end

    if PrepareInputs
        CpEnd = ScanEnd + days(420);
        fprintf('Preparing CP-SAT inputs: %s .. %s\n', isoDate(ScanStart), isoDate(CpEnd));
        ultrasat.planner.prepareLcsSolverInputs( ...
            'StartDate', ScanStart, ...
            'EndDate', CpEnd, ...
            'OutputDir', InputDir, ...
            'LoadCache', false, ...
            'SaveCache', true);
    end

    Fields = fullfile(InputDir, 'lcs_fields.csv');
    Windows = fullfile(InputDir, 'lcs_visibility_windows.csv');
    Windows1Gap = fullfile(InputDir, 'lcs_visibility_windows_1dgap.csv');
    Elig = fullfile(InputDir, 'lcs_field_eligibility.csv');
    Config = fullfile(InputDir, 'lcs_params.json');

    Cmd = sprintf(['cd /d "%s" && "%s" -m lcs_cpsat.scan_cli ', ...
        '--scan-start %s --scan-end %s --time-limit %.0f ', ...
        '--fields "%s" --windows "%s" --windows-1dgap "%s" ', ...
        '--elig "%s" --config "%s" --out "%s" --write-full-outputs'], ...
        SolverDir, PythonExe, isoDate(ScanStart), isoDate(ScanEnd), TimeLimit, ...
        Fields, Windows, Windows1Gap, Elig, Config, OutputDir);

    fprintf('Running CP-SAT scanner...\n');
    [Status, Text] = system(Cmd);
    fprintf('%s\n', Text);
    if Status ~= 0
        error('scanCpSat: CP-SAT scanner failed with status %d', Status);
    end
end


function compareScans(V3Dir, CpDir, OutputDir)
    fprintf('\n--- Compare scans ---\n');
    V3IndexPath = fullfile(V3Dir, 'lcs_plan_index.csv');
    CpIndexPath = fullfile(CpDir, 'lcs_plan_index.csv');

    if ~isfile(V3IndexPath)
        error('compareScans: missing V3 index: %s', V3IndexPath);
    end
    if ~isfile(CpIndexPath)
        error('compareScans: missing CP-SAT index: %s', CpIndexPath);
    end

    V3 = readtable(V3IndexPath, 'TextType', 'string');
    CP = readtable(CpIndexPath, 'TextType', 'string');

    Dates = unique([V3.plan_start_date; CP.plan_start_date]);
    Rows = repmat(emptyCompareRow(), numel(Dates), 1);

    for I = 1:numel(Dates)
        D = Dates(I);
        Iv3 = find(V3.plan_start_date == D, 1);
        Icp = find(CP.plan_start_date == D, 1);

        Rows(I).plan_start_date = char(D);
        if ~isempty(Iv3)
            Rows(I).v3_status = char(V3.status(Iv3));
            Rows(I).v3_feasible = isFeasibleStatus(V3.status(Iv3));
            Rows(I).v3_num_observations = V3.num_observations(Iv3);
            Rows(I).v3_detail = char(V3.detail(Iv3));
        end
        if ~isempty(Icp)
            Rows(I).cpsat_status = char(CP.status(Icp));
            Rows(I).cpsat_feasible = isFeasibleStatus(CP.status(Icp));
            Rows(I).cpsat_num_observations = CP.num_observations(Icp);
            Rows(I).cpsat_detail = char(CP.detail(Icp));
        end

        Rows(I).compatible = Rows(I).v3_feasible == Rows(I).cpsat_feasible;
        if Rows(I).compatible && Rows(I).v3_feasible
            Rows(I).compatible = Rows(I).v3_num_observations == Rows(I).cpsat_num_observations;
        end
        if Rows(I).v3_feasible && Rows(I).cpsat_feasible
            Rows(I).relation = ternary(Rows(I).compatible, 'match_feasible', 'both_feasible_count_mismatch');
        elseif ~Rows(I).v3_feasible && ~Rows(I).cpsat_feasible
            Rows(I).relation = 'match_infeasible';
        elseif Rows(I).v3_feasible
            Rows(I).relation = 'v3_only';
        else
            Rows(I).relation = 'cpsat_only';
        end
    end

    Compare = struct2table(Rows, 'AsArray', true);
    ComparePath = fullfile(OutputDir, 'v3_vs_cpsat_comparison.csv');
    writetable(Compare, ComparePath);

    Nbad = sum(~Compare.compatible);
    Nv3 = sum(Compare.v3_feasible);
    Ncp = sum(Compare.cpsat_feasible);
    fprintf('V3 feasible    : %d\n', Nv3);
    fprintf('CP-SAT feasible: %d\n', Ncp);
    fprintf('Mismatches     : %d\n', Nbad);
    fprintf('Comparison CSV : %s\n', ComparePath);
end


function dumpV3Plan(Obj, StartDate, PlanDir)
    Schedule = Obj.Schedule;
    if ~isempty(Schedule)
        Schedule.start_date = cellstr(datestr(StartDate + days(Schedule.start - 1), 'yyyy-mm-dd'));
        Schedule.end_date = cellstr(datestr(StartDate + days(Schedule.end - 1), 'yyyy-mm-dd'));
    end
    writetable(Schedule, fullfile(PlanDir, 'schedule.csv'));

    FullWindows = Obj.Full_windows;
    FullWindows.start_date = cellstr(datestr(StartDate + days(FullWindows.start - 1), 'yyyy-mm-dd'));
    FullWindows.end_date = cellstr(datestr(StartDate + days(FullWindows.end - 1), 'yyyy-mm-dd'));
    writetable(FullWindows, fullfile(PlanDir, 'full_windows.csv'));

    DailyPath = fullfile(PlanDir, 'daily_schedule.csv');
    writeDailyScheduleMatrix(Obj, StartDate, DailyPath);

    Stamp = dateStamp(StartDate);
    PlanCsv = fullfile(PlanDir, sprintf('lcs_plan_%s.csv', Stamp));
    writeObservationList(Obj, StartDate, PlanCsv);

    Summary = summarizeV3(Obj);
    Summary.start_date = isoDate(StartDate);
    writeJson(fullfile(PlanDir, 'summary.json'), Summary);
end


function writeDailyScheduleMatrix(Obj, StartDate, CsvPath)
    M = Obj.Daily_schedule;
    Ndays = size(M, 1);
    Nslots = size(M, 2);
    SlotNames = arrayfun(@(S) sprintf('slot_%d', S), 1:Nslots, 'UniformOutput', false);
    DayNums = (Obj.First_day : Obj.First_day + Ndays - 1)';
    DateStrs = cellstr(datestr(StartDate + days(DayNums - 1), 'yyyy-mm-dd'));
    T = array2table(M, 'VariableNames', SlotNames);
    T = [table(DayNums, DateStrs, 'VariableNames', {'day', 'date'}), T];
    writetable(T, CsvPath);
end


function writeObservationList(Obj, StartDate, CsvPath)
    M = Obj.Daily_schedule;
    ObsDatetime = {};
    FieldId = [];

    for D = 1:size(M, 1)
        for S = 1:size(M, 2)
            F = M(D, S);
            if isnan(F)
                continue;
            end
            T = StartDate + days(D - 1) + Obj.DailyWindowStartTime + days((S - 1) * Obj.SlotTime);
            ObsDatetime{end+1, 1} = datestr(T, 'yyyy-mm-ddTHH:MM:SS'); %#ok<AGROW>
            FieldId(end+1, 1) = F; %#ok<AGROW>
        end
    end

    Tobs = table(ObsDatetime, FieldId, 'VariableNames', {'obs_datetime', 'field_id'});
    writetable(Tobs, CsvPath);
end


function Summary = summarizeV3(Obj)
    MaskA = strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0;
    MaskB = ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0;
    MaskC = strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0;
    MaskD = strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0;

    Summary = struct();
    Summary.nA = sum(MaskA);
    Summary.nB_rows = sum(MaskB);
    Summary.nB_fields = numel(unique(Obj.Schedule.Field(MaskB)));
    Summary.nC = sum(MaskC);
    Summary.nD = sum(MaskD);
    Summary.set_c_start_ind = Obj.SetC_start_ind;
    Summary.num_observations = sum(~isnan(Obj.Daily_schedule(:)));
    Summary.daily_schedule_rows = size(Obj.Daily_schedule, 1);
    Summary.daily_schedule_slots = size(Obj.Daily_schedule, 2);
end


function IsOk = isV3Feasible(Obj, Summary)
    IsOk = ~isempty(Obj.Schedule) && ~isempty(Obj.Daily_schedule) && ...
        Summary.nA == Obj.SetAnumel && ...
        Summary.nB_rows == 3 * Obj.SetBnumel && ...
        Summary.nB_fields == Obj.SetBnumel && ...
        Summary.nC == Obj.SetCnumel && ...
        Summary.nD <= Obj.SetDnumel;
end


function Dates = scanDates(ScanStart, ScanEnd)
    N = days(ScanEnd - ScanStart) + 1;
    Dates = ScanStart + days(0:(N - 1));
end


function Row = emptyIndexRow()
    Row = struct( ...
        'plan_start_date', '', ...
        'status', '', ...
        'plan_file', '', ...
        'plan_dir', '', ...
        'num_observations', 0, ...
        'detail', '');
end


function Row = emptyCompareRow()
    Row = struct( ...
        'plan_start_date', '', ...
        'v3_status', '', ...
        'cpsat_status', '', ...
        'v3_feasible', false, ...
        'cpsat_feasible', false, ...
        'v3_num_observations', 0, ...
        'cpsat_num_observations', 0, ...
        'compatible', false, ...
        'relation', '', ...
        'v3_detail', '', ...
        'cpsat_detail', '');
end


function Result = isFeasibleStatus(Status)
    Status = upper(string(Status));
    Result = Status == "FEASIBLE" || Status == "OPTIMAL";
end


function ensureDir(PathName)
    if ~isfolder(PathName)
        mkdir(PathName);
    end
end


function S = isoDate(D)
    S = datestr(D, 'yyyy-mm-dd');
end


function S = dateStamp(D)
    S = datestr(D, 'yyyymmdd');
end


function Value = ternary(Cond, A, B)
    if Cond
        Value = A;
    else
        Value = B;
    end
end


function writeJson(PathName, Data)
    Fid = fopen(PathName, 'w');
    if Fid < 0
        error('writeJson: cannot open file: %s', PathName);
    end
    Cleaner = onCleanup(@() fclose(Fid));
    fprintf(Fid, '%s\n', jsonencode(Data, 'PrettyPrint', true));
    delete(Cleaner);
end


function GridFile = defaultGridFile(PlannerDir)
    LocalGrid = fullfile(PlannerDir, 'data', 'LCS_nonoverlapping_grid_surveys.csv');
    if isfile(LocalGrid)
        GridFile = LocalGrid;
        return;
    end

    EnvPath = getenv('ASTROPACK_DATA_PATH');
    if ~isempty(EnvPath)
        EnvGrid = fullfile(EnvPath, 'ULTRASAT', 'LCS_nonoverlapping_grid_surveys.csv');
        if isfile(EnvGrid)
            GridFile = EnvGrid;
            return;
        end
    end

    error('defaultGridFile: LCS grid file not found');
end
