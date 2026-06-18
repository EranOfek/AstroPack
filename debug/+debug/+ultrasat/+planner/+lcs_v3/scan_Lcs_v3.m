%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v3/scan_Lcs_v3.m
% Author      : Chen Tishler
% Created     : 08/06/2026
% Updated     : 08/06/2026
% Description : Scan LCS start dates with LcsHelper_v3 only.
%
% Run by      : debug.ultrasat.planner.lcs_v3.scan_Lcs_v3()
%==========================================================================

function scan_Lcs_v3(Args)
    arguments
        Args.Year double = 2029
        Args.ScanStart datetime = NaT
        Args.ScanEnd   datetime = NaT
        Args.OutputDir char = ''
        Args.GridFile char = ''
        Args.ContinueOnError logical = true
    end

    ThisDir = fileparts(mfilename('fullpath'));
    RepoRoot = getenv('ASTROPACK_PATH');
    if isempty(RepoRoot)
        error('ASTROPACK_PATH is not set');
    end
    PlannerDir = fullfile(RepoRoot, 'matlab', 'astro', '+ultrasat', '+planner');

    if isnat(Args.ScanStart)
        Args.ScanStart = datetime(Args.Year, 1, 1);
    end
    if isnat(Args.ScanEnd)
        Args.ScanEnd = datetime(Args.Year, 12, 31);
    end
    if isempty(Args.OutputDir)
        Args.OutputDir = fullfile(ThisDir, 'output', 'scans', sprintf('%04d', Args.Year));
    end
    if isempty(Args.GridFile)
        Args.GridFile = defaultGridFile(PlannerDir);
    end

    ensureDir(Args.OutputDir);

    fprintf('========== LCS v3 scan ==========\n');
    fprintf('Year       : %d\n', Args.Year);
    fprintf('Scan range : %s .. %s\n', isoDate(Args.ScanStart), isoDate(Args.ScanEnd));
    fprintf('Output dir : %s\n', Args.OutputDir);
    fprintf('Grid file  : %s\n', Args.GridFile);

    Dates = scanDates(Args.ScanStart, Args.ScanEnd);
    Rows = repmat(emptyIndexRow(), numel(Dates), 1);

    for I = 1:numel(Dates)
        D = Dates(I);
        Stamp = dateStamp(D);
        fprintf('[v3 %3d/%3d] %s\n', I, numel(Dates), isoDate(D));

        try
            Obj = ultrasat.planner.LcsHelper_v3( ...
                'StartDate', D, ...
                'AllSkyTable', Args.GridFile, ...
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
                PlanDir = fullfile(Args.OutputDir, isoDate(D), 'success');
                ensureDir(PlanDir);
                dumpV3Plan(Obj, D, PlanDir);
                Rows(I).plan_file = sprintf('lcs_plan_%s.csv', Stamp);
                Rows(I).plan_dir = fullfile(isoDate(D), 'success');
                copyfile(fullfile(PlanDir, Rows(I).plan_file), ...
                    fullfile(Args.OutputDir, Rows(I).plan_file));
            end
        catch ME
            Rows(I).plan_start_date = isoDate(D);
            Rows(I).status = 'ERROR';
            Rows(I).detail = ME.message;
            if ~Args.ContinueOnError
                rethrow(ME);
            end
        end
    end

    Index = struct2table(Rows, 'AsArray', true);
    IndexPath = fullfile(Args.OutputDir, 'lcs_plan_index.csv');
    writetable(Index, IndexPath);
    fprintf('v3 index written: %s\n', IndexPath);
    printScanSummary(Index);
    fprintf('========== LCS v3 scan DONE ==========\n');
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

    writeDailyScheduleMatrix(Obj, StartDate, fullfile(PlanDir, 'daily_schedule.csv'));

    Stamp = dateStamp(StartDate);
    writeObservationList(Obj, StartDate, fullfile(PlanDir, sprintf('lcs_plan_%s.csv', Stamp)));

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


function printScanSummary(Index)
    IsFeasible = strcmp(Index.status, 'FEASIBLE');
    Dates = datetime(Index.plan_start_date(IsFeasible), 'InputFormat', 'yyyy-MM-dd');
    fprintf('Feasible dates: %d\n', numel(Dates));
    if isempty(Dates)
        fprintf('First feasible: (none)\n');
        fprintf('Last feasible : (none)\n');
        fprintf('Max gap       : (none)\n');
        return
    end

    Dates = sort(Dates);
    fprintf('First feasible: %s\n', isoDate(Dates(1)));
    fprintf('Last feasible : %s\n', isoDate(Dates(end)));
    if numel(Dates) == 1
        fprintf('Max gap       : 0 days\n');
    else
        Gaps = days(diff(Dates));
        fprintf('Max gap       : %d days\n', max(Gaps));
    end
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
    LocalGrid = fullfile(PlannerDir, 'data', 'LCS_fields.csv');
    if isfile(LocalGrid)
        GridFile = LocalGrid;
        return;
    end

    EnvPath = getenv('ASTROPACK_DATA_PATH');
    if ~isempty(EnvPath)
        EnvGrid = fullfile(EnvPath, 'ULTRASAT', 'LCS_fields.csv');
        if isfile(EnvGrid)
            GridFile = EnvGrid;
            return;
        end
    end

    error('defaultGridFile: LCS grid file not found');
end
