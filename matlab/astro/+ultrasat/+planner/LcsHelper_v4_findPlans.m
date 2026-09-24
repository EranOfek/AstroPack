%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsHelper_v4_findPlans.m
% Author      : Chen Tishler
% Created     : 10/06/2026
% Modified    : 17/06/2026
% Description : Find feasible LCS v4 plans centered on a given start date.
%
%   Scans outward from StartDate — first forward (offset 0, +1, +2, ...),
%   then in alternation backward (offset -1, -2, ...) — until NumPlans
%   feasible plans are collected in each direction, or MaxRadius is reached.
%
% Usage:
%   Plans = ultrasat.planner.LcsHelper_v4_findPlans(datetime(2029,1,5), 2)
%   Plans = ultrasat.planner.LcsHelper_v4_findPlans(datetime(2029,1,5), 1, 'MaxRadius', 10)
%
% Returns a table sorted by date. Columns:
%   plan_start_date, offset_days, status, num_observations,
%   nA, nB, nC, nD, variant_used, detail
%==========================================================================

function Plans = LcsHelper_v4_findPlans(StartDate, NumPlans, Args)
    % Scan outward from StartDate to collect feasible LCS v4 plans in both directions
    arguments
        StartDate datetime
        NumPlans  double = 2
        Args.MaxRadius double  = 60
        Args.GridFile  char    = ''
        Args.Verbose   logical = true
    end

    % Resolve LCS grid CSV: explicit arg, then local data/, then ASTROPACK_DATA_PATH
    ThisDir    = fileparts(mfilename('fullpath'));
    GridFile   = resolveGridFile(Args.GridFile, ThisDir);
    StartDate  = dateshift(StartDate, 'start', 'day');

    % AfterRows: on/after StartDate; BeforeRows: before StartDate
    AfterRows  = emptyRows(0);
    BeforeRows = emptyRows(0);
    FwdOffset  = 0;   % 0, +1, +2, ... days from StartDate
    BwdOffset  = 1;   % -1, -2, ... days from StartDate

    % Alternate forward/backward scans until both directions hit NumPlans or MaxRadius
    while (height(AfterRows) < NumPlans || height(BeforeRows) < NumPlans)
        bothExhausted = FwdOffset > Args.MaxRadius && BwdOffset > Args.MaxRadius;
        if bothExhausted
            break;
        end

        % Forward pass: try StartDate + FwdOffset (includes offset 0 = StartDate itself)
        if height(AfterRows) < NumPlans && FwdOffset <= Args.MaxRadius
            D = StartDate + days(FwdOffset);
            Row = tryDate(D, FwdOffset, GridFile, NumPlans, height(AfterRows), 'after', Args.Verbose);
            if strcmp(Row.status, 'FEASIBLE')
                AfterRows = [AfterRows; Row]; %#ok<AGROW>
            end
            FwdOffset = FwdOffset + 1;
        end

        % Backward pass: try StartDate - BwdOffset
        if height(BeforeRows) < NumPlans && BwdOffset <= Args.MaxRadius
            D = StartDate - days(BwdOffset);
            Row = tryDate(D, -BwdOffset, GridFile, NumPlans, height(BeforeRows), 'before', Args.Verbose);
            if strcmp(Row.status, 'FEASIBLE')
                BeforeRows = [BeforeRows; Row]; %#ok<AGROW>
            end
            BwdOffset = BwdOffset + 1;
        end
    end

    % Merge and sort chronologically by offset from StartDate
    Plans = sortrows([BeforeRows; AfterRows], 'offset_days');

    % If verbose, print summary
    if Args.Verbose
        nBefore = height(BeforeRows);
        nAfter  = height(AfterRows);        
        fprintf('[LcsHelper_v4_findPlans] Found %d plans (%d before, %d on/after %s)\n', ...
            height(Plans), nBefore, nAfter, isoDate(StartDate));
        if nBefore < NumPlans || nAfter < NumPlans
            fprintf('[LcsHelper_v4_findPlans] Warning: target was %d per direction; MaxRadius=%d days\n', ...
                NumPlans, Args.MaxRadius);
        end
    end
end


function Row = tryDate(D, OffsetDays, GridFile, NumPlans, CurrentCount, Direction, Verbose)
    % Build one LcsHelper_v4 plan for date D and return a standardized result row
    if Verbose
        if OffsetDays >= 0
            OffsetStr = sprintf('+%d', OffsetDays);
        else
            OffsetStr = sprintf('%d', OffsetDays);
        end
        fprintf('[LcsHelper_v4_findPlans] Trying %s (offset=%s) ...', isoDate(D), OffsetStr);
    end

    % Pre-fill row with date metadata; status defaults to INFEASIBLE
    Row = emptyRows(1);
    Row.plan_start_date = isoDate(D);
    Row.offset_days     = OffsetDays;

    try
        % Run full v4 pipeline for this candidate start date
        Obj = ultrasat.planner.LcsHelper_v4( ...
            'StartDate', D, ...
            'AllSkyTable', GridFile, ...
            'Verbose', false, ...
            'prep_before_schedule', true, ...
            'build_the_schedule', true);

        Summary    = summarizeV4(Obj);
        IsFeasible = isV4Feasible(Obj, Summary);

        if IsFeasible
            % Copy set counts and variant into output row for downstream comparison
            Row.status           = 'FEASIBLE';
            Row.num_observations = Summary.num_observations;
            Row.nA               = Summary.nA;
            Row.nB               = Summary.nB_fields;
            Row.nC               = Summary.nC;
            Row.nD               = Summary.nD;
            Row.variant_used     = Summary.variant_used;
            Row.detail           = sprintf('A=%d B=%d C=%d D=%d Variant_used=%d', ...
                Summary.nA, Summary.nB_fields, Summary.nC, Summary.nD, Summary.variant_used);

            if Verbose
                NewCount = CurrentCount + 1;
                fprintf(' FEASIBLE (%s: %d/%d)\n', Direction, NewCount, NumPlans);
            end
        else
            Row.status = 'INFEASIBLE';
            if Verbose
                fprintf(' INFEASIBLE\n');
            end
        end
    catch ME
        % Pipeline exception: record message but continue scanning other dates
        Row.status = 'ERROR';
        Row.detail = ME.message;
        if Verbose
            fprintf(' ERROR: %s\n', ME.message);
        end
    end
end


function Summary = summarizeV4(Obj)
    % Count placed fields per set category and total daily observations
    % Only count rows with Field > 0 (actually placed, not placeholder)
    MaskA = strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0;
    MaskB = ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0;
    MaskC = strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0;
    MaskD = strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0;

    Summary = struct();
    Summary.nA               = sum(MaskA);
    Summary.nB_rows          = sum(MaskB);
    Summary.nB_fields        = numel(unique(Obj.Schedule.Field(MaskB)));
    Summary.nC               = sum(MaskC);
    Summary.nD               = sum(MaskD);
    Summary.variant_used     = Obj.Variant_used;
    Summary.num_observations = sum(~isnan(Obj.Daily_schedule(:)));
end


function IsOk = isV4Feasible(Obj, Summary)
    % True when schedule counts match v4 set quotas and a variant was selected
    IsOk = ~isempty(Obj.Schedule) && ~isempty(Obj.Daily_schedule) && ...
        Summary.nA == Obj.SetAnumel && ...
        Summary.nB_rows == 3 * Obj.SetBnumel && ...
        Summary.nB_fields == Obj.SetBnumel && ...
        Summary.nC == Obj.SetCnumel && ...
        Summary.nD <= Obj.SetDnumel && ...
        Summary.variant_used > 0;
end


function T = emptyRows(N)
    % Create empty or pre-allocated result table with standard output columns
    if N == 0
        T = table( ...
            string.empty(0,1), ...
            zeros(0,1), ...
            string.empty(0,1), ...
            zeros(0,1), ...
            zeros(0,1), zeros(0,1), zeros(0,1), zeros(0,1), ...
            zeros(0,1), ...
            string.empty(0,1), ...
            'VariableNames', { ...
                'plan_start_date', 'offset_days', 'status', ...
                'num_observations', 'nA', 'nB', 'nC', 'nD', ...
                'variant_used', 'detail'});
    else
        T = table( ...
            repmat("", N, 1), ...
            zeros(N, 1), ...
            repmat("INFEASIBLE", N, 1), ...
            zeros(N, 1), ...
            zeros(N, 1), zeros(N, 1), zeros(N, 1), zeros(N, 1), ...
            zeros(N, 1), ...
            repmat("", N, 1), ...
            'VariableNames', { ...
                'plan_start_date', 'offset_days', 'status', ...
                'num_observations', 'nA', 'nB', 'nC', 'nD', ...
                'variant_used', 'detail'});
    end
end


function GridFile = resolveGridFile(GridFile, PlannerDir)
    % Resolve LCS grid CSV: explicit arg, then local data/, then ASTROPACK_DATA_PATH
    if ~isempty(GridFile)
        return;
    end
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
    error('LcsHelper_v4_findPlans: LCS grid file not found');
end


function S = isoDate(D)
    % Format datetime as yyyy-mm-dd string for table output
    S = datestr(D, 'yyyy-mm-dd');
end
