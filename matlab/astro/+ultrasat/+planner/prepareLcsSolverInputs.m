function prepareLcsSolverInputs(Args)
% Prepare input files for the LCS CP-SAT Python solver
%
% Runs LcsHelper visibility computation (calc_vis_matrix + calc_cont_vis_windows)
% and writes all solver input files to a data/lcs_solver_inputs/ folder.
% Supports saving/loading a .mat cache to avoid recomputing visibility on re-runs.
%
% Output files in OutputDir:
%   lcs_fields.csv                  - Field catalog (copy of original CSV)
%   lcs_params.json                 - Scalar solver parameters
%   lcs_daily_visibility.csv        - Day x field binary visibility matrix
%   lcs_visibility_windows.csv      - Continuous visibility windows per field (strict)
%   lcs_visibility_windows_1dgap.csv - Continuous visibility windows (1-day-gap merged)
%   lcs_field_eligibility.csv       - Hard eligibility flags per field (no set pre-assignment)
%
% Example:
%   ultrasat.planner.prepareLcsSolverInputs();
%   ultrasat.planner.prepareLcsSolverInputs('StartDate', datetime('2029-02-01'), ...
%       'LoadCache', false, 'SaveCache', true);

    arguments
        Args.StartDate    = [];     % Campaign start datetime (default: 2029-02-01)
        Args.EndDate      = [];     % Campaign end datetime   (default: StartDate + 420 days)
        Args.FieldsFile   = '';     % Path to fields CSV; default: data/LCS_nonoverlapping_grid_surveys.csv
        Args.OutputDir    = '';     % Output folder; default: <this_file>/../data/lcs_solver_inputs/
        Args.SaveCache    = false;  % Save computed visibility to .mat cache after computing
        Args.LoadCache    = true;   % Load from .mat cache if it exists (skip recompute)
        Args.CacheFile    = '';     % Cache file path; default: OutputDir/lcs_vis_cache.mat
    end

    % ---- Resolve paths ----
    ThisDir = fileparts(mfilename('fullpath'));
    DataDir = fullfile(ThisDir, 'data');

    if isempty(Args.OutputDir)
        Args.OutputDir = fullfile(DataDir, 'lcs_solver_inputs');
    end
    if isempty(Args.FieldsFile)
        Args.FieldsFile = fullfile(DataDir, 'LCS_nonoverlapping_grid_surveys.csv');
    end
    if isempty(Args.CacheFile)
        Args.CacheFile = fullfile(Args.OutputDir, 'lcs_vis_cache.mat');
    end

    if ~isfolder(Args.OutputDir)
        mkdir(Args.OutputDir);
        fprintf('Created output directory: %s\n', Args.OutputDir);
    end

    % ---- Construct LcsHelper (no computation) ----
    fprintf('[1/4] Constructing LcsHelper...\n');
    LCS = ultrasat.planner.LcsHelper('AllSkyTable', Args.FieldsFile, ...
        'StartDate', Args.StartDate, 'EndDate', Args.EndDate);

    % ---- Step 1+2: visibility matrix and windows (with cache) ----
    if Args.LoadCache && isfile(Args.CacheFile)
        fprintf('[2/4] Loading visibility cache from:\n      %s\n', Args.CacheFile);
        C = load(Args.CacheFile);
        LCS.vis_day_field            = C.vis_day_field;
        LCS.All_fields_windows       = C.All_fields_windows;
        LCS.All_fields_windows_1dgap = C.All_fields_windows_1dgap;
        LCS.Longest_window_per_field = C.Longest_window_per_field;
    else
        fprintf('[2/4] Computing visibility matrix (this may take several minutes)...\n');
        LCS.calc_vis_matrix();

        fprintf('      Computing continuous visibility windows...\n');
        [LCS.All_fields_windows, LCS.All_fields_windows_1dgap, ...
            LCS.Longest_window_per_field] = LCS.calc_cont_vis_windows();

        if Args.SaveCache
            fprintf('      Saving visibility cache to:\n      %s\n', Args.CacheFile);
            vis_day_field            = LCS.vis_day_field;           %#ok<NASGU>
            All_fields_windows       = LCS.All_fields_windows;      %#ok<NASGU>
            All_fields_windows_1dgap = LCS.All_fields_windows_1dgap; %#ok<NASGU>
            Longest_window_per_field = LCS.Longest_window_per_field; %#ok<NASGU>
            save(Args.CacheFile, 'vis_day_field', 'All_fields_windows', ...
                'All_fields_windows_1dgap', 'Longest_window_per_field');
        end
    end

    % ---- Step 3: export files ----
    fprintf('[3/4] Exporting solver input files...\n');

    exportFieldsCsv(LCS, Args);
    exportParamsJson(LCS, Args);
    exportDailyVisibility(LCS, Args);
    exportVisibilityWindows(LCS, Args);
    exportFieldEligibility(LCS, Args);

    fprintf('[4/4] Done. Files written to:\n      %s\n', Args.OutputDir);
    listOutputFiles(Args.OutputDir);
end


% =========================================================================
%  Internal export helpers
% =========================================================================

function exportFieldsCsv(LCS, Args)
    % Copy the original fields CSV as lcs_fields.csv
    Src = Args.FieldsFile;
    Dst = fullfile(Args.OutputDir, 'lcs_fields.csv');
    if isfile(Src)
        copyfile(Src, Dst);
        fprintf('  Wrote lcs_fields.csv  (%d fields)\n', height(LCS.AllSky));
    else
        % FieldsFile was a table; write AllSky directly
        writetable(LCS.AllSky, Dst);
        fprintf('  Wrote lcs_fields.csv  (%d fields, from table)\n', height(LCS.AllSky));
    end
end


function exportParamsJson(LCS, Args)
    % Build scalar parameter struct and write as pretty-printed JSON
    P.start_date            = datestr(LCS.StartDate, 'yyyy-mm-dd');
    P.end_date              = datestr(LCS.EndDate,   'yyyy-mm-dd');
    P.num_days              = LCS.Last_day;
    P.capacity_last_day     = LCS.SetA_Nwindows * LCS.Min_window;  % 360-day LCS plan
    P.first_day             = LCS.First_day;
    P.num_fields            = height(LCS.AllSky);
    P.daily_lcs_slots       = LCS.Daily_LCS_slots;
    P.slot_time_days        = LCS.SlotTime;
    P.daily_window_start_time_seconds = seconds(LCS.DailyWindowStartTime);
    P.min_window_days       = LCS.Min_window;
    P.max_window_cut_days   = LCS.Max_window_cut;
    P.max_extinction        = LCS.max_ext;
    P.set_a_total           = LCS.SetA_Nwindows * 8;
    P.set_a_n_groups        = LCS.SetA_Nwindows;
    P.set_a_fields_per_group = 8;
    P.set_b_count           = LCS.SetBnumel;
    P.set_c_count           = LCS.SetCnumel;
    P.set_d_count           = 4;
    P.cadence_setA_days     = 1;
    P.cadence_setB45_days   = 1;
    P.cadence_setB90_days   = 4;
    P.cadence_setC_days     = 4;
    P.b45_duration_days     = LCS.Min_window;
    P.b90_duration_days     = LCS.Max_window_cut - LCS.Min_window;
    P.sun_min_dist_deg      = 70;
    P.moon_min_dist_deg     = 34;
    P.earth_min_dist_deg    = 56;
    P.whole_daily_window    = LCS.Whole_daily_window;
    P.allow_1dgap           = LCS.Allow1dgap;

    JsonStr = jsonencode(P, 'PrettyPrint', true);
    Dst = fullfile(Args.OutputDir, 'lcs_params.json');
    Fid = fopen(Dst, 'w');
    fprintf(Fid, '%s\n', JsonStr);
    fclose(Fid);
    fprintf('  Wrote lcs_params.json\n');
end


function exportDailyVisibility(LCS, Args)
    % Write binary day x field visibility matrix
    % Rows: day 1..Last_day; Columns: day, field_<id>, ...
    FieldIDs = LCS.AllSky.Field;
    Nfields  = length(FieldIDs);
    NumDays  = LCS.Last_day - LCS.First_day + 1;
    DayNums  = (LCS.First_day : LCS.Last_day)';

    ColNames = [{'day'}, arrayfun(@(F) sprintf('field_%d', F), FieldIDs(:)', 'UniformOutput', false)];

    VisData  = [DayNums, double(LCS.vis_day_field(LCS.First_day:LCS.Last_day, :))];
    VisTable = array2table(VisData, 'VariableNames', ColNames);

    Dst = fullfile(Args.OutputDir, 'lcs_daily_visibility.csv');
    writetable(VisTable, Dst);
    fprintf('  Wrote lcs_daily_visibility.csv  (%d days x %d fields)\n', NumDays, Nfields);
end


function exportVisibilityWindows(LCS, Args)
    % Export strict and 1-day-gap visibility windows
    ColMap = {'Field','Av_ext','vis_start','vis_end','window'};
    NewNames = {'field_id','avg_extinction','vis_start_day','vis_end_day','window_len_days'};

    % Strict windows
    WStrict = LCS.All_fields_windows;
    WStrict.Properties.VariableNames = NewNames;
    Dst1 = fullfile(Args.OutputDir, 'lcs_visibility_windows.csv');
    writetable(WStrict, Dst1);
    fprintf('  Wrote lcs_visibility_windows.csv  (%d windows)\n', height(WStrict));

    % 1-day-gap merged windows
    W1gap = LCS.All_fields_windows_1dgap;
    W1gap.Properties.VariableNames = NewNames;
    Dst2 = fullfile(Args.OutputDir, 'lcs_visibility_windows_1dgap.csv');
    writetable(W1gap, Dst2);
    fprintf('  Wrote lcs_visibility_windows_1dgap.csv  (%d windows)\n', height(W1gap));
end


function exportFieldEligibility(LCS, Args)
    % Derive hard physical eligibility flags per field (no heuristic set pre-assignment)
    %
    % eligible_abc        : max_window >= min_window AND extinction <= max_ext
    % eligible_long_window: max_window >= max_window_cut (required for Sets B or C)
    % eligible_d          : max_window >= min_window AND extinction >  max_ext
    Lwpf = LCS.Longest_window_per_field;

    EligTable = table();
    EligTable.field_id              = Lwpf.Field;
    EligTable.avg_extinction        = Lwpf.Av_ext;
    EligTable.max_window_days       = Lwpf.max_window;
    EligTable.max_window_1dgap_days = Lwpf.max_window_1dgap;
    EligTable.eligible_abc          = uint8(Lwpf.max_window >= LCS.Min_window ...
                                            & Lwpf.Av_ext   <= LCS.max_ext);
    EligTable.eligible_long_window  = uint8(Lwpf.max_window >= LCS.Max_window_cut);
    EligTable.eligible_d            = uint8(Lwpf.max_window >= LCS.Min_window ...
                                            & Lwpf.Av_ext   >  LCS.max_ext);

    Dst = fullfile(Args.OutputDir, 'lcs_field_eligibility.csv');
    writetable(EligTable, Dst);
    fprintf('  Wrote lcs_field_eligibility.csv  (%d fields)\n', height(EligTable));
end


function listOutputFiles(OutputDir)
    % Print a summary table of all files written
    D = dir(fullfile(OutputDir, '*'));
    D = D(~[D.isdir]);
    if isempty(D)
        return;
    end
    fprintf('\n  %-46s  %8s\n', 'File', 'Size');
    fprintf('  %s\n', repmat('-', 1, 57));
    for k = 1:length(D)
        SizeStr = formatBytes(D(k).bytes);
        fprintf('  %-46s  %8s\n', D(k).name, SizeStr);
    end
    fprintf('\n');
end


function S = formatBytes(N)
    if N < 1024
        S = sprintf('%d B', N);
    elseif N < 1024^2
        S = sprintf('%.1f KB', N/1024);
    else
        S = sprintf('%.1f MB', N/1024^2);
    end
end
