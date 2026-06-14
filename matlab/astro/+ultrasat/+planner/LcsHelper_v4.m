%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.LcsHelper_v4.m
% Author      : Yossi Shvartzvald
% Created     : 07/06/2026
% Updated     : 14/06/2026
% Description : ULTRASAT Low Cadence Survey planner helper (variant-based).
%               Companion files (same sort group):
%                 LcsHelper_v4_findPlans.m
%                 LcsHelper_v4_validate.m
%                 LcsHelper_v4_validateScanOutputs.m
%==========================================================================
% LcsHelper_v4 - ULTRASAT Low Cadence Survey planner helper (variant-based).
%
% Pipeline:
%   1. calc_vis_matrix             [unchanged from v3]
%   2. calc_cont_vis_windows_v2    [unchanged from v3]
%   3. categorizeFields_v4         (extinction-only A/B/C/leftover split)
%   4. categorize_then_schedule    (orchestrates SetA + variant-based SetB/C)
%   5. schedule_SetD_v4 + calcDailySchedule
%
% Key design choices (vs v3):
%   - SetB/SetC scheduling no longer uses SetC_start_ind. Instead, 4 fixed
%     OPTIMAL variants (window-occupancy configurations) are tried in order
%     within each shuffle attempt.
%   - Each variant is a complete spec of C_blocks, B_blocks, and per-block
%     "1"-position counts. Each variant ensures a balanced 4x4 + 4x5
%     occupancy profile (no window > 5 counts).
%   - Categorization runs ONCE; not reset between variants. Extinction
%     ranking is preserved across variant attempts and shuffle iterations.
%   - SetA is variant-independent: matched once per attempt, then a snapshot
%     is restored before each variant's SetB/SetC matching.
%   - On failure across all 4 variants: aggregate unplaced fields and
%     shuffle the MOST COMMON unplaced field (option b).
%   - SetA shift rescue, SetD pre-clean + Case A/B/C, calcDailySchedule
%     are inherited from v3 unchanged (just renamed).

%==========================================================================
% FUNCTION CALL TREE
%--------------------------------------------------------------------------
% LcsHelper_v4()                        [constructor]
%   prepTablesBeforeSchedule()
%     calc_vis_matrix()
%     calc_cont_vis_windows_v2()
%     categorizeFields_v4()
%       local_make_field_table()
%
%   categorize_then_schedule()
%     categorizeFields_v4()
%       local_make_field_table()
%     schedule_SetA_v4()
%       local_match_setA()
%     schedule_SetC_v4()
%       matchpairs()  [MATLAB built-in]
%     schedule_SetB_v4()
%       matchpairs()  [MATLAB built-in]
%     shuffle_on_failure()
%       swap_out_setB()
%       swap_out_setC()
%       swap_setA_long_to_C_or_B()
%     schedule_SetD_v4()
%       clean_inds_before_setD()
%         local_compute_slot_occupancy()
%         local_apply_setA_moves()
%           local_assign_moved_setA_group()
%       local_commit_setD()
%     calcDailySchedule()
%     LcsHelper_v4_validate()           [external companion file]
%
%   plotSchedule()                      [visual inspection, not scheduling]
%   plotCatB()                          [visual inspection, not scheduling]
%
% LOCAL HELPERS (file-scope)
%   local_make_field_table()
%   local_match_setA()
%   local_pick_most_common_unplaced()
%     local_unplaced_winner()
%   local_unplaced_winner()
%   local_compute_slot_occupancy()
%   local_apply_setA_moves()
%     local_assign_moved_setA_group()
%   local_assign_moved_setA_group()
%   local_commit_setD()
%   consecutive_trues_cols()
%   fill_isolated_gaps()
%
% EXTERNAL COMPANION FILES (same sort group)
%   LcsHelper_v4_findPlans.m
%   LcsHelper_v4_validate.m
%   LcsHelper_v4_validateScanOutputs.m
%==========================================================================

classdef LcsHelper_v4 < Component

    % ========================== PUBLIC PROPERTIES ==========================
    properties(Access = public)

        % Configurations
        Whole_daily_window = false;                                    % true = field must be visible in ALL daily slots; false = ANY slot
        Allow1dgap         = false;                                    % true = allow a single 1-day gap when measuring continuous windows
        Verbose            = false;                                    % if true, diagnostic prints are emitted

        % Definitions
        AllSky      table                                              % full-sky field catalogue table (columns: Field, RA, Dec, A_U)

        StartDate datetime  = '2029-02-01 00:00:00';                   % mission start date; day-1 anchor for all day offsets
        EndDate datetime  ;                                            % mission end date; computed as StartDate+Last_day when not supplied
        First_day = 1;                                                 % first planning day index (offset from StartDate)
        Last_day  = 420;                                               % last planning day index; derived from EndDate when EndDate is given

        DailyWindowStartTime duration =  duration(00,00,00);           % UTC time-of-day offset added when converting day indices to JD

        Daily_LCS_slots = 11;                                          % number of LCS observation slots available per day
        SlotTime        = 3*300/60/60/24;                              % duration of one observation slot [days] (3 x 300 s)

        Min_window     = 45;                                           % minimum continuous visibility window a field must have [days]; block length L
        Max_window_cut = 135;                                          % minimum window to qualify a field for SetB or SetC [days]
        max_ext        = 1;                                            % maximum UV extinction A_U allowed for the "low extinction" pool

        SetAnumel = 48;                                                % target cardinality of Set A (48 fields)
        SetBnumel = 16;                                                % target cardinality of Set B (16 fields)
        SetCnumel = 16;                                                % target cardinality of Set C (16 fields)
        SetDnumel = 4;                                                 % target cardinality of Set D (4 fields)

        SetA_Nwindows = 6;                                             % number of 45-day windows (groups 1..6) allocated to Set A

        % Intermediate results
        Nominal_windows table                                          % 8-row table of nominal window start/end day indices
        Full_windows    table                                          % adjusted window table used for actual scheduling (anchored at First_day)

        vis_day_field        logical                                    % [NumDays x NFields] logical: daily visibility (ALL or ANY per Whole_daily_window)
        vis3d_slot_day_field logical                                    % [Nslots x NumDays x NFields] logical: per-slot/day/field visibility bitmap
        vis2d_day_field_ALL  logical                                    % [NumDays x NFields] logical: true iff ALL slots are visible on that day
        vis2d_day_field_ANY  logical                                    % [NumDays x NFields] logical: true iff ANY slot is visible on that day

        Cont_visibilty_per_field        double                          % [NumDays x NFields] forward run-length of consecutive visible days
        Longest_window_per_field        double                          % [1 x NFields] maximum continuous visibility window per field [days]
        Cont_visibilty_per_field_1dgap  double                          % same as above but with isolated 1-day gaps filled in
        Longest_window_per_field_1dgap  double                          % maximum continuous window with 1-day gap tolerance [days]

        SetA_fields  table                                             % candidate table for Set A, sorted by max_window ascending
        SetB_fields  table                                             % candidate table for Set B, sorted by max_window ascending
        SetC_fields  table                                             % candidate table for Set C, sorted by max_window ascending

        % Long-extinction "leftovers" not yet in SetA/B/C, kept for shuffling
        Long_leftover_fields  table                                    % long-window fields not assigned to SetB/SetC; shuffling candidate pool

        % Tracks which SetA group was shifted by schedule_SetA_v4 (0 if none)
        SetA_shifted_group  double = 0                                 % group index shifted by schedule_SetA_v4 for rescue (0 = none)

        % Which variant (1..4) of the optimal SetB/SetC configurations
        % succeeded (0 if no schedule was found).
        Variant_used  double = 0                                       % winning variant index 1..4; 0 if no feasible schedule was found

        % SetD bookkeeping (populated by schedule_SetD_v4)
        SetD_ranked_fields  table                                      % ranked SetD candidate table with placement results (scheduled, ind)
        inds_open    = []                                              % list (with repetition) of window indices that still have free daily slots
        inds_2move   = []                                              % list (with repetition) of inds with over-full slots

        % Final schedules
        Schedule        table                                          % final schedule table (columns: category, group, ind, start, end, Field)
        Daily_schedule                                                 % [NumDays x Daily_LCS_slots] observed field IDs per day/slot (NaN = empty)
    end

    % ========================== CONSTANT VARIANTS ==========================
    properties(Constant)
        % Catalogue of the 4 optimal SetB/SetC window-configuration variants.
        % Each variant fully specifies the block layout and the per-block
        % "1"-position assignments for SetB fields.
        %
        % Fields:
        %   C_blocks(k) - # SetC fields with block B_k (k=1..6).
        %                 Block B_k = (W_k, W_{k+1}, W_{k+2}); SetC field
        %                 covers all 3 windows for 135 days @ 4d cadence.
        %   B_blocks(k) - # SetB fields with block B_k.
        %   ones_at(k, w) - among B_blocks(k) fields, how many have their
        %                 "1"-window (the W45 sub-block) at window W_w.
        %                 Must satisfy sum_w ones_at(k, w) = B_blocks(k).
        Variants = struct( ...                                          % struct array of the 4 optimal SetB/SetC window-configuration variants
            'name',     {'Optimal_1', 'Optimal_2', 'Optimal_3', 'Optimal_4'}, ...
            'C_blocks', {[4 4 0 0 4 4], [0 4 4 4 4 0], [4 4 0 4 4 0], [0 4 4 0 4 4]}, ...
            'B_blocks', {[4 2 2 2 2 4], [4 3 1 1 3 4], [4 2 2 2 2 4], [4 2 2 2 2 4]}, ...
            'ones_at',  {[4 0 0 0 0 0 0 0; ...   % Optimal 1
                          0 2 0 0 0 0 0 0; ...
                          0 0 0 0 2 0 0 0; ...
                          0 0 0 2 0 0 0 0; ...
                          0 0 0 0 0 0 2 0; ...
                          0 0 0 0 0 0 0 4], ...
                         [4 0 0 0 0 0 0 0; ...   % Optimal 2
                          0 3 0 0 0 0 0 0; ...
                          0 0 0 0 1 0 0 0; ...
                          0 0 0 1 0 0 0 0; ...
                          0 0 0 0 0 0 3 0; ...
                          0 0 0 0 0 0 0 4], ...
                         [4 0 0 0 0 0 0 0; ...   % Optimal 3
                          0 2 0 0 0 0 0 0; ...
                          0 0 0 2 0 0 0 0; ...
                          0 0 0 0 2 0 0 0; ...
                          0 0 0 0 0 0 2 0; ...
                          0 0 0 0 0 0 0 4], ...
                         [4 0 0 0 0 0 0 0; ...   % Optimal 4
                          0 2 0 0 0 0 0 0; ...
                          0 0 0 2 0 0 0 0; ...
                          0 0 0 0 2 0 0 0; ...
                          0 0 0 0 0 0 2 0; ...
                          0 0 0 0 0 0 0 4]});
    end


    % ========================== CONSTRUCTOR ==========================
    methods
        function Obj = LcsHelper_v4(Args)
            % LcsHelper_v4 - Construct and optionally run the LCS v4 scheduling pipeline.
            %
            %   Obj = LcsHelper_v4()
            %       Create a helper object with all default parameters. No computation
            %       is triggered until the caller invokes prepTablesBeforeSchedule()
            %       and categorize_then_schedule() explicitly.
            %
            %   Obj = LcsHelper_v4('Name', Value, ...)
            %       As above with one or more name-value options (see Args below).
            %
            % Typical usage:
            %   h = LcsHelper_v4('StartDate', datetime('2029-02-01'), ...
            %                    'prep_before_schedule', true, ...
            %                    'build_the_schedule',   true);
            %
            % After construction:
            %   h.plotSchedule();
            %   h.calcDailySchedule();
            arguments
                Args.StartDate  = [];                                    % mission start date; shifts to midnight if time component is present
                Args.EndDate    = [];                                    % mission end date; if empty, derived as StartDate + Last_day
                Args.AllSkyTable = '~/matlab/data/ULTRASAT/LCS_nonoverlapping_grid_surveys.csv';  % path to CSV or a pre-loaded table with columns RA, Dec, A_U
                Args.DailyWindowStartTime duration = duration.empty;     % UTC time-of-day offset for daily visibility slot JD computation

                Args.prep_before_schedule    = false;                    % if true, call prepTablesBeforeSchedule() in the constructor
                Args.build_the_schedule      = false;                    % if true (and prep=true), call categorize_then_schedule() too
                Args.validate_after_schedule = true;                       % if true (and build=true), run LcsHelper_v4_validate and error on failure

                Args.Whole_daily_window = false;                           % passed to Obj.Whole_daily_window (see property comment)
                Args.Allow1dgap         = false;                           % passed to Obj.Allow1dgap (see property comment)
                Args.Verbose            = false;                           % passed to Obj.Verbose; enables diagnostic fprintf output
            end

            if ~isempty(Args.StartDate)
                Obj.StartDate = dateshift(Args.StartDate, 'start', 'day');
            end

            if isempty(Args.EndDate)
                Obj.EndDate = Obj.StartDate + Obj.Last_day;
            else
                Obj.EndDate  = dateshift(Args.EndDate, 'start', 'day');
                Obj.Last_day = days(Obj.EndDate - Obj.StartDate);
            end

            if ~isempty(Args.DailyWindowStartTime)
                Obj.DailyWindowStartTime = Args.DailyWindowStartTime;
            end

            if ischar(Args.AllSkyTable)
                Obj.AllSky = readtable(Args.AllSkyTable);
                AU_ind = find(Obj.AllSky.Properties.VariableNames == "AU");
                if ~isempty(AU_ind)
                    Obj.AllSky.Properties.VariableNames(AU_ind) = "A_U";
                end
            elseif istable(Args.AllSkyTable)
                Obj.AllSky = table('Size', [height(Args.AllSkyTable), 4], ...
                    'VariableNames', {'Field','RA','Dec','AU'}, ...
                    'VariableTypes', {'double','double','double','double'});
                Obj.AllSky.Field = (1:height(Args.AllSkyTable))';
                Obj.AllSky.RA    = Args.AllSkyTable.RA;
                Obj.AllSky.Dec   = Args.AllSkyTable.Dec;
                Obj.AllSky.A_U   = Args.AllSkyTable.A_U;
            end

            Obj.Whole_daily_window = Args.Whole_daily_window;
            Obj.Allow1dgap         = Args.Allow1dgap;
            Obj.Verbose            = Args.Verbose;

            if Args.prep_before_schedule
                Obj.prepTablesBeforeSchedule;
                if Args.build_the_schedule
                    Obj.categorize_then_schedule;
                    if Args.validate_after_schedule
                        [nFail, ~] = ultrasat.planner.LcsHelper_v4_validate(Obj, ...
                            'Verbose', false, 'DumpCsv', false);
                        if nFail > 0
                            error('LcsHelper_v4: schedule validation failed (%d check(s) failed)', nFail);
                        end
                    end
                end
            end
        end
    end


    % ========================== METHODS ==========================
    methods

        % ========================== MAIN FUNCTIONS ==========================

        function prepTablesBeforeSchedule(Obj)
            % Set up the time windows and compute visibility tables.
            % Categorization happens here (extinction-only).
            % Scheduling happens in categorize_then_schedule().
            arguments
                Obj
            end

            % Set Nominal_windows and Full_windows: fixed grid at First_day.
            Obj.Nominal_windows.start = Obj.First_day + Obj.Min_window * (0:7)';
            Obj.Nominal_windows.end   = Obj.Nominal_windows.start + Obj.Min_window - 1;
            Obj.Full_windows = Obj.Nominal_windows;

            % Step 1: per-day visibility for each field
            Obj.calc_vis_matrix();

            % Step 2: continuous visibility windows
            Obj.calc_cont_vis_windows_v2();

            % Step 3: extinction-only field categorization
            Obj.categorizeFields_v4();
        end


        function categorize_then_schedule(Obj, Args)
            % v4 orchestrator (variant-based SetB/SetC).
            %
            % Strategy:
            %   - Categorization runs ONCE (extinction-only).
            %   - SetA is matched ONCE per shuffle attempt (variant-independent).
            %   - For each shuffle attempt, try all 4 OPTIMAL variants in
            %     VariantOrder (default 1->2->3->4). The first variant whose
            %     SetC and SetB matchings both succeed wins.
            %   - If no variant succeeds in an attempt, aggregate unplaced
            %     SetC and SetB fields across all variants and shuffle the
            %     most common one (option b).
            %   - Retry up to MaxRetries shuffle attempts.
            %   - If A/B/C succeed and RunSetD is true, schedule SetD.
            %   - calcDailySchedule is the mandatory final step.
            arguments
                Obj
                Args.MaxRetries         = 10
                Args.RunSetD            = true
                Args.VariantOrder       = [1 2 3 4]
                Args.SkipVariants       = []   % exclude these variant indices
            end

            % Filter variant order by SkipVariants
            variant_order = Args.VariantOrder(~ismember(Args.VariantOrder, Args.SkipVariants));
            if isempty(variant_order)
                error('categorize_then_schedule: no variants left after SkipVariants filter');
            end

            % Reset variant_used (set on success)
            Obj.Variant_used = 0;

            % Categorize ONCE; extinction ranking preserved across all
            % attempts and variants. Only shuffle_on_failure mutates the
            % field-table assignments.
            Obj.categorizeFields_v4();

            success = false;
            for attempt = 1:Args.MaxRetries
                if Obj.Verbose, fprintf('-- attempt %d/%d --\n', attempt, Args.MaxRetries); end

                % --- SetA (variant-independent), once per attempt ---
                Obj.Schedule = table();
                [okA, unplaced_A] = Obj.schedule_SetA_v4();
                if ~okA
                    if Obj.Verbose
                        fprintf('  SetA failed: unplaced = %s\n', mat2str(unplaced_A));
                    end
                    shuffled = Obj.shuffle_on_failure(unplaced_A, [], []);
                    if ~shuffled
                        if Obj.Verbose, fprintf('  no shuffle candidates left; abort.\n'); end
                        break
                    end
                    continue
                end

                % Snapshot post-SetA state for restoration between variants
                SetA_snapshot          = Obj.Schedule;
                shifted_group_snapshot = Obj.SetA_shifted_group;

                % --- Try each variant ---
                unp_C_per_variant = cell(numel(variant_order), 1);
                unp_B_per_variant = cell(numel(variant_order), 1);
                variant_success = false;

                for vi = 1:numel(variant_order)
                    v = variant_order(vi);
                    if Obj.Verbose
                        fprintf('  trying variant %d (%s)\n', v, Obj.Variants(v).name);
                    end

                    % Restore SetA-only state
                    Obj.Schedule           = SetA_snapshot;
                    Obj.SetA_shifted_group = shifted_group_snapshot;

                    [okC, unplaced_C] = Obj.schedule_SetC_v4(v);
                    [okB, unplaced_B] = Obj.schedule_SetB_v4(v);

                    unp_C_per_variant{vi} = unplaced_C(:);
                    unp_B_per_variant{vi} = unplaced_B(:);

                    if okC && okB
                        Obj.Variant_used = v;
                        variant_success = true;
                        if Obj.Verbose
                            fprintf('SUCCESS: variant %d (%s), attempt=%d\n', ...
                                    v, Obj.Variants(v).name, attempt);
                        end
                        break
                    end
                end

                if variant_success
                    success = true;
                    break
                end

                % All variants failed this attempt; pick most-common unplaced
                % field across variants and shuffle.
                [unp_C_shuffle, unp_B_shuffle] = local_pick_most_common_unplaced(...
                    unp_C_per_variant, unp_B_per_variant);

                if Obj.Verbose
                    fprintf('  all variants failed; shuffling on:\n');
                    if ~isempty(unp_C_shuffle), fprintf('    SetC: %s\n', mat2str(unp_C_shuffle(:)')); end
                    if ~isempty(unp_B_shuffle), fprintf('    SetB: %s\n', mat2str(unp_B_shuffle(:)')); end
                end

                shuffled = Obj.shuffle_on_failure([], unp_C_shuffle, unp_B_shuffle);
                if ~shuffled
                    if Obj.Verbose, fprintf('  no shuffle candidates left; abort.\n'); end
                    break
                end
            end

            if ~success
                warning('categorize_then_schedule: no feasible variant schedule found.');
                return
            end

            % SetD scheduling: only runs if A/B/C succeeded
            if Args.RunSetD
                Obj.schedule_SetD_v4();
            end

            % Final mandatory step: build the per-day per-slot matrix.
            % Wrapped in try/catch as a safety net: a calcDailySchedule
            % failure issues a warning but preserves Obj.Schedule so
            % downstream code can still inspect it.
            try
                Obj.calcDailySchedule();
            catch ME
                warning('categorize_then_schedule: calcDailySchedule failed (%s); Obj.Schedule kept.', ...
                        ME.message);
            end
        end


        % ========================== STEP FUNCTIONS ==========================

        function calc_vis_matrix(Obj)
            % Step 1: build daily visibility for all LCS fields.
            arguments
                Obj
            end

            RAD = 180/pi;
            NumDays    = Obj.Last_day - Obj.First_day + 1;
            N_vis_slots = Obj.Daily_LCS_slots + 1;   % +1 for slew

            l = zeros(1, N_vis_slots * NumDays);
            for i = 1:NumDays
                for j = 1:N_vis_slots
                    k = (i-1) * N_vis_slots + j;
                    l(k) = (i-1) + (j-1) * Obj.SlotTime;
                end
            end

            Grid = [Obj.AllSky.RA, Obj.AllSky.Dec];
            JD   = juliandate(Obj.StartDate + Obj.DailyWindowStartTime) + l;

            Vis = ultrasat.ULTRASAT_restricted_visibility(JD', Grid./RAD, ...
                'MinSunDist', 70, 'MinMoonDist', 34, 'MinEarthDist', 56);

            Lim = Vis.PowerLimits & Vis.SunLimits & Vis.MoonLimits & Vis.EarthLimits;

            Obj.vis3d_slot_day_field = reshape(Lim, [N_vis_slots, NumDays, length(Grid)]);
            Obj.vis2d_day_field_ALL  = squeeze(all(Obj.vis3d_slot_day_field, 1));
            Obj.vis2d_day_field_ANY  = squeeze(any(Obj.vis3d_slot_day_field, 1));

            if Obj.Whole_daily_window
                Obj.vis_day_field = Obj.vis2d_day_field_ALL;
            else
                Obj.vis_day_field = Obj.vis2d_day_field_ANY;
            end
        end


        function calc_cont_vis_windows_v2(Obj)
            % Step 2: continuous visibility windows per field.
            arguments
                Obj
            end

            Obj.Cont_visibilty_per_field = consecutive_trues_cols(Obj.vis_day_field);
            Obj.Longest_window_per_field = max(Obj.Cont_visibilty_per_field, [], 1);

            vis_day_field_1dgap = fill_isolated_gaps(Obj.vis_day_field);
            Obj.Cont_visibilty_per_field_1dgap = consecutive_trues_cols(vis_day_field_1dgap);
            Obj.Longest_window_per_field_1dgap = max(Obj.Cont_visibilty_per_field_1dgap, [], 1);
        end


        function categorizeFields_v4(Obj)
            % Step 3: extinction-only categorization into Sets A/B/C.
            %
            %   - Identify Low_ext_fields  (Av_ext <= max_ext, max_window >= Min_window)
            %     with 1d-gap fallback if not enough fields
            %   - Identify Long_low_ext_fields  (also max_window >= Max_window_cut)
            %   - SetB = 16 lowest-Av_ext from Long
            %   - SetC = next 16 lowest-Av_ext from Long
            %   - SetA = Short_fields + remaining Long
            %   - Long_leftover_fields: same as the "remaining Long" stored
            %     separately as a shuffling candidate pool
            arguments
                Obj
            end

            F_A_U     = (Obj.AllSky.A_U)' <= Obj.max_ext;
            F_minW    = Obj.Longest_window_per_field      >= Obj.Min_window;
            F_maxWcut = Obj.Longest_window_per_field      >= Obj.Max_window_cut;

            use1gap = false(size(Obj.Longest_window_per_field));

            Low_ext_fields = F_A_U & F_minW;
            if sum(Low_ext_fields) < (Obj.SetAnumel + Obj.SetBnumel + Obj.SetCnumel + 1)
                F_minW_1dgap   = Obj.Longest_window_per_field_1dgap >= Obj.Min_window;
                Low_ext_fields = F_A_U & F_minW_1dgap;
                use1gap(F_minW_1dgap & ~F_minW) = true;
            end
            if sum(Low_ext_fields) < (Obj.SetAnumel + Obj.SetBnumel + Obj.SetCnumel)
                error('categorizeFields_v4: not enough fields for Sets A+B+C');
            end

            Long_low_ext_fields = Low_ext_fields & F_maxWcut;
            if sum(Long_low_ext_fields) < (Obj.SetBnumel + Obj.SetCnumel)
                F_maxWcut_1dgap     = Obj.Longest_window_per_field_1dgap >= Obj.Max_window_cut;
                Long_low_ext_fields = Low_ext_fields & F_maxWcut_1dgap;
                use1gap(F_maxWcut_1dgap & ~F_maxWcut) = true;
            end
            if sum(Long_low_ext_fields) < (Obj.SetBnumel + Obj.SetCnumel)
                error('categorizeFields_v4: not enough fields for Sets B+C');
            end

            % Build Short_fields (always SetA)
            Short_fields = local_make_field_table(...
                find(Low_ext_fields & ~Long_low_ext_fields)', ...
                Obj.AllSky.A_U, use1gap, ...
                Obj.Longest_window_per_field, Obj.Longest_window_per_field_1dgap);

            % Build Long_fields sorted by extinction
            Long_fields = local_make_field_table(...
                find(Long_low_ext_fields)', ...
                Obj.AllSky.A_U, use1gap, ...
                Obj.Longest_window_per_field, Obj.Longest_window_per_field_1dgap);
            Long_fields = sortrows(Long_fields, 'Av_ext');

            Obj.SetB_fields = Long_fields(1:Obj.SetBnumel, :);
            Obj.SetC_fields = Long_fields((Obj.SetBnumel+1):(Obj.SetBnumel+Obj.SetCnumel), :);

            % Long leftover = the rest of Long_fields (not in B or C)
            leftover_idx = (Obj.SetBnumel + Obj.SetCnumel + 1):height(Long_fields);
            Obj.Long_leftover_fields = Long_fields(leftover_idx, :);

            Obj.SetA_fields = [Short_fields; Obj.Long_leftover_fields];

            % Sort each set by max_window ascending (most-constrained first)
            Obj.SetA_fields = sortrows(Obj.SetA_fields, 'max_window');
            Obj.SetB_fields = sortrows(Obj.SetB_fields, 'max_window');
            Obj.SetC_fields = sortrows(Obj.SetC_fields, 'max_window');

            if Obj.Verbose
                fprintf('  categorize: %d SetA, %d SetB, %d SetC, %d Long_leftover\n', ...
                        height(Obj.SetA_fields), height(Obj.SetB_fields), ...
                        height(Obj.SetC_fields), height(Obj.Long_leftover_fields));
            end
        end


        function [ok, unplaced] = schedule_SetA_v4(Obj)
            % Place all 48 SetA fields. Anchor is First_day for all 6 groups;
            % if that fails, try shifting ONE group to rescue.
            arguments
                Obj
            end

            L      = Obj.Min_window;
            Nslots = 8;
            Ngrp   = Obj.SetA_Nwindows;
            ref    = Obj.First_day;

            % Reset shifted-group tracker (set later if Phase 2 triggers)
            Obj.SetA_shifted_group = 0;

            % Precompute per-field feasible start days
            NF        = height(Obj.SetA_fields);
            feas_days = cell(NF, 1);
            for i = 1:NF
                f = Obj.SetA_fields.Field(i);
                if Obj.SetA_fields.use1gap(i)
                    col = Obj.Cont_visibilty_per_field_1dgap(:, f);
                else
                    col = Obj.Cont_visibilty_per_field(:, f);
                end
                feas_days{i} = find(col >= L);
            end

            % ----- Phase 1: all groups at ref_day, matchpairs over all 48 slots -----
            slot_starts = zeros(Ngrp * Nslots, 1);
            slot_group  = zeros(Ngrp * Nslots, 1);
            slot_ind    = zeros(Ngrp * Nslots, 1);
            r = 0;
            for g = 1:Ngrp
                for s = 1:Nslots
                    r = r + 1;
                    slot_starts(r) = ref + (g-1)*Nslots*L + (s-1)*L;
                    slot_group(r)  = g;
                    slot_ind(r)    = s;
                end
            end

            [slot_field, n_placed] = local_match_setA(feas_days, slot_starts, NF, L);
            if Obj.Verbose, fprintf('  SetA phase-1 (no shift): %d/%d placed\n', n_placed, Ngrp*Nslots); end

            best_anchors    = ref * ones(Ngrp, 1);
            best_slot_field = slot_field;
            best_n_placed   = n_placed;

            % ----- Phase 2: if not all placed, try shifting ONE group -----
            if n_placed < Ngrp * Nslots
                if Obj.Verbose, fprintf('  SetA phase-2: trying single-group shifts...\n'); end
                MaxShift = 30;
                found    = false;
                for g_shift = 1:Ngrp
                    for sh = [-MaxShift:-1, 1:MaxShift]
                        new_anchor_g = ref + sh;
                        if new_anchor_g < Obj.First_day, continue; end
                        if new_anchor_g + Nslots*L - 1 > Obj.Last_day, continue; end

                        anchors = ref * ones(Ngrp, 1);
                        anchors(g_shift) = new_anchor_g;

                        % Build slot_starts under this shift
                        sst = zeros(Ngrp * Nslots, 1);
                        r2  = 0;
                        for g = 1:Ngrp
                            for s = 1:Nslots
                                r2 = r2 + 1;
                                sst(r2) = anchors(g) + (s-1)*L;
                            end
                        end

                        [sf, np] = local_match_setA(feas_days, sst, NF, L);
                        if np == Ngrp * Nslots
                            best_anchors    = anchors;
                            best_slot_field = sf;
                            best_n_placed   = np;
                            slot_starts     = sst;
                            found = true;
                            Obj.SetA_shifted_group = g_shift;
                            if Obj.Verbose
                                fprintf('  SetA shift: group %d shifted by %+d days; all placed\n', ...
                                        g_shift, sh);
                            end
                            break
                        elseif np > best_n_placed
                            best_anchors    = anchors;
                            best_slot_field = sf;
                            best_n_placed   = np;
                            slot_starts     = sst;
                            Obj.SetA_shifted_group = g_shift;
                        end
                    end
                    if found, break; end
                end
            end

            % ----- Materialise schedule (full 48 rows; Field==0 for empty) -----
            % Translate row indices (from matchpairs) to field IDs.
            total = Ngrp * Nslots;
            field_ids_in_slots = zeros(total, 1);
            for r2 = 1:total
                if best_slot_field(r2) > 0
                    field_ids_in_slots(r2) = Obj.SetA_fields.Field(best_slot_field(r2));
                end
            end

            Schedule = table();
            Schedule.category(1:total, 1) = {'A'};
            Schedule.group = slot_group;
            Schedule.ind   = slot_ind;
            Schedule.start = slot_starts;
            Schedule.end   = slot_starts + L - 1;
            Schedule.Field = field_ids_in_slots;

            Obj.Schedule = [Obj.Schedule; Schedule];

            n_unplaced = sum(field_ids_in_slots == 0);
            unplaced = setdiff(Obj.SetA_fields.Field, field_ids_in_slots(field_ids_in_slots > 0));
            ok = (n_unplaced == 0);

            if ok
                if Obj.Verbose
                    fprintf('  SetA: all %d fields placed (anchors %s)\n', ...
                            total, mat2str(best_anchors'));
                end
            else
                if Obj.Verbose
                    fprintf('  SetA: %d/%d placed; unplaced fields: %s\n', ...
                            total - n_unplaced, total, mat2str(unplaced(:)'));
                end
            end
        end


        function [ok, unplaced] = schedule_SetC_v4(Obj, variant_idx)
            % Place 16 SetC fields per the variant's C_blocks distribution.
            % Each block B_k = (W_k, W_{k+1}, W_{k+2}) covers 135 days
            % starting at Full_windows.start(k). C_blocks(k) is the number
            % of SetC fields to place in that block.
            %
            % Strategy:
            %   Build a slot list of length sum(C_blocks) = 16, each slot
            %   tagged with its block k. matchpairs over (SetC fields x
            %   slots), cost 0 if the field has 135-day continuous visibility
            %   starting at Full_windows.start(k), BIG otherwise. Bail on
            %   any unmatched slot/field.

            L_super = 3 * Obj.Min_window;
            V = Obj.Variants(variant_idx);

            % Build slot list
            C_blocks = V.C_blocks;
            slot_block = zeros(sum(C_blocks), 1);
            r = 0;
            for k = 1:numel(C_blocks)
                for i = 1:C_blocks(k)
                    r = r + 1;
                    slot_block(r) = k;
                end
            end
            Nslots = r;

            % SetC field pool (16 fields, extinction-sorted)
            NF = height(Obj.SetC_fields);
            if NF ~= Nslots
                warning('schedule_SetC_v4: SetC pool size %d != slot count %d', NF, Nslots);
            end

            % Bounds check: ensure the variant's blocks don't extend past Last_day
            for k = unique(slot_block)'
                if Obj.Full_windows.start(k) + L_super - 1 > Obj.Last_day
                    error('schedule_SetC_v4: block %d (variant %s) extends past Last_day', ...
                          k, V.name);
                end
            end

            % Build cost matrix: rows = field indices, cols = slots
            BIG = 1e6;
            COST = BIG * ones(NF, Nslots);
            nrows = size(Obj.Cont_visibilty_per_field, 1);
            for r2 = 1:NF
                f = Obj.SetC_fields.Field(r2);
                use1gap = Obj.SetC_fields.use1gap(r2);
                if use1gap
                    col = Obj.Cont_visibilty_per_field_1dgap(:, f);
                else
                    col = Obj.Cont_visibilty_per_field(:, f);
                end
                for s = 1:Nslots
                    sday = Obj.Full_windows.start(slot_block(s));
                    if sday >= 1 && sday <= nrows && col(sday) >= L_super
                        COST(r2, s) = 0;
                    end
                end
            end

            pairs = matchpairs(COST, BIG);
            n_placed = 0;
            field_to_slot = zeros(NF, 1);
            for r3 = 1:size(pairs, 1)
                if COST(pairs(r3, 1), pairs(r3, 2)) < BIG
                    field_to_slot(pairs(r3, 1)) = pairs(r3, 2);
                    n_placed = n_placed + 1;
                end
            end

            unplaced = Obj.SetC_fields.Field(field_to_slot == 0);
            ok = (n_placed == Nslots);

            if Obj.Verbose
                fprintf('  SetC[%s]: %d/%d placed\n', V.name, n_placed, Nslots);
                if ~ok
                    fprintf('  SetC[%s]: unplaced fields: %s\n', V.name, mat2str(unplaced(:)'));
                end
            end

            if ~ok, return; end

            % Materialize Schedule rows. Convention (matches v3):
            %   category = 'C'
            %   group    = 10 + block_index    (so groups 11..16)
            %   ind      = within-block counter (1..C_blocks(k))
            %   start    = Full_windows.start(block)
            %   end      = Full_windows.start(block) + 3*L - 1
            SetC_Schedule = table();
            block_counter = zeros(numel(C_blocks), 1);
            for r4 = 1:NF
                s = field_to_slot(r4);
                if s == 0, continue; end
                k = slot_block(s);
                block_counter(k) = block_counter(k) + 1;
                row = table();
                row.category = {'C'};
                row.group    = 10 + k;
                row.ind      = block_counter(k);
                row.start    = Obj.Full_windows.start(k);
                row.end      = Obj.Full_windows.start(k) + L_super - 1;
                row.Field    = Obj.SetC_fields.Field(r4);
                SetC_Schedule = [SetC_Schedule; row]; %#ok<AGROW>
                Obj.SetC_fields.scheudled(r4) = 1;
            end

            Obj.Schedule = [Obj.Schedule; SetC_Schedule];
        end


        function [ok, unplaced] = schedule_SetB_v4(Obj, variant_idx)
            % Place 16 SetB fields per the variant's B_blocks distribution.
            % For each block B_k, B_blocks(k) fields are placed; each field
            % has a "1"-window assigned per the variant's ones_at(k, :)
            % counts. The field's W45 (high-cadence) goes at the "1"-window;
            % its two W90 (4-day cadence) blocks go at the other two windows
            % of B_k.
            %
            % Strategy:
            %   Build a slot list of length sum(B_blocks) = 16, each slot
            %   tagged with (block k, ones-window j). matchpairs over
            %   (SetB fields x slots), cost 0 if the field has 135-day
            %   continuous visibility starting at Full_windows.start(k).

            L_super = 3 * Obj.Min_window;
            V = Obj.Variants(variant_idx);
            B_blocks = V.B_blocks;
            ones_at  = V.ones_at;

            % Build slot list with (block, ones-window) tags
            slot_block = zeros(sum(B_blocks), 1);
            slot_ones  = zeros(sum(B_blocks), 1);
            r = 0;
            for k = 1:numel(B_blocks)
                % Iterate ones_at(k, :): how many fields at each window
                for j = 1:size(ones_at, 2)
                    for n = 1:ones_at(k, j)
                        r = r + 1;
                        slot_block(r) = k;
                        slot_ones(r)  = j;
                    end
                end
                % Sanity: ensure ones_at(k, :) sums to B_blocks(k)
                if sum(ones_at(k, :)) ~= B_blocks(k)
                    error('schedule_SetB_v4: variant %s: ones_at(%d,:) sum != B_blocks(%d)', ...
                          V.name, k, k);
                end
            end
            Nslots = r;

            NF = height(Obj.SetB_fields);
            if NF ~= Nslots
                warning('schedule_SetB_v4: SetB pool size %d != slot count %d', NF, Nslots);
            end

            % Cost matrix: rows = field indices, cols = slots
            BIG = 1e6;
            COST = BIG * ones(NF, Nslots);
            nrows = size(Obj.Cont_visibilty_per_field, 1);
            for r2 = 1:NF
                f = Obj.SetB_fields.Field(r2);
                use1gap = Obj.SetB_fields.use1gap(r2);
                if use1gap
                    col = Obj.Cont_visibilty_per_field_1dgap(:, f);
                else
                    col = Obj.Cont_visibilty_per_field(:, f);
                end
                for s = 1:Nslots
                    sday = Obj.Full_windows.start(slot_block(s));
                    if sday >= 1 && sday <= nrows && col(sday) >= L_super
                        COST(r2, s) = 0;
                    end
                end
            end

            pairs = matchpairs(COST, BIG);
            n_placed = 0;
            field_to_slot = zeros(NF, 1);
            for r3 = 1:size(pairs, 1)
                if COST(pairs(r3, 1), pairs(r3, 2)) < BIG
                    field_to_slot(pairs(r3, 1)) = pairs(r3, 2);
                    n_placed = n_placed + 1;
                end
            end

            unplaced = Obj.SetB_fields.Field(field_to_slot == 0);
            ok = (n_placed == Nslots);

            if Obj.Verbose
                fprintf('  SetB[%s]: %d/%d placed\n', V.name, n_placed, Nslots);
                if ~ok
                    fprintf('  SetB[%s]: unplaced fields: %s\n', V.name, mat2str(unplaced(:)'));
                end
            end

            if ~ok, return; end

            % Materialize: for each matched field, emit 3 rows
            %   B_45 at the ones-window (j) with group = 100 + j
            %   B_90 at the two other windows of block k with group = 200 + W
            % Within each group, ind is a counter (1, 2, ...) shared across
            % B-fields from all blocks contributing to that group.
            Ninds = numel(Obj.Full_windows.start);
            b45_counter = zeros(Ninds, 1);
            b90_counter = zeros(Ninds, 1);

            SetB_Schedule = table();
            for r4 = 1:NF
                s = field_to_slot(r4);
                if s == 0, continue; end
                k = slot_block(s);
                j = slot_ones(s);
                f = Obj.SetB_fields.Field(r4);

                % B_45 row at window j
                b45_counter(j) = b45_counter(j) + 1;
                T45 = table();
                T45.category = {'B_45'};
                T45.group    = 100 + j;
                T45.ind      = b45_counter(j);
                T45.start    = Obj.Full_windows.start(j);
                T45.end      = Obj.Full_windows.end(j);
                T45.Field    = f;

                % The two W90 windows are the other two of block k
                w_block = [k, k+1, k+2];
                if ~any(w_block == j)
                    error('schedule_SetB_v4: variant %s: ones-window %d not in block %d (covers %s)', ...
                          V.name, j, k, mat2str(w_block));
                end
                w90s = w_block(w_block ~= j);
                if numel(w90s) ~= 2
                    error('schedule_SetB_v4: variant %s: expected 2 W90 windows, got %d', ...
                          V.name, numel(w90s));
                end
                rows_to_append = T45;
                for w = w90s
                    b90_counter(w) = b90_counter(w) + 1;
                    T90 = table();
                    T90.category = {'B_90'};
                    T90.group    = 200 + w;
                    T90.ind      = b90_counter(w);
                    T90.start    = Obj.Full_windows.start(w);
                    T90.end      = Obj.Full_windows.end(w);
                    T90.Field    = f;
                    rows_to_append = [rows_to_append; T90]; %#ok<AGROW>
                end

                SetB_Schedule = [SetB_Schedule; rows_to_append]; %#ok<AGROW>
                Obj.SetB_fields.scheudled(r4) = 1;
            end

            Obj.Schedule = [Obj.Schedule; SetB_Schedule];
        end



        function shuffled = shuffle_on_failure(Obj, unplaced_A, unplaced_C, unplaced_B)
            % Apply ONE shuffle in response to scheduling failure.
            %
            % Strategy (option (c) from Q3):
            %   1. SetB failure: swap the unplaced SetB field with the
            %      lowest-Av_ext field from Long_leftover_fields.
            %      If none left in leftover -> swap with a SetC field
            %      (the highest-Av_ext one in SetC).
            %   2. SetC failure: same, but for SetC.
            %   3. SetA failure: take the worst-fit Long_leftover field
            %      currently in SetA and push it back into SetC or SetB.
            %      (Conservative attempt; may not always work.)
            %
            % Returns true if a shuffle was applied, false if no
            % candidate moves remain.

            shuffled = false;

            % --- Case 1: SetB failure ---
            if ~isempty(unplaced_B)
                f_bad = unplaced_B(1);   % move the first unplaceable
                shuffled = Obj.swap_out_setB(f_bad);
                if shuffled
                    if Obj.Verbose, fprintf('  SHUFFLE: removed f%d from SetB\n', f_bad); end
                    return
                end
            end

            % --- Case 2: SetC failure ---
            if ~isempty(unplaced_C)
                f_bad = unplaced_C(1);
                shuffled = Obj.swap_out_setC(f_bad);
                if shuffled
                    if Obj.Verbose, fprintf('  SHUFFLE: removed f%d from SetC\n', f_bad); end
                    return
                end
            end

            % --- Case 3: SetA failure ---
            if ~isempty(unplaced_A)
                % Identify Long_leftover fields currently in SetA
                long_in_A = intersect(Obj.SetA_fields.Field, ...
                                       Obj.Long_leftover_fields.Field);
                if isempty(long_in_A)
                    return  % cannot swap
                end
                % Try the one matching an unplaced SetA field, if any
                cand = intersect(unplaced_A, long_in_A);
                if isempty(cand)
                    f_bad = long_in_A(1);   % arbitrary
                else
                    f_bad = cand(1);
                end
                shuffled = Obj.swap_setA_long_to_C_or_B(f_bad);
                if shuffled
                    if Obj.Verbose, fprintf('  SHUFFLE: moved f%d from SetA-long to SetC/B\n', f_bad); end
                end
            end
        end


        function ok = swap_out_setB(Obj, f_remove)
            % Remove f_remove from SetB. Replace with the lowest-extinction
            % candidate from Long_leftover_fields (and if empty, with the
            % highest-extinction field currently in SetC).
            ok = false;
            if ~isempty(Obj.Long_leftover_fields)
                % Pick lowest Av_ext from leftover
                [~, idx] = min(Obj.Long_leftover_fields.Av_ext);
                f_in  = Obj.Long_leftover_fields.Field(idx);
                row_in = Obj.Long_leftover_fields(idx, :);

                % Remove f_remove from SetB; add f_in
                row_remove = Obj.SetB_fields(Obj.SetB_fields.Field == f_remove, :);
                Obj.SetB_fields(Obj.SetB_fields.Field == f_remove, :) = [];
                Obj.SetB_fields = [Obj.SetB_fields; row_in];

                % f_remove moves to Long_leftover (and SetA)
                Obj.Long_leftover_fields(idx, :) = [];
                Obj.Long_leftover_fields = [Obj.Long_leftover_fields; row_remove];

                % Update SetA: remove f_in (no longer in SetA), add f_remove
                Obj.SetA_fields(Obj.SetA_fields.Field == f_in, :) = [];
                Obj.SetA_fields = [Obj.SetA_fields; row_remove];

                Obj.SetA_fields = sortrows(Obj.SetA_fields, 'max_window');
                Obj.SetB_fields = sortrows(Obj.SetB_fields, 'max_window');
                ok = true;
                return
            end

            % Fallback: swap f_remove with a SetC field
            if ~isempty(Obj.SetC_fields)
                [~, idx] = max(Obj.SetC_fields.Av_ext);
                f_in = Obj.SetC_fields.Field(idx);
                row_in = Obj.SetC_fields(idx, :);

                row_remove = Obj.SetB_fields(Obj.SetB_fields.Field == f_remove, :);
                Obj.SetB_fields(Obj.SetB_fields.Field == f_remove, :) = [];
                Obj.SetB_fields = [Obj.SetB_fields; row_in];

                Obj.SetC_fields(idx, :) = [];
                Obj.SetC_fields = [Obj.SetC_fields; row_remove];

                Obj.SetB_fields = sortrows(Obj.SetB_fields, 'max_window');
                Obj.SetC_fields = sortrows(Obj.SetC_fields, 'max_window');
                ok = true;
            end
        end


        function ok = swap_out_setC(Obj, f_remove)
            % Remove f_remove from SetC. Replace with lowest-extinction from
            % Long_leftover, or fall back to a SetB swap.
            ok = false;
            if ~isempty(Obj.Long_leftover_fields)
                [~, idx] = min(Obj.Long_leftover_fields.Av_ext);
                f_in  = Obj.Long_leftover_fields.Field(idx);
                row_in = Obj.Long_leftover_fields(idx, :);

                row_remove = Obj.SetC_fields(Obj.SetC_fields.Field == f_remove, :);
                Obj.SetC_fields(Obj.SetC_fields.Field == f_remove, :) = [];
                Obj.SetC_fields = [Obj.SetC_fields; row_in];

                Obj.Long_leftover_fields(idx, :) = [];
                Obj.Long_leftover_fields = [Obj.Long_leftover_fields; row_remove];

                Obj.SetA_fields(Obj.SetA_fields.Field == f_in, :) = [];
                Obj.SetA_fields = [Obj.SetA_fields; row_remove];

                Obj.SetA_fields = sortrows(Obj.SetA_fields, 'max_window');
                Obj.SetC_fields = sortrows(Obj.SetC_fields, 'max_window');
                ok = true;
                return
            end

            % Fallback: swap with a SetB field
            if ~isempty(Obj.SetB_fields)
                [~, idx] = max(Obj.SetB_fields.Av_ext);
                f_in = Obj.SetB_fields.Field(idx);
                row_in = Obj.SetB_fields(idx, :);

                row_remove = Obj.SetC_fields(Obj.SetC_fields.Field == f_remove, :);
                Obj.SetC_fields(Obj.SetC_fields.Field == f_remove, :) = [];
                Obj.SetC_fields = [Obj.SetC_fields; row_in];

                Obj.SetB_fields(idx, :) = [];
                Obj.SetB_fields = [Obj.SetB_fields; row_remove];

                Obj.SetB_fields = sortrows(Obj.SetB_fields, 'max_window');
                Obj.SetC_fields = sortrows(Obj.SetC_fields, 'max_window');
                ok = true;
            end
        end


        function ok = swap_setA_long_to_C_or_B(Obj, f_remove)
            % f_remove is a Long_leftover field currently in SetA. Try to
            % push it into SetC (preferred), else SetB. Swap with the
            % highest-extinction field there to keep the swap "favorable."
            ok = false;
            row_remove = Obj.SetA_fields(Obj.SetA_fields.Field == f_remove, :);

            % Try SetC
            if ~isempty(Obj.SetC_fields)
                [~, idx] = max(Obj.SetC_fields.Av_ext);
                f_swap = Obj.SetC_fields.Field(idx);
                row_swap = Obj.SetC_fields(idx, :);

                Obj.SetC_fields(idx, :) = [];
                Obj.SetC_fields = [Obj.SetC_fields; row_remove];

                Obj.SetA_fields(Obj.SetA_fields.Field == f_remove, :) = [];
                Obj.SetA_fields = [Obj.SetA_fields; row_swap];

                Obj.Long_leftover_fields(Obj.Long_leftover_fields.Field == f_remove, :) = [];
                % f_swap was not in Long_leftover before (it was in SetC);
                % don't add it to Long_leftover.

                Obj.SetA_fields = sortrows(Obj.SetA_fields, 'max_window');
                Obj.SetC_fields = sortrows(Obj.SetC_fields, 'max_window');
                ok = true;
            end
        end


        function [ok, inds_open, inds_2move] = clean_inds_before_setD(Obj)
            % Pre-clean step: before placing SetD, ensure inds_2move is empty
            % by moving SetA fields (from non-shifted groups only) from
            % over-budget inds to inds in inds_open.
            %
            % Updates Obj.Schedule in-place via local_apply_setA_moves.
            % Returns the post-clean (inds_open, inds_2move) plus an ok
            % flag (true iff inds_2move was fully cleared).

            [inds_open, inds_2move, ok_occ] = local_compute_slot_occupancy(...
                Obj.Schedule, Obj.Full_windows, Obj.Daily_LCS_slots);

            if ~ok_occ
                ok = false;
                return
            end

            if isempty(inds_2move)
                ok = true;
                return
            end

            % Identify eligible SetA rows (non-shifted groups only)
            schedA_mask = strcmp(Obj.Schedule.category, 'A') & ...
                          Obj.Schedule.Field > 0 & ...
                          Obj.Schedule.group ~= Obj.SetA_shifted_group;
            eligible_rows  = find(schedA_mask);
            eligible_field = Obj.Schedule.Field(eligible_rows);
            eligible_ind   = Obj.Schedule.ind(eligible_rows);

            % Precompute per-row feasibility (which inds the field can occupy)
            nrows = size(Obj.Cont_visibilty_per_field, 1);
            Ninds = numel(Obj.Full_windows.start);
            L     = Obj.Min_window;
            feas_target = cell(numel(eligible_rows), 1);
            for r = 1:numel(eligible_rows)
                f = eligible_field(r);
                ug_row = find(Obj.SetA_fields.Field == f, 1);
                if isempty(ug_row), use1gap = false;
                else,                use1gap = Obj.SetA_fields.use1gap(ug_row);
                end
                if use1gap
                    col = Obj.Cont_visibilty_per_field_1dgap(:, f);
                else
                    col = Obj.Cont_visibilty_per_field(:, f);
                end
                fi = [];
                for k = 1:Ninds
                    s = Obj.Full_windows.start(k);
                    if s >= 1 && s <= nrows && col(s) >= L
                        fi(end+1) = k; %#ok<AGROW>
                    end
                end
                feas_target{r} = fi;
            end

            % matchpairs over (eligible SetA rows at over-budget inds) x
            % (inds_open entries). Cost 0 iff the field is at an
            % over-budget ind and visible at the open ind.
            Nrows = numel(eligible_rows);
            Ncols = numel(inds_open);
            BIG = 1e6;
            COST = BIG * ones(Nrows, Ncols);
            for r = 1:Nrows
                src = eligible_ind(r);
                if ~any(inds_2move == src), continue; end
                fi = feas_target{r};
                for j = 1:Ncols
                    if any(fi == inds_open(j))
                        COST(r, j) = 0;
                    end
                end
            end
            pairs = matchpairs(COST, BIG);

            % Apply matches honoring multiplicity of inds_2move / inds_open.
            % Snapshot inds_open BEFORE we start consuming, because pairs(r,2)
            % indexes into the original column ordering; after deletions the
            % running inds_open shifts.
            inds_open_snapshot = inds_open;
            moves = {};
            for r = 1:size(pairs, 1)
                row = pairs(r, 1);
                col = pairs(r, 2);
                if COST(row, col) >= BIG, continue; end
                src = eligible_ind(row);
                dst = inds_open_snapshot(col);
                pos_2move = find(inds_2move == src, 1);
                if isempty(pos_2move), continue; end
                pos_open  = find(inds_open == dst, 1);
                if isempty(pos_open), continue; end
                f = eligible_field(row);
                inds_2move(pos_2move) = [];
                inds_open(pos_open)   = [];
                moves{end+1} = struct('Field', f, 'from_ind', src, 'to_ind', dst); %#ok<AGROW>
            end

            % Commit moves to Obj.Schedule
            Obj.Schedule = local_apply_setA_moves(Obj.Schedule, moves, ...
                                                   Obj.Full_windows);

            if Obj.Verbose
                fprintf('  pre-clean: %d SetA moves; inds_open=%s inds_2move=%s\n', ...
                        numel(moves), mat2str(inds_open), mat2str(inds_2move));
            end

            ok = isempty(inds_2move);
        end


        function schedule_SetD_v4(Obj, Args)
            % Place up to NumToPlace SetD fields, in WG5 rank order, while
            % strictly preserving every A/B/C field already in the schedule.
            %
            % Pre-clean (clean_inds_before_setD): if the A/B/C schedule has
            % any inds_2move (over-budget inds), move SetA fields from those
            % inds to inds_open to clear them. Only non-shifted-group SetA
            % fields are eligible.
            %
            % Per-rank loop:
            %   Case A: rank-r field has visibility at some ind in inds_open
            %     -> place directly; consume that inds_open entry.
            %   Case B: rank-r field has visibility at some ind k_setD that
            %     is NOT in inds_open, but a non-shifted-group SetA field at
            %     k_setD can move to some k_open in inds_open
            %     -> swap: the SetA field moves to k_open, the SetD field
            %        takes k_setD. inds_open loses k_open. inds_2move
            %        stays empty (one in, one out at k_setD).
            %     Pick the SetA field with the FEWEST alternative inds
            %     (preserve flexibility of less-flexible fields by using
            %     them up first).
            %   Case C: neither (A) nor (B) succeeds -> skip this rank.
            %
            % Termination:
            %   - Success: 4 SetD fields placed.
            %   - Otherwise: ranking exhausted; warn with how many succeeded.
            arguments
                Obj
                Args.Rank        = [79 12 48 28 16 88 55 32 213 26]
                Args.NumToPlace  = 4
            end

            L     = Obj.Min_window;
            Ninds = numel(Obj.Full_windows.start);

            % ---- Pre-clean: clear inds_2move via SetA moves ----
            [ok_pre, inds_open, inds_2move] = Obj.clean_inds_before_setD();
            if ~ok_pre
                if Obj.Verbose
                    fprintf('  SetD: pre-clean failed (inds_2move=%s); aborting SetD\n', ...
                            mat2str(inds_2move));
                end
                Obj.inds_open  = inds_open;
                Obj.inds_2move = inds_2move;
                warning('schedule_SetD_v4: cannot clear inds_2move before SetD; SetD skipped.');
                return
            end

            if Obj.Verbose
                fprintf('  SetD: post-clean inds_open=%s inds_2move=%s\n', ...
                        mat2str(inds_open), mat2str(inds_2move));
            end

            % ---- Build SetD ranking table ----
            Nrank = numel(Args.Rank);
            T = table();
            T.Field      = Args.Rank(:);
            T.Av_ext     = Obj.AllSky.A_U(T.Field);
            T.scheudled  = false(Nrank, 1);
            T.ind        = zeros(Nrank, 1);

            % Precompute per-SetD-field feasible inds (strict visibility)
            nrows = size(Obj.Cont_visibilty_per_field, 1);
            setD_feas_inds = cell(Nrank, 1);
            for i = 1:Nrank
                f = T.Field(i);
                if f < 1 || f > size(Obj.Cont_visibilty_per_field, 2)
                    setD_feas_inds{i} = [];
                    continue
                end
                col = Obj.Cont_visibilty_per_field(:, f);
                fi  = [];
                for k = 1:Ninds
                    s = Obj.Full_windows.start(k);
                    if s >= 1 && s <= nrows && col(s) >= L
                        fi(end+1) = k; %#ok<AGROW>
                    end
                end
                setD_feas_inds{i} = fi;
            end

            placed       = 0;
            slot_in_setD = 0;

            for i = 1:Nrank
                if placed >= Args.NumToPlace, break; end

                fi = setD_feas_inds{i};
                if isempty(fi)
                    if Obj.Verbose
                        fprintf('  SetD: rank %d (f%d) has no feasible ind; skip\n', ...
                                i, T.Field(i));
                    end
                    continue
                end

                % ---- Case A: direct placement at an open ind ----
                placed_ind = 0;
                case_A     = false;
                for k = fi
                    if any(inds_open == k)
                        placed_ind = k;
                        case_A     = true;
                        break
                    end
                end

                if case_A
                    pos = find(inds_open == placed_ind, 1);
                    inds_open(pos) = [];
                    Obj = local_commit_setD(Obj, T.Field(i), placed_ind, ...
                                            slot_in_setD + 1);
                    placed       = placed + 1;
                    slot_in_setD = slot_in_setD + 1;
                    T.scheudled(i) = true;
                    T.ind(i)       = placed_ind;
                    if Obj.Verbose
                        fprintf('  SetD: f%d (rank %d) -> ind %d (Case A: direct)\n', ...
                                T.Field(i), i, placed_ind);
                    end
                    continue
                end

                % ---- Case B: bump a SetA field out of a feasible ind ----
                %
                % For each k_setD in feas_inds(i):
                %   Find SetA fields at k_setD (non-shifted group) that have
                %   visibility at some k_open in inds_open.
                %   Among all such candidates, pick the one with FEWEST
                %   alternative inds (preserve flexible fields for later).
                best_choice = struct('found', false, 'k_setD', 0, ...
                                     'k_open', 0, 'f_setA', 0, ...
                                     'sched_row', 0, 'flex', Inf);

                schedA_mask = strcmp(Obj.Schedule.category, 'A') & ...
                              Obj.Schedule.Field > 0 & ...
                              Obj.Schedule.group ~= Obj.SetA_shifted_group;
                schedA_rows = find(schedA_mask);

                for k_setD = fi
                    % Look up SetA rows at this k_setD
                    rows_here = schedA_rows(Obj.Schedule.ind(schedA_rows) == k_setD);
                    for r_idx = 1:numel(rows_here)
                        sched_row = rows_here(r_idx);
                        f_setA    = Obj.Schedule.Field(sched_row);
                        % Feasibility of this SetA field at the various
                        % open inds (and overall flex count)
                        ug_row = find(Obj.SetA_fields.Field == f_setA, 1);
                        if isempty(ug_row), use1gap = false;
                        else,                use1gap = Obj.SetA_fields.use1gap(ug_row);
                        end
                        if use1gap
                            col = Obj.Cont_visibilty_per_field_1dgap(:, f_setA);
                        else
                            col = Obj.Cont_visibilty_per_field(:, f_setA);
                        end

                        valid_k_open = [];
                        for kk = unique(inds_open)
                            s = Obj.Full_windows.start(kk);
                            if s >= 1 && s <= nrows && col(s) >= L
                                valid_k_open(end+1) = kk; %#ok<AGROW>
                            end
                        end
                        if isempty(valid_k_open), continue; end

                        % flex = number of inds where this SetA field could go
                        % (including k_setD itself); the smaller, the
                        % more "constrained" - prefer to use it now.
                        flex = 0;
                        for kk = 1:Ninds
                            s = Obj.Full_windows.start(kk);
                            if s >= 1 && s <= nrows && col(s) >= L
                                flex = flex + 1;
                            end
                        end
                        if flex < best_choice.flex
                            best_choice.found     = true;
                            best_choice.k_setD    = k_setD;
                            best_choice.k_open    = valid_k_open(1);
                            best_choice.f_setA    = f_setA;
                            best_choice.sched_row = sched_row;
                            best_choice.flex      = flex;
                        end
                    end
                end

                if best_choice.found
                    % Apply the SetA move: row's ind/start/end updated;
                    % group assigned as lowest available >= 7 at the new ind.
                    target_ind = best_choice.k_open;
                    new_group  = local_assign_moved_setA_group(...
                                    Obj.Schedule, target_ind, 7);
                    Obj.Schedule.ind(best_choice.sched_row)   = target_ind;
                    Obj.Schedule.start(best_choice.sched_row) = Obj.Full_windows.start(target_ind);
                    Obj.Schedule.end(best_choice.sched_row)   = Obj.Full_windows.end(target_ind);
                    Obj.Schedule.group(best_choice.sched_row) = new_group;

                    % Consume the k_open entry
                    pos = find(inds_open == best_choice.k_open, 1);
                    inds_open(pos) = [];

                    % Place SetD at k_setD (no change to inds_open: one
                    % SetA out, one SetD in, ind stays exactly at 11)
                    Obj = local_commit_setD(Obj, T.Field(i), best_choice.k_setD, ...
                                            slot_in_setD + 1);
                    placed       = placed + 1;
                    slot_in_setD = slot_in_setD + 1;
                    T.scheudled(i) = true;
                    T.ind(i)       = best_choice.k_setD;

                    if Obj.Verbose
                        fprintf(['  SetD: f%d (rank %d) -> ind %d (Case B: bump SetA f%d ' ...
                                 'from ind %d to ind %d, group=%d, flex=%d)\n'], ...
                                T.Field(i), i, best_choice.k_setD, ...
                                best_choice.f_setA, best_choice.k_setD, ...
                                best_choice.k_open, new_group, best_choice.flex);
                    end
                    continue
                end

                % ---- Case C: skip this rank ----
                if Obj.Verbose
                    fprintf('  SetD: rank %d (f%d) cannot be placed (no Case A, no Case B); skip\n', ...
                            i, T.Field(i));
                end
            end

            % Commit final state
            Obj.SetD_ranked_fields = T;
            Obj.inds_open          = inds_open;
            Obj.inds_2move         = inds_2move;   % should still be []

            if Obj.Verbose
                fprintf('  SetD: %d/%d placed; final inds_open=%s inds_2move=%s\n', ...
                        placed, Args.NumToPlace, ...
                        mat2str(inds_open), mat2str(inds_2move));
            end

            if placed < Args.NumToPlace
                warning('schedule_SetD_v4: only %d/%d SetD fields placed (ranking exhausted).', ...
                        placed, Args.NumToPlace);
            end
        end


        function calcDailySchedule(Obj)
            % Build Obj.Daily_schedule from Obj.Schedule.
            %
            % Daily_schedule is a (Last_day - First_day + 1) x Daily_LCS_slots
            % matrix; entry (d, s) = field ID observed in slot s on day d
            % (NaN if no observation).
            %
            % Conventions:
            %   - SetA, SetB_45, SetD: observed daily (every day in the range).
            %   - SetB_90, SetC: observed every 4 days, with offset based on
            %     mod(ind, 4) so that the four cadence offsets within an ind
            %     interleave to share daily slots.
            %
            % After populating, the function adjusts within-day slot order so
            % each field lands in a slot where it is actually visible across
            % the full daily window (using vis2d_day_field_ALL / vis3d_slot_day_field).
            arguments
                Obj
            end

            % Base daily schedule
            Obj.Daily_schedule = nan(Obj.Last_day - Obj.First_day + 1, ...
                                      Obj.Daily_LCS_slots);

            for i = 1:height(Obj.Schedule)
                for curr_d = Obj.Schedule.start(i):Obj.Schedule.end(i)
                    if ~(any(strcmp(Obj.Schedule.category{i}, {'C','B_90'})) ...
                            && mod((curr_d - Obj.Schedule.start(i) + 1), 4) ...
                               ~= mod(Obj.Schedule.ind(i), 4))
                        open_slot = find(isnan(Obj.Daily_schedule(curr_d, :)), 1);
                        % If no open slot, silently no-op (v1 behavior:
                        % assigning to an empty index has no effect in MATLAB).
                        Obj.Daily_schedule(curr_d, open_slot) = Obj.Schedule.Field(i);
                    end
                end
            end

            % Adjust within-day order so fields land in slots where they
            % are visible (according to the per-slot visibility table).
            for d = 1:height(Obj.Daily_schedule)
                if any(~isnan(Obj.Daily_schedule(d, :)))
                    currFields = Obj.Daily_schedule(d, ~isnan(Obj.Daily_schedule(d, :)));
                    fieldInds2move = find(~Obj.vis2d_day_field_ALL(d, currFields));
                    if ~isempty(fieldInds2move)
                        fields2move = currFields(fieldInds2move);
                        possible_newInds = squeeze(Obj.vis3d_slot_day_field(:, d, currFields(fieldInds2move)));
                        possible_newInds((numel(currFields)+1):end, :) = false;
                        for ii = 1:numel(fields2move)
                            % Guard: if no feasible slot for this field in
                            % [1..numel(currFields)], skip its reordering
                            % (the field stays where it is).
                            first_hit = find(possible_newInds(:, ii), 1);
                            if isempty(first_hit), continue; end
                            if first_hit < (Obj.Daily_LCS_slots / 2)
                                newInd = first_hit;
                            else
                                newInd = find(possible_newInds(:, ii), 1, 'last');
                            end
                            field2move = fields2move(ii);
                            currFields(currFields == field2move) = [];
                            currFields = [currFields(1:(newInd-1)), field2move, currFields(newInd:end)]; %#ok<AGROW>
                            possible_newInds(newInd, (ii+1):end) = false;
                        end
                        Obj.Daily_schedule(d, ~isnan(Obj.Daily_schedule(d, :))) = currFields;
                    end
                end
            end
        end


        % === Plotting and inspection tools (not part of core scheduling) ===

        function plotSchedule(Obj, Args)
            % Plot the Schedule. Categories A/B/C/D are placed at different
            % y-positions; moved SetA rows (group >= 7) appear in the SetA
            % band when group is 7, but may visually drift into the SetB
            % band for groups 8+ since the plot was designed for the
            % original 6-group layout. This is cosmetic only.
            arguments
                Obj
                Args.AxesHandle       = [];   % appUIAxes

                Args.SeperateCatColor = 'r';
                Args.SavePlot         = false;
                Args.FN2SavePlot      = 'LCS_Schedule';
                Args.FormatSavePlot   = 'png';
                Args.PlotTitle        = 'LCS schedule';
            end

            if isempty(Args.AxesHandle)
                h = figure('WindowStyle', 'docked', 'Color', [1 1 1]); clf;
                ax = axes(h);
            else
                ax = Args.AxesHandle;
            end

            hold(ax, 'on');
            box(ax, 'on');

            % Sort the schedule by category
            schedule = sortrows(Obj.Schedule, 'category');

            % --- SetC: assign each group its own line via interval coloring ---
            % In v4, each SetC group covers a 3-window block (135 days) and
            % within-group ind counts the fields in that block. Groups whose
            % time spans overlap must occupy DIFFERENT lines; groups that
            % don't overlap can share a line. Within a line, the inds are
            % stacked vertically. The SetC y-band is [16, 20] (4 units).
            setc_rows = find(strcmp(schedule.category, 'C'));
            line_of_group = containers.Map('KeyType', 'double', 'ValueType', 'double');
            Nlines = 0;
            if ~isempty(setc_rows)
                setc_groups = schedule.group(setc_rows);
                [unique_groups, ~] = unique(setc_groups);
                Ng = numel(unique_groups);

                g_start = zeros(Ng, 1);
                g_end   = zeros(Ng, 1);
                for gi = 1:Ng
                    rows_g = setc_rows(setc_groups == unique_groups(gi));
                    g_start(gi) = schedule.start(rows_g(1));
                    g_end(gi)   = schedule.end(rows_g(1));
                end

                % Greedy interval coloring: process groups in start-day order
                [~, order] = sort(g_start);
                line_end_day = [];   % latest end day on each line (grows as needed)
                for ki = 1:numel(order)
                    k = order(ki);
                    L = find(line_end_day < g_start(k), 1);
                    if isempty(L)
                        line_end_day(end+1) = g_end(k); %#ok<AGROW>
                        L = numel(line_end_day);
                    else
                        line_end_day(L) = g_end(k);
                    end
                    line_of_group(unique_groups(k)) = L;
                end
                Nlines = numel(line_end_day);
            end
            y_base_C  = 17;    % first SetC line at y=17
            line_h_C  = 1;     % fixed 1-unit spacing between lines (17, 18, 19, ...)

            for i = 1:height(schedule)
                switch schedule.category{i}
                    case 'A'
                        plot(ax, [schedule.start(i), schedule.end(i)], ...
                             ones(2,1) * schedule.group(i), '-k');
                    case 'C'
                        L = line_of_group(schedule.group(i));
                        % Within each line, inds are clustered tightly with
                        % small offsets (matching the B_90 visual style):
                        % offset = (ind-1)/8 -> 0, 0.125, 0.25, 0.375 ...
                        y_C = y_base_C + (L - 1) * line_h_C + ...
                              (schedule.ind(i) - 1) / 8;
                        plot(ax, [schedule.start(i), schedule.end(i)], ...
                             ones(2,1) * y_C, '--k');
                    case 'B_45'
                        plot(ax, [schedule.start(i), schedule.end(i)], ...
                             ones(2,1) * (mod((schedule.ind(i)-1), 4) + ...
                                          floor((schedule.ind(i)-1) / 4) + 9), '-k');
                    case 'B_90'
                        plot(ax, [schedule.start(i), schedule.end(i)], ...
                             ones(2,1) * (mod((schedule.ind(i)-1), 4) / 8 + ...
                                          floor((schedule.ind(i)-1) / 4) + 13), '--k');
                    case 'D'
                        plot(ax, [schedule.start(i), schedule.end(i)], ...
                             ones(2,1) * schedule.group(i) - 300 + 20, '-k');
                end
            end

            ylim(ax, [0, 25]);
            set(ax, 'Ydir', 'reverse');
            set(ax, 'YTickLabels', []);

            % Category band markers
            yline(ax, 8,  ['-' Args.SeperateCatColor], 'Category A (48 fields, 45d window @ 1d cadance)');
            yline(ax, 16, ['-' Args.SeperateCatColor], 'Category B (16 fields, 45d window @ 1d cadance + 90d window @ 4d cadance)');
            yline(ax, 20, ['-' Args.SeperateCatColor], 'Category C  (16 fields, 135d window @ 4d cadance)');
            yline(ax, 25, ['-' Args.SeperateCatColor], 'Category D (4 fields, 45d window @ 1d cadance)');

            xlabel(ax, sprintf('Time since %s [days]', Obj.StartDate));
            title(ax, Args.PlotTitle);
            hold(ax, 'off');

            if Args.SavePlot
                saveas(ax, Args.FN2SavePlot, Args.FormatSavePlot);
            end
        end


        function plotCatB(Obj, Args)
            % Plot category B Schedule per field (one row per SetB field,
            % showing the 45d W45 (solid) and the two 90d W90 blocks (dashed)).
            arguments
                Obj
                Args.AxesHandle       = [];

                Args.SeperateCatColor = 'k';
                Args.SavePlot         = false;
                Args.FN2SavePlot      = 'CatB_Schedule';
                Args.FormatSavePlot   = 'png';
                Args.PlotTitle        = 'Category B Schedule per Field';
            end

            if isempty(Args.AxesHandle)
                h = figure('WindowStyle', 'docked', 'Color', [1 1 1]); clf;
                ax = axes(h);
            else
                ax = Args.AxesHandle;
            end
            hold(ax, 'on');
            box(ax, 'on');

            for i = 1:Obj.SetBnumel
                Ind = find(Obj.Schedule.Field == Obj.SetB_fields.Field(i));
                for j = 1:numel(Ind)
                    if strcmp(Obj.Schedule.category(Ind(j)), 'B_45')
                        plot(ax, [Obj.Schedule.start(Ind(j)), Obj.Schedule.end(Ind(j))], ...
                             ones(2,1) * i, '-k');
                    else
                        plot(ax, [Obj.Schedule.start(Ind(j)), Obj.Schedule.end(Ind(j))], ...
                             ones(2,1) * i, '--k');
                    end
                end
            end

            ylim(ax, [0.5, 16.5]);
            xlabel(ax, sprintf('Time since %s [days]', Obj.StartDate));
            title(ax, Args.PlotTitle);
            hold(ax, 'off');

            if Args.SavePlot
                saveas(ax, Args.FN2SavePlot, Args.FormatSavePlot);
            end
        end

    end


    % ========================== STATIC ==========================
    methods (Static)
        function Result = unitTest()
            upLCS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'LCS'); %#ok<NASGU>
            Result = true;
        end
    end

end


% ===== local helper functions ========================================

function T = local_make_field_table(field_ids, AllSky_AU, use1gap, longest_strict, longest_1dgap)
    % Build a standard field table from a list of field IDs.
    field_ids = field_ids(:);
    T = table();
    T.Field      = field_ids;
    T.Av_ext     = AllSky_AU(field_ids);
    T.use1gap    = use1gap(field_ids)';
    T.max_window = longest_strict(field_ids)';
    T.max_window(T.use1gap) = longest_1dgap(field_ids(T.use1gap))';
    T.scheudled(:) = false;
end


function [slot_field, n_placed] = local_match_setA(feas_days, slot_starts, NF, L) %#ok<INUSD>
    % Solve the SetA placement as a single max-cardinality bipartite
    % matching: NF SetA fields vs numel(slot_starts) slots.
    % Edge (i, j) exists iff feas_days{i} contains slot_starts(j).
    % Returns slot_field as Nslot x 1 column where slot_field(j) is the
    % SetA-table ROW INDEX of the field placed in slot j (0 = empty).
    % The caller is responsible for translating index -> field ID.
    Nslot = numel(slot_starts);
    BIG   = 1e6;
    COST  = BIG * ones(Nslot, NF);
    for j = 1:Nslot
        s = slot_starts(j);
        for i = 1:NF
            if any(feas_days{i} == s)
                COST(j, i) = 0;
            end
        end
    end
    pairs = matchpairs(COST, BIG);
    slot_field = zeros(Nslot, 1);
    n_placed   = 0;
    for r = 1:size(pairs, 1)
        if COST(pairs(r,1), pairs(r,2)) < BIG
            slot_field(pairs(r,1)) = pairs(r,2);
            n_placed = n_placed + 1;
        end
    end
end


function [unp_C_best, unp_B_best] = local_pick_most_common_unplaced(unp_C_lists, unp_B_lists)
    % Aggregate unplaced field lists across all variants and return the
    % single most-common field for SetC and SetB respectively (option b).
    %
    % Inputs:
    %   unp_C_lists  - cell array of column vectors, one per variant tried,
    %                  containing fields that SetC failed to place
    %   unp_B_lists  - same for SetB
    % Outputs:
    %   unp_C_best   - column vector with at most 1 element (the most
    %                  common SetC-unplaced field across variants)
    %   unp_B_best   - same for SetB
    %
    % If one set's max-count is strictly higher than the other, only that
    % set's winner is returned and the other is empty. This forces
    % shuffle_on_failure to act on the more-blocked set. If counts tie,
    % both are returned (SetB will be tried first by shuffle_on_failure).

    [fc, nc] = local_unplaced_winner(unp_C_lists);
    [fb, nb] = local_unplaced_winner(unp_B_lists);

    if nc > nb
        unp_C_best = fc;
        unp_B_best = [];
    elseif nb > nc
        unp_C_best = [];
        unp_B_best = fb;
    else
        % Tie (including both empty): return both (or both empty)
        unp_C_best = fc;
        unp_B_best = fb;
    end
end


function [field_win, max_cnt] = local_unplaced_winner(lists)
    % Aggregate a cell array of column vectors and return the
    % most-frequently-appearing field id and its count. Returns
    % ([], 0) if all lists are empty.
    all_f = [];
    for k = 1:numel(lists)
        all_f = [all_f; lists{k}(:)]; %#ok<AGROW>
    end
    if isempty(all_f)
        field_win = [];
        max_cnt = 0;
        return
    end
    uf = unique(all_f);
    cnt = zeros(numel(uf), 1);
    for k = 1:numel(uf)
        cnt(k) = sum(all_f == uf(k));
    end
    [max_cnt, idx] = max(cnt);
    field_win = uf(idx);
end


function [inds_open, inds_2move, ok] = local_compute_slot_occupancy(Schedule, Full_windows, Daily_LCS_slots)
    % Compute per-ind slot occupancy from a Schedule table. Convention:
    %
    %   filled(k) = nA(k) + nB45(k) + n4(k)/4
    %
    %   where:
    %     nA(k)   = SetA  rows with ind == k
    %     nB45(k) = B_45  rows with group == 100+k  (ind is a within-group counter)
    %     n4(k)   = (B_90 rows with group == 200+k) +
    %               (SetC rows whose super-window covers ind k)
    %
    % The 4-day-cadence pool (n4) MUST be divisible by 4 in each ind: the
    % four cadence offsets (mod 4) must be filled to share one daily slot.
    % If divisibility fails in any ind, we emit a (non-Verbose-gated)
    % warning and return ok=false; the caller is expected to abort.
    %
    % Outputs:
    %   inds_open(j)  - lists ind values with repetition; an ind k with
    %                   (11 - filled(k)) open slots appears that many times
    %   inds_2move(j) - lists inds (with repetition) that are over-filled;
    %                   an ind k with (filled(k) - 11) excess slots appears
    %                   that many times
    %   ok            - true if the divisibility check passed
    Ninds = numel(Full_windows.start);
    nA   = zeros(1, Ninds);
    nB45 = zeros(1, Ninds);
    nB90 = zeros(1, Ninds);
    nC   = zeros(1, Ninds);

    if isempty(Schedule) || height(Schedule) == 0
        ok = true;
        inds_open  = repelem(1:Ninds, Daily_LCS_slots);
        inds_2move = [];
        return
    end

    for r = 1:height(Schedule)
        cat = Schedule.category{r};
        ind = Schedule.ind(r);
        grp = Schedule.group(r);
        if strcmp(cat, 'A')
            if ind >= 1 && ind <= Ninds, nA(ind) = nA(ind) + 1; end
        elseif strcmp(cat, 'B_45')
            % SetB rows use the v1 convention: ind is a within-group
            % counter (not the Full_windows ind). The Full_windows ind
            % is encoded in group (= 100 + Full_windows ind for B_45).
            fw_ind = grp - 100;
            if fw_ind >= 1 && fw_ind <= Ninds, nB45(fw_ind) = nB45(fw_ind) + 1; end
        elseif strcmp(cat, 'B_90')
            % Similarly for B_90: group = 200 + Full_windows ind.
            fw_ind = grp - 200;
            if fw_ind >= 1 && fw_ind <= Ninds, nB90(fw_ind) = nB90(fw_ind) + 1; end
        elseif strcmp(cat, 'C')
            % A SetC row's ind is the local slot within the super-window
            % (1..8), and its 'start' day is the super-window start.
            % Find which Full_windows ind the start matches, then mark
            % three consecutive inds as covered (start_ind .. start_ind+2).
            s = Schedule.start(r);
            start_ind = find(Full_windows.start == s, 1);
            if isempty(start_ind), continue; end
            for kk = start_ind:start_ind+2
                if kk >= 1 && kk <= Ninds, nC(kk) = nC(kk) + 1; end
            end
        end
        % SetD rows: ignored here. SetD placement happens AFTER this
        % function is first called; if SetD rows are already in the
        % Schedule, they should be excluded from occupancy calculations
        % (their contribution is tracked separately via inds_2move).
    end

    % Divisibility check
    ok = true;
    n4 = nB90 + nC;
    for k = 1:Ninds
        if mod(n4(k), 4) ~= 0
            warning(['local_compute_slot_occupancy: ind %d has n4 = %d ' ...
                     '(nB90=%d, nC=%d), not divisible by 4. Aborting.'], ...
                    k, n4(k), nB90(k), nC(k));
            ok = false;
        end
    end
    if ~ok
        inds_open  = [];
        inds_2move = [];
        return
    end

    filled = nA + nB45 + n4 / 4;

    inds_open  = [];
    inds_2move = [];
    for k = 1:Ninds
        if filled(k) < Daily_LCS_slots
            inds_open = [inds_open, repmat(k, 1, Daily_LCS_slots - filled(k))]; %#ok<AGROW>
        elseif filled(k) > Daily_LCS_slots
            inds_2move = [inds_2move, repmat(k, 1, filled(k) - Daily_LCS_slots)]; %#ok<AGROW>
        end
    end
end


function Schedule = local_apply_setA_moves(Schedule, moves, Full_windows)
    % Apply a list of (Field, from_ind, to_ind) move structs to the
    % Schedule's SetA rows. Each move updates the row's ind / start / end
    % and reassigns the group to the lowest available value >= 7 at the
    % new ind (to distinguish moved fields from original groups 1-6).
    % If a SetA field appears in multiple rows (shouldn't happen), only
    % the first matching row is updated per move.
    for k = 1:numel(moves)
        m = moves{k};
        row = find(strcmp(Schedule.category, 'A') & ...
                   Schedule.Field == m.Field & ...
                   Schedule.ind   == m.from_ind, 1);
        if isempty(row)
            warning('local_apply_setA_moves: cannot find SetA row for field %d at ind %d', ...
                    m.Field, m.from_ind);
            continue
        end
        % Assign group BEFORE writing the new ind, so the lookup that
        % considers existing rows at the target ind sees the current
        % schedule state (this row is still at from_ind).
        new_group = local_assign_moved_setA_group(Schedule, m.to_ind, 7);
        Schedule.ind(row)   = m.to_ind;
        Schedule.start(row) = Full_windows.start(m.to_ind);
        Schedule.end(row)   = Full_windows.end(m.to_ind);
        Schedule.group(row) = new_group;
    end
end


function new_group = local_assign_moved_setA_group(Schedule, target_ind, start_group)
    % Return the lowest integer >= start_group such that no SetA row
    % currently has (group, ind) = (new_group, target_ind). Used when
    % moving a SetA field into target_ind.
    g = start_group;
    while true
        taken = any(strcmp(Schedule.category, 'A') & ...
                    Schedule.group == g & ...
                    Schedule.ind   == target_ind);
        if ~taken, break; end
        g = g + 1;
    end
    new_group = g;
end


function Obj = local_commit_setD(Obj, f, k, slot_in_setD)
    % Append a SetD row to Obj.Schedule with category='D',
    % group = 300 + slot_in_setD, ind = k, start/end = Full_windows(k).
    row = table();
    row.category = {'D'};
    row.group    = 300 + slot_in_setD;
    row.ind      = k;
    row.start    = Obj.Full_windows.start(k);
    row.end      = Obj.Full_windows.end(k);
    row.Field    = f;
    Obj.Schedule = [Obj.Schedule; row];
end


function counts = consecutive_trues_cols(M)
    % CONSECUTIVE_TRUES_COLS  Count consecutive-true run length per column.
    %
    %   counts(i,j) = number of consecutive true values starting at (i,j)
    %   going downward in column j of the logical matrix M.
    %
    %   Inlined into LcsHelper_v4.m (was an external helper) so the class
    %   does not depend on a separate file on the MATLAB path.
    [nRows, nCols] = size(M);
    counts = zeros(nRows, nCols);

    % Last row
    counts(nRows, :) = M(nRows, :);

    % Walk upward
    for i = nRows-1:-1:1
        counts(i, :) = M(i, :) .* (1 + counts(i+1, :));
    end
end


function M = fill_isolated_gaps(M)
    % FILL_ISOLATED_GAPS  Fill single-false gaps along dim 1 in a logical matrix.
    %
    %   M = fill_isolated_gaps(M) finds, in each column of the logical
    %   matrix M, every triplet of consecutive rows equal to [true; false; true]
    %   and sets the middle false to true (turning 1 0 1 into 1 1 1).
    %
    %   Operates column-wise; vectorised, no loops.
    %
    %   Inlined into LcsHelper_v4.m (was an external helper) so the class
    %   does not depend on a separate file on the MATLAB path.
    arguments
        M (:,:) logical
    end

    if size(M, 1) < 3
        return
    end

    % Mask of middle rows that sit in a 1-0-1 pattern
    gap = M(1:end-2, :) & ~M(2:end-1, :) & M(3:end, :);

    % Set those middle entries to true
    M([false(1, size(M,2)); gap; false(1, size(M,2))]) = true;
end
