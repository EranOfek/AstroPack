%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v4/debug_LcsHelper_v4_steps.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 07/06/2026
% Description : Step-by-step debug for LcsHelper_v4 pipeline methods.
%               Plan start date: 2029-05-01.
%
% Run by      : debug.ultrasat.planner.lcs_v4.debug_LcsHelper_v4_steps()
%==========================================================================

function debug_LcsHelper_v4_steps()

    fprintf('========== DEBUG LcsHelper_v4 STEPS ==========\n');

    debug_ensureDataPath();

    debug_LcsHelper_v4_visMatrix();
    debug_LcsHelper_v4_contVis();
    debug_LcsHelper_v4_categorize();
    debug_LcsHelper_v4_scheduleSteps();

    fprintf('========== DEBUG LcsHelper_v4 STEPS DONE ==========\n');
end


function debug_LcsHelper_v4_visMatrix()
    % Step 1: calc_vis_matrix

    fprintf('\n--- debug_LcsHelper_v4_visMatrix ---\n');

    Obj = debug_lcsHelperV4_newHelper(true);
    Obj.calc_vis_matrix();

    fprintf('vis_day_field size: %s\n', mat2str(size(Obj.vis_day_field)));
    fprintf('vis3d_slot_day_field size: %s\n', mat2str(size(Obj.vis3d_slot_day_field)));
    fprintf('Visible field-days (ANY): %d\n', sum(Obj.vis_day_field(:)));
    fprintf('debug_LcsHelper_v4_visMatrix: OK\n');
end


function debug_LcsHelper_v4_contVis()
    % Step 2: calc_cont_vis_windows_v2

    fprintf('\n--- debug_LcsHelper_v4_contVis ---\n');

    Obj = debug_lcsHelperV4_newHelper(true);
    Obj.calc_vis_matrix();
    Obj.calc_cont_vis_windows_v2();

    lw = Obj.Longest_window_per_field;
    fprintf('Longest_window_per_field: min=%d max=%d mean=%.1f\n', ...
        min(lw), max(lw), mean(lw));
    fprintf('Fields with max window >= Min_window (%d): %d\n', ...
        Obj.Min_window, sum(lw >= Obj.Min_window));
    fprintf('debug_LcsHelper_v4_contVis: OK\n');
end


function debug_LcsHelper_v4_categorize()
    % Step 3: prepTablesBeforeSchedule (vis + cont vis + categorize)

    fprintf('\n--- debug_LcsHelper_v4_categorize ---\n');

    Obj = debug_lcsHelperV4_newHelper(true);
    Obj.prepTablesBeforeSchedule();

    fprintf('SetA fields: %d (expected %d)\n', ...
        height(Obj.SetA_fields), Obj.SetAnumel);
    fprintf('SetB fields: %d (expected %d)\n', ...
        height(Obj.SetB_fields), Obj.SetBnumel);
    fprintf('SetC fields: %d (expected %d)\n', ...
        height(Obj.SetC_fields), Obj.SetCnumel);
    fprintf('Long_leftover fields: %d\n', height(Obj.Long_leftover_fields));

    if height(Obj.SetA_fields) ~= Obj.SetAnumel
        error('debug_LcsHelper_v4_categorize: SetA count mismatch');
    end
    if height(Obj.SetB_fields) ~= Obj.SetBnumel
        error('debug_LcsHelper_v4_categorize: SetB count mismatch');
    end
    if height(Obj.SetC_fields) ~= Obj.SetCnumel
        error('debug_LcsHelper_v4_categorize: SetC count mismatch');
    end

    fprintf('debug_LcsHelper_v4_categorize: OK\n');
end


function debug_LcsHelper_v4_scheduleSteps()
    % Steps 4-8: schedule SetA/C/B/D and build daily schedule

    fprintf('\n--- debug_LcsHelper_v4_scheduleSteps ---\n');

    Obj = debug_lcsHelperV4_newHelper(true);
    Obj.prepTablesBeforeSchedule();

    Obj.Schedule = table();

    [okA, unplacedA] = Obj.schedule_SetA_v4();
    if ~okA
        error('debug_LcsHelper_v4_scheduleSteps: schedule_SetA_v4 failed; unplaced=%s', ...
            mat2str(unplacedA(:)'));
    end
    fprintf('schedule_SetA_v4: OK (%d rows)\n', ...
        sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0));

    BaseSchedule = Obj.Schedule;
    okVariant = false;
    unplacedC = [];
    unplacedB = [];
    for VariantIdx = 1:numel(Obj.Variants)
        Obj.Schedule = BaseSchedule;
        [okC, unplacedC] = Obj.schedule_SetC_v4(VariantIdx);
        if ~okC
            continue
        end
        [okB, unplacedB] = Obj.schedule_SetB_v4(VariantIdx);
        if okB
            Obj.Variant_used = VariantIdx;
            okVariant = true;
            break
        end
    end
    if ~okVariant
        error('debug_LcsHelper_v4_scheduleSteps: no v4 variant placed SetC/SetB; unplacedC=%s unplacedB=%s', ...
            mat2str(unplacedC(:)'), mat2str(unplacedB(:)'));
    end
    fprintf('schedule_SetC_v4/schedule_SetB_v4: OK (variant %d, C=%d rows, B=%d rows)\n', ...
        Obj.Variant_used, ...
        sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0), ...
        sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0));

    Obj.schedule_SetD_v4();
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);
    fprintf('schedule_SetD_v4: %d/%d placed\n', nD, Obj.SetDnumel);

    Obj.calcDailySchedule();
    if isempty(Obj.Daily_schedule)
        error('debug_LcsHelper_v4_scheduleSteps: Daily_schedule is empty');
    end

    fprintf('calcDailySchedule: OK (size %s)\n', mat2str(size(Obj.Daily_schedule)));
    fprintf('Total schedule rows: %d\n', height(Obj.Schedule));
    fprintf('debug_LcsHelper_v4_scheduleSteps: OK\n');
end


function Obj = debug_lcsHelperV4_newHelper(Verbose)
    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV4_gridFile(), ...
        'Verbose', Verbose);
end


function gridFile = debug_lcsHelperV4_gridFile()
    gridFile = fullfile(getenv('ASTROPACK_DATA_PATH'), ...
        'ULTRASAT', 'LCS_fields.csv');
    if ~isfile(gridFile)
        error('debug_LcsHelper_v4_steps: grid file not found: %s', gridFile);
    end
end


function debug_ensureDataPath()
    if ~isempty(getenv('ASTROPACK_DATA_PATH'))
        return;
    end
    fprintf('ASTROPACK_DATA_PATH not set. Using fallback for local testing...\n');
    if ispc
        setenv('ASTROPACK_DATA_PATH', 'C:\AstroPack\matlab\data');
    else
        setenv('ASTROPACK_DATA_PATH', '~/matlab/data');
    end
end
