%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/+lcs_v3/debug_LcsHelper_v3.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 07/06/2026
% Description : End-to-end debug for LcsHelper_v3 (full pipeline, no SetD,
%               and plotting). Plan start date: 2029-05-01.
%
% Run by      : debug.ultrasat.planner.lcs_v3.debug_LcsHelper_v3()
%==========================================================================

function debug_LcsHelper_v3()

    fprintf('========== DEBUG LcsHelper_v3 ==========\n');

    debug_ensureDataPath();

    debug_LcsHelper_v3_fullPipeline();
    debug_LcsHelper_v3_noSetD();
    debug_LcsHelper_v3_plotSchedule();

    fprintf('========== DEBUG LcsHelper_v3 DONE ==========\n');
end


function debug_LcsHelper_v3_fullPipeline()
    % Full pipeline via constructor convenience flags

    fprintf('\n--- debug_LcsHelper_v3_fullPipeline ---\n');

    Obj = ultrasat.planner.LcsHelper_v3( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV3_gridFile(), ...
        'Verbose', true, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    if isempty(Obj.Schedule) || height(Obj.Schedule) == 0
        error('debug_LcsHelper_v3_fullPipeline: Schedule is empty');
    end
    if isempty(Obj.Daily_schedule)
        error('debug_LcsHelper_v3_fullPipeline: Daily_schedule is empty');
    end

    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);

    fprintf('Schedule rows: %d (A=%d, B=%d, C=%d, D=%d)\n', ...
        height(Obj.Schedule), nA, nB, nC, nD);
    fprintf('Daily_schedule size: %s\n', mat2str(size(Obj.Daily_schedule)));
    fprintf('SetC_start_ind: %d\n', Obj.SetC_start_ind);
    fprintf('debug_LcsHelper_v3_fullPipeline: OK\n');
end


function debug_LcsHelper_v3_noSetD()
    % Schedule SetA/B/C only (skip SetD)

    fprintf('\n--- debug_LcsHelper_v3_noSetD ---\n');

    Obj = ultrasat.planner.LcsHelper_v3( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV3_gridFile(), ...
        'Verbose', true, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', false);

    Obj.categorize_then_schedule('RunSetD', false);

    if isempty(Obj.Schedule) || height(Obj.Schedule) == 0
        error('debug_LcsHelper_v3_noSetD: Schedule is empty');
    end

    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);

    if nA ~= Obj.SetAnumel
        error('debug_LcsHelper_v3_noSetD: expected %d SetA fields, got %d', ...
            Obj.SetAnumel, nA);
    end
    if nB ~= 3 * Obj.SetBnumel
        error('debug_LcsHelper_v3_noSetD: expected %d SetB rows, got %d', ...
            3 * Obj.SetBnumel, nB);
    end
    if nC ~= Obj.SetCnumel
        error('debug_LcsHelper_v3_noSetD: expected %d SetC fields, got %d', ...
            Obj.SetCnumel, nC);
    end
    if nD ~= 0
        error('debug_LcsHelper_v3_noSetD: expected no SetD fields, got %d', nD);
    end

    fprintf('Schedule rows (no SetD): %d (A=%d, B=%d, C=%d)\n', ...
        height(Obj.Schedule), nA, nB, nC);
    fprintf('debug_LcsHelper_v3_noSetD: OK\n');
end


function debug_LcsHelper_v3_plotSchedule()
    % Full pipeline then plot schedule and category B

    fprintf('\n--- debug_LcsHelper_v3_plotSchedule ---\n');

    Obj = ultrasat.planner.LcsHelper_v3( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV3_gridFile(), ...
        'Verbose', false, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    Obj.plotSchedule('PlotTitle', 'LcsHelper_v3 schedule (2029-05-01)');
    Obj.plotCatB('PlotTitle', 'LcsHelper_v3 category B (2029-05-01)');

    fprintf('plotSchedule and plotCatB: OK\n');
    fprintf('debug_LcsHelper_v3_plotSchedule: OK\n');
end


function gridFile = debug_lcsHelperV3_gridFile()
    gridFile = fullfile(getenv('ASTROPACK_DATA_PATH'), ...
        'ULTRASAT', 'LCS_fields.csv');
    if ~isfile(gridFile)
        error('debug_LcsHelper_v3: grid file not found: %s', gridFile);
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
