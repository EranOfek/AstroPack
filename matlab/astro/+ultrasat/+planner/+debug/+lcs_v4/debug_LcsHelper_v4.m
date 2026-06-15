%==========================================================================
% Project     : ULTRASAT Observation Planner
% File        : ultrasat.planner.debug.lcs_v4.debug_LcsHelper_v4.m
% Author      : Chen Tishler
% Created     : 07/06/2026
% Updated     : 07/06/2026
% Description : End-to-end debug for LcsHelper_v4 (full pipeline, no SetD,
%               and plotting). Plan start date: 2029-05-01.
% Run by      : ultrasat.planner.debug.debug_LcsHelper_v4()
%==========================================================================

function debug_LcsHelper_v4()

    fprintf('========== DEBUG LcsHelper_v4 ==========\n');

    debug_ensureDataPath();

    debug_LcsHelper_v4_fullPipeline();
    debug_LcsHelper_v4_noSetD();
    debug_LcsHelper_v4_plotSchedule();

    fprintf('========== DEBUG LcsHelper_v4 DONE ==========\n');
end


function debug_LcsHelper_v4_fullPipeline()
    % Full pipeline via constructor convenience flags

    fprintf('\n--- debug_LcsHelper_v4_fullPipeline ---\n');

    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV4_gridFile(), ...
        'Verbose', true, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    if isempty(Obj.Schedule) || height(Obj.Schedule) == 0
        error('debug_LcsHelper_v4_fullPipeline: Schedule is empty');
    end
    if isempty(Obj.Daily_schedule)
        error('debug_LcsHelper_v4_fullPipeline: Daily_schedule is empty');
    end

    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);

    fprintf('Schedule rows: %d (A=%d, B=%d, C=%d, D=%d)\n', ...
        height(Obj.Schedule), nA, nB, nC, nD);
    fprintf('Daily_schedule size: %s\n', mat2str(size(Obj.Daily_schedule)));
    fprintf('Variant_used: %d\n', Obj.Variant_used);
    fprintf('debug_LcsHelper_v4_fullPipeline: OK\n');
end


function debug_LcsHelper_v4_noSetD()
    % Schedule SetA/B/C only (skip SetD)

    fprintf('\n--- debug_LcsHelper_v4_noSetD ---\n');

    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV4_gridFile(), ...
        'Verbose', true, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', false);

    Obj.categorize_then_schedule('RunSetD', false);

    if isempty(Obj.Schedule) || height(Obj.Schedule) == 0
        error('debug_LcsHelper_v4_noSetD: Schedule is empty');
    end

    nA = sum(strcmp(Obj.Schedule.category, 'A') & Obj.Schedule.Field > 0);
    nB = sum(ismember(Obj.Schedule.category, {'B_45', 'B_90'}) & Obj.Schedule.Field > 0);
    nC = sum(strcmp(Obj.Schedule.category, 'C') & Obj.Schedule.Field > 0);
    nD = sum(strcmp(Obj.Schedule.category, 'D') & Obj.Schedule.Field > 0);

    if nA ~= Obj.SetAnumel
        error('debug_LcsHelper_v4_noSetD: expected %d SetA fields, got %d', ...
            Obj.SetAnumel, nA);
    end
    if nB ~= 3 * Obj.SetBnumel
        error('debug_LcsHelper_v4_noSetD: expected %d SetB rows, got %d', ...
            3 * Obj.SetBnumel, nB);
    end
    if nC ~= Obj.SetCnumel
        error('debug_LcsHelper_v4_noSetD: expected %d SetC fields, got %d', ...
            Obj.SetCnumel, nC);
    end
    if nD ~= 0
        error('debug_LcsHelper_v4_noSetD: expected no SetD fields, got %d', nD);
    end

    fprintf('Schedule rows (no SetD): %d (A=%d, B=%d, C=%d)\n', ...
        height(Obj.Schedule), nA, nB, nC);
    fprintf('debug_LcsHelper_v4_noSetD: OK\n');
end


function debug_LcsHelper_v4_plotSchedule()
    % Full pipeline then plot schedule and category B

    fprintf('\n--- debug_LcsHelper_v4_plotSchedule ---\n');

    Obj = ultrasat.planner.LcsHelper_v4( ...
        'StartDate', datetime('2029-05-01'), ...
        'AllSkyTable', debug_lcsHelperV4_gridFile(), ...
        'Verbose', false, ...
        'prep_before_schedule', true, ...
        'build_the_schedule', true);

    Obj.plotSchedule('PlotTitle', 'LcsHelper_v4 schedule (2029-05-01)');
    Obj.plotCatB('PlotTitle', 'LcsHelper_v4 category B (2029-05-01)');

    fprintf('plotSchedule and plotCatB: OK\n');
    fprintf('debug_LcsHelper_v4_plotSchedule: OK\n');
end


function gridFile = debug_lcsHelperV4_gridFile()
    gridFile = fullfile(getenv('ASTROPACK_DATA_PATH'), ...
        'ULTRASAT', 'LCS_nonoverlapping_grid_surveys.csv');
    if ~isfile(gridFile)
        error('debug_LcsHelper_v4: grid file not found: %s', gridFile);
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
