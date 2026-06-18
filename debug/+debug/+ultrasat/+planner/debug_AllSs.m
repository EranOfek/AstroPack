%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_AllSs.m
% Author      : Chen Tishler
% Created     : 31/05/2026
% Description : Step-by-step debug for AllSS (All-Sky Survey) plans.
%               Code paths mirror uplanner.unitTest AllSS block.
%
% Run by      : debug.ultrasat.planner.debug_AllSs()
%==========================================================================

function debug_AllSs()

    fprintf('========== DEBUG ALLSS PLANNER ==========\n');

    debug_ensureDataPath();

    debug_AllSs_constructGrid();
    debug_AllSs_buildWeekly();
    debug_AllSs_buildSemester();

    fprintf('========== DEBUG ALLSS PLANNER DONE ==========\n');
end


function debug_AllSs_constructGrid()
    % Constructor auto-calls constructAllSSgrid; inspect UniqTarg grid

    fprintf('\n--- debug_AllSs_constructGrid ---\n');

    DitherLeg = 3.0;
    upAllSS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'AllSS', ...
        'ExtragalDitherLeg', DitherLeg);

    nTarg = height(upAllSS.UniqTarg);
    fprintf('UniqTarg rows: %d\n', nTarg);

    if ismember('DitherGroup', upAllSS.UniqTarg.Properties.VariableNames)
        nExtragal = sum(upAllSS.UniqTarg.DitherGroup > 0);
        nGalactic = sum(upAllSS.UniqTarg.DitherGroup == 0);
        fprintf('Galactic points (DitherGroup==0): %d\n', nGalactic);
        fprintf('Extragalactic dither groups (DitherGroup>0): %d\n', nExtragal);
    end

    if ~isempty(upAllSS.SchedStatus)
        fprintf('SchedStatus rows: %d\n', height(upAllSS.SchedStatus));
    end

    fprintf('debug_AllSs_constructGrid: OK\n');
end


function debug_AllSs_buildWeekly()
    % Short 7-day AllSS smoke build (first buildAllSS block in unitTest)

    fprintf('\n--- debug_AllSs_buildWeekly ---\n');

    matFile = debug_getAllSsMatPath();
    if isempty(matFile)
        return;
    end

    DitherLeg = 3.0;
    upAllSS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'AllSS', ...
        'ExtragalDitherLeg', DitherLeg, 'Load', matFile);

    upAllSS.StartTime = datetime('2028-07-01') + hours(12);
    upAllSS.EndTime = upAllSS.StartTime + days(7);
    upAllSS.DailyWindowMaxDuration = hours(24);
    upAllSS.BufferEarthDist = 8.0;
    upAllSS.ExtragalMinIntervals = [0 0 0];
    upAllSS.EmptyDay = false;

    upAllSS.buildAllSS('AllowPartial', true, 'Verbose', true, ...
        'MergeSameTargets', false, 'AverageSlew', 60);

    fprintf('Weekly plan rows: %d\n', height(upAllSS.Plan));
    fprintf('debug_AllSs_buildWeekly: OK\n');
end


function debug_AllSs_buildSemester()
    % Full-semester AllSS build after weekly segment (second unitTest block)

    fprintf('\n--- debug_AllSs_buildSemester ---\n');

    matFile = debug_getAllSsMatPath();
    if isempty(matFile)
        return;
    end

    DitherLeg = 3.0;
    upAllSS = ultrasat.planner.uplanner('AstPlanner', 'YS', 'Type', 'AllSS', ...
        'ExtragalDitherLeg', DitherLeg, 'Load', matFile);

    upAllSS.StartTime = datetime('2028-07-01') + hours(12);
    upAllSS.EndTime = upAllSS.StartTime + calmonths(6) - days(1);
    upAllSS.ExtragalMinIntervals = [1 3 9];
    upAllSS.BufferEarthDist = 0.5;
    upAllSS.BufferSunDist = 0.5;
    upAllSS.BufferMoonDist = 0.5;
    upAllSS.DailyWindowMaxDuration = hours(5.5);
    upAllSS.EmptyDay = false;

    % First week (same as unitTest intermediate step)
    upAllSS.EndTime = upAllSS.StartTime + days(7);
    upAllSS.DailyWindowMaxDuration = hours(24);
    upAllSS.BufferEarthDist = 8.0;
    upAllSS.ExtragalMinIntervals = [0 0 0];
    upAllSS.buildAllSS('AllowPartial', true, 'Verbose', true, ...
        'MergeSameTargets', false, 'AverageSlew', 60);

    fprintf('After week 1: %d plan rows\n', height(upAllSS.Plan));

    % Remainder of semester
    upAllSS.StartTime = upAllSS.EndTime;
    upAllSS.EndTime = upAllSS.StartTime + calmonths(6) - days(8);
    upAllSS.DailyWindowMaxDuration = hours(5.5);
    upAllSS.ExtragalMinIntervals = [1 3 9];
    upAllSS.buildAllSS('AllowPartial', true, 'Verbose', true, ...
        'MergeSameTargets', false, 'AverageSlew', 60);

    fprintf('After semester: %d plan rows\n', height(upAllSS.Plan));
    fprintf('debug_AllSs_buildSemester: OK\n');
end


function matPath = debug_getAllSsMatPath()
    % Resolve alss_uniq_targ.mat from ASTROPACK_DATA_PATH or unitTest path

    candidates = {};

    dataRoot = getenv('ASTROPACK_DATA_PATH');
    if ~isempty(dataRoot)
        candidates{end+1} = fullfile(dataRoot, 'ULTRASAT', 'alss_uniq_targ.mat'); %#ok<AGROW>
    end

    if ispc
        candidates{end+1} = fullfile('C:\AstroPack\matlab\data', 'ULTRASAT', 'alss_uniq_targ.mat');
    end
    candidates{end+1} = fullfile('~', 'matlab', 'data', 'ULTRASAT', 'alss_uniq_targ.mat');

    matPath = '';
    for k = 1:numel(candidates)
        p = candidates{k};
        if isfile(p)
            matPath = p;
            fprintf('Using AllSS mat file: %s\n', p);
            return;
        end
    end

    fprintf('WARNING: alss_uniq_targ.mat not found. Skipping build tests.\n');
    fprintf('Expected under ASTROPACK_DATA_PATH/ULTRASAT/ or ~/matlab/data/ULTRASAT/.\n');
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
