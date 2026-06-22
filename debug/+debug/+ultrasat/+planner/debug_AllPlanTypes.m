%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+planner/debug_AllPlanTypes.m
% Author      : Chen Tishler
% Created     : 21/06/2026
% Updated     : 21/06/2026
% Description : Run all plan-type debug drivers in sequence: HCS, LCS, DDT,
%               TOO, AllSS. Each step delegates to its dedicated debug_*.m
%               script under the same package.
%
% Run by      : debug.ultrasat.planner.debug_AllPlanTypes()
%==========================================================================

function debug_AllPlanTypes()

    fprintf('\n========== DEBUG ALL PLAN TYPES ==========\n');

    debug_ensureDataPath();

    Steps = {
        'HCS',   @debug.ultrasat.planner.debug_Hcs,
        'LCS',   @debug.ultrasat.planner.debug_Lcs,
        'DDT',   @debug.ultrasat.planner.debug_Ddt,
        'TOO',   @debug.ultrasat.planner.debug_Too
        %'AllSS', @debug.ultrasat.planner.debug_AllSs
    };

    nOk = 0;
    nFail = 0;

    for k = 1:size(Steps, 1)
        PlanType = Steps{k, 1};
        StepFn = Steps{k, 2};
        fprintf('\n========== STEP %d/%d: %s ==========\n', k, size(Steps, 1), PlanType);
        try
            StepFn();
            nOk = nOk + 1;
            fprintf('========== STEP %s: OK ==========\n', PlanType);
        catch ME
            nFail = nFail + 1;
            fprintf(2, '========== STEP %s: FAILED ==========\n', PlanType);
            fprintf(2, '  %s\n', ME.message);
            if ~isempty(ME.stack)
                fprintf(2, '  at %s (line %d)\n', ME.stack(1).name, ME.stack(1).line);
            end
        end
    end

    fprintf('\n========== DEBUG ALL PLAN TYPES DONE ==========\n');
    fprintf('Completed: %d OK, %d failed (of %d plan types)\n', nOk, nFail, size(Steps, 1));
end


function debug_ensureDataPath()
    % ASTROPACK_DATA_PATH required by uplanner BaseDataDir

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
