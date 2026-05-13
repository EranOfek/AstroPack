%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+too_planner/debug/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 13/05/2026
% Description : Debug function for too_planner processRequest (flat API)
%==========================================================================

function debug_processRequest()
    debug_processRequestHealth();
    debug_processRequestTooPlanner();
end


function debug_processRequestHealth()
    fprintf('\n=== TOO Planner: Health Test ===\n');
    try
        Input = struct('action', 'health');
        Output = ultrasat.services.too_planner.processRequest(Input);
        fprintf('status  : %s\n', Output.status);
        fprintf('message : %s\n', Output.message);

        if ~strcmp(Output.status, 'ok')
            error('Health check returned non-ok status: %s', Output.status);
        end
        fprintf('Test PASSED\n');
    catch ex
        fprintf('Exception: %s\n', ex.message);
        for s = 1:length(ex.stack)
            fprintf('  at %s (line %d)\n', ex.stack(s).name, ex.stack(s).line);
        end
    end
    fprintf('=== TEST COMPLETE ===\n\n');
end


function debug_processRequestTooPlanner()
    fprintf('\n=== TOO Planner: Plan Test (flat API) ===\n');
    try
        %-----------------------------------------------------------
        % 1. Create test CSV probability map
        %-----------------------------------------------------------
        csv_filename = fullfile(fileparts(mfilename('fullpath')), 'too_debug.csv');
        fid = fopen(csv_filename, 'w');
        fprintf(fid, 'UNIQ,PROBDENSITY,RA,DEC\n');
        fprintf(fid, '1040,2.207529977052296e-08,56.24999999999999,12.024699180565822\n');
        fprintf(fid, '1041,1.0761150121646524e-09,59.0625,14.477512185929925\n');
        fprintf(fid, '1044,5.385870698225297e-11,61.87499999999999,16.957763300004142\n');
        fprintf(fid, '1045,3.237669037930435e-12,64.6875,19.47122063449069\n');
        fclose(fid);

        output_folder = fullfile(fileparts(mfilename('fullpath')), 'output');
        if ~exist(output_folder, 'dir')
            mkdir(output_folder);
        end

        %-----------------------------------------------------------
        % 2. Prepare flat Input struct (matching TooPlannerParams)
        %-----------------------------------------------------------
        Input = struct( ...
            'action', 'too_planner', ...
            'planner_name', 'AK', ...
            'csv_filename', csv_filename, ...
            'output_folder', output_folder, ...
            'plans', struct( ...
                'label', 'fast_4', ...
                'TOOMaxTargets', 4, ...
                'TOOMinCoveredProb', 0.3, ...
                'TOOWindowDurationHours', 3, ...
                'Verbosity', 0, ...
                'DrawMaps', 1 ...
            ), ...
            'timeout', 300 ...
        );

        fprintf('Input (flat):\n');
        disp(Input);

        %-----------------------------------------------------------
        % 3. Call processRequest
        %-----------------------------------------------------------
        fprintf('\nCalling processRequest...\n');
        Output = ultrasat.services.too_planner.processRequest(Input);
        fprintf('status                : %s\n', Output.status);
        fprintf('message               : %s\n', Output.message);
        fprintf('summary_file          : %s\n', Output.summary_file);
        fprintf('total_plans_attempted : %d\n', Output.total_plans_attempted);
        fprintf('total_plans_succeeded : %d\n', Output.total_plans_succeeded);
        fprintf('total_plans_failed    : %d\n', Output.total_plans_failed);

        if isfield(Output, 'plans') && ~isempty(Output.plans)
            fprintf('\n=== Plan Results (%d items) ===\n', numel(Output.plans));
            for i = 1:numel(Output.plans)
                fprintf('Plan %d: run_id=%s, status=%s, exposures=%d\n', ...
                    i, Output.plans(i).run_id, Output.plans(i).status, ...
                    Output.plans(i).exposures_scheduled);
            end
        end

        if ~strcmp(Output.status, 'ok')
            error('processRequest returned non-ok status: %s', Output.status);
        end
        fprintf('Test PASSED\n');
    catch ex
        fprintf('Exception in debug_processRequestTooPlanner: %s\n', ex.message);
        for s = 1:length(ex.stack)
            fprintf('  at %s (line %d)\n', ex.stack(s).name, ex.stack(s).line);
        end
    end
    fprintf('=== TEST COMPLETE ===\n\n');
end
