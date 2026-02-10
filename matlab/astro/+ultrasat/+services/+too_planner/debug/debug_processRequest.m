%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/debug/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : Debug function for processRequest
%==========================================================================

function debug_processRequest()
    debug_processRequestTooPlanner();
end


function debug_processRequestTooPlanner()
    try
        %-----------------------------------------------------------
        % 1. Prepare input (flat: action, filename)
        %-----------------------------------------------------------
        % Write the CSV file in the local folder with the specified header and rows
        csv_filename = fullfile(fileparts(mfilename('fullpath')), 'too_debug.csv');
        fid = fopen(csv_filename, 'w');
        % Write header
        fprintf(fid, 'UNIQ,PROBDENSITY,RA,DEC\n');
        % Write rows
        fprintf(fid, '1040,2.207529977052296e-08,56.24999999999999,12.024699180565822\n');
        fprintf(fid, '1041,1.0761150121646524e-09,59.0625,14.477512185929925\n');
        fprintf(fid, '1044,5.385870698225297e-11,61.87499999999999,16.957763300004142\n');
        fprintf(fid, '1045,3.237669037930435e-12,64.6875,19.47122063449069\n');
        fclose(fid);

        output_folder = fullfile(fileparts(mfilename('fullpath')), 'output');
        if ~exist(output_folder, 'dir')
            mkdir(output_folder);
        end

        csv_filename = fullfile(fileparts(mfilename('fullpath')), 'too_debug1.csv');

        % Prepare the struct to be saved as JSON (remove csv_filename and output_folder, as we only want the struct minus 'action' and 'filename')
        inputStruct = struct( ...
            'action', 'too_planner', ...
            'planner_name', 'Chen Tishler', ...
            'csv_filename', csv_filename, ...
            'output_folder', output_folder, ...
            'plans', struct( ...
                'label', 'fast_4', ...
                'TOOMaxTargets', 4, ...
                'TOOMinCoveredProb', 0.3, ...
                'TOOWindowDurationHours', 3, ...
                'Verbosity', 0, ...
                'DrawMaps', 1 ...
            ) ...
        );

        % Save the struct to a JSON file
        json_filename = fullfile(fileparts(mfilename('fullpath')), 'too_debug.json');
        json_str = jsonencode(inputStruct, "PrettyPrint", true);
        fid_json = fopen(json_filename, 'w');
        fwrite(fid_json, json_str, 'char');
        fclose(fid_json);

        % Prepare Input struct with just action and filename (json config file)
        Input = struct( ...
            'action', 'too_planner', ...
            'filename', json_filename ...
        );
     
        %-----------------------------------------------------------
        % 2. Call processRequest
        %-----------------------------------------------------------
        Output = ultrasat.services.too_planner.processRequest(Input);
        fprintf('status  : %s\n', Output.status);    
        fprintf('message : %s\n', Output.message);
        fprintf('summaryFileName : %s\n', Output.summaryFileName);
    catch ex
        fprintf('Exception in debug_processRequestTooPlanner: %s\n', ex.message);
        for s = 1:length(ex.stack)
            fprintf('  at %s (line %d)\n', ex.stack(s).name, ex.stack(s).line);
        end
    end
end

