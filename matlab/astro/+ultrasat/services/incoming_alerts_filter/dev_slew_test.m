% ========================================================================
% File: dev_slew_test.m
% Purpose: Developer test for ProcessSlewJson()
% Author : Chen Tishler (2025)
% ========================================================================

fprintf('\n=== ULTRASAT DEV TEST: ProcessSlewJson ===\n');

try
    %-----------------------------------------------------------
    % 1. Prepare input data
    %-----------------------------------------------------------
    input_struct = struct( ...
        'ra1', 10.5, ...                 % degrees
        'dec1', -20.0, ...
        'ra2', 15.8, ...
        'dec2', -22.1, ...
        'time', '2028-07-01T12:00:00Z' ...
    );

    json_in = jsonencode(input_struct);
    fprintf('Input JSON:\n%s\n', json_in);

    %-----------------------------------------------------------
    % 2. Call the function under test
    %-----------------------------------------------------------
    fprintf('\nCalling ProcessSlewJson...\n');
    result_struct = processSlewJson(json_in);

    %-----------------------------------------------------------
    % 3. Display and validate the result
    %-----------------------------------------------------------
    fprintf('\nResult message: %s\n', result_struct.message);
    fprintf('Result code   : %d\n', result_struct.result);
    fprintf('Raw JSON text : %s\n', result_struct.json_text);

    if result_struct.result ~= 0
        error('ProcessSlewJson returned nonzero result');
    end

    % Remove escape characters before decoding
    json_clean = strrep(result_struct.json_text, '\"', '"');
    output_data = jsondecode(json_clean);
    
    fprintf('\nDecoded output:\n');
    disp(output_data);

    %-----------------------------------------------------------
    % 4. Validate numeric and logical outputs
    %-----------------------------------------------------------
    if isfield(output_data, 'slew_time') && isfield(output_data, 'direct_slew')
        fprintf('Slew time (sec): %.3f\n', output_data.slew_time);
        fprintf('Direct slew     : %d\n', output_data.direct_slew);
        if output_data.slew_time > 0
            fprintf('Test PASSED: valid slew time result.\n');
        else
            fprintf('Test FAILED: invalid slew time.\n');
        end
    else
        fprintf('Test FAILED: output fields missing.\n');
    end

catch ME
    fprintf('\nException in dev_slew_test: %s\n', ME.message);
    for s = 1:length(ME.stack)
        fprintf('  at %s (line %d)\n', ME.stack(s).name, ME.stack(s).line);
    end
end

fprintf('=== TEST COMPLETE ===\n\n');
