% =========================================================================
%  ULTRASAT SOC — Slew Batch Processing Test
%  Author : Chen Tishler (2025)
%
%  Description:
%     This script tests the processSlewJsonBatch() function.
%     It creates an input struct with 5 slew calculation requests,
%     converts it to JSON, runs the batch processor, and prints results.
%
%  Requirements:
%     - processSlewJsonBatch.m must be in MATLAB path
%     - ultrasat.tools.calcSlew must exist
%
% =========================================================================

clear; clc;

fprintf('\n=== ULTRASAT SOC: Slew Batch Test ===\n');

% -------------------------------------------------------------------------
% Create 5 test items
% -------------------------------------------------------------------------
items = [
    struct('ra1', 10.5,  'dec1', -20.0, 'ra2', 15.8,  'dec2', -22.1, 'time', '2028-07-01T12:00:00Z')
    struct('ra1', 50.0,  'dec1', 10.0,  'ra2', 60.0,  'dec2', 15.0,  'time', '2028-07-02T00:00:00Z')
    struct('ra1', 120.5, 'dec1', 30.0,  'ra2', 121.0, 'dec2', 31.0,  'time', '2028-07-02T12:00:00Z')
    struct('ra1', 200.0, 'dec1', -45.0, 'ra2', 210.0, 'dec2', -46.0, 'time', '2028-07-03T06:00:00Z')
    struct('ra1', 330.0, 'dec1', 10.0,  'ra2', 340.0, 'dec2', 15.0,  'time', '2028-07-04T00:00:00Z')
];

input_struct = struct('items', items);

% -------------------------------------------------------------------------
% Encode JSON input
% -------------------------------------------------------------------------
json_in = jsonencode(input_struct);

fprintf('\nInput JSON:\n%s\n\n', json_in);

% -------------------------------------------------------------------------
% Run the batch processing function
% -------------------------------------------------------------------------
fprintf('Running processSlewJsonBatch...\n');
result_struct = processSlewJsonBatch(json_in);

fprintf('\nProcess message: %s\n', result_struct.message);
fprintf('Result code: %d\n', result_struct.result);

% -------------------------------------------------------------------------
% Decode output JSON
% -------------------------------------------------------------------------
try
    output_data = jsondecode(strrep(result_struct.json_text, '\"', '"'));
catch ex
    fprintf('Error decoding JSON: %s\n', ex.message);
    return;
end

% -------------------------------------------------------------------------
% Display per-item results
% -------------------------------------------------------------------------
if isfield(output_data, 'results')
    N = numel(output_data.results);
    fprintf('\n=== Slew Batch Results (%d items) ===\n', N);
    for i = 1:N
        slew_time = output_data.results(i).slew_time;
        direct_slew = output_data.results(i).direct_slew;
        fprintf('Item %d: SlewTime = %.2f sec, Direct = %d\n', ...
            i, slew_time, direct_slew);
    end
else
    fprintf('No results field found in output JSON.\n');
end

fprintf('\n=== Test completed ===\n\n');
