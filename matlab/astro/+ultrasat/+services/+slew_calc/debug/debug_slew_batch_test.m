% =========================================================================
%  ULTRASAT SOC — Slew Batch Processing Test (flat API)
%  Author : Chen Tishler (2025), updated for flat API (2026)
%
%  Tests processSlewBatch() / processRequest() with flat input (pairs).
%  Output: message, result, results (array of slew, direct).
% =========================================================================

clear; clc;

fprintf('\n=== ULTRASAT SOC: Slew Batch Test (flat API) ===\n');

% -------------------------------------------------------------------------
% Create pairs: from/to structs
% -------------------------------------------------------------------------
pairs = [
    struct('from', struct('ra', 10.5,  'dec', -20.0, 'roll', 0), 'to', struct('ra', 15.8,  'dec', -22.1, 'roll', 0))
    struct('from', struct('ra', 50.0,  'dec', 10.0,  'roll', 0), 'to', struct('ra', 60.0,  'dec', 15.0,  'roll', 0))
    struct('from', struct('ra', 120.5, 'dec', 30.0,  'roll', 0), 'to', struct('ra', 121.0, 'dec', 31.0,  'roll', 0))
];

item = struct('action', 'slew_batch', 'pairs', pairs);
item.time = '2028-07-01T12:00:00Z';

fprintf('\nInput (flat): action=slew_batch, %d pairs\n', numel(pairs));

% -------------------------------------------------------------------------
% Run processRequest
% -------------------------------------------------------------------------
fprintf('Running processRequest...\n');
out = ultrasat.services.slew_calc_service.processRequest(item);

fprintf('\nMessage: %s\n', out.message);
fprintf('Result code: %d\n', out.result);

% -------------------------------------------------------------------------
% Display results (flat: out.results)
% -------------------------------------------------------------------------
if isfield(out, 'results')
    N = numel(out.results);
    fprintf('\n=== Slew Batch Results (%d items) ===\n', N);
    for i = 1:N
        fprintf('Item %d: slew = %.2f sec, direct = %d\n', ...
            i, out.results(i).slew, out.results(i).direct);
    end
else
    fprintf('No results field in output.\n');
end

fprintf('\n=== Test completed ===\n\n');
