% ========================================================================
% File: dev_slew_test.m
% Purpose: Developer test for processSlew() / processRequest() flat API
% Author : Chen Tishler (2025), updated for flat API (2026)
% ========================================================================

fprintf('\n=== ULTRASAT DEV TEST: processSlew (flat API) ===\n');

try
    %-----------------------------------------------------------
    % 1. Prepare input (flat: action, from, to)
    %-----------------------------------------------------------
    item = struct( ...
        'action', 'slew', ...
        'from', struct('ra', 10.5, 'dec', -20.0, 'roll', 0), ...
        'to',   struct('ra', 15.8, 'dec', -22.1, 'roll', 0) ...
    );
    item.time = '2028-07-01T12:00:00Z';

    fprintf('Input item (flat):\n');
    disp(item);

    %-----------------------------------------------------------
    % 2. Call processRequest (or processSlew directly)
    %-----------------------------------------------------------
    fprintf('\nCalling processRequest...\n');
    out = ultrasat.services.slew_calc_service.processRequest(item);

    %-----------------------------------------------------------
    % 3. Display and validate (flat output: message, result, slew, direct)
    %-----------------------------------------------------------
    fprintf('\nResult message: %s\n', out.message);
    fprintf('Result code   : %d\n', out.result);
    if isfield(out, 'slew')
        fprintf('Slew (sec)    : %.3f\n', out.slew);
        fprintf('Direct        : %d\n', out.direct);
    end

    if out.result ~= 0
        error('processRequest returned nonzero result');
    end
    if isfield(out, 'slew') && out.slew > 0
        fprintf('Test PASSED: valid slew result.\n');
    else
        fprintf('Test FAILED: invalid or missing slew.\n');
    end

catch ME
    fprintf('\nException in dev_slew_test: %s\n', ME.message);
    for s = 1:length(ME.stack)
        fprintf('  at %s (line %d)\n', ME.stack(s).name, ME.stack(s).line);
    end
end

fprintf('=== TEST COMPLETE ===\n\n');
