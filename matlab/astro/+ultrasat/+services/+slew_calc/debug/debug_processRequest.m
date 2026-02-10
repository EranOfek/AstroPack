%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/debug/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : Debug function for processRequest
%==========================================================================

function debug_processRequest()
    debug_processRequestSingle();
    debug_processRequestBatch();
end


function debug_processRequestSingle()
    
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
        out = ultrasat.services.slew_calc.processRequest(item);
    
        %-----------------------------------------------------------
        % 3. Display and validate (flat output: message, result, slew, direct)
        %-----------------------------------------------------------
        fprintf('Status   : %s\n', out.status);        
        fprintf('message  : %s\n', out.message);

        if isfield(out, 'slew')
            fprintf('Slew (sec)    : %.3f\n', out.slew);
            fprintf('Direct        : %d\n', out.direct);
        end
    
        if ~strcmp(out.status, 'ok')
            error('processRequest returned non-ok status: %s', out.status);
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
end


function debug_processRequestBatch()
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
    out = ultrasat.services.slew_calc.processRequest(item);

    fprintf('status  : %s\n', out.status);    
    fprintf('message : %s\n', out.message);
    
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
end
