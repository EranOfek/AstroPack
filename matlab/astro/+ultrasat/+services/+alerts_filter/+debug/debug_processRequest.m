%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : ultrasat/+services/+alerts_filter/+debug/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : Debug function for ultrasat.services.alerts_filter.processRequest
%==========================================================================

function debug_processRequest()
    fprintf('--- Debugging processRequest ---\n');

    debug_processFilterLvc();
end

% -------------------------------------------------------------------------

function debug_processFilterLvc()
    
    try
    
    catch ME
        fprintf('\nException in debug_processFilterLvc: %s\n', ME.message);
        for s = 1:length(ME.stack)
            fprintf('  at %s (line %d)\n', ME.stack(s).name, ME.stack(s).line);
        end
    end
    
    fprintf('=== TEST COMPLETE ===\n\n');
end

