%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : +debug/+ultrasat/+services/+alerts_filter/debug_processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 24/05/2026
% Description : Debug function for ultrasat.services.alerts_filter.processRequest
%
% Run by      : debug.ultrasat.services.alerts_filter.debug_processRequest()
%==========================================================================

function debug_processRequest()
    % Debug script for alerts_filter processRequest: filter_lvk action via flat API.

    fprintf('--- Debugging processRequest ---\n');

    debug_processFilterLvk();
end

% -------------------------------------------------------------------------

function debug_processFilterLvk()
    % Exercise processRequest with action=filter_lvk; load alert file and run filter.

    try
        % Placeholder for filter_lvk request: action, alert_file, optional filter name.

    catch ME
        fprintf('\nException in debug_processFilterLvk: %s\n', ME.message);
        for s = 1:length(ME.stack)
            fprintf('  at %s (line %d)\n', ME.stack(s).name, ME.stack(s).line);
        end
    end

    fprintf('=== TEST COMPLETE ===\n\n');
end
