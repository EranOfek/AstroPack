%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% File        : +ultrasat/+alerts_filters/+lvc/+debug/debug_LvcFilterResult.m
% Author      : Chen Tishler
% Created     : 12/05/2026
% Updated     : 12/05/2026
% Description : Debug script for ultrasat.alerts_filters.lvc.models.LvcFilterResult
%==========================================================================

function debug_LvcFilterResult()
    fprintf('--- Debugging LvcFilterResult ---\n');

    debugDefaultConstructor();    
end

% -------------------------------------------------------------------------

function debugDefaultConstructor()
    fprintf('\n--- Default constructor ---\n');
    result = ultrasat.alerts_filters.lvc.models.LvcFilterResult();
    disp(result);
end

% -------------------------------------------------------------------------

