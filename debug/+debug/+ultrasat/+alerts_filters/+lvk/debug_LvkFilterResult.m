%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : +debug/+ultrasat/+alerts_filters/+lvk/debug_LvkFilterResult.m
% Author      : Chen Tishler
% Created     : 12/05/2026
% Updated     : 24/05/2026
% Description : Debug script for ultrasat.alerts_filters.lvk.models.LvkFilterResult
%
% Run by      : debug.ultrasat.alerts_filters.lvk.debug_LvkFilterResult()
%==========================================================================

function debug_LvkFilterResult()
    fprintf('--- Debugging LvkFilterResult ---\n');

    debugDefaultConstructor();    
end

% -------------------------------------------------------------------------

function debugDefaultConstructor()
    fprintf('\n--- Default constructor ---\n');
    result = ultrasat.alerts_filters.lvk.models.LvkFilterResult();
    disp(result);
end

% -------------------------------------------------------------------------

