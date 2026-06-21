%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +debug/+ultrasat/+api/+core/debug_Config.m
% Author      : Chen Tishler
% Created     : 01/12/2024
% Updated     : 11/02/2025
% Description : Debugging function for Config class.
%
% Run by      : debug.ultrasat.api.core.debug_Config()
%==========================================================================

function debug_Config()
    % debug_Config - Tests the configuration of the API
    fprintf('--- Testing Configuration ---\n');
    config = ultrasat.api.core.Config.getApiConfig();
    fprintf('  [SUCCESS] Configuration returned.\n');

    disp(config);

    fprintf('----------------------------------------\n\n');
end

