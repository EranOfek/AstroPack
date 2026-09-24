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
    % Verify Config.getApiConfig returns API configuration struct.

    fprintf('--- Testing Configuration ---\n');

    % --- Step 1: Load API config ---
    config = ultrasat.api.core.Config.getApiConfig();
    fprintf('  [SUCCESS] Configuration returned.\n');

    disp(config);

    fprintf('----------------------------------------\n\n');
end
