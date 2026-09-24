%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_reset.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor reset and re-initialize
%==========================================================================

function debug_reset()
    % debug_reset  Clear singleton, re-init, and write one heartbeat.
    %
    % Example:
    %   soc.monitor.debug.debug_reset();
    fprintf('--- debug_reset ---\n');
    ConfigFilename = soc.monitor.debug.createDebugConfigFile();
    soc.monitor.reset();
    Client = soc.monitor.init(ConfigFilename);
    soc.monitor.heartbeat();
    fprintf('Reinitialized client, JSONL file: %s\n', Client.getJsonlFilename());
    fprintf('Expected records in new file after reset: 1\n');
    fprintf('--- debug_reset done ---\n\n');
end
