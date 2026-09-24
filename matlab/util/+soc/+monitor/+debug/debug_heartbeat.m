%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_heartbeat.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor heartbeat record
%==========================================================================

function debug_heartbeat()
    % debug_heartbeat  Write heartbeat record after init.
    %
    % Example:
    %   soc.monitor.debug.debug_heartbeat();
    fprintf('--- debug_heartbeat ---\n');
    soc.monitor.debug.debug_init();
    Client = soc.monitor.get_client();
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    soc.monitor.heartbeat();
    fprintf('Expected records after this step: 1\n');
    fprintf('--- debug_heartbeat done ---\n\n');
end
