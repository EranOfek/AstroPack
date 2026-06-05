%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_init.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor init and client configuration
%==========================================================================

function debug_init()
    % debug_init  Initialize monitor client and print configuration.
    %
    % Example:
    %   soc.monitor.debug.debug_init();
    fprintf('--- debug_init ---\n');
    ConfigFilename = soc.monitor.debug.createDebugConfigFile();
    soc.monitor.reset();
    Client = soc.monitor.init(ConfigFilename);
    fprintf('Config file: %s\n', ConfigFilename);
    disp(soc.monitor.MonitorConfig.toStruct(Client.Config));
    fprintf('Instance ID: %s\n', Client.InstanceId);
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    fprintf('Expected records after this step: 0\n');
    fprintf('--- debug_init done ---\n\n');
end
