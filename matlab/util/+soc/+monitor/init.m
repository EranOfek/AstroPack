%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.init.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Initialize monitor singleton from JSON config file
%==========================================================================

function Client = init(ConfigFilename)
    % init  Initialize soc.monitor singleton from JSON config file.
    %
    % Input  : ConfigFilename - path to monitor config JSON
    % Output : Client - soc.monitor.MonitorClient
    %
    % Example:
    %   soc.monitor.init('C:/SOC/config/monitor_config.json');
    arguments
        ConfigFilename (1,1) string
    end
    Config = soc.monitor.MonitorConfig.fromFile(ConfigFilename);
    Client = soc.monitor.MonitorClient(Config);
    soc.monitor.get_client(Client);
end
