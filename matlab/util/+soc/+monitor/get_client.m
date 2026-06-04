%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.get_client.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Return package singleton MonitorClient
%==========================================================================

function Client = get_client(NewClient)
    % get_client  Return package singleton MonitorClient.
    %
    % Output : Client - soc.monitor.MonitorClient
    %
    % When called with one argument, sets the singleton (used by init/reset).
    % When called with no arguments, returns existing client or creates default.
    persistent TheClient;

    if nargin >= 1
        TheClient = NewClient;
        Client = TheClient;
        return;
    end

    if isempty(TheClient) || ~isvalid(TheClient)
        TheClient = soc.monitor.MonitorClient.createDefault();
    end
    Client = TheClient;
end
