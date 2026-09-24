%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.heartbeat.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write pipeline heartbeat monitoring record
%==========================================================================

function heartbeat()
    % heartbeat  Write pipeline heartbeat monitoring record.
    %
    % Example:
    %   soc.monitor.heartbeat();
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindHeartbeat, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusAlive, ...
        message = "Pipeline heartbeat");
    Client.writeRecord(Record);
end
