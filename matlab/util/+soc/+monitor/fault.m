%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.fault.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write fault monitoring record with event code
%==========================================================================

function fault(EventCode, Message, Info)
    % fault  Write fault monitoring record.
    arguments
        EventCode (1,1) string
        Message (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindFault, ...
        severity = soc.monitor.MonitorConst.SeverityError, ...
        status = soc.monitor.MonitorConst.StatusFailed, ...
        message = Message, ...
        event_code = EventCode, ...
        data = Info);
    Client.writeRecord(Record);
end
