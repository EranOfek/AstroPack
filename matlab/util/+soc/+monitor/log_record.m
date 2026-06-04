%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.log_record.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write general log monitoring record
%==========================================================================

function log_record(Severity, Message, Info)
    % log_record  Write general log/debug monitoring record.
    arguments
        Severity (1,1) string
        Message (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindLog, ...
        severity = Severity, ...
        status = soc.monitor.MonitorConst.StatusOk, ...
        message = Message, ...
        data = Info);
    Client.writeRecord(Record);
end
