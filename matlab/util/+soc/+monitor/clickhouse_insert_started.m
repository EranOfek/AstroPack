%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.clickhouse_insert_started.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write ClickHouse insert start monitoring record
%==========================================================================

function clickhouse_insert_started(ImageId, Info)
    % clickhouse_insert_started  Write ClickHouse insert start record.
    arguments
        ImageId (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindClickhouseLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusStarted, ...
        message = "ClickHouse insert started", ...
        image_id = ImageId, ...
        event_code = soc.monitor.MonitorConst.EventClickhouseInsertStarted, ...
        data = Info);
    Client.writeRecord(Record);
end
