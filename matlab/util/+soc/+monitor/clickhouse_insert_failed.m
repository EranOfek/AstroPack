%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.clickhouse_insert_failed.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write ClickHouse insert failed monitoring record
%==========================================================================

function clickhouse_insert_failed(ImageId, Info)
    % clickhouse_insert_failed  Write ClickHouse insert failed record.
    arguments
        ImageId (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindClickhouseLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityError, ...
        status = soc.monitor.MonitorConst.StatusFailed, ...
        message = "ClickHouse insert failed", ...
        image_id = ImageId, ...
        event_code = soc.monitor.MonitorConst.EventClickhouseInsertFailed, ...
        data = Info);
    Client.writeRecord(Record);
end
