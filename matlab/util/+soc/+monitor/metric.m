%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.metric.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write metric monitoring record
%==========================================================================

function metric(MetricName, MetricValue, MetricUnit, Info)
    % metric  Write metric monitoring record.
    %
    % Example:
    %   soc.monitor.metric('detections_count', 1234, 'count', struct());
    arguments
        MetricName (1,1) string
        MetricValue
        MetricUnit (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindMetric, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusOk, ...
        message = "Metric recorded", ...
        metric_name = MetricName, ...
        metric_value = MetricValue, ...
        metric_unit = MetricUnit, ...
        data = Info);
    Client.writeRecord(Record);
end
