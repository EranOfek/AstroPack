%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_fault_metric_log.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor fault, metric, and log records
%==========================================================================

function debug_fault_metric_log()
    % debug_fault_metric_log  Write fault, metric, and log_record examples.
    %
    % Example:
    %   soc.monitor.debug.debug_fault_metric_log();
    fprintf('--- debug_fault_metric_log ---\n');
    soc.monitor.debug.debug_init();
    Info = struct('test', true);
    soc.monitor.fault("pipeline.test_fault", "Test fault message", Info);
    soc.monitor.metric("detections_count", 1234, "count", Info);
    soc.monitor.log_record("debug", "Test log message", Info);
    fprintf('Expected additional records: 3\n');
    fprintf('--- debug_fault_metric_log done ---\n\n');
end
