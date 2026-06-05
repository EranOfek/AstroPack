%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_clickhouse.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug ClickHouse insert monitoring records
%==========================================================================

function debug_clickhouse()
    % debug_clickhouse  Demonstrate ClickHouse insert started, done, and failed.
    %
    % Example:
    %   soc.monitor.debug.debug_clickhouse();
    fprintf('--- debug_clickhouse ---\n');
    soc.monitor.debug.debug_init();
    ImageId = 'img_debug_ch_001';

    fprintf('Success path:\n');
    soc.monitor.clickhouse_insert_started(ImageId, struct('table', 'detections'));
    soc.monitor.clickhouse_insert_done(ImageId, struct('rows', 1000));

    fprintf('Failure path:\n');
    soc.monitor.clickhouse_insert_started(ImageId, struct('table', 'sources'));
    soc.monitor.clickhouse_insert_failed(ImageId, struct('error', 'connection timeout'));

    fprintf('Expected additional records: 4\n');
    fprintf('--- debug_clickhouse done ---\n\n');
end
