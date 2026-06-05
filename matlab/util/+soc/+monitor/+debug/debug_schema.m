%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_schema.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor schema constants and record building
%==========================================================================

function debug_schema()
    % debug_schema  Run all schema and record-building debug checks.
    %
    % Example:
    %   soc.monitor.debug.debug_schema();
    fprintf('=== soc.monitor schema debug ===\n\n');
    debug_constants();
    debug_utc_timestamp();
    debug_make_record();
    debug_validate_record();
    fprintf('\n=== soc.monitor schema debug done ===\n');
end

function debug_constants()
    fprintf('--- debug_constants ---\n');
    fprintf('SchemaVersion: %s\n', soc.monitor.MonitorConst.SchemaVersion);
    fprintf('KindHeartbeat: %s\n', soc.monitor.MonitorConst.KindHeartbeat);
    fprintf('SeverityInfo: %s\n', soc.monitor.MonitorConst.SeverityInfo);
    fprintf('StatusAlive: %s\n', soc.monitor.MonitorConst.StatusAlive);
    fprintf('EventImageStarted: %s\n', soc.monitor.MonitorConst.EventImageStarted);
    Clearable = soc.monitor.MonitorConst.clearableEventCodes();
    fprintf('Clearable event codes (%d):\n', numel(Clearable));
    for I = 1:numel(Clearable)
        fprintf('  %s\n', Clearable{I});
    end
    fprintf('--- debug_constants done ---\n\n');
end

function debug_utc_timestamp()
    fprintf('--- debug_utc_timestamp ---\n');
    DtStr = soc.monitor.utc_now_str();
    fprintf('utc_now_str: %s\n', DtStr);
    assert(endsWith(DtStr, 'Z'), 'Timestamp must end with Z');
    fprintf('--- debug_utc_timestamp done ---\n\n');
end

function debug_make_record()
    fprintf('--- debug_make_record ---\n');
    soc.monitor.reset();
    Client = soc.monitor.MonitorClient.createDefault();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindHeartbeat, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusAlive, ...
        message = "Schema debug heartbeat");
    disp(Record);
    JsonLine = jsonencode(Record);
    fprintf('JSON line length: %d\n', strlength(JsonLine));
    fprintf('--- debug_make_record done ---\n\n');
end

function debug_validate_record()
    fprintf('--- debug_validate_record ---\n');
    ValidRecord = struct( ...
        'schema_version', '1.0', ...
        'dt', soc.monitor.utc_now_str(), ...
        'source', soc.monitor.MonitorConst.SourceMatlabPipeline, ...
        'pipeline_id', 'test', ...
        'instance_id', 'main_1', ...
        'record_kind', soc.monitor.MonitorConst.KindHeartbeat, ...
        'severity', soc.monitor.MonitorConst.SeverityInfo, ...
        'status', soc.monitor.MonitorConst.StatusAlive, ...
        'message', 'test', ...
        'data', struct() ...
    );
    InvalidRecord = struct('message', 'incomplete');
    assert(soc.monitor.MonitorConst.validateRecord(ValidRecord), 'Valid record rejected');
    assert(~soc.monitor.MonitorConst.validateRecord(InvalidRecord), 'Invalid record accepted');
    fprintf('validateRecord: OK\n');
    fprintf('--- debug_validate_record done ---\n\n');
end
