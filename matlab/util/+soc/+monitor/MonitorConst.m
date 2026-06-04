%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.MonitorConst.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Shared monitoring schema constants for soc.monitor
%==========================================================================

classdef MonitorConst
    % MonitorConst  Shared monitoring schema constants for soc.monitor.
    %
    % Example:
    %   soc.monitor.MonitorConst.SchemaVersion
    %   soc.monitor.MonitorConst.KindHeartbeat
    %   soc.monitor.MonitorConst.EventImageStarted

    properties (Constant)
        % Schema
        SchemaVersion = '1.0'

        % Sources
        SourceMatlabPipeline = 'matlab_pipeline'
        SourcePythonForwarder = 'python_forwarder'
        SourcePythonExternalMonitor = 'python_external_monitor'
        SourceBackendService = 'backend_service'

        % Record kinds
        KindHeartbeat = 'heartbeat'
        KindImageLifecycle = 'image_lifecycle'
        KindStageLifecycle = 'stage_lifecycle'
        KindProductLifecycle = 'product_lifecycle'
        KindClickhouseLifecycle = 'clickhouse_lifecycle'
        KindFault = 'fault'
        KindMetric = 'metric'
        KindLog = 'log'
        KindState = 'state'
        KindExternalCheck = 'external_check'
        KindSocEvent = 'soc_event'

        % Severities
        SeverityDebug = 'debug'
        SeverityInfo = 'info'
        SeverityNotice = 'notice'
        SeverityWarning = 'warning'
        SeverityError = 'error'
        SeverityCritical = 'critical'

        % Statuses
        StatusAlive = 'alive'
        StatusStarted = 'started'
        StatusDone = 'done'
        StatusFailed = 'failed'
        StatusTimeout = 'timeout'
        StatusOk = 'ok'
        StatusWarning = 'warning'
        StatusError = 'error'
        StatusCleared = 'cleared'
        StatusCreated = 'created'
        StatusSkipped = 'skipped'

        % Event codes
        EventHeartbeatTimeout = 'pipeline.heartbeat.timeout'
        EventProcessCrashed = 'pipeline.process.crashed'
        EventProcessNotRunning = 'pipeline.process.not_running'
        EventImageStarted = 'pipeline.image.started'
        EventImageDone = 'pipeline.image.done'
        EventImageFailed = 'pipeline.image.failed'
        EventStageStarted = 'pipeline.stage.started'
        EventStageDone = 'pipeline.stage.done'
        EventStageFailed = 'pipeline.stage.failed'
        EventStageTimeout = 'pipeline.stage.timeout'
        EventProductCreated = 'pipeline.product.created'
        EventProductMissing = 'pipeline.product.missing'
        EventClickhouseInsertStarted = 'pipeline.clickhouse.insert.started'
        EventClickhouseInsertDone = 'pipeline.clickhouse.insert.done'
        EventClickhouseInsertFailed = 'pipeline.clickhouse.insert.failed'
        EventDiskFull = 'pipeline.disk.full'
        EventDiskWarning = 'pipeline.disk.warning'
        EventMemoryHigh = 'pipeline.memory.high'
        EventCpuHigh = 'pipeline.cpu.high'
        EventBacklogHigh = 'pipeline.backlog.high'
        EventLogStale = 'pipeline.log.stale'
        EventExternalCheckFailed = 'pipeline.external_check.failed'
    end

    methods (Static)
        function Codes = clearableEventCodes()
            % clearableEventCodes  Event codes that represent clearable abnormal state.
            Codes = { ...
                soc.monitor.MonitorConst.EventHeartbeatTimeout, ...
                soc.monitor.MonitorConst.EventProcessNotRunning, ...
                soc.monitor.MonitorConst.EventStageTimeout, ...
                soc.monitor.MonitorConst.EventDiskFull, ...
                soc.monitor.MonitorConst.EventMemoryHigh, ...
                soc.monitor.MonitorConst.EventCpuHigh, ...
                soc.monitor.MonitorConst.EventBacklogHigh, ...
                soc.monitor.MonitorConst.EventLogStale ...
            };
        end

        function isValid = validateRecord(Record)
            % validateRecord  Simple required-field validation for a monitoring record.
            isValid = false;
            RequiredFields = { ...
                'schema_version', 'dt', 'source', 'pipeline_id', ...
                'instance_id', 'record_kind', 'severity', 'status', ...
                'message', 'data' ...
            };
            if ~isstruct(Record)
                return;
            end
            for I = 1:numel(RequiredFields)
                if ~isfield(Record, RequiredFields{I})
                    return;
                end
            end
            if ~isstruct(Record.data)
                return;
            end
            isValid = true;
        end
    end
end
