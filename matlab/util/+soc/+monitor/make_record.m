%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.make_record.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Build monitoring JSON record struct
%==========================================================================

function Record = make_record(Client, Args)
    % make_record  Build a monitoring JSON record struct.
    %
    % Input  : Client - soc.monitor.MonitorClient
    %          Args   - name-value fields for record_kind, severity, status,
    %                   message, data, and optional top-level fields
    % Output : Record - struct ready for jsonencode
    %
    % Example:
    %   Record = soc.monitor.make_record(Client, record_kind='heartbeat', ...
    %       severity='info', status='alive', message='Pipeline heartbeat');
    arguments
        Client (1,1) soc.monitor.MonitorClient
        Args.record_kind (1,1) string
        Args.severity (1,1) string = soc.monitor.MonitorConst.SeverityInfo
        Args.status (1,1) string
        Args.message (1,1) string
        Args.data struct = struct()
        Args.image_id string = ""
        Args.filename string = ""
        Args.stage string = ""
        Args.product_type string = ""
        Args.product_filename string = ""
        Args.event_code string = ""
        Args.metric_name string = ""
        Args.metric_value = []
        Args.metric_unit string = ""
        Args.duration_sec = []
        Args.correlation_id string = ""
        Args.parent_correlation_id string = ""
    end

    DataField = Args.data;
    if isempty(DataField)
        DataField = struct();
    end

    Record = struct( ...
        'schema_version', char(Client.Config.SchemaVersion), ...
        'dt', soc.monitor.utc_now_str(), ...
        'source', char(soc.monitor.MonitorConst.SourceMatlabPipeline), ...
        'pipeline_id', char(Client.Config.PipelineId), ...
        'instance_id', char(Client.InstanceId), ...
        'record_kind', char(Args.record_kind), ...
        'severity', char(Args.severity), ...
        'status', char(Args.status), ...
        'message', char(Args.message), ...
        'data', DataField ...
    );

    Record = addOptionalField(Record, 'image_id', Args.image_id);
    Record = addOptionalField(Record, 'filename', Args.filename);
    Record = addOptionalField(Record, 'stage', Args.stage);
    Record = addOptionalField(Record, 'product_type', Args.product_type);
    Record = addOptionalField(Record, 'product_filename', Args.product_filename);
    Record = addOptionalField(Record, 'event_code', Args.event_code);
    Record = addOptionalField(Record, 'metric_name', Args.metric_name);
    Record = addOptionalField(Record, 'metric_value', Args.metric_value);
    Record = addOptionalField(Record, 'metric_unit', Args.metric_unit);
    Record = addOptionalField(Record, 'duration_sec', Args.duration_sec);
    Record = addOptionalField(Record, 'correlation_id', Args.correlation_id);
    Record = addOptionalField(Record, 'parent_correlation_id', Args.parent_correlation_id);
end

function Record = addOptionalField(Record, FieldName, Value)
    if isempty(Value)
        return;
    end
    if isstring(Value)
        Record.(FieldName) = char(Value);
    else
        Record.(FieldName) = Value;
    end
end
