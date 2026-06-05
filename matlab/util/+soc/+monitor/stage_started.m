%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.stage_started.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write pipeline stage start monitoring record
%==========================================================================

function stage_started(ImageId, StageName, Info)
    % stage_started  Write pipeline stage start record.
    %
    % Example:
    %   soc.monitor.stage_started('img_001', 'crop', struct());
    arguments
        ImageId (1,1) string
        StageName (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindStageLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusStarted, ...
        message = "Stage started", ...
        image_id = ImageId, ...
        stage = StageName, ...
        event_code = soc.monitor.MonitorConst.EventStageStarted, ...
        data = Info);
    Client.writeRecord(Record);
end
