%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.stage_done.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write pipeline stage done monitoring record
%==========================================================================

function stage_done(ImageId, StageName, Info)
    % stage_done  Write pipeline stage done record.
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
        status = soc.monitor.MonitorConst.StatusDone, ...
        message = "Stage done", ...
        image_id = ImageId, ...
        stage = StageName, ...
        event_code = soc.monitor.MonitorConst.EventStageDone, ...
        data = Info);
    Client.writeRecord(Record);
end
