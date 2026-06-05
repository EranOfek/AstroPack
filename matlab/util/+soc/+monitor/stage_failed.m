%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.stage_failed.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write pipeline stage failed monitoring record
%==========================================================================

function stage_failed(ImageId, StageName, Info)
    % stage_failed  Write pipeline stage failed record.
    %
    % Example:
    %   soc.monitor.stage_failed('img_001', 'crop', struct('reason', Msg));
    arguments
        ImageId (1,1) string
        StageName (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindStageLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityError, ...
        status = soc.monitor.MonitorConst.StatusFailed, ...
        message = "Stage failed", ...
        image_id = ImageId, ...
        stage = StageName, ...
        event_code = soc.monitor.MonitorConst.EventStageFailed, ...
        data = Info);
    Client.writeRecord(Record);
end
