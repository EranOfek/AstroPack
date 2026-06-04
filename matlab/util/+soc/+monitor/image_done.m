%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.image_done.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write image processing done monitoring record
%==========================================================================

function image_done(ImageId, Info)
    % image_done  Write image processing done record.
    %
    % Example:
    %   soc.monitor.image_done('img_001', struct());
    arguments
        ImageId (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindImageLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusDone, ...
        message = "Image processing done", ...
        image_id = ImageId, ...
        event_code = soc.monitor.MonitorConst.EventImageDone, ...
        data = Info);
    Client.writeRecord(Record);
end
