%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.image_failed.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write image processing failed monitoring record
%==========================================================================

function image_failed(ImageId, Info)
    % image_failed  Write image processing failed record.
    %
    % Example:
    %   soc.monitor.image_failed('img_001', struct('reason', 'timeout'));
    arguments
        ImageId (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindImageLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityError, ...
        status = soc.monitor.MonitorConst.StatusFailed, ...
        message = "Image processing failed", ...
        image_id = ImageId, ...
        event_code = soc.monitor.MonitorConst.EventImageFailed, ...
        data = Info);
    Client.writeRecord(Record);
end
