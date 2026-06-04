%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.image_started.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write image processing start monitoring record
%==========================================================================

function image_started(Filename, Info)
    % image_started  Write image processing start record.
    %
    % Example:
    %   soc.monitor.image_started('IMG_001.fits', struct('telescope', 'ULTRASAT'));
    arguments
        Filename (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindImageLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusStarted, ...
        message = "Image processing started", ...
        filename = Filename, ...
        event_code = soc.monitor.MonitorConst.EventImageStarted, ...
        data = Info);
    Client.writeRecord(Record);
end
