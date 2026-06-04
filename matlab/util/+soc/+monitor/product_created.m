%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.product_created.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Write product creation monitoring record
%==========================================================================

function product_created(ImageId, ProductType, ProductFilename, Info)
    % product_created  Write product creation record.
    arguments
        ImageId (1,1) string
        ProductType (1,1) string
        ProductFilename (1,1) string
        Info struct = struct()
    end
    Info = soc.monitor.normalize_info(Info);
    Client = soc.monitor.get_client();
    Record = soc.monitor.make_record(Client, ...
        record_kind = soc.monitor.MonitorConst.KindProductLifecycle, ...
        severity = soc.monitor.MonitorConst.SeverityInfo, ...
        status = soc.monitor.MonitorConst.StatusCreated, ...
        message = "Product created", ...
        image_id = ImageId, ...
        product_type = ProductType, ...
        product_filename = ProductFilename, ...
        event_code = soc.monitor.MonitorConst.EventProductCreated, ...
        data = Info);
    Client.writeRecord(Record);
end
