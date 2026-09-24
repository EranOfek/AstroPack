%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_image_failure.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug stage_failed, fault, and image_failed records
%==========================================================================

function debug_image_failure()
    % debug_image_failure  Demonstrate error-path monitoring records.
    %
    % Example:
    %   soc.monitor.debug.debug_image_failure();
    fprintf('--- debug_image_failure ---\n');
    soc.monitor.debug.debug_init();
    ImageId = 'img_debug_fail_001';
    FitsPath = 'bad_image.fits';
    Msg = 'not enough stars for astrometry';

    soc.monitor.image_started(FitsPath, struct('telescope', 'ULTRASAT'));
    soc.monitor.stage_started(ImageId, 'astrometry', struct());
    soc.monitor.stage_failed(ImageId, 'astrometry', struct('reason', Msg));
    soc.monitor.fault(soc.monitor.MonitorConst.EventStageFailed, 'Astrometry failed', ...
        struct('image_id', ImageId, 'stage', 'astrometry', 'reason', Msg));
    soc.monitor.image_failed(ImageId, struct('reason', Msg));

    fprintf('Expected additional records: 5\n');
    fprintf('--- debug_image_failure done ---\n\n');
end
