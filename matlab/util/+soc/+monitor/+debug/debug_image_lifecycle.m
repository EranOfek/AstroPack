%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_image_lifecycle.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor image started and done records
%==========================================================================

function debug_image_lifecycle()
    % debug_image_lifecycle  Write image_started and image_done records.
    %
    % Example:
    %   soc.monitor.debug.debug_image_lifecycle();
    fprintf('--- debug_image_lifecycle ---\n');
    soc.monitor.debug.debug_init();
    Filename = 'debug_image_001.fits';
    ImageId = 'img_debug_001';
    Info = struct('telescope', 'ULTRASAT');
    fprintf('filename: %s, image_id: %s\n', Filename, ImageId);
    soc.monitor.image_started(Filename, Info);
    soc.monitor.image_done(ImageId, Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_image_lifecycle done ---\n\n');
end
