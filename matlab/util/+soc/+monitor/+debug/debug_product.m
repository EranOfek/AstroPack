%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_product.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug product_created monitoring record
%==========================================================================

function debug_product()
    % debug_product  Demonstrate product_created for pipeline outputs.
    %
    % Example:
    %   soc.monitor.debug.debug_product();
    fprintf('--- debug_product ---\n');
    soc.monitor.debug.debug_init();
    ImageId = 'img_debug_001';
    soc.monitor.product_created(ImageId, 'catalog', 'output/detections.cat', ...
        struct('format', 'fits', 'size_bytes', 102400));
    soc.monitor.product_created(ImageId, 'image', 'output/processed.fits', ...
        struct('format', 'fits'));
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_product done ---\n\n');
end
