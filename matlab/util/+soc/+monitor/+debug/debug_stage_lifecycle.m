%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_stage_lifecycle.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor stage started and done records
%==========================================================================

function debug_stage_lifecycle()
    % debug_stage_lifecycle  Write stage_started and stage_done records.
    %
    % Example:
    %   soc.monitor.debug.debug_stage_lifecycle();
    fprintf('--- debug_stage_lifecycle ---\n');
    soc.monitor.debug.debug_init();
    ImageId = 'img_debug_001';
    Info = struct('worker', 'debug');
    fprintf('image_id: %s, stage: crop\n', ImageId);
    soc.monitor.stage_started(ImageId, "crop", Info);
    soc.monitor.stage_done(ImageId, "crop", Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_stage_lifecycle done ---\n\n');
end
