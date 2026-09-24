%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_monitor.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Run all soc.monitor debug scenarios end-to-end
%==========================================================================

function debug_monitor()
    % debug_monitor  Run all soc.monitor debug scenarios in sequence.
    %
    % Example:
    %   soc.monitor.debug.debug_monitor();
    fprintf('=== soc.monitor debug ===\n\n');
    soc.monitor.debug.debug_init();
    runHeartbeat();
    runImageLifecycle();
    runStageLifecycle();
    runFaultMetricLog();
    soc.monitor.debug.debug_reset();
    soc.monitor.debug.printJsonlSummary();
    fprintf('\n=== soc.monitor debug done ===\n');
end

function runHeartbeat()
    fprintf('--- debug_heartbeat ---\n');
    Client = soc.monitor.get_client();
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    soc.monitor.heartbeat();
    fprintf('Expected records after this step: 1\n');
    fprintf('--- debug_heartbeat done ---\n\n');
end

function runImageLifecycle()
    fprintf('--- debug_image_lifecycle ---\n');
    Filename = 'debug_image_001.fits';
    ImageId = 'img_debug_001';
    Info = struct('telescope', 'ULTRASAT');
    fprintf('filename: %s, image_id: %s\n', Filename, ImageId);
    soc.monitor.image_started(Filename, Info);
    soc.monitor.image_done(ImageId, Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_image_lifecycle done ---\n\n');
end

function runStageLifecycle()
    fprintf('--- debug_stage_lifecycle ---\n');
    ImageId = 'img_debug_001';
    Info = struct('worker', 'debug');
    fprintf('image_id: %s, stage: crop\n', ImageId);
    soc.monitor.stage_started(ImageId, "crop", Info);
    soc.monitor.stage_done(ImageId, "crop", Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_stage_lifecycle done ---\n\n');
end

function runFaultMetricLog()
    fprintf('--- debug_fault_metric_log ---\n');
    Info = struct('test', true);
    soc.monitor.fault("pipeline.test_fault", "Test fault message", Info);
    soc.monitor.metric("detections_count", 1234, "count", Info);
    soc.monitor.log_record("debug", "Test log message", Info);
    fprintf('Expected additional records: 3\n');
    fprintf('--- debug_fault_metric_log done ---\n\n');
end
