%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_pipeline_loop.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Example pipeline integration loop using soc.monitor
%==========================================================================

function debug_pipeline_loop()
    % debug_pipeline_loop  Realistic per-image monitoring loop for pipeline integrators.
    %
    % Example:
    %   soc.monitor.debug.debug_pipeline_loop();
    fprintf('--- debug_pipeline_loop ---\n');
    ConfigFilename = soc.monitor.debug.createDebugConfigFile();
    soc.monitor.reset();
    soc.monitor.init(ConfigFilename);
    soc.monitor.heartbeat();

    FitsPath = 'IMG_20260604_001234.fits';
    ImageId = 'IMG_20260604_001234';
    VisitInfo = struct('visit_id', 42, 'telescope', 'ULTRASAT');

    soc.monitor.image_started(FitsPath, VisitInfo);
    try
        soc.monitor.stage_started(ImageId, 'preprocess', struct());
        % ... preprocess ...
        soc.monitor.stage_done(ImageId, 'preprocess', struct());

        soc.monitor.stage_started(ImageId, 'photometry', struct());
        % ... photometry ...
        soc.monitor.stage_done(ImageId, 'photometry', struct('n_sources', 512));

        CatPath = 'output/detections.cat';
        soc.monitor.product_created(ImageId, 'catalog', CatPath, struct());
        soc.monitor.clickhouse_insert_started(ImageId, struct('table', 'detections'));
        % ... insert ...
        soc.monitor.clickhouse_insert_done(ImageId, struct('rows', 512));

        soc.monitor.image_done(ImageId, struct());
        fprintf('Success path: image_done written\n');
    catch ME
        soc.monitor.fault(soc.monitor.MonitorConst.EventStageFailed, ME.message, ...
            struct('identifier', ME.identifier));
        soc.monitor.image_failed(ImageId, struct('reason', ME.message));
        fprintf('Error path: fault and image_failed written\n');
    end

    fprintf('Simulated error path:\n');
    ImageId2 = 'IMG_20260604_009999';
    soc.monitor.image_started('bad_image.fits', struct());
    soc.monitor.stage_started(ImageId2, 'astrometry', struct());
    soc.monitor.stage_failed(ImageId2, 'astrometry', struct('reason', 'not enough stars'));
    soc.monitor.fault(soc.monitor.MonitorConst.EventStageFailed, 'Astrometry failed', ...
        struct('image_id', ImageId2, 'stage', 'astrometry'));
    soc.monitor.image_failed(ImageId2, struct('reason', 'astrometry failed'));

    Client = soc.monitor.get_client();
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    fprintf('--- debug_pipeline_loop done ---\n\n');
end
