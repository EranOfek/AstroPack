%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.debug_monitor.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Debug soc.monitor client end-to-end scenarios
%==========================================================================

function debug_monitor()
    % debug_monitor  Run all soc.monitor debug scenarios (entry point).
    debug();
end

function debug()
    % debug  Run all soc.monitor debug functions in sequence.
    fprintf('=== soc.monitor debug ===\n\n');
    debug_func1_basic_init();
    debug_func2_heartbeat();
    debug_func3_image_lifecycle();
    debug_func4_stage_lifecycle();
    debug_func5_fault_metric_log();
    debug_func6_reset_and_reinit();
    printJsonlSummary();
    fprintf('\n=== soc.monitor debug done ===\n');
end

function debug_func1_basic_init()
    fprintf('--- debug_func1_basic_init ---\n');
    ConfigFilename = createDebugConfigFile();
    soc.monitor.reset();
    Client = soc.monitor.init(ConfigFilename);
    fprintf('Config file: %s\n', ConfigFilename);
    disp(soc.monitor.MonitorConfig.toStruct(Client.Config));
    fprintf('Instance ID: %s\n', Client.InstanceId);
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    fprintf('Expected records after this step: 0\n');
    fprintf('--- debug_func1_basic_init done ---\n\n');
end

function debug_func2_heartbeat()
    fprintf('--- debug_func2_heartbeat ---\n');
    Client = soc.monitor.get_client();
    fprintf('JSONL file: %s\n', Client.getJsonlFilename());
    soc.monitor.heartbeat();
    fprintf('Expected records after this step: 1\n');
    fprintf('--- debug_func2_heartbeat done ---\n\n');
end

function debug_func3_image_lifecycle()
    fprintf('--- debug_func3_image_lifecycle ---\n');
    Filename = 'debug_image_001.fits';
    ImageId = 'img_debug_001';
    Info = struct('telescope', 'ULTRASAT');
    fprintf('filename: %s, image_id: %s\n', Filename, ImageId);
    soc.monitor.image_started(Filename, Info);
    soc.monitor.image_done(ImageId, Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_func3_image_lifecycle done ---\n\n');
end

function debug_func4_stage_lifecycle()
    fprintf('--- debug_func4_stage_lifecycle ---\n');
    ImageId = 'img_debug_001';
    Info = struct('worker', 'debug');
    fprintf('image_id: %s, stage: crop\n', ImageId);
    soc.monitor.stage_started(ImageId, "crop", Info);
    soc.monitor.stage_done(ImageId, "crop", Info);
    fprintf('Expected additional records: 2\n');
    fprintf('--- debug_func4_stage_lifecycle done ---\n\n');
end

function debug_func5_fault_metric_log()
    fprintf('--- debug_func5_fault_metric_log ---\n');
    Info = struct('test', true);
    soc.monitor.fault("pipeline.test_fault", "Test fault message", Info);
    soc.monitor.metric("detections_count", 1234, "count", Info);
    soc.monitor.log_record("debug", "Test log message", Info);
    fprintf('Expected additional records: 3\n');
    fprintf('--- debug_func5_fault_metric_log done ---\n\n');
end

function debug_func6_reset_and_reinit()
    fprintf('--- debug_func6_reset_and_reinit ---\n');
    ConfigFilename = createDebugConfigFile();
    soc.monitor.reset();
    Client = soc.monitor.init(ConfigFilename);
    soc.monitor.heartbeat();
    fprintf('Reinitialized client, JSONL file: %s\n', Client.getJsonlFilename());
    fprintf('Expected records in new file after reset: 1\n');
    fprintf('--- debug_func6_reset_and_reinit done ---\n\n');
end

function ConfigFilename = createDebugConfigFile()
    JsonlFolder = getDebugJsonlFolder();
    ConfigStruct = struct( ...
        'pipeline_id', 'debug_pipeline', ...
        'instance_name', 'debug_main', ...
        'jsonl_folder', char(JsonlFolder), ...
        'schema_version', soc.monitor.MonitorConst.SchemaVersion, ...
        'write_enabled', true, ...
        'print_to_console', true ...
    );
    ConfigFilename = fullfile(JsonlFolder, 'monitor_config_debug.json');
    JsonText = jsonencode(ConfigStruct);
    Fid = fopen(ConfigFilename, 'w');
    if Fid < 0
        error('Cannot write debug config file: %s', ConfigFilename);
    end
    Cleaner = onCleanup(@() fclose(Fid));
    fprintf(Fid, '%s', JsonText);
end

function Folder = getDebugJsonlFolder()
    if ispc
        Folder = 'C:/SOC/monitor/debug_jsonl';
    else
        Folder = '/var/opt/soc/monitor/debug_jsonl';
    end
    if ~isfolder(Folder)
        mkdir(Folder);
    end
end

function printJsonlSummary()
    fprintf('--- JSONL summary ---\n');
    Folder = getDebugJsonlFolder();
    Files = dir(fullfile(Folder, 'pipeline_monitor_*.jsonl'));
    if isempty(Files)
        fprintf('No JSONL files found in %s\n', Folder);
        return;
    end
    for I = 1:numel(Files)
        FullPath = fullfile(Files(I).folder, Files(I).name);
        fprintf('File: %s (%d bytes)\n', FullPath, Files(I).bytes);
        Lines = readJsonlLines(FullPath);
        fprintf('  Line count: %d\n', numel(Lines));
        MaxShow = min(3, numel(Lines));
        for J = 1:MaxShow
            fprintf('  [%d] %s\n', J, Lines{J});
        end
    end
end

function Lines = readJsonlLines(Filename)
    Lines = {};
    Fid = fopen(Filename, 'r');
    if Fid < 0
        return;
    end
    Cleaner = onCleanup(@() fclose(Fid));
    while true
        Line = fgetl(Fid);
        if ~ischar(Line)
            break;
        end
        if strlength(string(Line)) > 0
            Lines{end + 1} = Line; %#ok<AGROW>
        end
    end
end
