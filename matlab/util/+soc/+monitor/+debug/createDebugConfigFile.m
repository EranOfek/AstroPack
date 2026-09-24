%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.debug.createDebugConfigFile.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Create temporary debug monitor config JSON file
%==========================================================================

function ConfigFilename = createDebugConfigFile()
    % createDebugConfigFile  Write temporary debug monitor config JSON file.
    %
    % Example:
    %   ConfigFile = soc.monitor.debug.createDebugConfigFile();
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
