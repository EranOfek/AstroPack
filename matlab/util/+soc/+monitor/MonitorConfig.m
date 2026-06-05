%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.MonitorConfig.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : Configuration for JSONL monitor client
%==========================================================================

classdef MonitorConfig
    % MonitorConfig  Configuration for soc.monitor JSONL client.
    %
    % Example:
    %   Config = soc.monitor.MonitorConfig.fromFile('C:/SOC/config/monitor_config.json');
    %   Config = soc.monitor.MonitorConfig.defaultConfig();

    properties
        PipelineId (1,1) string = "ultrasat_pipeline"
        InstanceName (1,1) string = "main"
        JsonlFolder (1,1) string = ""
        SchemaVersion (1,1) string = soc.monitor.MonitorConst.SchemaVersion
        WriteEnabled (1,1) logical = true
        PrintToConsole (1,1) logical = false
    end

    methods
        function obj = MonitorConfig()
            % MonitorConfig  Default constructor.
        end
    end

    methods (Static)
        function Config = fromFile(ConfigFilename)
            % fromFile  Load configuration from JSON file.
            %
            % Example:
            %   Config = soc.monitor.MonitorConfig.fromFile('C:/SOC/config/monitor_config.json');
            arguments
                ConfigFilename (1,1) string
            end
            if ~isfile(ConfigFilename)
                error('soc.monitor.MonitorConfig:FileNotFound', ...
                    'Monitor config file not found: %s', ConfigFilename);
            end
            RawText = fileread(ConfigFilename);
            JsonStruct = jsondecode(RawText);
            Config = soc.monitor.MonitorConfig.fromStruct(JsonStruct);
        end

        function Config = fromStruct(JsonStruct)
            % fromStruct  Build MonitorConfig from decoded JSON struct.
            Config = soc.monitor.MonitorConfig();
            if isfield(JsonStruct, 'pipeline_id')
                Config.PipelineId = string(JsonStruct.pipeline_id);
            end
            if isfield(JsonStruct, 'instance_name')
                Config.InstanceName = string(JsonStruct.instance_name);
            end
            if isfield(JsonStruct, 'jsonl_folder')
                Config.JsonlFolder = string(JsonStruct.jsonl_folder);
            end
            if isfield(JsonStruct, 'schema_version')
                Config.SchemaVersion = string(JsonStruct.schema_version);
            end
            if isfield(JsonStruct, 'write_enabled')
                Config.WriteEnabled = logical(JsonStruct.write_enabled);
            end
            if isfield(JsonStruct, 'print_to_console')
                Config.PrintToConsole = logical(JsonStruct.print_to_console);
            end
        end

        function Config = defaultConfig()
            % defaultConfig  Built-in defaults when no config file is provided.
            %
            % Example:
            %   Config = soc.monitor.MonitorConfig.defaultConfig();
            Config = soc.monitor.MonitorConfig();
            if ispc
                Config.JsonlFolder = "C:/SOC/monitor/jsonl";
            else
                Config.JsonlFolder = "/var/opt/soc/monitor/jsonl";
            end
        end

        function StructOut = toStruct(Config)
            % toStruct  Export configuration as struct for debug display.
            %
            % Example:
            %   S = soc.monitor.MonitorConfig.toStruct(Config);
            StructOut = struct( ...
                'pipeline_id', char(Config.PipelineId), ...
                'instance_name', char(Config.InstanceName), ...
                'jsonl_folder', char(Config.JsonlFolder), ...
                'schema_version', char(Config.SchemaVersion), ...
                'write_enabled', Config.WriteEnabled, ...
                'print_to_console', Config.PrintToConsole ...
            );
        end
    end
end
