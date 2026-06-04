%==========================================================================
% Project     : ULTRASAT SOC - Pipeline Monitor
% Filename    : soc.monitor.MonitorClient.m
% Author      : Chen Tishler
% Created     : 04/06/2026
% Updated     : 04/06/2026
% Description : JSONL monitoring writer handle class
%==========================================================================

classdef MonitorClient < handle
    % MonitorClient  JSONL monitoring writer for the ULTRASAT pipeline.
    %
    % Example:
    %   Client = soc.monitor.MonitorClient(Config);
    %   Client.writeRecord(Record);

    properties
        Config (1,1) soc.monitor.MonitorConfig
        InstanceId (1,1) string
    end

    methods
        function obj = MonitorClient(Config)
            % MonitorClient  Construct client from MonitorConfig.
            arguments
                Config (1,1) soc.monitor.MonitorConfig
            end
            obj.Config = Config;
            Pid = feature('getpid');
            obj.InstanceId = Config.InstanceName + "_" + string(Pid);
            obj.ensureJsonlFolder();
        end

        function Filename = getJsonlFilename(obj)
            % getJsonlFilename  Daily JSONL path for this pipeline instance.
            Dt = datetime('now', 'TimeZone', 'UTC');
            DateStr = char(Dt, 'yyyyMMdd');
            BaseName = sprintf('pipeline_monitor_%s_%s_%s.jsonl', ...
                char(obj.Config.PipelineId), ...
                char(obj.InstanceId), ...
                DateStr);
            Filename = fullfile(char(obj.Config.JsonlFolder), BaseName);
        end

        function writeRecord(obj, Record)
            % writeRecord  Append one JSON line to the JSONL file.
            if ~obj.Config.WriteEnabled
                return;
            end
            try
                if ~soc.monitor.MonitorConst.validateRecord(Record)
                    obj.printWarning('Invalid monitoring record; record not written.');
                    return;
                end
                JsonLine = jsonencode(Record);
                Filename = obj.getJsonlFilename();
                Fid = fopen(Filename, 'a');
                if Fid < 0
                    obj.printWarning(sprintf('Cannot open JSONL file: %s', Filename));
                    return;
                end
                Cleaner = onCleanup(@() fclose(Fid));
                fprintf(Fid, '%s\n', JsonLine);
                if obj.Config.PrintToConsole
                    fprintf('soc.monitor: %s\n', JsonLine);
                end
            catch ME
                obj.printWarning(ME.message);
            end
        end
    end

    methods (Static)
        function Client = createDefault()
            % createDefault  Client with built-in default configuration.
            Config = soc.monitor.MonitorConfig.defaultConfig();
            Client = soc.monitor.MonitorClient(Config);
        end
    end

    methods (Access = private)
        function ensureJsonlFolder(obj)
            Folder = char(obj.Config.JsonlFolder);
            if strlength(Folder) == 0
                return;
            end
            if ~isfolder(Folder)
                mkdir(Folder);
            end
        end

        function printWarning(obj, Msg)
            if obj.Config.PrintToConsole
                warning('soc.monitor:WriteFailed', '%s', Msg);
            end
        end
    end
end
