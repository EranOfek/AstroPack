%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.OnDemandFileIpc
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : JsonFileIpc subclass with inbox claim/reclaim and work TTL.
%==========================================================================

classdef OnDemandFileIpc < ultrasat.services.common.JsonFileIpc
    % OnDemandFileIpc monitors ON_DEMAND_PATH/<type>/inbox with multi-worker claim.
    %
    % Extends JsonFileIpc for on-demand pipelines. SOC workers (too_planner, slew_calc)
    % keep the base class without claim; this class adds:
    %   * two-phase rename claim (claiming -> claimed.<WorkerId>.json)
    %   * .out.json named from original trigger basename (not claimed name)
    %   * reclaim of stale claimed triggers (ClaimTimeoutSec)
    %   * optional deletion of terminal workdirs (WorkRetentionDays)
    %
    % Input:   InputPath, ProcessedPath, Callback - same as JsonFileIpc
    %          WorkerId - suffix for claim filenames
    %          ClaimTimeoutSec - reclaim threshold
    %          WorkRoot - .../<type>/work for TTL cleanup
    %          WorkRetentionDays - days; [] or <=0 = keep forever
    %          CleanupIntervalSec - maintenance tick interval

    properties (SetAccess = public)

        WorkerId (1,1) string = "worker"       % Claim suffix; unique per MATLAB process
        ClaimTimeoutSec (1,1) double = 3600      % Reclaim claimed files older than this [s]
        WorkRoot (1,1) string = ""              % Root of dated work/ tree for TTL cleanup
        WorkRetentionDays = []                  % Delete ok/error workdirs after N days; [] = forever
        CleanupIntervalSec (1,1) double = 3600  % How often to run reclaim + work cleanup
    end

    properties (Access = private)
        LastMaintenanceTime = tic();            % Timer for maintenance tick
    end

    methods
        function Obj = OnDemandFileIpc(Args)
            % Constructor — forwards JsonFileIpc paths and sets claim/TTL properties.
            arguments
                Args.InputPath = ''
                Args.ProcessedPath = ''
                Args.KeepProcessedFilesDays = 7
                Args.Callback = []
                Args.WatchdogFileName = []
                Args.WatchdogInterval = 10
                Args.WorkerId (1,1) string = ""
                Args.ClaimTimeoutSec (1,1) double = 3600
                Args.WorkRoot (1,1) string = ""
                Args.WorkRetentionDays = []
                Args.CleanupIntervalSec (1,1) double = 3600
            end

            Obj@ultrasat.services.common.JsonFileIpc( ...
                'InputPath', Args.InputPath, ...
                'ProcessedPath', Args.ProcessedPath, ...
                'KeepProcessedFilesDays', Args.KeepProcessedFilesDays, ...
                'Callback', Args.Callback, ...
                'WatchdogFileName', Args.WatchdogFileName, ...
                'WatchdogInterval', Args.WatchdogInterval);

            Obj.setName('OnDemandFileIpc');

            if ~isempty(Args.WorkerId)
                Obj.WorkerId = string(Args.WorkerId);
            end
            Obj.ClaimTimeoutSec = Args.ClaimTimeoutSec;
            Obj.WorkRoot = string(Args.WorkRoot);
            Obj.WorkRetentionDays = Args.WorkRetentionDays;
            Obj.CleanupIntervalSec = Args.CleanupIntervalSec;

            Obj.msgLog(LogLevel.Info, 'WorkerId: %s, ClaimTimeoutSec: %g', ...
                Obj.WorkerId, Obj.ClaimTimeoutSec);
        end

        function tick(Obj)
            % Do one loop tick: maintenance, then parent poll/process/archive/watchdog.
            Obj.runMaintenanceIfDue();
            tick@ultrasat.services.common.JsonFileIpc(Obj);
        end

        function processSingleInputFile(Obj)
            % Process one eligible inbox trigger: claim, callback, archive.
            List = dir(fullfile(Obj.InputPath, Obj.InputMask));
            [~, IndexList] = sort(string({List.name}), 'ascend');

            for ii = IndexList
                if List(ii).isdir
                    continue;
                end
                Name = List(ii).name;
                if Obj.shouldSkipInboxName(Name)
                    continue;
                end

                FileName = fullfile(List(ii).folder, Name);
                if ismember(FileName, Obj.AlreadyProcessedFiles)
                    continue;
                end

                ClaimedPath = Obj.claimTriggerFile(FileName);
                if ClaimedPath == ""
                    continue;
                end

                ultrasat.services.on_demand.logOnDemand('INF', ...
                    sprintf('claimed trigger %s', Obj.originalTriggerBasename(ClaimedPath)), ...
                    'CorrId', char(Obj.WorkerId));

                Obj.processJsonFile(ClaimedPath);

                if ~Obj.moveOrDeleteProcessedFile(ClaimedPath)
                    Obj.AlreadyProcessedFiles(end + 1) = ClaimedPath;
                end
                return;
            end
        end

        function processJsonFile(Obj, FileName)
            % Load JSON, invoke Callback, write .out.json using original trigger basename.
            Obj.msgLog(LogLevel.Info, 'processJsonFile started: %s', strrep(FileName, '\', '/'));
            try
                fid = fopen(FileName);
                RawBytes = fread(fid, inf);
                Text = char(RawBytes');
                fclose(fid);

                InputStruct = jsondecode(Text);
                InputStruct.IpcInputJsonFilename = FileName;
                InputStruct.IpcOriginalTriggerBasename = Obj.originalTriggerBasename(FileName);

                try
                    OutputStruct = Obj.Callback(InputStruct);
                catch Ex
                    Obj.msgLog(LogLevel.Error, 'processJsonFile: callback: %s', Ex.message);
                    OutputStruct = struct();
                    OutputStruct.status = 'error';
                    OutputStruct.message = sprintf('Error calling callback function: %s', Ex.message);
                end

                OutputText = jsonencode(OutputStruct, 'PrettyPrint', true);

                [filepath, ~, ~] = fileparts(FileName);
                OrigBase = Obj.originalTriggerBasename(FileName);
                [~, origName, origExt] = fileparts(OrigBase);
                TmpFileName = fullfile(filepath, [origName, '.out.json.tmp']);
                OutputFileName = fullfile(filepath, [origName, '.out.json']);

                fid = fopen(TmpFileName, 'wt');
                fprintf(fid, OutputText);
                fclose(fid);
                movefile(TmpFileName, OutputFileName);

                CorrId = Obj.corrIdFromInput(InputStruct);
                if isfield(OutputStruct, 'status')
                    ultrasat.services.on_demand.logOnDemand('INF', ...
                        sprintf('ipc status=%s', OutputStruct.status), 'CorrId', CorrId);
                end

                Obj.msgLog(LogLevel.Info, 'processJsonFile done: %s', strrep(OutputFileName, '\', '/'));
            catch Ex
                Obj.msgLog(LogLevel.Error, 'processJsonFile: %s: %s', FileName, Ex.message);
                ultrasat.services.on_demand.logOnDemand('ERR', Ex.message, 'CorrId', char(Obj.WorkerId));
            end
        end
    end

    methods (Access = private)
        function CorrId = corrIdFromInput(Obj, InputStruct)
            % corrIdFromInput Build correlation id action:request_id for chlog.
            CorrId = char(Obj.WorkerId);
            if isfield(InputStruct, 'action') && isfield(InputStruct, 'request_id') ...
                    && ~isempty(InputStruct.request_id)
                CorrId = sprintf('%s:%s', char(InputStruct.action), char(InputStruct.request_id));
            end
        end

        function Tf = shouldSkipInboxName(~, Name)
            % shouldSkipInboxName Skip outputs, temps, and in-flight claim names.
            Tf = endsWith(Name, '.out.json') || endsWith(Name, '.tmp') || ...
                contains(Name, '.claiming.') || contains(Name, '.claimed.');
        end

        function ClaimedPath = claimTriggerFile(Obj, FileName)
            ClaimedPath = "";
            [Folder, BaseName, Ext] = fileparts(FileName);
            if contains(BaseName, '.claimed.') || contains(BaseName, '.claiming.')
                return;
            end

            W = char(Obj.WorkerId);
            ClaimingName = fullfile(Folder, sprintf('%s.claiming.%s%s', BaseName, W, Ext));
            ClaimedName = fullfile(Folder, sprintf('%s.claimed.%s%s', BaseName, W, Ext));

            try
                movefile(FileName, ClaimingName);
            catch
                return;
            end
            if ~isfile(ClaimingName)
                return;
            end

            try
                movefile(ClaimingName, ClaimedName);
                ClaimedPath = ClaimedName;
            catch
                try
                    movefile(ClaimingName, FileName);
                catch
                end
            end
        end

        function Orig = originalTriggerBasename(~, FileName)
            [~, Name, Ext] = fileparts(FileName);
            Idx = strfind(Name, '.claimed.');
            if ~isempty(Idx)
                Name = Name(1:Idx(1) - 1);
            end
            Idx = strfind(Name, '.claiming.');
            if ~isempty(Idx)
                Name = Name(1:Idx(1) - 1);
            end
            Orig = [Name, Ext];
        end

        function runMaintenanceIfDue(Obj)
            Elapsed = toc(Obj.LastMaintenanceTime);
            if Elapsed < Obj.CleanupIntervalSec
                return;
            end
            Obj.reclaimStaleClaims();
            Obj.cleanTerminalWorkdirs();
            Obj.LastMaintenanceTime = tic();
        end

        function reclaimStaleClaims(Obj)
            List = dir(fullfile(Obj.InputPath, '*.claimed.*.json'));
            if isempty(List)
                List = dir(fullfile(Obj.InputPath, '*.json'));
                List = List(arrayfun(@(D) contains(D.name, '.claimed.'), List));
            end
            Cutoff = now - (Obj.ClaimTimeoutSec / 86400);

            for K = 1:numel(List)
                if List(K).isdir
                    continue;
                end
                if List(K).datenum > Cutoff
                    continue;
                end
                FileName = fullfile(List(K).folder, List(K).name);
                Orig = Obj.originalTriggerBasename(FileName);
                Target = fullfile(List(K).folder, Orig);
                try
                    if isfile(Target)
                        Target = fullfile(List(K).folder, ...
                            [erase(List(K).name, '.json'), '.retry.json']);
                    end
                    movefile(FileName, Target, 'f');
                    Obj.msgLog(LogLevel.Warning, 'reclaimStaleClaims: %s -> %s', ...
                        strrep(FileName, '\', '/'), strrep(Target, '\', '/'));
                catch Ex
                    Obj.msgLog(LogLevel.Error, 'reclaimStaleClaims: %s', Ex.message);
                end
            end
        end

        function cleanTerminalWorkdirs(Obj)
            if isempty(Obj.WorkRoot) || ~isfolder(Obj.WorkRoot)
                return;
            end
            Retention = Obj.WorkRetentionDays;
            if isempty(Retention) || (isnumeric(Retention) && (isnan(Retention) || Retention <= 0))
                return;
            end
            Cutoff = now - double(Retention);

            List = dir(fullfile(char(Obj.WorkRoot), '**', 'status.json'));
            for K = 1:numel(List)
                StatusPath = fullfile(List(K).folder, List(K).name);
                WorkDir = List(K).folder;
                try
                    Status = jsondecode(fileread(StatusPath));
                catch
                    continue;
                end
                if ~isfield(Status, 'state')
                    continue;
                end
                State = char(Status.state);
                if ~any(strcmp(State, {'ok', 'error'}))
                    continue;
                end
                AgeDt = List(K).datenum;
                if isfield(Status, 'completed_at') && ~isempty(Status.completed_at)
                    try
                        AgeDt = datenum(datetime(Status.completed_at, ...
                            'InputFormat', "yyyy-MM-dd'T'HH:mm:ss'Z'", 'TimeZone', 'UTC'));
                    catch
                    end
                end
                if AgeDt > Cutoff
                    continue;
                end
                try
                    rmdir(WorkDir, 's');
                    Obj.msgLog(LogLevel.Debug, 'cleanTerminalWorkdirs: %s', ...
                        strrep(WorkDir, '\', '/'));
                catch Ex
                    Obj.msgLog(LogLevel.Error, 'cleanTerminalWorkdirs: %s', Ex.message);
                end
            end
        end
    end
end