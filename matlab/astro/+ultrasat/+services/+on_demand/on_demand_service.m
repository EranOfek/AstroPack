%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.on_demand_service
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Long-running on-demand worker (ON_DEMAND_PATH, claim + IPC).
%==========================================================================

function on_demand_service(Args)
    % on_demand_service Main entry for one processing type inbox under ON_DEMAND_PATH.
    %
    % Mirrors too_planner_service + JsonFileIpc, but uses OnDemandFileIpc (claim, TTL).
    % Optional ClickHouse logging: set ON_DEMAND_CHLOG=1 or MsgLogger.UseChLogger.
    %
    % Input  : Args.Type       - forced_photometry | custom_coadd
    %          Args.ConfigFile - optional worker_config.json path
    %          Args.OnDemandPath - optional override for ON_DEMAND_PATH
    % Output : -
    % Example: ultrasat.services.on_demand.on_demand_service('Type', 'forced_photometry')

    arguments
        Args.Type (1,1) string
        Args.ConfigFile (1,1) string = ""
        Args.OnDemandPath (1,1) string = ""
    end

    fprintf('on_demand_service started: type=%s\n', Args.Type);

    Cfg = ultrasat.services.on_demand.loadWorkerConfig( ...
        'Type', Args.Type, ...
        'OnDemandPath', Args.OnDemandPath, ...
        'ConfigFile', Args.ConfigFile);

    LogName = sprintf('on_demand_%s_service', Cfg.Type);
    ultrasat.services.common.setLogFile(LogName, 'matlab_services/on_demand/');

    % Optional io.chlog (never required for service operation)
    ChLog = ultrasat.services.on_demand.openOnDemandChLogger(Cfg.Type);
    ultrasat.services.on_demand.logOnDemand('INF', 'on_demand_service starting', ...
        'CorrId', sprintf('on_demand_%s', Cfg.Type));

    TypeRoot = fullfile(Cfg.OnDemandPath, Cfg.Type);
    InputPath = fullfile(TypeRoot, 'inbox');
    ProcessedPath = fullfile(TypeRoot, 'processed');
    WorkRoot = fullfile(TypeRoot, 'work');
    WatchdogFileName = fullfile(TypeRoot, [Cfg.Type, '.watchdog']);

    Callback = @(Input) wrapProcessRequest(Input, Cfg.WorkerId);

    io.msgLog(LogLevel.Info, 'creating OnDemandFileIpc');
    Ipc = ultrasat.services.on_demand.OnDemandFileIpc( ...
        'InputPath', InputPath, ...
        'ProcessedPath', ProcessedPath, ...
        'KeepProcessedFilesDays', Cfg.ProcessedTriggerRetentionDays, ...
        'Callback', Callback, ...
        'WatchdogFileName', WatchdogFileName, ...
        'WatchdogInterval', Cfg.WatchdogIntervalSec, ...
        'WorkerId', Cfg.WorkerId, ...
        'ClaimTimeoutSec', Cfg.ClaimTimeoutSec, ...
        'WorkRoot', WorkRoot, ...
        'WorkRetentionDays', Cfg.WorkRetentionDays, ...
        'CleanupIntervalSec', Cfg.CleanupIntervalSec);

    io.msgLog(LogLevel.Info, 'calling processLoop (DelaySec=%g)', Cfg.PollDelaySec);
    Ipc.processLoop('DelaySec', Cfg.PollDelaySec);

    ultrasat.services.on_demand.logOnDemand('INF', 'on_demand_service terminated', ...
        'CorrId', sprintf('on_demand_%s', Cfg.Type));
    closeOnDemandChLogger(ChLog);
    io.msgLog(LogLevel.Info, 'on_demand_service terminated');
end

% -------------------------------------------------------------------------

function Output = wrapProcessRequest(Input, WorkerId)
    % wrapProcessRequest Inject worker id before processRequest dispatch.
    Input.IpcWorkerId = char(WorkerId);
    Output = ultrasat.services.on_demand.processRequest(Input);
end

% -------------------------------------------------------------------------

function closeOnDemandChLogger(ChLog)
    % closeOnDemandChLogger Flush and close optional ChLogger handle.
    if isempty(ChLog) || ~io.chlog.ChLogger.isValidHandle(ChLog)
        return;
    end
    try
        ChLog.flush();
        ChLog.close();
    catch
    end
end