%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.loadWorkerConfig
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Load on-demand worker configuration (defaults + optional JSON file).
%==========================================================================

function Cfg = loadWorkerConfig(Args)
    % loadWorkerConfig Merge defaults with worker_config.json on share.
    %
    % Input  : Args.Type, Args.OnDemandPath, Args.ConfigFile
    % Output : Cfg struct (WorkerId, ClaimTimeoutSec, retention, poll delays)
    % Example: Cfg = loadWorkerConfig('Type', 'forced_photometry')

    arguments
        Args.Type (1,1) string
        Args.OnDemandPath (1,1) string = ""
        Args.ConfigFile (1,1) string = ""
    end

    Type = char(Args.Type);
    OnDemandPath = char(Args.OnDemandPath);
    if isempty(OnDemandPath)
        OnDemandPath = getenv('ON_DEMAND_PATH');
    end
    if isempty(OnDemandPath)
        error('OnDemand:MissingPath', 'ON_DEMAND_PATH must be set or OnDemandPath provided');
    end

    Cfg = struct();
    Cfg.Type = Type;
    Cfg.OnDemandPath = OnDemandPath;
    Cfg.WorkerId = defaultWorkerId("");
    Cfg.ClaimTimeoutSec = 3600;
    Cfg.ProcessedTriggerRetentionDays = 7;
    Cfg.WorkRetentionDays = [];
    Cfg.CleanupIntervalSec = 3600;
    Cfg.PollDelaySec = 0.25;
    Cfg.WatchdogIntervalSec = 10;

    ConfigPath = char(Args.ConfigFile);
    if isempty(ConfigPath)
        ConfigPath = fullfile(OnDemandPath, Type, 'worker_config.json');
    end

    if isfile(ultrasat.tools.localPathForIo(ConfigPath))
        JsonPath = ultrasat.tools.localPathForIo(ConfigPath);
        Raw = fileread(JsonPath);
        FileCfg = jsondecode(Raw);
        Cfg = mergeConfigStruct(Cfg, FileCfg);
    end

    if isfield(Cfg, 'worker_id') && ~isempty(Cfg.worker_id)
        Cfg.WorkerId = sanitizeWorkerId(char(Cfg.worker_id));
    else
        Cfg.WorkerId = defaultWorkerId(Cfg.WorkerId);
    end

    if isfield(Cfg, 'claim_timeout_sec') && ~isempty(Cfg.claim_timeout_sec)
        Cfg.ClaimTimeoutSec = double(Cfg.claim_timeout_sec);
    end
    if isfield(Cfg, 'processed_trigger_retention_days')
        Cfg.ProcessedTriggerRetentionDays = double(Cfg.processed_trigger_retention_days);
    end
    if isfield(Cfg, 'work_retention_days') && ~isempty(Cfg.work_retention_days)
        Cfg.WorkRetentionDays = double(Cfg.work_retention_days);
    end
    if isfield(Cfg, 'cleanup_interval_sec')
        Cfg.CleanupIntervalSec = double(Cfg.cleanup_interval_sec);
    end
    if isfield(Cfg, 'poll_delay_sec')
        Cfg.PollDelaySec = double(Cfg.poll_delay_sec);
    end
    if isfield(Cfg, 'watchdog_interval_sec')
        Cfg.WatchdogIntervalSec = double(Cfg.watchdog_interval_sec);
    end
end

% -------------------------------------------------------------------------

function Cfg = mergeConfigStruct(Cfg, FileCfg)
    Fn = fieldnames(FileCfg);
    for K = 1:numel(Fn)
        Cfg.(Fn{K}) = FileCfg.(Fn{K});
    end
end

% -------------------------------------------------------------------------

function Id = defaultWorkerId(Fallback)
    Id = char(Fallback);
    if ~isempty(Id)
        Id = sanitizeWorkerId(Id);
        return;
    end
    Host = getenv('COMPUTERNAME');
    if isempty(Host)
        Host = getenv('HOSTNAME');
    end
    if isempty(Host)
        Host = 'worker';
    end
    Pid = 0;
    try
        Pid = feature('getpid');
    catch
        Pid = round(rem(now, 1) * 1e6);
    end
    Id = sanitizeWorkerId(sprintf('%s_%d', Host, Pid));
end

% -------------------------------------------------------------------------

function Id = sanitizeWorkerId(Id)
    Id = regexprep(Id, '[^\w\-]', '_');
    if isempty(Id)
        Id = 'worker';
    end
end