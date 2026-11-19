%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : +ultrasat/+services/+on_demand/openOnDemandChLogger.m
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Optionally open io.chlog.ChLogger for on_demand workers.
%==========================================================================

function L = openOnDemandChLogger(Type)
    % openOnDemandChLogger Start ClickHouse logger when ON_DEMAND_CHLOG or MsgLogger requests it.
    %
    % Input  : Type - processing type string (e.g. forced_photometry)
    % Output : L    - ChLogger handle, or [] when disabled / unavailable
    % Example: L = openOnDemandChLogger('forced_photometry');

    L = [];

    if ~onDemandChLogEnabled()
        return;
    end

    try
        ProcessName = sprintf('on_demand_%s', char(Type));
        L = io.chlog.ChLogger.open([], 'Process', ProcessName);
        io.msgLog(LogLevel.Info, 'openOnDemandChLogger: process=%s', ProcessName);
    catch Ex
        io.msgLog(LogLevel.Warning, 'openOnDemandChLogger: %s', Ex.message);
        L = [];
    end
end

% -------------------------------------------------------------------------

function Tf = onDemandChLogEnabled()
    % onDemandChLogEnabled True when env or Configuration enables chlog for on_demand.

    Tf = false;
    EnvVal = getenv('ON_DEMAND_CHLOG');
    if ~isempty(EnvVal)
        Tf = any(strcmpi(strtrim(EnvVal), {'1', 'true', 'yes', 'on'}));
        if Tf
            return;
        end
    end

    try
        C = Configuration.getSingleton();
        if isstruct(C.Data) && isfield(C.Data, 'MsgLogger') && isstruct(C.Data.MsgLogger)
            if isfield(C.Data.MsgLogger, 'UseChLogger') && logical(C.Data.MsgLogger.UseChLogger)
                Tf = true;
            end
        end
    catch
    end
end