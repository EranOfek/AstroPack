%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.debug.debug_onDemandIpcTick
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : One OnDemandFileIpc tick with health trigger in temp inbox.
%
% Run by      : ultrasat.services.on_demand.debug.debug_onDemandIpcTick()
%==========================================================================

function debug_onDemandIpcTick()
    fprintf('\n========== DEBUG ON_DEMAND IPC TICK ==========\n');

    Root = fullfile(tempdir, 'on_demand_ipc_debug');
    Type = 'forced_photometry';
    TypeRoot = fullfile(Root, Type);
    Inbox = fullfile(TypeRoot, 'inbox');
    Processed = fullfile(TypeRoot, 'processed');
    WorkRoot = fullfile(TypeRoot, 'work');
    mkdir(Inbox);

    TriggerPath = fullfile(Inbox, '99-99-99-0001.json');
    Health = struct('action', 'health');
    fid = fopen(TriggerPath, 'wt');
    fprintf(fid, '%s', jsonencode(Health));
    fclose(fid);

    Ipc = ultrasat.services.on_demand.OnDemandFileIpc( ...
        'InputPath', Inbox, ...
        'ProcessedPath', Processed, ...
        'WorkRoot', WorkRoot, ...
        'Callback', @ultrasat.services.on_demand.processRequest, ...
        'WorkerId', 'debug_ipc', ...
        'ClaimTimeoutSec', 60, ...
        'WatchdogInterval', 0);

    Ipc.tick();

    OutPath = fullfile(Inbox, '99-99-99-0001.out.json');
    if ~isfile(OutPath)
        error('debug_onDemandIpcTick:NoOut', 'Expected %s', OutPath);
    end
    fprintf('IPC out: %s\n', fileread(OutPath));
    fprintf('========== DEBUG ON_DEMAND IPC TICK DONE ==========\n');
end