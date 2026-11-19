%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.processForcedPhotometry
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Forced photometry on-demand handler (validate + dry_run; science TBD).
%==========================================================================

function Output = processForcedPhotometry(Trigger)
    % processForcedPhotometry Load workdir request, update status, run or stub.
    %
    % Input  : Trigger - inbox JSON struct (request_file, work_folder, ...)
    % Output : IPC response struct with status, message, artifact paths

    Output = struct();
    Output.status = 'error';
    Output.message = '';
    Output.action = 'forced_photometry';

    WorkFolder = "";
    try
        [WorkReq, WorkFolder] = ultrasat.services.on_demand.loadWorkRequest(Trigger);

        RequestId = "";
        if isfield(Trigger, 'request_id') && ~isempty(Trigger.request_id)
            RequestId = string(Trigger.request_id);
        elseif isfield(WorkReq, 'request_id')
            RequestId = string(WorkReq.request_id);
        end
        Output.request_id = char(RequestId);

        WorkerId = "";
        if isfield(Trigger, 'IpcWorkerId')
            WorkerId = char(Trigger.IpcWorkerId);
        end

        ultrasat.services.on_demand.writeStatusJson(WorkFolder, struct( ...
            'state', 'running', ...
            'request_id', char(RequestId), ...
            'action', 'forced_photometry', ...
            'started_at', ultrasat.services.on_demand.onDemandUtcNow(), ...
            'claimed_at', ultrasat.services.on_demand.onDemandUtcNow(), ...
            'worker_id', WorkerId, ...
            'heartbeat_at', ultrasat.services.on_demand.onDemandUtcNow()));

        ultrasat.services.on_demand.validateForcedPhotometryRequest(WorkReq);

        if ultrasat.services.on_demand.isDryRunRequest(WorkReq)
            NSrc = numel(WorkReq.sources);
            ResultPayload = struct( ...
                'dry_run', true, ...
                'action', 'forced_photometry', ...
                'request_id', char(RequestId), ...
                'sources_processed', NSrc, ...
                'message', 'forced_photometry: dry_run mock result');
            ResultFile = ultrasat.services.on_demand.writeResultJson(WorkFolder, ResultPayload);

            ultrasat.services.on_demand.writeStatusJson(WorkFolder, struct( ...
                'state', 'ok', ...
                'completed_at', ultrasat.services.on_demand.onDemandUtcNow(), ...
                'message', ''));

            Output.status = 'ok';
            Output.message = 'forced_photometry: dry_run OK';
            Output = ultrasat.services.on_demand.appendWorkArtifactPaths(Output, WorkFolder, ResultFile);
        else
            Msg = 'forced_photometry: science handler not implemented';
            ultrasat.services.on_demand.writeStatusJson(WorkFolder, struct( ...
                'state', 'error', ...
                'completed_at', ultrasat.services.on_demand.onDemandUtcNow(), ...
                'message', Msg));
            Output.status = 'error';
            Output.message = Msg;
            Output = ultrasat.services.on_demand.appendWorkArtifactPaths(Output, WorkFolder, "");
        end
    catch Ex
        Msg = Ex.message;
        Output.status = 'error';
        Output.message = Msg;
        if strlength(WorkFolder) > 0
            try
                ultrasat.services.on_demand.writeStatusJson(WorkFolder, struct( ...
                    'state', 'error', ...
                    'completed_at', ultrasat.services.on_demand.onDemandUtcNow(), ...
                    'message', Msg));
            catch
            end
            Output = ultrasat.services.on_demand.appendWorkArtifactPaths(Output, WorkFolder, "");
        end
    end
end