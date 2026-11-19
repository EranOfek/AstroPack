%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.loadWorkRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Load workdir request.json from inbox trigger fields.
%==========================================================================

function [WorkReq, WorkFolder] = loadWorkRequest(Trigger)
    % loadWorkRequest Validate trigger pointers and decode workdir request.json.

    if ~isfield(Trigger, 'request_file') || isempty(Trigger.request_file)
        error('OnDemand:MissingField', 'Missing required field: request_file');
    end
    if ~isfield(Trigger, 'work_folder') || isempty(Trigger.work_folder)
        error('OnDemand:MissingField', 'Missing required field: work_folder');
    end

    WorkFolder = string(ultrasat.tools.localPathForIo(Trigger.work_folder));
    RequestPath = string(ultrasat.tools.localPathForIo(Trigger.request_file));

    if ~isfile(RequestPath)
        error('OnDemand:RequestNotFound', 'request_file not found: %s', char(RequestPath));
    end

    Raw = fileread(RequestPath);
    WorkReq = jsondecode(Raw);
end