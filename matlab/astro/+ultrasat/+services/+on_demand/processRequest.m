%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.on_demand.processRequest
% Author      : Chen Tishler
% Created     : 19/11/2026
% Modified    : 19/11/2026
% Description : Dispatch on-demand inbox actions (flat JSON API).
%==========================================================================

function Output = processRequest(Input)
    % Process request: dispatch on action (flat JSON trigger or health).
    %
    % Input  : struct with .action ('health'|'forced_photometry'|'custom_coadd')
    %            Non-health triggers include request_file, work_folder, request_id.
    % Output : ApiBase-style: .status ('ok'|'error'), .message; handlers add paths.
    %
    % Author : Chen Tishler (2026), pattern from slew_calc / too_planner

    Output = struct();
    Output.status = '?';
    Output.message = 'MATLAB: Exception in processRequest';

    try
        if ~isfield(Input, 'action') || isempty(Input.action)
            Output.status = 'error';
            Output.message = 'missing action';
            return;
        end

        Action = char(Input.action);
        CorrId = corrIdFromTrigger(Input);

        if strcmp(Action, 'health')
            Output.status = 'ok';
            Output.message = 'health: OK';
            ultrasat.services.on_demand.logOnDemand('INF', 'health: OK', 'CorrId', CorrId);
        elseif strcmp(Action, 'forced_photometry')
            Output = ultrasat.services.on_demand.processForcedPhotometry(Input);
        elseif strcmp(Action, 'custom_coadd') || strcmp(Action, 'custom_coaddition')
            Output = ultrasat.services.on_demand.processCustomCoadd(Input);
        else
            Output.status = 'error';
            Output.message = 'processRequest: unknown action';
            ultrasat.services.on_demand.logOnDemand('WRN', Output.message, 'CorrId', CorrId);
        end
    catch Ex
        Output.status = 'error';
        Output.message = sprintf('exception: %s', Ex.message);
        ultrasat.services.on_demand.logOnDemand('ERR', Output.message, 'CorrId', 'on_demand');
    end
end

% ===========================================================================

function CorrId = corrIdFromTrigger(Input)
    % corrIdFromTrigger Correlation string for logging.
    if isfield(Input, 'request_id') && ~isempty(Input.request_id)
        CorrId = sprintf('%s:%s', char(Input.action), char(Input.request_id));
    else
        CorrId = char(Input.action);
    end
end