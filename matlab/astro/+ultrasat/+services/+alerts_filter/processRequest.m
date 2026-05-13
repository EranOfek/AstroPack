%==========================================================================
% Project     : ULTRASAT Incoming Alerts Filter
% Filename    : ultrasat/+services/+alerts_filter/processRequest.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 12/05/2026
% Description : MATLAB service to process AlertsFilter requests using JsonFileIpc
%==========================================================================

function Output = processRequest(Input)
    % Process AlertsFilter request for ULTRASAT

    Output = struct;
    Output.message   = 'processAlertsFilter: started';
    Output.status    = 'error';

    try
        if ~isfield(Input, 'action')
            error('AlertsFilter:MissingAction', 'Missing required field: action');
        end

        action = string(Input.action);

        if action == "health"
            Output.status  = 'ok';            
            Output.message = 'health: OK';        
        elseif action == "filter_lvc"
            Output = processFilterLvc(Input);
        else
            Output.message = sprintf('processRequest: unknown action: %s', action);
            Output.status = 'error';
        end
    catch ex
        Output.message = sprintf("processRequest: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        Output.status = 'error';
        io.msgLog(LogLevel.Error, Output.message);
    end
end

% ===========================================================================

function Output = processFilterLvc(Input)
    % Process AlertsFilter request for ULTRASAT

    % Get the logger
    logger = MsgLogger.getSingleton();

    try
        if ~isfield(Input, 'alert_file') || isempty(Input.alert_file)
            error('AlertsFilter:MissingAlertFile', 'Missing required field: alert_file');
        end

        if ~isfield(Input, 'filter') || isempty(Input.filter)
            Input.filter = 'simple';
        end

        alertFile = char(string(Input.alert_file));
        if ~isfile(alertFile)
            error('AlertsFilter:AlertFileNotFound', 'Alert file not found: %s', alertFile);
        end

        logger.info("Loading LVC alert file: %s", alertFile);
        alert = ultrasat.alerts_filters.lvc.models.LvcParsedAlert.loadFromJsonFile(alertFile);

        % Set the alert in the input
        Input.alert = alert;        

        % Process the alert        
        result = ultrasat.alerts_filters.lvc.filters.lvc_filter(Input, logger);
        
        % Return the result
        Output = struct;
        Output.status  = 'ok';
        Output.message = 'processFilterLvc: OK';        
        Output.result = result.toStruct();
    catch ex
        Output = struct;
        Output.status  = 'error';
        Output.message = sprintf("processFilterLvc: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end

