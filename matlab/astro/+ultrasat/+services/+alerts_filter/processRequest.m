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
    Output.summaryFileName = '';

    try
        if strcmp(Input.action, 'health')
            Output.status  = 'ok';            
            Output.message = 'health: OK';        
        elseif strcmp(Input.action, 'filter_lvc')
            Output = processFilterLvc(Input);
        else
            Output.message = 'processRequest: unknown action';
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

    % Load the alert from the input file
    alert = []];
    try
        alert = ultrasat.alerts_filters.lvc.models.LvcParsedAlert.loadFromJsonFile(Input.alert_file);
    catch ex
        Output = struct;
        Output.status  = 'error';
        Output.message = sprintf("processFilterLvc: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end

    if isempty(alert)
        return;
    end

    try
        % Set the alert in the input
        Input.alert = alert;        

        % Process the alert        
        result = ultrasat.alerts_filters.lvc.filters.lvc_filter(Input, logger);
        
        % Return the result
        Output = struct;
        Output.status  = 'ok';
        Output.message = 'processFilterLvc: OK';        
        Output.result = result;
    catch ex
        Output = struct;
        Output.status  = 'error';
        Output.message = sprintf("processFilterLvc: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end

