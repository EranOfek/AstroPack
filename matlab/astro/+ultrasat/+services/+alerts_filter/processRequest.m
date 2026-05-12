%==========================================================================
% Project     : ULTRASAT SOC
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

    try
        % Create helper (if class-based environment, else call directly)
        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(Params.filename);
        Result = struct;
        Result.summaryFileName = summaryFileName;
        Message = 'TooPlanner: OK';
    catch ex
        Result = struct;
        Result.summaryFileName = '';
        Message = sprintf("doProcessToo: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Message);
    end
end


