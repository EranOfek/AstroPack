function Output = processRequest(Input)
    % Process AlertsFilter request for ULTRASAT

    Output = struct;
    Output.message   = 'processAlertsFilter: started';
    Output.status    = 'error';
    Output.summaryFileName = '';

    try
        if strcmp(Input.action, 'alerts_filter')
            Output = processAlertsFilter(Input);
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

function Output = processAlertsFilter(Input)
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


