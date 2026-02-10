function Output = processRequest(Input)
    % Process TooPlanner request for ULTRASAT

    Output = struct;
    Output.message   = 'processTooJson: started';
    Output.status    = 'error';
    Output.summaryFileName = '';

    try
        if strcmp(Input.action, 'too_planner')
            Output = processTooPlanner(Input);
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

function Output = processTooPlanner(Input)
    % Process TooPlanner request for ULTRASAT

    try
        % Create helper (if class-based environment, else call directly)
        runner = ultrasat.planner.TooPlannerRunner();
        summaryFileName = runner.runFromJson(Input.filename);
        Output = struct;
        Output.summaryFileName = summaryFileName;
        Output.status  = 'ok';
        Output.message = 'TooPlanner: OK';
    catch ex
        Output.status  = 'error';
        Output.message = sprintf("processTooPlanner: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end
