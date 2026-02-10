function Output = processRequest(Input)
    % Process TooPlanner request for ULTRASAT

    Output = struct;
    Output.status    = 'error';    
    Output.message   = 'processTooJson: started';
    Output.summaryFileName = '';

    try
        if strcmp(Input.action, 'health')
            Output.status  = 'ok';            
            Output.message = 'health: OK';        
        elseif strcmp(Input.action, 'too_planner')
            Output = processTooPlanner(Input);
        else
            Output.status = 'error';            
            Output.message = 'processRequest: unknown action';
        end
    catch ex
        Output.status = 'error';        
        Output.message = sprintf("processRequest: error: identifier='%s', message='%s'", ex.identifier, ex.message);
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

        % Prepare output struct
        Output = struct;
        Output.status  = 'ok';
        Output.message = 'TooPlannerRunner: OK';        
        Output.summaryFileName = summaryFileName;

    catch ex
        Output.status  = 'error';
        Output.message = sprintf("processTooPlanner: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Output.message);
    end
end
