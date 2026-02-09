function Result = processTooJson(json_text)
    % Process TooPlanner request for ULTRASAT
    % 
    % Input   : - json_text (string) with fields:
    %              json_file (string) with path to JSON file
    % Output  : struct ResponseMessage with fields:
    %              message, result, json_text
    %
    % Author  : Chen Tishler (2026)
    % Example : 
    %   json_in = '{"json_file": "s:/too_planner/runner1.json"}';
    %   out = processTooJson(json_in);

    % Decode JSON input
    input_data = jsondecode(json_text);

    out = struct;
    out.message   = 'MATLAB: processTooJson started';
    out.result    = -1;
    out.json_text = '';

    % Actual processing
    [too_out, message] = doProcessToo(input_data);

    % Done
    out.message   = message;
    too_out.message = '';
    out.result    = 0;
    out.json_text = jsonencode(too_out);
    out.json_text = strrep(out.json_text, '"', '\"');  % Escape quotes for JSON string safety

    Result = out;
end

% ------------------------------------------------------------------------

function [Result, Message] = doProcessToo(Params)
    % Process TooPlanner request
    % See ultrasat.planner.TooPlannerRunner.m
    % Input  : Params struct with:
    %            json_file (string) with path to JSON file
    % Output : Result struct with:
    %            message (string) with info or error
    %            result (int) with 0 for success, -1 for error
    %            json_text (string) with JSON response
    %
    % Author : Chen Tishler (2026)

    %io.msgLog(LogLevel.Debug, 'doProcessToo: started - Params:');
    %disp(Params);

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
