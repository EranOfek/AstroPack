
function Result = processSlewJson(json_text)
    % Process Slew calculation for ULTRASAT
    % 
    % Input   : - json_text (string) with fields:
    %              ra1, dec1, ra2, dec2, time (ISO format)
    % Output  : struct ResponseMessage with fields:
    %              message, result, json_text
    %
    % Author  : Chen Tishler (2025)
    % Example : 
    %   json_in = '{"ra1":10.5,"dec1":-20.0,"ra2":15.8,"dec2":-22.1,"time":"2028-07-01T12:00:00Z"}';
    %   out = processSlewJson(json_in);

    % Decode JSON input
    input_data = jsondecode(json_text);

    out = struct;
    out.message   = 'MATLAB: processSlew started';
    out.result    = -1;
    out.json_text = '';

    % Actual processing
    [slew_out, message] = doProcessSlew(input_data);

    % Done
    out.message   = message;
    slew_out.message = '';
    out.result    = 0;
    out.json_text = jsonencode(slew_out);
    out.json_text = strrep(out.json_text, '"', '\"');  % Escape quotes for JSON string safety

    Result = out;
end

% ------------------------------------------------------------------------

function [Result, Message] = doProcessSlew(Params)
    % Process Slew calculation
    % See ultrasat.tools.calcSlew
    %
    % Input  : Params struct with:
    %            ra1, dec1, ra2, dec2, time (ISO string)
    % Output : Result struct with:
    %            slew_time (sec)
    %            direct_slew (bool)
    %          Message string with info or error
    %
    % Author : Chen Tishler (2025)

    %io.msgLog(LogLevel.Debug, 'doProcessSlew: started - Params:');
    %disp(Params);

    try
        % Create helper (if class-based environment, else call directly)
        dt = parseIsoDatetime(Params.time);  % convert ISO to datetime
        jd = juliandate(dt);           % datetime to Julian

        [T_sec, DirectSlewBool] = ultrasat.tools.calcSlew( ...
            Params.ra1, Params.dec1, Params.ra2, Params.dec2, ...
            'Units', 'deg', 'JD', jd);

        % Truncate or round to 1 digits after decimal
        T_sec = round(T_sec, 1);

        io.msgLog(LogLevel.Info, sprintf( ...
            'doProcessSlew: calcSlew(ra1=%.3f, dec1=%.3f, ra2=%.3f, dec2=%.3f, JD=%.5f) -> SLEW=%.1f sec, direct=%d', ...
            Params.ra1, Params.dec1, Params.ra2, Params.dec2, jd, T_sec, DirectSlewBool));

        Result = struct;
        Result.slew   = T_sec;
        Result.direct = DirectSlewBool;
        Message = 'calcSlew: OK';
        %io.msgLog(LogLevel.Debug, sprintf('doProcessSlew: done - slew=%.2f sec, direct=%d', T_sec, DirectSlewBool));
    catch ex
        Message = sprintf("doProcessSlew: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Message);
        Result = struct;
        Result.slew   = -1;
        Result.direct = false;
    end
end
