
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

    io.msgLog(LogLevel.Debug, 'doProcessSlew: started - Params:');
    disp(Params);

    try
        % Create helper (if class-based environment, else call directly)
        dt = parseIsoDatetime(Params.time);  % convert ISO to datetime
        jd = juliandate(dt);           % datetime to Julian

        [T_sec, DirectSlewBool] = ultrasat.tools.calcSlew( ...
            Params.ra1, Params.dec1, Params.ra2, Params.dec2, ...
            'Units', 'deg', 'JD', jd);

        % Truncate or round to 3 digits after decimal
        T_sec = round(T_sec, 1);

        Result = struct;
        Result.slew_time   = T_sec;
        Result.direct_slew = DirectSlewBool;
        Message = 'doProcessSlew: success';
        io.msgLog(LogLevel.Debug, sprintf('doProcessSlew: done - slew=%.2f sec, direct=%d', T_sec, DirectSlewBool));

    catch ex
        Message = sprintf("doProcessSlew: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Message);
        Result = struct;
        Result.slew_time   = -1;
        Result.direct_slew = false;
    end
end

%========================================================================

function dt = parseIsoDatetime(str)
    % @TODO - Move to common file - like +api/TimeUtils.m ?

    % parseIsoDatetime  Parse ISO 8601 datetime strings with 'Z' or timezone offsets.
    %
    %   dt = parseIsoDatetime(str)
    %
    %   Supports:
    %       2025-01-01T00:00:00Z
    %       2025-01-01T00:00:00.000Z
    %       2025-01-01T00:00:00.000000Z
    %       2025-01-01T00:00:00+00:00
    %       2025-01-01T00:00:00.000+00:00
    %
    %   Returns datetime with TimeZone = 'UTC'.
    %   Returns NaT if parsing fails.

    dt = NaT;

    try
        if isempty(str)
            return;
        end

        % Convert string type if needed
        if isstring(str)
            str = char(str);
        end

        str = strtrim(str);

        % List of acceptable input formats (from most to least precise)
        fmts = { ...
            'yyyy-MM-dd''T''HH:mm:ss.SSSSSS''Z''', ...
            'yyyy-MM-dd''T''HH:mm:ss.SSS''Z''', ...
            'yyyy-MM-dd''T''HH:mm:ss''Z''', ...
            'yyyy-MM-dd''T''HH:mm:ss.SSSSSSXXX', ...
            'yyyy-MM-dd''T''HH:mm:ss.SSSXXX', ...
            'yyyy-MM-dd''T''HH:mm:ssXXX' ...
        };

        % Try each format until one works
        for i = 1:numel(fmts)
            try
                dt = datetime(str, 'InputFormat', fmts{i}, 'TimeZone', 'UTC');
                if ~isnat(dt)
                    return;
                end
            catch
                % continue trying
            end
        end

        % If still NaT, issue a warning
        if isnat(dt)
            warning('parseIsoDatetime:UnknownFormat', ...
                'String does not match expected ISO 8601 formats: "%s"', str);
        end

    catch ME
        warning('parseIsoDatetime:Failed', ...
            'Failed to parse datetime string "%s": %s', str, ME.message);
    end
end

