function Result = processSlewJsonBatch(json_text)
    % Process batch of Slew calculations for ULTRASAT
    %
    % Input  : - json_text (string) with field "items": array of structs
    %              Each item has: ra1, dec1, ra2, dec2, time (ISO format)
    % Output : struct ResponseMessage with fields:
    %              message, result, json_text
    %
    % Author : Chen Tishler (2025)
    % Example:
    %   json_in = '{"items":[
    %       {"ra1":10.5,"dec1":-20,"ra2":15.8,"dec2":-22.1,"time":"2028-07-01T12:00:00Z"},
    %       {"ra1":50.0,"dec1":10,"ra2":60.0,"dec2":15,"time":"2028-07-02T00:00:00Z"}
    %   ]}';
    %   out = processSlewJsonBatch(json_in);

    % Decode JSON input
    input_data = jsondecode(json_text);

    out = struct;
    out.message   = 'MATLAB: processSlewBatch started';
    out.result    = -1;
    out.json_text = '';

    try
        if ~isfield(input_data, 'items')
            error('processSlewJsonBatch:MissingField', ...
                'Input JSON must contain an "items" array.');
        end

        items = input_data.items;
        N = numel(items);
        results = repmat(struct('slew_time', [], 'direct_slew', []), 1, N);

        % Loop through all batch items
        for i = 1:N
            try
                [res, msg] = doProcessSlew(items(i));
                results(i).slew   = res.slew;
                results(i).direct = res.direct;
            catch ex
                io.msgLog(LogLevel.Error, ...
                    sprintf('processSlewJsonBatch: error in item %d: %s', i, ex.message));
                results(i).slew   = -1;
                results(i).direct = false;
            end
        end

        % Prepare JSON output
        batch_out = struct('results', {results});
        out.json_text = jsonencode(batch_out);
        out.json_text = strrep(out.json_text, '"', '\"');  % escape for JSON transport
        out.message   = sprintf('MATLAB: processSlewBatch completed (%d items)', N);
        out.result    = 0;

    catch ex
        out.message = sprintf('processSlewJsonBatch: exception: %s', ex.message);
        out.result  = -1;
        io.msgLog(LogLevel.Error, out.message);
    end

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

        io.msgLog(LogLevel.Debug, sprintf( ...
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
