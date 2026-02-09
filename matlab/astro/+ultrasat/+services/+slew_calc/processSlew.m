function Result = processSlew(item)
% Process single slew: flat input struct (from, to, optional time), flat output.
%
% Input  : item struct with .from (ra, dec, roll), .to (ra, dec, roll), optional .time (ISO)
% Output : struct with .message, .result, .slew, .direct (no json_text)

    out = struct;
    out.message = 'MATLAB: processSlew started';
    out.result  = -1;
    out.slew    = [];
    out.direct  = [];

    try
        ra1 = item.from.ra;
        dec1 = item.from.dec;
        ra2 = item.to.ra;
        dec2 = item.to.dec;
        timeIso = '';
        if isfield(item, 'time') && ~isempty(item.time)
            timeIso = item.time;
        end
        [res, msg] = ultrasat.services.slew_calc_service.doProcessSlew(ra1, dec1, ra2, dec2, timeIso);
        out.message = msg;
        out.result  = 0;
        out.slew    = res.slew;
        out.direct  = res.direct;
    catch ex
        out.message = sprintf('MATLAB: processSlew exception: %s', ex.message);
        out.result  = -1;
        out.slew    = -1;
        out.direct  = false;
    end

    Result = out;
end
