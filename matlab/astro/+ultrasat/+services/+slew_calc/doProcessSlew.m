function [Result, Message] = doProcessSlew(ra1, dec1, ra2, dec2, TimeIso)
% Single slew calculation: ra/dec in deg, optional TimeIso (ISO string).
% Used by processSlew and processSlewBatch.
%
% Input  : ra1, dec1, ra2, dec2 (deg), TimeIso (optional, ISO datetime or empty)
% Output : Result struct with .slew (sec), .direct (bool); Message string.

    if nargin < 5
        TimeIso = '';
    end

    if ~isempty(TimeIso) && ischar(TimeIso)
        dt = ultrasat.services.slew_calc_service.parseIsoDatetime(TimeIso);
        if ~isnat(dt)
            jd = juliandate(dt);
        else
            jd = celestial.time.julday('2028-01-01T00:00:00');
        end
    else
        jd = celestial.time.julday('2028-01-01T00:00:00');
    end

    try
        [T_sec, DirectSlewBool] = ultrasat.tools.calcSlew( ...
            ra1, dec1, ra2, dec2, 'Units', 'deg', 'JD', jd);
        T_sec = round(T_sec, 1);
        io.msgLog(LogLevel.Info, sprintf( ...
            'doProcessSlew: calcSlew(ra1=%.3f, dec1=%.3f, ra2=%.3f, dec2=%.3f, JD=%.5f) -> SLEW=%.1f sec, direct=%d', ...
            ra1, dec1, ra2, dec2, jd, T_sec, DirectSlewBool));
        Result = struct('slew', T_sec, 'direct', DirectSlewBool);
        Message = 'calcSlew: OK';
    catch ex
        Message = sprintf("doProcessSlew: error: identifier='%s', message='%s'", ex.identifier, ex.message);
        io.msgLog(LogLevel.Error, Message);
        Result = struct('slew', -1, 'direct', false);
    end
end
