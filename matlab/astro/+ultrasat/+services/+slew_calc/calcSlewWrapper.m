%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat/+services/+slew_calc/calcSlewWrapper.m
% Author      : Chen Tishler
% Created     : 02/11/2025
% Modified    : 10/02/2026
% Description : Wrapper for calcSlew to calculate slew time between targets and return result in struct
%==========================================================================

function Result = calcSlewWrapper(ra1, dec1, ra2, dec2, TimeIso)
    % Wrapper for calcSlew to calculate slew time between targets and return result in struct
    % Input:   ra1, dec1, ra2, dec2 - ra and dec in degrees
    %          TimeIso - optional ISO string
    % Output:  Result - result struct with .slew (sec), .direct (bool)
    % Example: Result = calcSlewWrapper(10.5, -20.0, 15.8, -22.1, '2028-07-01T12:00:00Z');

    % Single slew calculation: ra/dec in deg, optional TimeIso (ISO string).
    if nargin < 5
        TimeIso = '';
    end

    % Parse time string
    if ~isempty(TimeIso) && ischar(TimeIso)
        dt = ultrasat.api.utils.DateTimeUtils.parseIsoDatetime(TimeIso);
        if ~isnat(dt)
            jd = juliandate(dt);
        else
            jd = celestial.time.julday('2028-01-01T00:00:00');
        end
    else
        jd = celestial.time.julday('2028-01-01T00:00:00');
    end

    % Calculate slew between two points
    [T_sec, DirectSlewBool] = ultrasat.tools.calcSlew( ...
        ra1, dec1, ra2, dec2, 'Units', 'deg', 'JD', jd);

    % Round to 1 digit after decimal point
    T_sec = round(T_sec, 1);

    io.msgLog(LogLevel.Info, sprintf( ...
        'calcSlew(ra1=%.3f, dec1=%.3f, ra2=%.3f, dec2=%.3f, JD=%.5f) -> SLEW=%.1f sec, direct=%d', ...
        ra1, dec1, ra2, dec2, jd, T_sec, DirectSlewBool));

    % Return result in struct
    Result = struct('slew', T_sec, 'direct', DirectSlewBool);
end
