%==========================================================================
% Project     : ULTRASAT
% Filename    : +ultrasat/+tools/parseUtcDatetime.m
% Created     : 15/11/2026
% Updated     : 15/11/2026
% Author      : Chen Tishler
% Description : Parse alert/service UTC timestamps into zoned datetime.
%==========================================================================

function dt = parseUtcDatetime(x)
    % parseUtcDatetime Parse ISO-like UTC text to a zoned datetime (UTC).
    %
    % Accepts yyyy-MM-ddTHH:mm:ss[.fff...][Z|+00:00] or space instead of T.
    % Empty input returns NaT. Invalid input raises BadFormat.

    if isempty(x)
        dt = NaT('TimeZone', 'UTC');
        return;
    end

    if isdatetime(x)
        dt = x;
        if isempty(dt.TimeZone)
            dt.TimeZone = 'UTC';
        end
        return;
    end

    text = strtrim(char(string(x)));
    if isempty(text)
        dt = NaT('TimeZone', 'UTC');
        return;
    end

  % Strip UTC suffix markers.
    if endsWith(text, 'Z')
        text = text(1:end-1);
    end
    if endsWith(text, '+00:00')
        text = text(1:end-6);
    end

    text = strrep(text, ' ', 'T');

  % Truncate fractional seconds to milliseconds for stable InputFormat.
    dotIdx = strfind(text, '.');
    if ~isempty(dotIdx)
        tIdx = strfind(text, 'T');
        if ~isempty(tIdx) && dotIdx(1) > tIdx(1)
            head = text(1:dotIdx(1));
            frac = text(dotIdx(1)+1:end);
            if length(frac) > 3
                frac = frac(1:3);
            end
            text = [head frac]; %#ok<AGROW>
        end
    end

    try
        if contains(text, '.')
            dt = datetime(text, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss.SSS', 'TimeZone', 'UTC');
        else
            dt = datetime(text, 'InputFormat', 'yyyy-MM-dd''T''HH:mm:ss', 'TimeZone', 'UTC');
        end
    catch
        error('ultrasat:parseUtcDatetime:BadFormat', ...
            'Cannot parse UTC datetime: %s', char(x));
    end
end