%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.common.parseIsoDatetime.m
% Author      : Chen Tishler
% Created     : 02/11/2021
% Modified    : 10/02/2026
% Description : JSON file IPC class
%==========================================================================

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
