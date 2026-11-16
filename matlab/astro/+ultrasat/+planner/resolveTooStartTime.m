%==========================================================================
% Project     : ULTRASAT Observation Planner
% Filename    : +ultrasat/+planner/resolveTooStartTime.m
% Created     : 15/11/2026
% Updated     : 15/11/2026
% Author      : Chen Tishler
% Description : Resolve too_start_time spec to UTC datetime for buildTOO.
%==========================================================================

function [startTime, info] = resolveTooStartTime(spec, alertFile)
    % resolveTooStartTime Convert request too_start_time to UTC datetime.
    %
    % spec:
    %   empty or "now" -> datetime('now','TimeZone','UTC')
    %   "+hh:mm:ss"     -> time_created from alertFile + duration
    %   otherwise       -> absolute UTC via parseUtcDatetime
    %
    % info: struct for logging/metadata (mode, spec, alert_time_utc, start_time_utc).

    if nargin < 2
        alertFile = "";
    end

    info = struct();
    info.spec = char(string(spec));
    info.mode = "";
    info.alert_time_utc = "";
    info.start_time_utc = "";

    specStr = strtrim(string(spec));
    if strlength(specStr) == 0 || strcmpi(specStr, "now")
        startTime = datetime('now', 'TimeZone', 'UTC');
        info.mode = "now";
        info.start_time_utc = isoUtcString(startTime);
        return;
    end

  % Relative offset from parsed alert time_created (e.g. "+00:03:00").
    if startsWith(specStr, "+")
        info.mode = "relative";
        relText = extractAfter(specStr, 1);
        if strlength(relText) == 0
            error('ultrasat:planner:BadStartSpec', 'Empty relative too_start_time');
        end

        alertPath = char(string(alertFile));
        if strlength(string(alertFile)) == 0 || ~isfile(alertPath)
            error('ultrasat:planner:AlertTimeUnavailable', ...
                'Relative too_start_time requires alert_file; missing or not found: %s', alertPath);
        end

        try
            alert = ultrasat.alerts_filters.lvk.models.LvkParsedAlert.loadFromJsonFile(alertPath);
        catch Ex
            error('ultrasat:planner:AlertTimeUnavailable', ...
                'Cannot load alert_file %s: %s', alertPath, Ex.message);
        end

        alertTime = alert.time_created;
        if isnat(alertTime)
            error('ultrasat:planner:AlertTimeUnavailable', ...
                'alert_file has no time_created: %s', alertPath);
        end

        if isempty(alertTime.TimeZone)
            alertTime.TimeZone = 'UTC';
        end

        try
            delta = duration(relText);
        catch
            error('ultrasat:planner:BadStartSpec', ...
                'Invalid relative duration in too_start_time: %s', char(relText));
        end

        startTime = alertTime + delta;
        startTime.TimeZone = 'UTC';
        info.alert_time_utc = isoUtcString(alertTime);
        info.start_time_utc = isoUtcString(startTime);
        return;
    end

  % Absolute ISO timestamp.
    info.mode = "absolute";
    startTime = ultrasat.tools.parseUtcDatetime(specStr);
    if isnat(startTime)
        error('ultrasat:planner:BadStartSpec', ...
            'Absolute too_start_time did not parse: %s', char(specStr));
    end
    info.start_time_utc = isoUtcString(startTime);
end


function s = isoUtcString(dt)
    % Format zoned datetime as yyyy-MM-ddTHH:mm:ss.SSSZ for JSON metadata.
    if isnat(dt)
        s = "";
        return;
    end
    if isempty(dt.TimeZone)
        dt.TimeZone = 'UTC';
    end
    s = char(string(datestr(dt, 'yyyy-mm-ddTHH:MM:SS.FFFZ')));
end