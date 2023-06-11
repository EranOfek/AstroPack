function Result = getLogDate(Args)
    % Return the current date, either in Coordinated Universal Time (UTC) or in the local timezone.
    % If Utc is true, the function returns the current date according to UTC.
    % If Utc is false, the function returns the current date according to the local timezone. 
    % However, the date switches over at noon instead of at midnight. 
    % This means that before noon, the function considers it to be the
    % previous day.
    arguments
        Args.Utc = true;
    end

    % If Utc is true, get the current date in UTC format
    if Args.Utc
        Result = datetime('now', 'TimeZone', 'UTC');
    else
        % If Utc is false, get the current date in local time zone
        Now = datetime('now');

        % If the current time is before noon, subtract one day from the date
        if hour(Now) < 12
            Result = dateshift(Now, 'start', 'day') - days(1);
        else
            Result = dateshift(Now, 'start', 'day');
        end
    end
end        
