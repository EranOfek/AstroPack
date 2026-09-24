function [Result] = getTimeStamp(Time)
    % Get time stamp for now.
    % Input  : - datenum time. Default is now.
    % Output : - ISO time stamp.
    % Author : Eran Ofek (2024 Jul) 
    % Example: tools.timeStamp.getTimeStamp

    arguments
        Time = [];
    end
    if isempty(Time)
        Time = now;
    end

    Result = sprintf('%s',datestr(Time,'yyyy-mm-ddTHH:MM:SS'));

end
