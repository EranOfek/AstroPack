function [Flag, Result] = isUTC
    % (Linux only) Check if computer time zone is UTC
    % Input  : null
    % Output : - True if time zone is UTC
    %          - Time zone string
    % Author : Eran Ofek (2024 Dec) 
    % Example: [F,R]=celestial.time.isUTC;

    if isunix 
        [~,Result]=system('timedatectl show --property=Timezone --valu');
        Result = regexprep(Result,'\s*\r*\n*','');
        if strcmp(Result, 'UTC')
            Flag = true;
        else
            Flag = false;
        end
    else
        Result = [];
        warning('isUTC works only on linux machines')
        Flag   = false;
    end

end
