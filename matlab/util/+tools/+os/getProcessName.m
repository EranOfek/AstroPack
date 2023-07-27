function Result = getProcessName()
    % Return true if current operating system is Linux
    % Author: Chen Tishler, 07/2023    
    [~, name, ext] = fileparts(mfilename('fullpath'));
    Result = strcat(name, ext);
end


            