function Result = getPid()
    % Return current process id
    % Author: Chen Tishler, 07/2023

    persistent Pid;
    if isempty(Pid)
        Pid = feature('getpid');
    end
    Result = Pid;
end
