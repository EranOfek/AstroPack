function Result = getPid()
    % Return current process id
    % Author: Chen Tishler (Jul 2023)

    persistent Pid;
    if isempty(Pid)
        Pid = feature('getpid');
    end
    Result = Pid;
end
