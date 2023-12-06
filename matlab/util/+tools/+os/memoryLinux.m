function [Mem, CPU] = memoryLinux
    % Get the MATLAB memory and CPU usage (for Linux only).
    % Output : - Memory usage [%].
    %          - CPU usage [%].
    % Author : Eran Ofek (Dec 2021)
    % Example: [Mem, CPU] = tools.os.memoryLinux
    
    PID = tools.os.matlab_pid;
    Cmd = sprintf('ps -p %d -o %%cpu,%%mem',PID);
    [Return,Output] = system(Cmd);
    
    CPU = str2double(Output(11:14));
    Mem = str2double(Output(16:19));
    
    
end