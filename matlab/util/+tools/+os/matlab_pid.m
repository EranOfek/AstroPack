function [PID, Host] = matlab_pid
% Return the matlab PID and computer host name
% Example: [PID, Host] = tools.os.matlab_pid;

Str   = java.lang.management.ManagementFactory.getRuntimeMXBean.getName.char;
SpStr = strsplit(Str,'@');
PID   = str2double(SpStr{1});
Host  = SpStr{2}; 

% see also:
% p=feature('getpid')