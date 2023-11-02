% The updateWatchdogFile function writes the current timestamp to a specified 
% file if a given interval (watchdog_write_interval) has elapsed since its 
% last write. The function utilizes a persistent variable to keep track of 
% the last time it wrote to the file. This function is designed to be 
% called periodically, allowing external processes or scripts to monitor 
% the timestamp written to the file as a "heartbeat" or "alive" signal. 
% 
% updateWatchdogFile(WatchdogFilename, WatchdogWriteInterval) 
% 
% Input Arguments: 
% 
% WatchdogFilename (type: string or char): The path to the file where the 
% current timestamp will be written. If the file doesn't exist, it will be 
% created. If the file already exists, it will be overwritten with the new 
% timestamp. 
% 
% WatchdogWriteInterval (type: double): The time interval (in seconds) 
% that should elapse since the last write before the function writes the 
% current timestamp to the file again. 
% 
% Output: 
% 
% No direct output is returned by the function. However, the specified 
% file will be updated with the current timestamp if the given interval 
% has elapsed since the last write. Usage Example: 
% 
% updateWatchdogFile('heartbeat.txt', 10); 
% 
% Notes: 
% 
% The function uses a persistent variable named LastWriteTimes to 
% remember the timestamp of the last write across calls. This means that 
% the elapsed time calculation is consistent even if the function is 
% called multiple times in different parts of a script or during different 
% iterations of a loop. 
% 
% The initial value of LastWriteTimes is set to a time sufficiently long 
% ago to ensure that the function writes to the file upon its first call. 
% 
% If the function is called before the WatchdogWriteInterval has elapsed 
% since the last write, it will not update the file. Only once the 
% specified interval has passed will the function write the current 
% timestamp to the file again. 
% 
% 

function updateWatchdogFile(WatchdogFilename, WatchdogWriteInterval)
    persistent LastWriteTimes; % This will store last write times for multiple files
    persistent Pid;
    persistent ProcessName;
    persistent CmdLine;

    % Initialize the LastWriteTimes map during the first call
    if isempty(LastWriteTimes)
        LastWriteTimes = containers.Map('KeyType', 'char', 'ValueType', 'any');
        Pid = tools.os.getPid();
        ProcessName = tools.os.getProcessName();
        CmdLine = tools.os.getCmdLine();
    end

    CurrentTime = datetime('now', 'TimeZone', 'UTC');
    
    % Check if this specific file has a stored last write time
    if isKey(LastWriteTimes, WatchdogFilename)
        LastTime = LastWriteTimes(WatchdogFilename);
    else
        % Set the last time to a time long enough ago to ensure the first call writes to the file
        LastTime = datetime('now', 'TimeZone', 'UTC') - minutes(10);
    end
    
    ElapsedTime = seconds(CurrentTime - LastTime);

    % Check if WatchdogWriteInterval has elapsed since the last write
    if ElapsedTime >= WatchdogWriteInterval
        % Write the current time to the file
        fid = fopen(WatchdogFilename, 'w');
        fprintf(fid, '%s\n', datestr(CurrentTime, 'yyyy-mm-dd HH:MM:SS'));
        fprintf(fid, '%d\n', Pid);
        fprintf(fid, '%s\n', ProcessName);
        fprintf(fid, '%s\n', CmdLine);
        fclose(fid);
        
        % Update the last write time for this specific file
        LastWriteTimes(WatchdogFilename) = CurrentTime;
    end
end