function Result = getProcessName()
    % Return current process name
    % Author: Chen Tishler, 11/2023
    persistent ProcessName;
    
    if isempty(ProcessName)
        List = tools.os.getProcessList();
        Pid = feature('getpid');
        
        % Extract the numeric values from the first column of the cell array
        numbers = cell2mat(List(:, 1));
        
        % Find the index where the number matches
        idx = find(numbers == Pid);
    
        % If the number is found, return the corresponding string
        if ~isempty(idx)
            ProcessName = List{idx, 2};
        else
            ProcessName = [];
        end
    end
    Result = ProcessName;
end


            