function Result = getCmdLine()
    % Return full command line
    % Author: Chen Tishler, 07/2023
    if ispc
        % On Windows, use wmic process
        [~, cmdLine] = system(['wmic process where processid="' num2str(feature('getpid')) '" get CommandLine']);
    else
        % On Unix-based systems (Linux, macOS), use ps
        [~, cmdLine] = system(['ps -p ' num2str(feature('getpid')) ' -o args=']);
    end
    
    % Remove leading/trailing white space
    Result = strtrim(cmdLine);  
end
