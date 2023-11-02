function Result = getCmdLine()
    % Return full command line
    % Author: Chen Tishler, 07/2023
    persistent CmdLine;
    if isempty(CmdLine)
        if ispc
            % On Windows, use wmic process
            [~, cmdLine] = system(['wmic process where processid="' num2str(feature('getpid')) '" get CommandLine']);

            % Split the string by newline characters to get individual lines
            lines = strsplit(cmdLine, '\n');
            
            % Find the line that contains the actual command. It should be the line after 'CommandLine'
            cmdIndex = find(cellfun(@(x) contains(x, 'CommandLine'), lines), 1) + 1;
            
            % Get the actual command line string
            cmdLine = lines{cmdIndex};
        else
            % On Unix-based systems (Linux, macOS), use ps
            [~, cmdLine] = system(['ps -p ' num2str(feature('getpid')) ' -o args=']);
        end
        
        % Remove leading/trailing white space
        CmdLine = strtrim(cmdLine);  
    end
    Result = CmdLine;
end
