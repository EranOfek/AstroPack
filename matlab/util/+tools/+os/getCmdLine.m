function Result = getCmdLine()
    % getCmdLine  Return full command line used to start this MATLAB process.
    %
    %   Result = getCmdLine()
    %
    %   Works on both Windows and Unix-based systems.
    %   On modern Windows (where WMIC is deprecated), uses PowerShell fallback.
    %
    % Author : Chen Tishler, 07/2023
    % Updated: 11/2025

    persistent CmdLine;
    if isempty(CmdLine)
        try
            pid = feature('getpid');
            if ispc
                % --- Try WMIC first (legacy) ---
                [status, cmdLine] = system(sprintf('wmic process where processid=%d get CommandLine 2>nul', pid));
                
                if status ~= 0 || contains(cmdLine, 'not recognized', 'IgnoreCase', true)
                    % --- WMIC not available, use PowerShell fallback ---
                    psCmd = sprintf(['powershell -NoProfile -Command "Get-CimInstance Win32_Process ' ...
                                     '-Filter \\\"ProcessId=%d\\\" | Select-Object -ExpandProperty CommandLine"'], pid);
                    [status, cmdLine] = system(psCmd);
                end
            else
                % --- Unix / Linux / macOS ---
                [status, cmdLine] = system(sprintf('ps -p %d -o args=', pid));
            end

            % --- Validate result ---
            if status ~= 0 || isempty(strtrim(cmdLine))
                CmdLine = '(unknown)';
            else
                CmdLine = strtrim(cmdLine);
            end
        catch ME
            % In case of any unexpected error
            CmdLine = sprintf('(error retrieving cmd line: %s)', ME.message);
        end
    end
    Result = CmdLine;
end
