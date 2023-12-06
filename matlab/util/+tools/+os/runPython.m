function [Status, Output] = runPython(ScriptName, Args)
    % Run python with specified script
    % Assume that we have python3 on our path.
    % On Windows: python3.exe folder should be on SYSTEM PATH (not USER PATH).
    %
    % Author  : Chen Tishler
    % Input   : ScriptName - full path to python script to run
    % Output  : Status: 0 on success, otherwise process exit code
    %           Output: Process console output
    % Eaxmple : runPython('./my_script.py')

    % Prepare command line, assume we have 'python3' installed
    % Note: python3 
    % Windows: python3.exe should be on SYSTEM PATH (not USER PATH).
    %
    % See: https://stackoverflow.com/questions/47539201/python-is-not-recognized-windows-10
    % For example: Add both C:\Python38 and C:\Python38\Scripts
    %
    arguments
        ScriptName   = ''   %
        Args.ArgsStr = ''   %
    end
    
    Cmd = sprintf('python3 %s %s', ScriptName, Args.ArgsStr);
    io.msgLog(LogLevel.Info, 'runPython: %s', Cmd);

    % This may take a while...
    [Status, Output] = system(Cmd);
    
    %
    io.msgLog(LogLevel.Info, 'runPython: Status: %d', Status);
    io.msgLog(LogLevel.Info, 'runPython: Output: %s', Output);
    if Status ~= 0
        io.msgLog(LogLevel.Error, 'runPython: FAILED to execute, make sure that python3 is found on your PATH: %s', Cmd);
    end

end
