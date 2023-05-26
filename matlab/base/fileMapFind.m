function Result = FileMapFind(FileName, Args)
    % Use singleton FileMap object to locate file, when it does not include
    % a path. This is required to solve the 'addpath issue' for compiled
    % applications.
        % Description: load a mat file containing a single variable to a variable
    %              name (rather than a structure, like load.m).
    %              If multiple variables are returned then will behave like
    %              load.m
    % Input  : - Mat file name.
    %          * Additional parameters to pass to the load.m function.
    % Output : - Variable name.
    % Author : Chen Tishler, Dec. 2022
    arguments
        FileName
        Args.Single = true
        Args.Assert = true
    end
    
    io.msgLog(LogLevel.Debug, 'FileMapFind: %s', FileName);
    if Args.Single
        Result = FileMap.getSingleton().findFile1(FileName);
        if Args.Assert
            assert(numel(Result) > 0);
        end
    else
        Result = FileMap.getSingleton().findFile(FileName);
        if Args.Assert
            assert(numel(Result) > 0);
        end
    end
end
