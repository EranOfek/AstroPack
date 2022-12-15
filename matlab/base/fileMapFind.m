function Result = FileMapFind(FileName, Single)
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
        Single = true
    end
    
    if Single
        Result = FileMap.getSingleton().findFile1(FileName);
        assert(numel(Result) > 0);
    else
        Result = FileMap.getSingleton().findFile(FileName);
        assert(numel(Result) > 0);
    end
end
