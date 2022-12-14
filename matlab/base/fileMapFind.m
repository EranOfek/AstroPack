function Result = FileMapFind(FileName)
    % Load a mat file into a variable
        % Description: load a mat file containing a single variable to a variable
    %              name (rather than a structure, like load.m).
    %              If multiple variables are returned then will behave like
    %              load.m
    % Input  : - Mat file name.
    %          * Additional parameters to pass to the load.m function.
    % Output : - Variable name.
    % Author : Chen Tishler, Dec. 2022

    Result = FileMap.getSingleton().findFile1(FileName);
	assert(numel(Result) > 0);
end
