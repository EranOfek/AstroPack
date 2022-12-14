function Result = readtable2(TableFile,varargin)
    % Load a mat file into a variable
    % Package: Util.IO
    % Description: load a mat file containing a single variable to a variable
    %              name (rather than a structure, like load.m).
    %              If multiple variables are returned then will behave like
    %              load.m
    % Input  : - Mat file name.
    %          * Additional parameters to pass to the load.m function.
    % Output : - Variable name.
    % Author : Chen Tishler, Dec. 2022

    FileName = fileMapFind(TableFile);

    Result = readtable(FileName, varargin{:});

end
