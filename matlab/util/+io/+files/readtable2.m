function Var = readtable2(TableFile,varargin)
    % Load table file into a variable, this is a simple wrapper around
    % readtable(). When running under 'isdeployed', and file name does not
    % contain path, use fileMapFind to locate the file.
    %
    % Input  : - Table file name.
    %          * Additional parameters to pass to the readtable.m function.
    % Output : - Same as readtable()
    % Author : Chen Tishler, Dec 2022
    % Example: 

    % @Deploy - Use singleton file mapper to locate the file
    if isdeployed    
        FileName = fileMapFind(TableFile);
    end

    Var = readtable(FileName, varargin{:});
end
