function Var = load1(MatFile, varargin)
    % Load a mat file into a variable, this is a simple wrapper around
    % load(). When running under 'isdeployed', and file name does not
    % contain path, use fileMapFind to locate the file.
    %
    % Input  : - Mat file name.
    %          * Additional parameters to pass to the load.m function.
    % Output : - Same as load()
    % Author : Chen Tishler, Dec 2022
    % Example: 
    
    if isdeployed
        MatFile = fileMapFind(MatFile);
    end
    
    Var = load(MatFile, varargin{:});
end
