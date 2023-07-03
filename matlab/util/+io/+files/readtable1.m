function Var = readtable1(TableFile,varargin)
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
    %io.msgLog(LogLevel.Debug, 'readtable1: %s', TableFile);
    if isdeployed    
        if ~isfile(TableFile)        
            FileName = fileMapFind(TableFile);
        end
    end

    Var = readtable(FileName, varargin{:});
end
