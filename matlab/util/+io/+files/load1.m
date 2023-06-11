function Var = load1(MatFile, varargin)
    % Load a mat file into a variable, this is a simple wrapper around
    % load(). When running under 'isdeployed', and file name does not
    % contain path, use fileMapFind to locate the file.
    %
    % Input  : - Mat file name.
    %          * Additional parameters to pass to the load.m function.
    % Output : - Same as load()
    % Author : Chen Tishler, May 2023
    % Example: io.files.load1(filename,'UP');
    % Example: UP = io.files.load1(filename);
    
    io.msgLog(LogLevel.Debug, 'load1: %s', MatFile);
    
    % Executed only in deployed (compiled) app
    if isdeployed
        if ~isfile(MatFile)
            MatFile = fileMapFind(MatFile);
        end
    end
    
    if nargout == 0
        % Without return value, assign each variable to the caller's
        % workspace using assignin()
        Tmp = load(MatFile, varargin{:});
        varnames = fieldnames(Tmp);
        for i = 1:length(varnames)
            assignin('caller', varnames{i}, Tmp.(varnames{i}));
        end        
    else
        % Return single var to the caller
        % @Todo - Do we need to behave like load2() and return the single item
        % from the struct, or just return the struct as it is ???
        Tmp = load(MatFile, varargin{:});
        if isstruct(Tmp)
           FN = fieldnames(Tmp);
           if length(FN) == 1
               Var = Tmp.(FN{1});
           else
               Var = Tmp;
           end
        else
            Var = Tmp;
        end
    end
end
