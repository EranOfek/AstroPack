function [FuncArgs,ErrorMsg] = updateParFromConfig(FuncArgs, Conf, ConfName)
    % Update function arguments from a config
    %   The function get structure of function arguments, if a field exist
    %   in the configuration file then its content will be written to the
    %   structure of arguments.
    % Input  : - the parameter structure to be changed (usually, Args)
    %          - Configuration object. If empty, then will create.
    %            Default is [].
    %          - the name of the config .yml file   
    % Output : - The update structure of arguments.
    % Author : Eran Ofek (May 2025)
    % Example: Args.MinNumImageVisit=5; Args.BB={'a',[]};  Args = tools.args.updateParFromConfig(Args,[], 'LAST.Pipeline.Reduction');
   
    arguments
        FuncArgs      
        Conf                   = [];
        ConfName               = 'LAST.Pipeline.Reduction';
    end
    
    if isempty(Conf)
        % make a class object only once    
        Conf = Configuration.getSingleton;
    end
    
    ConfigArgs = tools.struct.string2fields(Conf.Data, ConfName);

    if isempty(ConfigArgs)
        ErrorMsg = sprintf('No configuration file: %s',ConfName);
    else
        ErrMsg = '';
        ConfigArgs_FN = fieldnames(ConfigArgs);
        FuncArgs_FN   = fieldnames(FuncArgs);
    
        N = numel(FuncArgs_FN);
        for I=1:1:N
            Ind = find(strcmp(FuncArgs_FN{I}, ConfigArgs_FN));
            if isempty(Ind)
                % not found, use default
            else
                FN = FuncArgs_FN{I};
                FuncArgs.(FN) = ConfigArgs.(FN);
            end
        end
    end

end
