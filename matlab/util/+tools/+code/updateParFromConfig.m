function Result = updateParFromConfig(FuncArgs, ConfName)
    % Update function arguments from a config
    %     
    % Input  : - the parameter structure to be changed (usually, Args)
    %          - the name of the config .yml file    
    % Output : - the update structure (usually, Args)
    % Author : A.M. Krassilchtchikov (2024 Apr) 
    % Example: Args = tools.code.updateParFromConfig(Args);
    %          Args = tools.code.updateParFromConfig(Args,'LASTpipeline_def');
    %          Args = tools.code.updateParFromConfig(Args,'LASTpipeline_v1.0');
    arguments
        FuncArgs               
        ConfName               = 'LASTpipeline_def';        
    end
    
    % make a class object only once
    persistent GlobalConf
    if isempty(GlobalConf)
        GlobalConf = Component;
    end
    
    % read the configuration from the object
    Conf = GlobalConf.Config.Data.(ConfName);
    
    % get common field names and replace these fields from Conf
    CommonFields = intersect(fieldnames(FuncArgs), fieldnames(Conf));
    
    for Ifield = 1:numel(CommonFields)
        FuncArgs.(CommonFields{Ifield}) = Conf.(CommonFields{Ifield});
    end
    
    % save the result
    Result = FuncArgs;    
end
