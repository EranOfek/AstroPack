function [FullPath,FunStr] = newFun(FullFunName, EditFun, Args)
    % Create a new function in existing package
    % Input  : - A function name, including package names.
    %            E.g., 'astro.cosmo.newfun'
    %          - A logical indicating if to open an editor for the function.
    %            Default is true.
    %          * ...,key,val,...
    %            'FunExt' - Function extension. Default is '.m'.
    % Output : - The full path and function name.
    %          - String containing the function content.
    % Author : Eran Ofek (Nov 2023)
    % Example: [FullPath,FunStr] = tools.code.newFun('tools.code.yyyu')
   
    arguments
        FullFunName       = [];
        EditFun logical   = true;
        Args.FunExt       = '.m';
    end
    
    
    if isempty(FullFunName)
        FunctionName = 'funname';
        FullPath     = [];
    else
        DotPos = strfind(FullFunName, '.');
        if isempty(DotPos)
            FullPath = [];
        else
            Pos = DotPos(end);
            PackageName  = FullFunName(1:Pos-1);
            FunctionName = FullFunName(Pos+1:end);
        
            SplitPackgeName = split(PackageName, '.');
            
            W = what(sprintf('+%s',SplitPackgeName{1}));
            FullPath = W.path;
            for I=2:numel(SplitPackgeName)
                FullPath = sprintf('%s%s+%s', FullPath, filesep, SplitPackgeName{I});
            end
            
            FullPath = sprintf('%s%s%s%s', FullPath, filesep, FunctionName, Args.FunExt);
            
        end
    end
    
    % create Function
    FunStr = '';
    FunStr = sprintf('%sfunction [Result] = %s(X, Y, Args)\n',FunStr,FunctionName);
    FunStr = sprintf('%s    %% One line description\n',FunStr);
    FunStr = sprintf('%s    %%     Optional detailed description\n',FunStr);
    FunStr = sprintf('%s    %% Input  : - \n',FunStr);
    FunStr = sprintf('%s    %%          - \n',FunStr);
    FunStr = sprintf('%s    %%          * ...,key,val,... \n',FunStr);
    FunStr = sprintf('%s    %% Output : - \n',FunStr);
    FunStr = sprintf('%s    %% Author : Eran Ofek (%s) \n',FunStr, datetime('now','Format','yyyy MMM'));
    FunStr = sprintf('%s    %% Example: \n',FunStr);
    FunStr = sprintf('%s\n',FunStr);
    FunStr = sprintf('%s    arguments\n',FunStr);
    FunStr = sprintf('%s        X\n',FunStr);
    FunStr = sprintf('%s        Y\n',FunStr);
    FunStr = sprintf('%s        Args.A                 = [];\n',FunStr);
    FunStr = sprintf('%s        Args.B                 = [];\n',FunStr);
    FunStr = sprintf('%s    end\n',FunStr);
    FunStr = sprintf('%s\n',FunStr);
    FunStr = sprintf('%send\n',FunStr);
    
    
    if ~isempty(FullPath)
        FID = fopen(FullPath, 'w');
        fprintf(FID,'%s',FunStr);
        
        if EditFun
            edit(FullFunName);
        end
    end
    
end