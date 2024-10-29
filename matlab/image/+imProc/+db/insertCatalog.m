function [Result] = insertCatalog(Obj, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Oct) 
    % Example: 

    arguments
        Obj
        Args.ColNameDic      
        
    end

    Nobj = numel(Obj);
    % read each catalog, selct columns, and convert their names
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Tmp = Obj(Iobj).CatData.Table;
        else
            Tmp = Obj(Iobj).Table;
        end

        % select tables
        Tmp = Tmp.({Args.ColNameDic.ColName});
        % run functions
        %IndFun = find(~tools.cell.isempty_cell({Args.ColNameDic.ColFun}));
        %for If=1:1:numel(IndFun)

        % change column names
        Tmp.Properties.VariableNames = Args.ColNameDic.ColNameOut;

        % insert additional columns - cat by cat


        % concat all tables
        if Iobj==1
            T = Tmp;
        else
            T = [T;Tmp];
        end
    end

    % insert additional global columns

    % insert BJD

    


end
