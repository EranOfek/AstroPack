function [Result] = headers2table(Obj, Args)
    % Export headers in AstroHeader/AstroImage into a table.
    % Input  : - AstroHeader or AstroImage.
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Sep) 
    % Example: imProc.header.headers2table(AI)

    arguments
        Obj
        Args.ColNameDic        = {};
       
        Args.OutType           = 'table';   % 'AstroCatalog' with table
    end
    
    if iscell(Args.ColNameDic) || isstring(Args.ColNameDic)
        ColName = Args.ColNameDic;
        Ncol    = numel(ColName);
        ColFun  = cell(Ncol,1);
    elseif isstruct(Args.ColNameDic)
        ColName = {Args.ColNameDic.ColName};
        if isfield(Args.ColNameDic, 'ColNameOut')
            ColNameOut = {Args.ColNameDic.ColNameOut};
        end
        if isfield(Args.ColNameDic, 'ColFun')
            ColFun = {Args.ColNameDic.ColFun};
        end
    else
       
    end
    
    Ncol = numel(ColName);
    %FunList
    %OutClass
    
    Nobj = numel(Obj);
   
    OutCell = cell(Nobj, Ncol);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            AH = Obj(Iobj).HeaderData;
        else
            % assume Obj is AstroHeader
            AH = Obj(Iobj);
        end
        
        for Icol=1:1:Ncol
            Tmp = AH.getVal(ColName{Icol});
            if ~isempty(ColFun{Icol})
                Tmp = ColFun{Icol}(Tmp);     
            end
            
            OutCell{Iobj, Icol} = Tmp; 
        end
    end
    
    TmpTbl = cell2table(OutCell, 'VariableNames',ColName);
    
    if all(tools.cell.isempty_cell(ColFun))
        OutTbl = TmpTbl;
    else
        OutTbl = table;
        IcolOut = 0;
        for Icol=1:1:Ncol
            [~,NcolCol] = size(TmpTbl.(ColName{Icol}));
            if NcolCol==1
                OutTbl.(ColNameOut{Icol}) = TmpTbl.(ColName{Icol});
            else
                % multiple columns - break
                for IcolCol=1:1:NcolCol
                    OutTbl.(ColNameOut{Icol}{IcolCol}) = Tmp.(ColName{Icol})(:,IcolCol);
                end
            end
        end            
        
    end    
    
end
