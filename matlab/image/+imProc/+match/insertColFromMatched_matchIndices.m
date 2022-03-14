function [Result,ResInd] = insertColFromMatched_matchIndices(Obj, Matched, ResInd, Args)
    % Given two catalogs, matched them and insert some matched columns to the first catalog
    %   Given two AstroCatalog objects or AstroImage with AstroCatalog,
    %   match the second catalog to the first catalog, and insert selected
    %   columns from the second catalog to the first catalog.
    %   Non existing entries are filled with NaNs.
    % Input  : - (Obj) An AstroImage or AstroCatalog object.
    %          - (Matched) An AstroImage or AstroCatalog object. The number of
    %            elements must be eqaul to that of the first input
    %            argument.
    %          - A ResInd structure array which is the output of the 
    %            imProc.match.matchReturnIndices function with
    %            argumens(Obj, Matched).
    %            If empty, will call imProc.match.matchReturnIndices(Obj,
    %            Matced). Default is [].
    %          * ...,key,val,...
    %            'matchReturnIndicesArgs' - A cell array of additional
    %                   arguments to pass to imProc.match.matchReturnIndices
    %                   Default is {}.
    %            'Col2copy' - A cell array of columns to copy from the
    %                   the second input argument (Matched) to the first
    %                   input argument (Obj).
    %            'InsertPos' - Location at which to insert the columns.
    %                   Default is Inf.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of the first input argument in which to add the
    %                   columns.
    % Output : - The input object with the added columns.
    %          - The structure array which is the output of imProc.match.matchReturnIndices
    % Author : Eran Ofek (Mar 2022)
    % Example: 
    
    arguments
        Obj 
        Matched
        ResInd                          = [];
        Args.matchReturnIndicesArgs     = {};
        Args.Col2copy cell              = {};
        Args.InsertPos                  = Inf;
        Args.CreateNewObj logical       = false;
    end
    
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj     = numel(Obj);
    Nmatched = numel(Matched);
    
    if Nobj~=Nmatched
        error('Number of elements in first two input arguments must be equal');
    end
    
    if isempty(ResInd)
        CalcResInd = true;
    else
        CalcResInd = false;
    end
    
    for Iobj=1:1:Nobj
        
        if isa(Obj, 'AstroImage')
            ObjI = Obj(Iobj).CatData;
        else
            % assume Obj is AstroCatalog
            ObjI = Obj(Iobj);
        end
        if isa(Matched, 'AstroImage')
            MatchedI = Matched(Iobj).CatData;
        else
            % assume Matched is AstroCatalog
            MatchedI = Matched(Iobj);
        end
            
        if CalcResInd
            ResInd(Iobj) = imProc.match.matchReturnIndices(ObjI, MatchedI, Args.matchReturnIndicesArgs{:});
        end
    
        if Ncols>0
            DD       = selectRows(MergedI, ResInd(Iobj).Obj1_IndInObj2, 'CreateNewObj',true);
            [DataCols, Units] = getCol(DD, Args.Col2copy);

            if isa(Result, 'AstroImage')
                insertCol(Result(Iobj).CatData, DataCols, Args.InsertPos, Args.Col2copy, Units);
            else
                insertCol(Result(Iobj), DataCols, Args.InsertPos, Args.Col2copy, Units);
            end
        end        
        
    end
    
end
