function [Result, SelObj] = insertCol_matchIndices(Obj, ResInd, Args)
    % Insert Dist/Nmatch columns to a single element AstroCatalog based on ResInd.
    %       This is a utility function for users of
    %       imProc.match.matchReturnIndices. The function gets the input of
    %       imProc.match.matchReturnIndices (called here ResInd), and
    %       produce two AstroCatalog objects:
    %       1. The original AstroCatalog with possibly Dist and Nmatch
    %       columns.
    %       2. Selected lines in the original AstroCatalog which has
    %       matches, with possibly Dist and Nmatch
    %       columns.
    % Input  : - An AstroCatalog object.
    %          - ResInd. This is the output of imProc.match.matchReturnIndices
    %          * ...,key,val,...
    %            'AddColDist' - Default is true.
    %            'ColDistPos' - Default is Inf.
    %            'ColDistName' - Default is 'Dist'.
    %            'ColDistUnits' - Default is 'arcsec'.
    %            'AddColNmatch' - Default is true.
    %            'ColNmatchPos' - Default is Inf.
    %            'ColNmatchName' - Default is 'Nmatch'.
    % Output : - The input catalog with added columns for the nearest match
    %            in the catsHTM catalog.
    %          - Select lines only from the input catalog. Only sources
    %            with matches are selected.
    % Author : Eran Ofek (Apr 2021)
    % Example: 
    
    arguments
        Obj AstroCatalog        % AstroCatalog or AstroImage
        ResInd                  % output of matchReturnIndices
        
        Args.AddColDist logical   = true;
        Args.ColDistPos           = Inf;
        Args.ColDistName          = 'Dist';
        Args.ColDistUnits         = 'arcsec';
        Args.AddColNmatch logical = true;
        Args.ColNmatchPos         = Inf;
        Args.ColNmatchName        = 'Nmatch';
        
    end
    
        
    Nobj = numel(Obj);
    if numel(ResInd)~=Nobj
        error('ResInd must have the same number of elements as the input object');
    end
    
    
    Result = Obj;
    for Iobj=1:1:Nobj
        % Obj, but with extra columns indicating if there is a match in CatH
        if Args.AddColDist
            DistData     = convert.angular('rad', Args.ColDistUnits, ResInd(Iobj).Obj2_Dist); %(FlagNN));
            %Result(Iobj) = insertCol(Result(Iobj), ResInd(Iobj).Obj2_Dist, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
            Result(Iobj) = insertCol(Result(Iobj), DistData, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
        end
        if Args.AddColNmatch
            Result(Iobj) = insertCol(Result(Iobj), ResInd(Iobj).Obj2_NmatchObj1 , Args.ColNmatchPos, Args.ColNmatchName, '');
        end


        if nargout>1
            % catalog of selected sources in Obj that has matches in CatH [size like CatH)]
            FlagNN = ~isnan(ResInd(Iobj).Obj2_IndInObj1);
            SelObj(Iobj) = selectRows(Obj(Iobj), FlagNN); %IndObj);
            % add Dist/Nmatch to SelObj
            if Args.AddColDist
                DistData     = convert.angular('rad', Args.ColDistUnits, ResInd(Iobj).Obj2_Dist(FlagNN));
                SelObj(Iobj) = insertCol(SelObj(Iobj), DistData, Args.ColDistPos, Args.ColDistName, Args.ColDistUnits);
            end
            if Args.AddColNmatch
                SelObj(Iobj) = insertCol(SelObj(Iobj), ResInd(Iobj).Obj2_NmatchObj1(FlagNN) , Args.ColNmatchPos, Args.ColNmatchName, '');
            end
        end
    end

end
