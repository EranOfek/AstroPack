function Result = match_catsHTMmerged(Obj, Args)
    % Match an AstroCatalog with the catsHTM MergedCat.
    %   MergedCat is a special catsHTM catalog of several merged catalogs.
    %   It contains 4 columns [RA, Dec, BitFlag, AngRadius]
    
    
    arguments
        Obj
        Args.SameField logical            = false;
        Args.CreateNewObj logical         = false;
        
        Args.MergedCatName                = 'MergedCat';
        Args.MergedCatRadiusCol           = 4;
        Args.Con                          = {};
    end
   
    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        else
            % input is AstroCatalog
            Cat = Obj(Iobj);
        end
        
        if Iobj==1 || ~Args.SameField
            [CircX, CircY, CircR] = Cat.boundingCircle('OutUnits','rad');
            CatH  = catsHTM.cone_search(Args.MergedCatName, CircX, CircY, CircR, 'RadiusUnits','rad', 'Con',Args.Con, 'OutType','astrocatalog');
            
            MaxSearchRadius = max(CatH.Catalog(:,Args.MergedCatRadiusCol));
        end
        
        
        % Cat need to be sorted
        Cat.sortrows('Dec');
        % match CatH against the Cat
        % catH must be the ref catalog
        ResInd = imProc.match.matchReturnIndices(Cat, CatH, 'CooType','sphere',...
                                                            'Radius',MaxSearchradius,...
                                                            'RadiusUnits','arcsec');
        
        % git here
        ResInd.Obj2_IndInObj1
        
        FlagNaN = ResInd.Obj2_Dist > CatH.Catalog(:,Args.MergedCatRadiusCol);
        ResInd.Obj2_IndInObj1(FlagNaN) = NaN;
        MergedCatFlag = zeros(numel(ResInd.Obj1_IndInObj2),1);
        
        
    %                   its matched indices in Obj1. NaN if no match.
    %            'Obj2_Dist' - A vector, for each source in Obj2, of the
    %                   angular distance ['rad' if 'sphere'] between the
    %                   matched sources.
    %            'Obj2_NmatchObj1' - A vector, for each source in Obj2, of the
    %                   number of matches within search radius.
        
    end

end