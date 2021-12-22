function Result = match_catsHTMmerged(Obj, Args)
    % Match an AstroCatalog with the catsHTM MergedCat.
    %   MergedCat is a special catsHTM catalog of several merged catalogs.
    %   It contains 4 columns [RA, Dec, BitFlag, AngRadius]
    % Input  : - An AstroImage or AstroCatalog object.
    %          * ...,key,val,...
    %            'SameField' - A logical indicating if all the catalogs
    %                   covers the same field of view.
    %                   Used only for expediting the code.
    %                   Default is false.
    %            'Con' - A cell array of constraints.
    %                   See catsHTM.cone_search for details.
    %                   Default is {}.
    %            'ColPos' - Position in which to add the new column.
    %                   Dfault is Inf.
    %            'FlagColNames' - Column name to add.
    %                   This column is the bit-wise or between all the
    %                   MergedCat sources found near the source.
    %                   Default is 'MergedCatMask'.
    %            'CreateNewObj' - Create a new copy of the inout object.
    %                   Default is false.
    %            'MergedCatName' - Name of the catsHTM MergedCat catalog.
    %                   Default is 'MergedCat'.
    %            'MergedCatMaskCol' - Col name in the MergedCat catalog
    %                   containing the bit mask.
    %                   Default is 3.
    %            'MergedCatRadiusCol' - Col name in the MergedCat catalog
    %                   containing the uncertany radius of the specific
    %                   catalog from which the MergedCat object arrived.
    %                   Default is 4.
    % Output : - The input object, in which a new column with the bit mask
    %            of all catalogs in MergedCat that were found near the
    %            specific soource.
    % Author : Eran Ofek (Dec 2021)
    % Example: AC = AstroCatalog({[1 1]},{'RA','Dec'});
    %          Result = imProc.match.match_catsHTMmerged(AC)
    
    arguments
        Obj
        Args.SameField logical            = false;
        Args.Con                          = {};
        Args.ColPos                       = Inf;
        Args.FlagColNames                 = 'MergedCatMask';
        
        Args.CreateNewObj logical         = false;
        
        Args.MergedCatName                = 'MergedCat';
        Args.MergedCatMaskCol             = 3;
        Args.MergedCatRadiusCol           = 4;
        
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
            [CircX, CircY, CircR] = Cat.boundingCircle('OutUnits','rad', 'CooType','sphere');
            CatH  = catsHTM.cone_search(Args.MergedCatName, CircX, CircY, CircR, 'RadiusUnits','rad', 'Con',Args.Con, 'OutType','astrocatalog');
            
            MaxSearchRadius = max(CatH.Catalog(:,Args.MergedCatRadiusCol));
        end
        
        
        % Cat need to be sorted
        Cat.sortrows('Dec');
        % match CatH against the Cat
        % catH must be the ref catalog
        ResInd = imProc.match.matchReturnIndices(Cat, CatH, 'CooType','sphere',...
                                                            'Radius',MaxSearchRadius,...
                                                            'RadiusUnits','arcsec');
        
        % Merged Mask bits for each source
        % For each source in the ref catalog (CatH), if there is a matching
        % source within the search radius, then add the bit mask of the ref
        % source to that of the Cat source.
        % The addition is 
        FlagNaN = ResInd.Obj2_Dist > CatH.Catalog(:,Args.MergedCatRadiusCol);
        ResInd.Obj2_IndInObj1(FlagNaN) = NaN;
        MergedCatFlag = zeros(numel(ResInd.Obj1_IndInObj2),1);
        Nref = numel(ResInd.Obj2_IndInObj1);
        for Iref=1:1:Nref
            if ~isnan(ResInd.Obj2_IndInObj1(Iref))
                IndCat = ResInd.Obj2_IndInObj1(Iref);
                MergedCatFlag(IndCat) = bitor(MergedCatFlag(IndCat), double(CatH.Catalog(Iref,Args.MergedCatMaskCol)));
            end
        end
     
        % Insert column to catalog
        Cat = insertCol(Cat, MergedCatFlag, Args.ColPos, Args.FlagColNames, '');
        
        if isa(Obj, 'AstroImage')
            Result(Iobj).CatData = Cat;
        else
            Result(Iobj) = Cat;
        end
        
    end

end