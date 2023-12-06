function Matched = matchedReturnCat(PosCat,Obj,Args)
    % Match AsstroCatalogs and return array of matched catalogs.
    %   OBSOLOETE: use imProc.match.match instead.
    % Input  : - An AstroCatalog object containing a single element catalog
    %            with X and Y positions.
    %          - A multi-element AstroCatalog or AstroImage object.
    %            Each element must contain a catalog which have the same
    %            coordinate system as all the other catalogs, and the
    %            coordinates have the same column names.
    %          * ...,key,val,...
    %            'CooType' - Coordinate system by which to match the catalogs
    %                   'sphere' | 'pix'. Default is 'sphere'.
    %            'Radius'  - Search radius. Default is 5.
    %            'RadiusUnits' - Search radius units (if spherical
    %                   coordinates search). Default is 'arcsec'.
    % Output : - An array of matched catalogs. Each catalog rows number is equal
    %            to that of the PosCat catalog, and it contains only the
    %            sources which are in the specific catalog.



arguments
    PosCat            
    
    Obj                % either AstroCatalog or AstroImage
    Args.CooType                 = 'sphere';
    Args.Radius                  = 3;
    Args.RadiusUnits             = 'arcsec';
end




Nobj    = numel(Obj);

if isa(Obj, 'AstroImage')
    IsAstroImage= true;
else
    % assuming AstroCatalog
    IsAstroImage= false;
end

Matched = AstroCatalog([Nobj 1]);
for Iobj=1:1:Nobj
    if IsAstroImage
        Cat = Obj(Iobj).CatData;
    else                % assuming AstroCataloh
        Cat = Obj(Iobj);
    end
    Ncol                  = size(Cat.Catalog, 2);
    
    ResMatchInd = imProc.match.matchReturnIndices(PosCat, Cat, 'Radius',Args.Radius,...
        'RadiusUnits',Args.RadiusUnits,...
        'CooType',Args.CooType);
    
    FFM = ~isnan(ResMatchInd.Obj1_IndInObj2);
    Matched(Iobj).Catalog = nan(numel(FFM), Ncol);
    Matched(Iobj).Catalog(FFM, :) = Cat.Catalog(ResMatchInd.Obj1_IndInObj2(FFM),:);
    Matched(Iobj).ColNames        = Cat.ColNames;
    Matched(Iobj).ColUnits        = Cat.ColUnits;
    Matched(Iobj).ColDesc         = Cat.ColDesc;
    
end




end