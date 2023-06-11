function [Result, ResInd, Matched] = unifiedSourcesCatalog(Obj, Args)
    % Match multiple catalogs and create a catalog of all unique (by position) sources.
    %       i.e., generate a list of all unique positions that appears in all the AstroCatalog object.
    % Input  : - A multi-element AstroCatalog or AstroImage object.
    %            Each element must contain a catalog which have the same
    %            coordinate system as all the other catalogs, and the
    %            coordinates have the same column names.
    %          * ...,key,val,...
    %            'CooType' - Coordinate system by which to match the catalogs
    %                   'sphere' | 'pix'. Default is 'sphere'.
    %            'Radius'  - Search radius. Default is 5.
    %            'RadiusUnits' - Search radius units (if spherical
    %                   coordinates search). Default is 'arcsec'.
    %            'ColNamesX' - A cell array of dictionary for X column name.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColNamesY' - A cell array of dictionary for Y column name.
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColNamesRA' - A cell array of dictionary for RA column name.
    %                   Default is AstroCatalog.DefNamesRA.
    %            'ColNamesDec' - A cell array of dictionary for Dec column name.
    %                   Default is AstroCatalog.DefNamesDec.
    % Output : - An AstroCatalog object containing a single element catalog
    %            with X and Y positions.
    %          - A structure array of matched indices.
    %            For each image, there are two vectors:
    %            .IndInUnified - index in unified catalog.
    %            .IndInObj - Index in original catalog.
    %          - An array of matched catalogs. Each catalog size is equal
    %            to that of the unified catalog, and it contains only the
    %            sources which are in the specific catalog.
    % Author : Eran Ofek (Sep 2021)
    % Example: AC=AstroCatalog({rand(10,3), rand(10,3), rand(10,3)},'ColNames',{'RA','Dec','Z'},'ColUnits',{'rad','rad',''});
    %          AC(1).Catalog = [AC(1).Catalog; AC(3).Catalog(1:5,:); AC(2).Catalog(1:2,:)];
    %          Result = imProc.match.unifiedSourcesCatalog(AC, 'CooType','sphere');
    
    arguments
        Obj                                     % either AstroCatalog or AstroImage
        Args.CooType                 = 'sphere';
        Args.Radius                  = 3;
        Args.RadiusUnits             = 'arcsec';
                
        Args.ColNamesX               = AstroCatalog.DefNamesX;
        Args.ColNamesY               = AstroCatalog.DefNamesY;
        Args.ColNamesRA              = AstroCatalog.DefNamesRA;
        Args.ColNamesDec             = AstroCatalog.DefNamesDec;
        
    end

    
    Nobj    = numel(Obj);
    Result  = AstroCatalog(size(Obj));

    Iobj = 1;
    if isa(Obj, 'AstroImage')
        Cat = Obj(Iobj).CatData;
        IsAstroImage = true;
    else
        % assuming AstroCatalog
        Cat = Obj(Iobj);
        IsAstroImage = false;
    end
    
    switch lower(Args.CooType)
        case 'sphere'
            
            [ColIndX, ColNameX] = colnameDict2ind(Cat, Args.ColNamesRA);
            [ColIndY, ColNameY] = colnameDict2ind(Cat, Args.ColNamesDec);
        case 'pix'
            
            [ColIndX, ColNameX] = colnameDict2ind(Cat, Args.ColNamesX);
            [ColIndY, ColNameY] = colnameDict2ind(Cat, Args.ColNamesY);
        otherwise
            error('Unknown CooType option');
    end
    
    [X, Xunit] = Cat.getCol(ColIndX);
    [Y, Yunit] = Cat.getCol(ColIndY);

    % MergedCoo is an AstroCatalog with two columns
    % X, Y of all sources found
    Result  = AstroCatalog({[X, Y]}, 'ColNames',{ColNameX{1},ColNameY{1}}, 'ColUnits',{Xunit{1}, Yunit{1}});
    
    Nsrc = size(Result.Catalog,1);
    ResInd(Iobj).IndInUnified = (1:1:Nsrc).';
    ResInd(Iobj).IndInObj     = (1:1:Nsrc).';
    
    
    for Iobj=2:1:Nobj
        
        % read AstroCatalog/AstroImage into Cat
        if IsAstroImage
            Cat = Obj(Iobj).CatData;
        else
            % assuming AstroCataloh
            Cat = Obj(Iobj);
        end
            
        % check if there is a valid RA/Dec in catalog
        %LonLat = getLonLat(Cat);

        %       The matched catalog result has the same number of
        %       sources as in the Obj2 catalog, and for each Obj2 source,
        %       the nearest source in Obj1 is listed. If there is no
        %       source within the search radius, then the entire line
        %       contains NaNs.
        %       The sources in Obj1 that doesn't have counterparts in
        %       Obj2 are listed in the unmatched catalog.

        % FFU:
        ResMatch = imProc.match.matchReturnIndices(Result, Cat, 'Radius',Args.Radius,...
                                                                'RadiusUnits',Args.RadiusUnits,...
                                                                'CooType',Args.CooType);
                                                                
        IndCatNaN = isnan(ResMatch.Obj2_IndInObj1);

        Result.Catalog = [Result.Catalog; Cat.Catalog(IndCatNaN, [ColIndX, ColIndY])];
        
        if nargout>1
            Ncat                      = size(Result.Catalog, 1);
            Nnan                      = sum(IndCatNaN);
            Nunified                  = Ncat + Nnan;
            ResInd(Iobj).IndInUnified = (Ncat+1:1:Nunified).';
            ResInd(Iobj).IndInObj     = find(IndCatNaN);
        end
    

    end
    
    % sort the Result by the second column (Declination)
    Result.sortrows(2);
    
    
    % merge the catalogs
    if nargout>2
        Matched = AstroCatalog([Nobj 1]);
        for Iobj=1:1:Nobj
            if IsAstroImage
                Cat = Obj(Iobj).CatData;
            else
                % assuming AstroCataloh
                Cat = Obj(Iobj);
            end
            Ncol                  = size(Cat.Catalog, 2);
            
            ResMatchInd = imProc.match.matchReturnIndices(Result, Cat, 'Radius',Args.Radius,...
                                                                'RadiusUnits',Args.RadiusUnits,...
                                                                'CooType',Args.CooType);
            
            FFM = ~isnan(ResMatchInd.Obj1_IndInObj2);
            Matched(Iobj).Catalog = nan(Nunified, Ncol);
            Matched(Iobj).Catalog(FFM, :) = Cat.Catalog(ResMatchInd.Obj1_IndInObj2(FFM),:);
            Matched(Iobj).ColNames        = Cat.ColNames;
            Matched(Iobj).ColUnits        = Cat.ColUnits;
            Matched(Iobj).ColDesc         = Cat.ColDesc;
            
        end
    end
    
    
end