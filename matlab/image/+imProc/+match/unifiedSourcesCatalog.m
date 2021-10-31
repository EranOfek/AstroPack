function Result = unifiedSourcesCatalog(Obj, Args)
    % Match multiple catalogs and create a catalog of all unique (by position) sources.
    %       i.e., generate a list of all unique positions that appears in all the AstroCatalog object.
    % Input  : - A multi-element AstroCatalog object.
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
    else
        % assuming AstroCatalog
        Cat = Obj(Iobj);
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
    

    for Iobj=2:1:Nobj
        
        % read AstroCatalog/AstroImage into Cat
        if isa(Obj(Iobj), 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj(Iobj), 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('Unknown Obj input class - 1st input argument must be an AstroImage or AstroCatalog object');
        end
            
        %       The matched catalog result has the same number of
        %       sources as in the Obj2 catalog, and for each Obj2 source,
        %       the nearest source in Obj1 is listed. If there is no
        %       source within the search radius, then the entire line
        %       contains NaNs.
        %       The sources in Obj1 that doesn't have counterparts in
        %       Obj2 are listed in the unmatched catalog.

        % FFU:
        %ResMatch = matchReturnIndices(Result, Cat, Args)
        %IndCatNaN = isnan(ResMatch.Obj2_IndInObj1);
        
        
        [~, UnMatchedObj] = imProc.match.match(Cat, Result, 'CooType',Args.CooType,...
                                                            'Radius',Args.Radius,...
                                                            'RadiusUnits',Args.RadiusUnits,...
                                                            'AddIndInRef',false,...
                                                            'AddDistCol',false,...
                                                            'ColCatX',ColIndX,...
                                                            'ColCatY',ColIndY,...
                                                            'ColRefX',1,...
                                                            'ColRefY',2);
        
        % add UnMatchedObj to MergedCoo
        Result = merge([Result, UnMatchedObj], {ColNameX{1}, ColNameY{1}});
    end
    
end