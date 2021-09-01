function Result = allSources(Obj, Args)
    %
    % Example: AC=AstroCatalog({rand(10,3), rand(10,3), rand(10,3)},'ColNames',{'RA','Dec','Z'},'ColUnits',{'rad','rad',''});
    %          Result = allSources(AC, 'CooType','sphere');
    
    arguments
        Obj                                     % either AstroCatalog or AstroImage
        Args.CooType                 = 'sphere';
        
        Args.ColNamesX               = AstroCatalog.DefNamesX;
        Args.ColNamesY               = AstroCatalog.DefNamesY;
        Args.ColNamesRA              = AstroCatalog.DefNamesRA;
        Args.ColNamesDec             = AstroCatalog.DefNamesDec;
        
    end

    
    Nobj    = numel(Obj);
    Result  = AstroCatalog(size(Obj));

    Iobj = 1;
    switch lower(Args.CooType)
        case 'sphere'
            [ColIndX, ColNameX] = colnameDict2ind(Obj(Iobj), Args.ColNamesRA);
            [ColIndY, ColNameY] = colnameDict2ind(Obj(Iobj), Args.ColNamesRDec);
        case 'pix'
            [ColIndX, ColNameX] = colnameDict2ind(Obj(Iobj), Args.ColNamesX);
            [ColIndY, ColNameY] = colnameDict2ind(Obj(Iobj), Args.ColNamesY);
        otherwise
            error('Unknown CooType option');
    end
    [X, Xunit] = MatchedCoo.getCol(ColIndX);
    [Y, Yunit] = MatchedCoo.getCol(ColIndY);

    % MergedCoo is an AstroCatalog with two columns
    % X, Y of all sources found
    MergedCoo  = AstroCatalog({[X, Y]}, 'ColNames',{'ColNameX','ColNameY'},'ColUnits',{Xunit, Yunit});
    

    for Iobj=2:1:Nobj
        
        % read AstroCatalog/AstroImage into Cat
        if isa(Obj(Iobj), 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj(Iobj), 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('Unknown Obj input class - 1st input argument must be an AstroImage or AstroCatalog object');
        end
            
        if Iobj==1
            % decide on CooType
            
        end
        


        %       The matched catalog result has the same number of
        %       sources as in the Obj2 catalog, and for each Obj2 source,
        %       the nearest source in Obj1 is listed. If there is no
        %       source within the search radius, then the entire line
        %       contains NaNs.
        %       The sources in Obj1 that doesn't have counterparts in
        %       Obj2 are listed in the unmatched catalog.
        [~, UnMatchedObj] = match(Obj(Iobj), MergedCoo, 'CooType',Args.CooType,...
                                                                 'ColCatX',ColIndX,...
                                                                 'ColCatY',ColIndY,...
                                                                 'ColRefX',1,...
                                                                 'ColRefY',2);
        % add UnMatchedObj to MergedCoo
        MergedCoo = merge([MergedCoo, UnMatchedObj], {ColNameX, ColNameY});
    end
    
end