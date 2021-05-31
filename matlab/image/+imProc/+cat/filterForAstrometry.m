function filterForAstrometry(ObjCat, OvjRef, Args)
    %
    % not good
   
        
    arguments
        Cat
        Ref
        Args.CatRemoveNaN(1,1) logical         = true;
        Args.CatRemoveBadColRow(1,1) logical   = true;
        Args.CatRemoveOverDense(1,1) logical   = true;
        Args.RefRemoveNaN(1,1) logical         = false;
        Args.RefRemoveBadColRow(1,1) logical   = false;
        Args.RefRemoveOverDense(1,1) logical   = true;
        Args.EqualizeDensity(1,1) logical      = true;
        Args.DiluteThreshold                   = 0.5;
        Args.ColRowPar cell                    = {};
        Args.OverdensePar cell                 = {};
        Args.CatHalfSize                       = [];
        Args.RefHalfSize                       = [];
        
        Args.ColCatX                           = AstroCatalog.DefNamesX;
        Args.ColCatY                           = AstroCatalog.DefNamesY;
        Args.ColCatMag                         = AstroCatalog.DefNamesMag;
        Args.ColRefX                           = AstroCatalog.DefNamesX;
        Args.ColRefY                           = AstroCatalog.DefNamesY;
        Args.ColRefMag                         = AstroCatalog.DefNamesMag;
    end
        
    if isnumeric(ObjCat)
        Ncat = 1;
    else
        Ncat = numel(ObjCat);
    end
    if isnumeric(ObjRef)
        Nref = 1;
    else
        Nref = numel(ObjRef);
    end
    
    Nmax = max(Ncat, Nref);
    for Imax=1:1:Nmax
        Icat = min(Imax, Ncat);
        Iref = min(Imax, Nref);
        
        % get Cat and column indices in Cat
        if isa(ObjCat, 'AstroCatalog')
            Cat         = ObjCat(Icat).Catalog;
            [ColCatX]   = colnameDict2ind(ObjCat(Icat), Args.ColCatX);
            [ColCatY]   = colnameDict2ind(ObjCat(Icat), Args.ColCatY);
            [ColCatMag] = colnameDict2ind(ObjCat(Icat), Args.ColCatMag);
            
        elseif isa(ObjCat,'AstroImage')
            Cat = ObjCat(Icat).CatData.Catalog;
            [ColCatX]   = colnameDict2ind(ObjCat(Icat).CatData, Args.ColCatX);
            [ColCatY]   = colnameDict2ind(ObjCat(Icat).CatData, Args.ColCatY);
            [ColCatMag] = colnameDict2ind(ObjCat(Icat).CatData, Args.ColCatMag);
        elseif isnumeric(ObjCat)
            Cat = ObjCat;
        else
            error('Unknown ObjCat type');
        end
        
        % get Ref and column indices in Ref
        if isa(ObjRef, 'AstroCatalog')
            Ref         = ObjRef(Icat).Catalog;
            [ColRefX]   = colnameDict2ind(ObjRef(Icat), Args.ColRefX);
            [ColRefY]   = colnameDict2ind(ObjRef(Icat), Args.ColRefY);
            [ColRefMag] = colnameDict2ind(ObjRef(Icat), Args.ColRefMag);
            
        elseif isa(ObjRef,'AstroImage')
            Ref = ObjRef(Icat).CatData.Catalog;
            [ColRefX]   = colnameDict2ind(ObjRef(Icat).CatData, Args.ColRefX);
            [ColRefY]   = colnameDict2ind(ObjRef(Icat).CatData, Args.ColRefY);
            [ColRefMag] = colnameDict2ind(ObjRef(Icat).CatData, Args.ColRefMag);
        elseif isnumeric(ObjRef)
            Ref = ObjRef;
        else
            error('Unknown ObjCat type');
        end
        
    
        [Cat,Ref,FlagCat,FlagRef] = prep_cat_for_astrometry(Cat, Ref, 'CatRemoveNaN',Args.CatRemoveNaN,...
                                                                      'CatRemoveBadColRow',Args.CatRemoveBadColRow,...
                                                                      'CatRemoveOverDense',Args.CatRemoveOverDense,...
                                                                      'RefRemoveNaN',Args.RefRemoveNaN,...
                                                                      'RefRemoveBadColRow',Args.RefRemoveBadColRow,...
                                                                      'RefRemoveNaN',Args.RefRemoveNaN,...
                                                                      'EqualizeDensity',Args.EqualizeDensity,...
                                                                      'DiluteThreshold',Args.DiluteThreshold,...
                                                                      'ColRowPar',Args.ColRowPar,...
                                                                      'OverdensePar',Args.OverdensePar,...
                                                                      'CatHalfSize',Args.CatHalfSize,...
                                                                      'RefHalfSize',Args.RefHalfSize,...
                                                                      'ColCatX',ColCatX,...
                                                                      'ColCatY',ColCatY,...
                                                                      'ColCatMag',ColCatMag,...
                                                                      'ColRefX',ColRefX,...
                                                                      'ColRefY',ColRefY,...
                                                                      'ColRefMag',ColRefMag);
                                                                      
  
    % need to save the data in Cat/Ref
    % what about CreateNewObj ?
        
end
    
    