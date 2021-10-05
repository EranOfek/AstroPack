function [ResultCat, ResultRef, Summary] = filterForAstrometry(ObjCat, ObjRef, Args)
    % Given two catalogs, match their surface density and filter sources.
    % Description: Given two catalogs (e.g., Cat and Ref), clean the catalogs
    %              by removing NaN coordinates,
    %              imUtil.cat.flag_overdense_colrow, imUtil.cat.flag_overdense,
    %              estimate their density using imUtil.cat.surface_density, and
    %              equalize their surface density by removing the faintest
    %              sources in one of the catalogs using
    %              imUtil.cat.dilute_cat_by_mag
    % Input  : - A catalog with [X,Y,Mag] columns.
    %            Either a matrix, or AstroCatalog or AstroImage object.
    %          - A Ref catalog wiyh [X,Y,Mag] columns.
    %            Either a matrix, or AstroCatalog or AstroImage object.
    %          * Pairs of ...,key,val,... Possible keywords include:
    %            'CatRemoveNaN' - A logical indicating if to remove NaN
    %                   coordinates from the Cat. Default is true.
    %            'CatRemoveBadColRow' - A logical indicating if to remove
    %                   source in overdense columns/rows
    %                   from the Cat. Default is true.
    %            'CatRemoveOverDense' - A logical indicating if to remove
    %                   source in overdense regions
    %                   from the Cat. Default is true.
    %            'RefRemoveNaN'  - A logical indicating if to remove NaN
    %                   coordinates from the Ref. Default is false.
    %            'RefRemoveBadColRow' - A logical indicating if to remove
    %                   source in overdense columns/rows
    %                   from the Ref. Default is true.
    %            'RefRemoveOverDense' - A logical indicating if to remove
    %                   source in overdense regions
    %                   from the Ref. Default is false.
    %            'EqualizeDensity' - A logical indicating if to equalize the
    %                   surface density of the two catalogs.
    %            'DiluteThreshold' - If the surface density of the Ref minus
    %                   Cat divided by Cat (abs value) is larger than this
    %                   factor then applay source diluation.
    %                   Default is 0.5.
    %            'ColRowPar' - A cell array of addotional parameters to pass to
    %                   imUtil.cat.flag_overdense_colrow.
    %                   Default is {}.
    %            'OverdensePar' - A cell array of addotional parameters to pass to
    %                   imUtil.cat.flag_overdense
    %                   Default is {}.
    %            'CatHalfSize' - Either radius, or [half width, half height] of
    %                   the Cat catalog. If empty, then estimate area using
    %                   convex hull. Default is empty.
    %            'RefHalfSize' - Either radius, or [half width, half height] of
    %                   the Ref catalog. If empty, then estimate area using
    %                   convex hull. Default is empty.
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    %            'ColCatX' - X coordinates column index/name in the Cat.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColCatY' - Y coordinates column index/name in the Cat.
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColCatMag' - Mag column index/name in the Cat.
    %                   Default is AstroCatalog.DefNamesMag.
    %            'ColRefX' - X coordinates column index/name in the Ref.
    %                   Default is AstroCatalog.DefNamesX.
    %            'ColRefY' - Y coordinates column index/name in the Ref.
    %                   Default is AstroCatalog.DefNamesY.
    %            'ColRefMag' - Mag column index/name in the Ref.
    %                   Default is AstroCatalog.DefNamesMag.
    % Output : - Cleaned Cat.
    %          - Cleaned Ref.
    %          - Summary of number of sources survived after each step.
    % Author : Eran Ofek (Jun 2021)
    % Example: [Cat,Ref]=imProc.cat.filterForAstrometry(rand(100,3).*1000,rand(200,3).*1000);
        
    arguments
        ObjCat
        ObjRef
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
        
        Args.CreateNewObj                      = [];
        
        Args.ColCatX                           = AstroCatalog.DefNamesX;
        Args.ColCatY                           = AstroCatalog.DefNamesY;
        Args.ColCatMag                         = AstroCatalog.DefNamesMag;
        Args.ColRefX                           = AstroCatalog.DefNamesX;
        Args.ColRefY                           = AstroCatalog.DefNamesY;
        Args.ColRefMag                         = AstroCatalog.DefNamesMag;
    end
    
    % convert ObjCat/ObjRef to AstroCatalog (if numeric)
    if isnumeric(ObjCat)
        Tmp = ObjCat;
        ObjCat = AstroCatalog;
        ObjCat.Catalog  = Tmp;
        ObjCat.ColNames = {'X','Y','Mag'};
    end
    if isnumeric(ObjRef)
        Tmp = ObjRef;
        ObjRef = AstroCatalog;
        ObjRef.Catalog  = Tmp;
        ObjRef.ColNames = {'X','Y','Mag'};
    end
        
    if isempty(Args.CreateNewObj)
        if nargout==0
            Args.CreateNewObj = false;
        else
            Args.CreateNewObj = true;
        end
    end
    if Args.CreateNewObj
        ResultCat = ObjCat.copy();
        ResultRef = ObjRef.copy();
    else
        ResultCat = ObjCat;
        ResultRef = ObjRef;
    end

    % size of AstroCatalog/AstroImage
    Ncat = numel(ObjCat);
    Nref = numel(ObjRef);
    
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
        else
            error('Unknown ObjCat type');
        end
        
    
        [Cat,Ref,FlagCat,FlagRef, Summary(Imax)] = imUtil.patternMatch.prep_cat_for_astrometry(Cat, Ref, 'CatRemoveNaN',Args.CatRemoveNaN,...
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
                                                                      
  
        % save the data in Cat/Ref
        if isa(ObjCat, 'AstroCatalog')
            ResultCat(Icat).Catalog = Cat;
        elseif isa(ObjCat,'AstroImage')
            ResultCat(Icat).CatData.Catalog = Cat;
        else
            error('Unknown ObjCat type');
        end
        
        if isa(ObjRef, 'AstroCatalog')
            ResultRef(Iref).Catalog = Ref;
        elseif isa(ObjRef,'AstroImage')
            ResultRef(Iref).CatData.Catalog = Ref;
        else
            error('Unknown ObjRef type');
        end
        
    end
        
end
    
    