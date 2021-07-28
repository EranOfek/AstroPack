function astrometryCheck(Obj, Args)
    %
    
    arguments
        Obj
        Args.WCS                              = [];
        Args.CatName                          = 'GAIAEDR3';  % or AstroCatalog
        Args.getAstrometricCatalogArgs cell   = {};
    end
   
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        % for each image/catalog
        if isa(Obj, 'AstroImage')
            if ~isemprt(Args.WCS)
                WCS = Args.WCS;
            else
                WCS = Obj(Iobj).WCS;
            end
            Cat = Obj.CatData;
        elseif isa(Obj, 'AstroCatalog')
            WCS = Args.WCS;
            Cat = Obj;
        else
            error('First input argument must be AstroImage or AstroCatalog');
        end
        if isemptyCatalog(Cat)
            error('Catalog must contain sources - catalog %d is empry',Iobj);
        end
        
        if isempty(WCS)
            % attempt to read RA/Dec from catalog
            [SrcRA, SrcDec] = getLonLat(Cat, 'rad');
        else
            % Calculate RA/Dec for sources in catalog
            [Xcat, Ycat] = getXY(Cat);
            [SrcRA, SrcDec] = WCS.xy2sky(Xcat, Ycat, 'rad', Args.IncludeDistortions);            
        end
        
        % retrieve astrometric catalog
        if ~isa(Args.CatName,'AstroCatalog')
            [CenterCoo, BestRadius] = boundingCircle([SrcRA, SrcDec]); % [in/out: radians]
        end
        [AstrometricCat] = getAstrometricCatalog(CenterCoo(1), CenterCoo(2), 'CatName',Args.CatName,...
                                                                     'Radius',BestRadius,...
                                                                     'RadiusUnits','rad',...
                                                                     'CooUnits','rad',...
                                                                     'OutUnits','deg',...
                                                                     Args.getAstrometricCatalogArgs{:});
        
        % Match AstrometricCat with SrcRA, SrcDec
        
        
        % Compare RA/Dec in catalog with RA/Dec in external ref catalog
        
        
        
        
    end
    
end