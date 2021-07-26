function astrometryCheck(Obj, Args)
    %
    
    arguments
        Obj
        Args.WCS      = [];
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
        
        % Compare RA/Dec in catalog with RA/Dec in external ref catalog
    
        
        
        
    end
    
end