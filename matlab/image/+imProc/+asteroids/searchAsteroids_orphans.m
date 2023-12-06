function searchAsteroids_orphans(Obj, Args)
    %
    
    arguments
        Obj                               % AstroImage or AstroCatalog
        
        Args.CooType              = 'sphere';
        Args.OrphanDist           = 120;
        Args.OrphanDistUnits      = 'arcsec';
        
        Args.OrphanSelectFun     = {'Nobs', @eq(X) X>0 & X<3};   % {ColName, Fun, ColName, Fun,...}
        Args.JD                  = [];
    end
    
    Nobj = numel(Obj);
    
    if isempty(Args.JD)
        if isa(Obj, 'AstroImage')
            Args.JD = julday(Obj);
        end
    end
    if isempty(Args.JD)
        Args.JD = (1:1:Nobj).';
    end
    
    switch lower(Args.CooType)
        case 'sphere'
            DistFun = @celestial.coo.sphere_dist_fast;
            Args.OrphanDist = convert.angular(Args.OrphanDistUnits, 'rad', Args.OrphanDist);   % [rad]
        case 'pix'
            DistFun = @tools.math.geometry.plane_dist;
        otherwise
            error('Unknown CooType option');
    end
    
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatDat;
        else
            Cat = Obj(Iobj);
        end
        
        [~, Flag] = queryFun(Cat, Args.OrphanSelectFun, 'ReturnResult', false, 'CreateNewObj',false);
        switch lower(Args.CooType)
            case 'sphere'
                XY = getLonLat(Cat, 'rad');
            case 'pix'
                XY = getXY(Cat);
        end
        XY = XY(Flag,:);
        
        % search all XY positions that has enough neighboors
        Nsrc = size(XY, 1);
        for Isrc=1:1:Nsrc
            Dist     = DistFun(XY(Isrc,1), XY(Isrc,2), XY(:,1), XY(:,2));
            FlagDist = Dist<Args.OrphanDist;
            
            but we nee the JD for each orphan...
        
        R = tools.math.fit.ransacLinear2d(XY(Flag,:), Args.JD);
    
    
end