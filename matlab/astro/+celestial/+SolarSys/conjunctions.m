function Result=conjunctions(Table, Args)
    %
   
    arguments
        Table AstroCatalog   % JD, RA, Dec, Delta, r, Mag,
        Args.SkipN           = 5;
        Args.ColJD           = 'JD';
        Args.CatName         = 'GAIADR3';
        Args.CatEpoch        = 2016;
        Args.CatColMag       = 'phot_bp_mean_mag';
        Args.CatColMag2      = 'phot_rp_mean_mag';
        Args.MagRange        = [0 15];
        Args.OcculterRadius  = 1000;     % [km]
        Args.ThresholdOccRad = 3;
    end
    
    RAD = 180./pi;
    
    Nrow = sizeCatalog(Table);
    
    Coo  = getLonLat(Table,'rad');
    RA   = Coo(:,1);
    Dec  = Coo(:,2);
    [CosX, CosY, CosZ] = celestial.coo.coo2cosined(RA, Dec);
    JD   = getCol(Table, Args.ColJD);
    Delta = getCol(Table, 'Delta');
    r     = getCol(Table, 'r');
    MagEp = getCol(Table, 'Mag');
    
    OcculterRadius = convert.length('km','au',Args.OcculterRadius);  % [au]
    
    Result = [];
    K = 0;
    for Irow=Args.SkipN:1:Nrow-Args.SkipN-1
        MeanCosX = CosX(Irow) + CosX(Irow+1);
        MeanCosY = CosY(Irow) + CosY(Irow+1);
        MeanCosZ = CosZ(Irow) + CosZ(Irow+1);
       
        [MeanRA, MeanDec]  = celestial.coo.cosined2coo(MeanCosX, MeanCosY, MeanCosZ);
        SearchRad          = celestial.coo.sphere_dist_fast(MeanRA, MeanDec, RA(Irow), Dec(Irow)).*1.1;
        
        Cat = catsHTM.cone_search(Args.CatName, MeanRA, MeanDec, SearchRad.*RAD.*3600, 'OutType','AstroCatalog');
        % select stars from Cat by magnitude
        Mag = getCol(Cat, Args.CatColMag);
        FlagMag = Mag>min(Args.MagRange) & Mag<max(Args.MagRange);
        Cat = selectRows(Cat, FlagMag);
        
        EpochOut = convert.time(JD(Irow),'JD','J');
        
        Cat = imProc.cat.applyProperMotion(Cat, Args.CatEpoch,EpochOut,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',true);
        
        
        % get RA/Dec
        CatCoo = getLonLat(Cat, 'rad');
        
        % Check nearest passage of object coordinate to each one of the
        % stars in Cat
        
        % verify that RA doesn't cross zero
        
        [MinDist,IP] = tools.math.geometry.dist_p2line([RA(Irow), Dec(Irow); RA(Irow+1), Dec(Irow+1)], CatCoo);
        
        % check if MinDist is smaller than asteroid + star size
        Mag  = getCol(Cat, Args.CatColMag);
        Mag2 = getCol(Cat, Args.CatColMag2);
        
        % occulter angular radius
        OcculterAngRadius = OcculterRadius./Delta(Irow);   % [radians]
        
        FlagOcc = MinDist<(OcculterAngRadius.*Args.ThresholdOccRad);
        if any(FlagOcc)
            [OccMinDist, MinI] = find(FlagOcc);
            
            K = K + 1;
            Result(K).OccMinDist;
            Result(K).Star = selectRows(Cat, MinI);
            
            %Result(K).Time = 
            
        end
            
        
    end
    
    
end