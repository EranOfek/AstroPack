function Result=conjunctions(Table, Args)
    %
    % Example: [EphemCat]=celestial.SolarSys.jpl_horizons('ObjectInd','2060', 'StartJD',[1 9 2022],'StopJD', [31 12 2023], 'StepSize',3,'StepSizeUnits','h');
    % Result = celestial.SolarSys.conjunctions(EphemCat);

    arguments
        Table AstroCatalog   % JD, RA, Dec, Delta, r, Mag,
        Args.SkipN           = 5;
        Args.ColJD           = 'JD';
        Args.CatName         = 'GAIADR3';
        Args.CatEpoch        = 2016;
        Args.CatColMag       = 'phot_bp_mean_mag';
        Args.CatColMag2      = 'phot_rp_mean_mag';
        Args.MagRange        = [0 15];

        Args.EphemColMag     = 'APmag';
        Args.OcculterRadius  = 1000;     % [km]
        Args.ThresholdOccRad = 3;
    end
    
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    
    Nrow = sizeCatalog(Table);
    
    Coo  = getLonLat(Table,'rad');
    RA   = Coo(:,1);
    Dec  = Coo(:,2);
    [CosX, CosY, CosZ] = celestial.coo.coo2cosined(RA, Dec);
    JD   = getCol(Table, Args.ColJD);
    Delta = getCol(Table, 'Delta');
    r     = getCol(Table, 'r');
    MagEp = getCol(Table, Args.EphemColMag);
    
    OcculterRadius = convert.length('km','au',Args.OcculterRadius);  % [au]
    
    Result = [];
    K = 0;
    for Irow=Args.SkipN:1:Nrow-Args.SkipN-1
        MeanCosX = CosX(Irow) + CosX(Irow+1);
        MeanCosY = CosY(Irow) + CosY(Irow+1);
        MeanCosZ = CosZ(Irow) + CosZ(Irow+1);
       
        [MeanRA, MeanDec]  = celestial.coo.cosined2coo(MeanCosX, MeanCosY, MeanCosZ);
        SearchRad          = celestial.coo.sphere_dist_fast(MeanRA, MeanDec, RA(Irow), Dec(Irow)).*1.1;
        
        SearchRad.*RAD.*3600

        Cat = catsHTM.cone_search(Args.CatName, MeanRA, MeanDec, SearchRad.*RAD.*3600, 'OutType','AstroCatalog');
        % select stars from Cat by magnitude
        Mag = getCol(Cat, Args.CatColMag);
        FlagMag = Mag>min(Args.MagRange) & Mag<max(Args.MagRange);
        Cat = selectRows(Cat, FlagMag);
        
        EpochOut = convert.time(JD(Irow),'JD','J');
        
        if Cat.sizeCatalog>0
            Cat = imProc.cat.applyProperMotion(Cat, Args.CatEpoch,EpochOut,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',true);
        end
        
        
        % get RA/Dec
        CatCoo = getLonLat(Cat, 'rad');
        
        % Check nearest passage of object coordinate to each one of the
        % stars in Cat
        
        % verify that RA doesn't cross zero
        
        %[MinDist,IP] = tools.math.geometry.dist_p2line([RA(Irow), Dec(Irow); RA(Irow+1), Dec(Irow+1)], CatCoo);
        
        MinDist = celestial.coo.minDist_PointArc(CatCoo, [RA(Irow), Dec(Irow)], [RA(Irow+1), Dec(Irow+1)]);

        % check if MinDist is smaller than asteroid + star size
        Mag  = getCol(Cat, Args.CatColMag);
        Mag2 = getCol(Cat, Args.CatColMag2);
        
        % occulter angular radius
        OcculterAngRadius = OcculterRadius./Delta(Irow);   % [radians]
        
        MinDist_inAndRadUnits = MinDist./OcculterAngRadius;

        FlagOcc = MinDist_inAndRadUnits<Args.ThresholdOccRad;
        if any(FlagOcc)
            [~, MinI] = find(FlagOcc);
            
            
            % interpolate ephemeris to high resolution

            K = K + 1;
            Result(K).JD              = JD(Irow);
            Result(K).OccMinDist      = OcculterAngRadius(MinI).*RAD.*ARCSEC_DEG.*1000;     % [mas]
            Result(K).OcculterRadius  = Args.OcculterRadius;  % [km]
            Result(K).OcculterAngRadius      = OcculterAngRadius.*RAD.*ARCSEC_DEG.*1000;    % [mas]
            Result(K).ImpactPar_inOcculterAngRadiusUnits = MinDist_inAndRadUnits(MinI);     % [occulter ang rad.]

            % select candidate from Cat
            Result(K).Star = selectRows(Cat, MinI);
            
            %Result(K).Time = 
            
        end
            
        
    end
    
    
end