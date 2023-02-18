function conjunctions(Table, Args)
    %
   
    arguments
        Table AstroCatalog   % JD, RA, Dec, Delta, r, Mag,
        Args.SkipN           = 5;
        Args.ColJD           = 'JD';
        Args.CatName         = 'GAIADR3';
        Args.CatEpoch       
    end
    
    RAD = 180./pi;
    
    Nrow = sizeCatalog(Table);
    
    Coo  = getLonLat(Table,'rad');
    RA   = Coo(:,1);
    Dec  = Coo(:,2);
    [CosX, CosY, CosZ] = celestial.coo.coo2cosined(RA, Dec);
    JD   = getCol(Table, Args.ColJD);
    
    for Irow=Args.SkipN:1:Nrow-Args.SkipN-1
        MeanCosX = CosX(Irow) + CosX(Irow+1);
        MeanCosY = CosY(Irow) + CosY(Irow+1);
        MeanCosZ = CosZ(Irow) + CosZ(Irow+1);
       
        [MeanRA, MeanDec]  = celestial.coo.cosined2coo(MeanCosX, MeanCosY, MeanCosZ);
        SearchRad          = celestial.coo.sphere_dist_cosd(MeanRA, MeanDec, RA(Irow), Dec(Irow)).*1.1;
        
        Cat = catsHTM.cone_search(Args.CatName, MeanRA, MeanDec, SearchRad.*RAD.*3600, 'OutType','AstroCatalog');
        
        EpochOut = convert.time(JD(Irow),'JD','J');
        
        Cat = imProc.cat.applyProperMotion(Cat, Args.CatEpoch,EpochOut,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',true);
        % select stars from Cat by magnitude
        
        % get RA/Dec
        CatCoo = getLonLat(Cat, 'rad');
        
        % Check nearest passage of object coordinate to each one of the
        % stars in Cat
        
        % verify that RA doesn't cross zero
        
        [MinDist,IP] = tools.math.geometry.dist_p2line([RA(Irow), Dec(Irow); RA(Irow+1), Dec(Irow+1)], CatCoo);
        
        % check if MinDist is smaller than asteroid + star size
        
    end
    
    
end