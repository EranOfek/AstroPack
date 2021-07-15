function Result = searchCatForKnownPM(Cat, PM, Args)
    % Given a catalog of sources positions and times, and a specific
    %       position+proper motion, look for sources in the catalog that
    %       are found in the proper motion trajectory.
    % Input  : - Catalog of observations (at least, RA, Dec, Epoch).
    %          - A six or eight columns array of 
    %            [Epoch_RA_JD, RA_rad, PM_RA, Epoch_Dec_JD, Dec_rad, PM_Dec, Parallax_mas, RV_kms]
    %            If eight columns, then apply parallax.
    %            PM is in mas/yr.
    %          * ...,key,val,...
    %            'ColRA' - Column index of RA in catalog. Default is 1.
    %            'ColDec' - Column index of Dec in catalog. Default is 2.
    %            'ColEpoch' - Column index of Epoch in catalog. Default is 3.
    %            'CatCooUnits' - catalog coo units. Default is 'rad'.
    %            'CatEpochUnits' - Catalog epoch units ['J'|'B'|'JD']. Default is 'JD'.
    %            'SearchRadius' - Search radius for sources in trajectory.
    %                   Default is 1.
    %            'SearchRadiusUnits' - Search radius units. 
    %                   Default is 'arcsec'.
    % Output : - A structure array of results. One element per tested PM.
    %            The following fields are available:
    %            'Nfound' - Number of sources found within search radius.
    %            'Std' - Std of angular distance of sources in search
    %                   radius.
    %            'Flag' - A vector of logicals indicating if a sources is
    %                   found within search radius.
    %            'RStdAll' - Robust std for all sources.
    % Author : Eran Ofek (Jul 2021)
    % Example: JD_OUT=celestial.time.julday([1 1 2020])+[-365.*5:100:365.*5]'
    %          [PredRA, PredDec] =celestial.coo.proper_motion(JD_OUT, JD_OUT(1), JD_OUT(1), 1,1,100,100)
    %          Cat = [PredRA, PredDec+rand(37,1).*3./RAD./3600, JD_OUT];
    %          Cat = [Cat; [1.1, 1.2, JD_OUT(1)]]
    %          Result = celestial.pm.searchCatForKnownPM(Cat, [JD_OUT(1), 1, 100, JD_OUT(1), 1, 100])
    
    arguments
        Cat
        PM                     % [Epoch_RA_JD, RA, PM_RA, Epoch_Dec_JD, Dec, PM_Dec, Parallax_mas, RV_kms]
        Args.ColRA                  = 1;
        Args.ColDec                 = 2;
        Args.ColEpoch               = 3;
        
        Args.CatCooUnits char       = 'rad';
        Args.CatEpochUnits char     = 'JD';
        Args.SearchRadius           = 1;
        Args.SearchRadiusUnits char = 'arcsec';
        
    end
    
    RAD        = 180./pi;
    ARCSEC_DEG = 3600;
    
    % convert epochs to JD
    CatJD = convert.time(Cat(:,Args.ColEpoch), Args.CatEpochUnits, 'JD');
    
    % convert coo to radians
    Factor  = convert.angular(Args.CatCooUnits,'rad');
    CatRA   = Factor.*Cat(:,Args.ColRA);
    CatDec  = Factor.*Cat(:,Args.ColDec);
    
    % search radius units
    SearchRadius = convert.angular(Args.SearchRadiusUnits,'arcsec');
    
    
    % predict RA/Dec based on known PM
    Npm = size(PM,1);
    for Ipm=1:1:Npm
        TotalPM = sqrt(sum(PM(Ipm,[3 6]).^2));
        if size(Cat,2)==8
            [PredRA, PredDec] = celestial.coo.proper_motion_parallax(CatJD, PM(Ipm,1), PM(Ipm,4), PM(Ipm,2), PM(Ipm,5), PM(Ipm,3), PM(Ipm,6), PM(Ipm,7), PM(Ipm,8));
        else
            [PredRA, PredDec] = celestial.coo.proper_motion(CatJD, PM(Ipm,1), PM(Ipm,4), PM(Ipm,2), PM(Ipm,5), PM(Ipm,3), PM(Ipm,6));
        end
        Dist = celestial.coo.sphere_dist(CatRA, CatDec, PredRA, PredDec);
        Dist = Dist.*RAD.*ARCSEC_DEG;   % arcsec
        Flag = Dist<SearchRadius;
        Result(Ipm).Nfound = sum(Flag);
        Result(Ipm).Std    = std(Dist(Flag));
        Result(Ipm).Flag   = Flag;
        Result(Ipm).TimeRangeFlag = range(CatJD(Flag));
        Result(Ipm).RStdAll = imUtil.background.rstd(Dist);
        
        % check the time-range of sources within 1" from each flaged source
        Ncand = sum(Flag);
        IndFlag = find(Flag);
        FlagGood = Flag;
        for Icand=1:1:Ncand
            IndCand = IndFlag(Icand);
            DD = celestial.coo.sphere_dist_fast(CatRA(IndCand), CatDec(IndCand), CatRA, CatDec);
            DD = DD.*ARCSEC_DEG.*RAD;
            RangeJD = range(CatJD((DD<Args.SearchRadius)));
            % time in days it takes a source with TotalPM to cross the SearchRadius
            % (365./(TotalPM./1000)).*Args.SearchRadius
            
            FlagGood(IndCand) = RangeJD < (365./(TotalPM./1000)).*Args.SearchRadius;
        end
        Result(Ipm).FlagGood   = FlagGood;
        Result(Ipm).Nflaggood  = sum(FlagGood);
        Result(Ipm).StdGood    = std(Dist(FlagGood));
        Result(Ipm).TimeRangeFlagGood = CatJD(FlagGood);
    end
    

end