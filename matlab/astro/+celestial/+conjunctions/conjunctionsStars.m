function Result=conjunctionsStars(Table, Args)
    % Search for conjunctions between a Solar system object and GAIA stars
    % Input  : - An Astrocatalog object containing the ephemeris for a
    %            Solar system object. Can be generated using celestial.SolarSys.jpl_horizons
    %          * ...,key,val,...
    %            Check code
    % Output : - A structure array with element per occultation candidate
    %            and the following fields:
    %            .JD - JD of minimum impact parameter
    %            .MinDist - Minimum impact parameter [mas]
    %            ...
    %            .Star - AstroCatalog object with the occulted star.
    % Author : Eran Ofek (Mar 2023)
    % Requirements: The GAIA-DR3 catsHTM catalog should be installed.
    % Example: [EphemCat]=celestial.SolarSys.jpl_horizons('ObjectInd','2060', 'StartJD',[1 9 2022],'StopJD', [31 12 2023], 'StepSize',3,'StepSizeUnits','h');
    %          Result = celestial.conjunctions.conjunctionsStars(EphemCat);

    arguments
        Table AstroCatalog   % JD, RA, Dec, Delta, r, Mag,
        Args.ObsCoo          = [35 32];
        Args.ObjName         = '';   % object name to add to Result
        
        Args.SkipN           = 5;
        Args.ColJD           = 'JD';
        Args.CatName         = 'GAIADR3';
        Args.CatEpoch        = 2016;
        Args.CatColMag       = 'phot_bp_mean_mag';
        Args.CatColMag2      = 'phot_rp_mean_mag';
        Args.MagRange        = [0 19];

        Args.EphemColMag     = 'APmag';
        Args.OcculterRadius  = 1000;     % [km]
        Args.ThresholdOccRad = 4;

        Args.InterpStep      = 10./86400;
        Args.Result          = [];  % concat results
        
        Args.GAIA_Cols = {'RA','Dec','phot_g_mean_mag','phot_bp_mean_mag','phot_rp_mean_mag'};
    end
    
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    SEC_DAY    = 86400;
    
    
    Ngaia = numel(Args.GAIA_Cols);
    
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
    
    if isempty(Args.Result)
        Result = [];
        K = 0;
    else
        Result = Args.Result;
        K      = numel(Result);
    end

    for Irow=Args.SkipN:1:Nrow-Args.SkipN-1
        MeanCosX = CosX(Irow) + CosX(Irow+1);
        MeanCosY = CosY(Irow) + CosY(Irow+1);
        MeanCosZ = CosZ(Irow) + CosZ(Irow+1);
       
        [MeanRA, MeanDec]  = celestial.coo.cosined2coo(MeanCosX, MeanCosY, MeanCosZ);
        SearchRad          = celestial.coo.sphere_dist_fast(MeanRA, MeanDec, RA(Irow), Dec(Irow)).*1.1;
        
        %SearchRad.*RAD.*3600

        Cat = catsHTM.cone_search(Args.CatName, MeanRA, MeanDec, SearchRad.*RAD.*3600, 'OutType','AstroCatalog');
        % select stars from Cat by magnitude
        Mag = getCol(Cat, Args.CatColMag);
        FlagMag = Mag>min(Args.MagRange) & Mag<max(Args.MagRange);
        Cat = selectRows(Cat, FlagMag);
        
        EpochOut = convert.time(JD(Irow),'JD','J');
        
        if Cat.sizeCatalog>0
            Cat = imProc.cat.applyProperMotion(Cat, Args.CatEpoch,EpochOut,'EpochInUnits','J','EpochOutUnits','J','ApplyPlx',true);
        end
        
        % go over all stars
        Nstar = Cat.sizeCatalog;
        for Istar=1:1:Nstar
            Cat1 = Cat.selectRows(Istar, 'CreateNewObj',true);

        
            % get RA/Dec
            CatCoo = getLonLat(Cat1, 'rad');
        
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
                IndFlagOcc = find(FlagOcc);
                Nind = numel(IndFlagOcc);
    
                for Iind=1:1:Nind
                    MinI = Iind;
                
                    %[~, MinI] = find(FlagOcc);
                    %MinI = MinI(1);
                    
                    % interpolate ephemeris to high resolution
                    InterpJD = (JD(Irow)-5./1440:Args.InterpStep:JD(Irow+1)+5./1440)';
        
                   
        
                    [InterpRA,InterpDec]=tools.interp.interp_diff_longlat(JD,[RA, Dec],InterpJD);
                    Dist = celestial.coo.sphere_dist_fast(InterpRA,InterpDec, CatCoo(MinI,1),CatCoo(MinI,2));
                    
                    % returns: [X, Y, 2nd derivative d^2Y/dX^2]
                    Extram  = tools.find.find_local_extremum(InterpJD, Dist);
                    [~,IminE] = min(Extram(:,2));
                    Extram    = Extram(IminE,:);
        
                    FlagMin = Extram(:,3)>0;
                    Extram  = Extram(FlagMin,:);
                  
                    if ~isempty(Extram)
                        MinDist_inAndRadUnits_Extram = Extram(1,2)./OcculterAngRadius;
        
                        if MinDist_inAndRadUnits_Extram<Args.ThresholdOccRad
                            BestJD      = Extram(1,1);
                            BestMinDist = Extram(1,2);
                        
                            % check that the occultation is obove local horizon
                            HorizCoo = celestial.coo.horiz_coo(Cat.Catalog(1,1:2),BestJD, Args.ObsCoo./RAD, 'h');
                            Sun      = celestial.SolarSys.get_sun(BestJD);
                            
                            
                            
                            
                            [MinDist, MinDistI] = min(Dist);
                
                            K = K + 1;
                            Result(K).JD                     = BestJD;
                            Result(K).Date  = celestial.time.jd2date(BestJD,'H');
                            
                            Result(K).ObjName                = Args.ObjName;
                            DeltaTime                        = JD(Irow+1) - JD(Irow);
                                        
                            Result(K).MinDist                = BestMinDist.*RAD.*ARCSEC_DEG.*1000;          % [mas]
                            Result(K).OcculterRadius         = Args.OcculterRadius;                         % [km]
                            Result(K).OcculterAngRadius      = OcculterAngRadius.*RAD.*ARCSEC_DEG.*1000;    % [mas]
                            %Result(K).ImpactPar_inOcculterAngRadiusUnits = MinDist_inAndRadUnits(MinI);     % [occulter ang rad.]
                            Result(K).ImpactPar_inOcculterAngRadiusUnits = MinDist_inAndRadUnits_Extram;     % [occulter ang rad.]
                
                            Result(K).AngSpeed               = celestial.coo.sphere_dist_fast(RA(Irow),Dec(Irow), RA(Irow+1),Dec(Irow+1)).*RAD.*ARCSEC_DEG.*1000./(DeltaTime.*SEC_DAY); % [mas/s]
                            Result(K).CrossingTime           = Result(K).OcculterAngRadius./Result(K).AngSpeed;   % [s] time to cross occulter radius
                
                            Result(K).MagMP = MagEp(Irow);   % Solar sys. object mag.
                            
                            
                            % select candidate from Cat
                            Result(K).RA      = Cat1.Catalog(1,1).*RAD;  % [deg]
                            Result(K).Dec     = Cat1.Catalog(1,2).*RAD;  % [deg]
                            Result(K).MagStar = getCol(Cat1, Args.CatColMag);
                            Result(K).Star    = selectRows(Cat1, MinI);
                                       
                            Result(K).Az      = HorizCoo(1).*RAD;   % [deg]
                            Result(K).Alt     = HorizCoo(2).*RAD;   % [deg]
                            Result(K).SunAlt  = Sun.Alt.*RAD;       % [deg]
                            
                            for Igaia=1:1:Ngaia
                                Result(K).(Args.GAIA_Cols{Igaia}) = getCol(Result(K).Star, Args.GAIA_Cols{Igaia});
                            end
                            
                            
                        end
            
                    end
                end
            end
        end
        
    end
    
    
end