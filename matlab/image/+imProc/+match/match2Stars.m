function match2Stars(Obj, StarCat, Args)
    %{
    Matches AstroCatalog entries to stars using external GAIA catalog. 
      Appends columns with match results.
    Input  : - An AstroCatalog or AstroImage/AstroZOGY with an AstroCatalog
               property.
             - An AstroCatalog containing the catalog of stars.
             * ...,key,val,...
               'StarCatName' - Name of the GAIA catalog. Default is
                      'GAIADR3'.
               'ColNMatchName' - Name of appended column with number of
                      accepted matches. Default is 'STAR_N'.
               'ColDistName' - Name of appended column with the distance
                      [arcsec] to the highest-probability accepted match.
                      Default is 'STAR_DIST'.
               'ColProbName' - Name of appended column holding the
                      probability-like score of the highest-probability
                      accepted stellar match. Default is 'STAR_PROB'.
               'PixelScale' - Pixel scale [arcsec/pix], used together with
                      the PSF FWHM to estimate the point-source size.
                      Default is 1.25.
               'ColBpName' - Name of appended column holding the BP
                      magnitude of the highest-probability accepted GAIA
                      match. Default is 'GAIA_BP'.
               'ColRpName' - Name of appended column holding the RP
                      magnitude of the highest-probability accepted GAIA
                      match. Default is 'GAIA_RP'.
               'ColBpMagGAIA' - Name of column holding the blue magnitude in
                      the GAIA catalog. Default is 'phot_bp_mean_mag'.
               'ColRpMagGAIA' - Name of column holding the red magnitude in
                      the GAIA catalog. Default is 'phot_rp_mean_mag'.
               'ColAstExcessNoiseGAIA' - Name of column holding the
                      astrometric excess noise in the GAIA catalog.
                      Default is 'astrometric_excess_noise'.
               'ColParallaxGAIA' - Name of column holding the parallax in
                      the GAIA catalog. Default is 'parallax'.
               'ColParallaxErrGAIA' - Name of column holding the parallax
                      error in the GAIA catalog. Default is
                      'parallax_error'.
               'ColPmraGAIA' - Name of column holding pmRA in the GAIA
                      catalog. Default is 'pmra'.
               'ColPmraErrGAIA' - Name of column holding pmRA error in the
                      GAIA catalog. Default is 'pmra_error'.
               'ColPmdecGAIA' - Name of column holding pmDec in the GAIA
                      catalog. Default is 'pmdec'.
               'ColPmdecErrGAIA' - Name of column holding pmDec error in the
                      GAIA catalog. Default is 'pmdec_error'.
               'ColInQsoCandGAIA' - Name of column holding the flag
                      indicating membership in the GAIA QSO candidates.
                      Default is 'in_qso_candidates'.
               'ColInGalaxyCandGAIA' - Name of column holding the flag
                      indicating membership in the GAIA galaxy candidates.
                      Default is 'in_galaxy_candidates'.
               'QsoPenalty' - Multiplicative penalty applied to STAR_PROB
                      if the matched source is flagged as a QSO candidate.
                      Default is 0.35.
               'GalaxyPenalty' - Multiplicative penalty applied to
                      STAR_PROB if the matched source is flagged as a
                      galaxy candidate. Default is 0.20.
               'MaxMagGAIA' - Maximum GAIA magnitude to be considered for
                      matching. Stars fainter than this are ignored.
                      Default is 21.
               'SearchRadius' - Reserved argument, currently not used in
                      this implementation. Default is 250.
               'SearchRadiusUnits' - Reserved argument, currently not used
                      in this implementation. Default is 'arcsec'.
               'UseSpecialBright'- Bool on whether to use parametrized
                      threshold distance for bright stars. Parametrization
                      is a*exp(-b*x)+c. Default is true.
               'BpBrightParams' - Parameters used for threshold estimation
                      of bright blue stars. Default is
                      [2964.12, 1.03, 131.03].
               'BpBrightTresh' - Blue magnitude threshold below which to
                      apply the bright-star BP parametrization.
                      Default is 5.8.
               'RpBrightParams' - Parameters used for threshold estimation
                      of bright red stars. Default is
                      [1882.92, 1.13, 51.47].
               'RpBrightTresh' - Red magnitude threshold below which to
                      apply the bright-star RP parametrization.
                      Default is 5.8.
               'AstroWeightFloor' - Baseline astrometric weight floor used
                      when parallax/proper motion are insignificant.
                      Final probability is
                      Pdist*(AstroWeightFloor + (1-AstroWeightFloor)*Pastro).
                      Default is 0.2.
    Output : - Updates the input catalog(s) in place by appending/replacing
               the following columns:
               * Args.ColNmatchName : number of accepted stellar matches.
               * Args.ColDistName   : distance to highest-probability
                 stellar match [arcsec].
               * Args.ColBpName     : BP magnitude of highest-probability
                 stellar match.
               * Args.ColRpName     : RP magnitude of highest-probability
                 stellar match.
               * Args.ColProbName   : probability-like score of the
                 highest-probability accepted stellar match.
    Method : - Derives a per-star matching threshold from GAIA BP/RP
               magnitude and astrometric excess noise.
             - For AstroImage/AstroZOGY inputs, enforces the threshold to be
               at least the image point-source scale inferred from the PSF.
             - Splits the star catalog into close/far subsets for
               performance, matches each subset separately, and merges the
               results.
             - For each accepted match, computes a geometric probability
               term exp(-0.5*(Dist/DistThresh)^2).
             - Computes an astrometric probability term from parallax and
               proper-motion significance.
             - STAR_PROB is the combined score of the accepted match with
               the highest total probability.
             - Applies multiplicative penalties to STAR_PROB for matches
               flagged as GAIA QSO or galaxy candidates.
    Author : Ruslan Konno (Feb 2024)
             Cleaned/refactored version with bug fixes.
    Example: AC = AstroCatalog({rand(10,2)},'ColNames',{'RA','Dec'});
             match2Stars(AC, StarCat);
    %}
    
    arguments
        Obj
        StarCat

        Args.StarCatName = 'GAIADR3';
        Args.ColNmatchName = 'STAR_N';
        Args.ColDistName = 'STAR_DIST';
        Args.ColProbName = 'STAR_PROB';

        Args.PixelScale = 1.25;

        Args.ColBpName = 'GAIA_BP';
        Args.ColRpName = 'GAIA_RP';

        Args.ColBpMagGAIA  = 'phot_bp_mean_mag';
        Args.ColRpMagGAIA  = 'phot_rp_mean_mag';
        Args.ColAstExcessNoiseGAIA = 'astrometric_excess_noise';

        Args.ColParallaxGAIA = 'Plx';
        Args.ColParallaxErrGAIA = 'ErrPlx';
        Args.ColPmraGAIA = 'PMRA';
        Args.ColPmraErrGAIA = 'ErrPMRA';
        Args.ColPmdecGAIA = 'PMDec';
        Args.ColPmdecErrGAIA = 'ErrPMDec';
        
        Args.ColInQsoCandGAIA = 'in_qso_candidates';
        Args.ColInGalaxyCandGAIA = 'in_galaxy_candidates';

        Args.QsoPenalty double = 0.35;
        Args.GalaxyPenalty double = 0.20;


        Args.MaxMagGAIA  = 21;
        Args.SearchRadius = 250;
        Args.SearchRadiusUnits = 'arcsec';

        Args.UseSpecialBright logical = true;
        Args.BpBrightParams = [2964.12, 1.03, 131.03];
        Args.BpBrightTresh = 5.8;
        Args.RpBrightParams = [1882.92, 1.13, 51.47];
        Args.RpBrightTresh = 5.8;

        Args.AstroWeightFloor double = 0.2;
    end

    Rad2Arcsec = 206265;
    Arcsec2Rad = 4.84814e-6;

    % Normalize object access
    switch class(Obj)
        case {'AstroImage','AstroZOGY'}
            ACObj = [Obj(:).CatData];
            ObjArr = Obj(:);
        case 'AstroCatalog'
            ACObj = Obj(:);
            ObjArr = Obj(:);
        otherwise
            error('Unsupported object class: %s', class(Obj));
    end

    % Optional Gaia magnitude cut
    if ~isempty(StarCat) && StarCat.isColumn(Args.ColBpMagGAIA) && StarCat.isColumn(Args.ColRpMagGAIA)
        BpMagsAll = StarCat.getCol(Args.ColBpMagGAIA);
        RpMagsAll = StarCat.getCol(Args.ColRpMagGAIA);

        MinMag = min(BpMagsAll, RpMagsAll);
        GoodMag = isfinite(MinMag) & (MinMag <= Args.MaxMagGAIA);
        StarCat = StarCat.selectRows(GoodMag);
    end

    if isempty(StarCat.Catalog)
        return
    end

    % Precompute per-star thresholds
    BpMags = StarCat.getCol(Args.ColBpMagGAIA);
    RpMags = StarCat.getCol(Args.ColRpMagGAIA);

    RefMag = min(BpMags, RpMags);
    RefMag(~isfinite(RefMag)) = 20;
    DistThresholdPerStar = 1.5 .* max(20 - min(RefMag, 20), 0);

    AstExcess = StarCat.getCol(Args.ColAstExcessNoiseGAIA);
    AstExcess(~isfinite(AstExcess)) = 0;
    AstrometricComp = 0.001 .* AstExcess;

    if Args.UseSpecialBright
        FlagBpBright = isfinite(BpMags) & (BpMags < Args.BpBrightTresh);
        FlagRpBright = isfinite(RpMags) & (RpMags < Args.RpBrightTresh);

        DistThresholdPerStar(FlagBpBright) = max( ...
            DistThresholdPerStar(FlagBpBright), ...
            1.5 .* (Args.BpBrightParams(1) .* exp(-Args.BpBrightParams(2) .* BpMags(FlagBpBright)) + Args.BpBrightParams(3)) ...
        );

        DistThresholdPerStar(FlagRpBright) = max( ...
            DistThresholdPerStar(FlagRpBright), ...
            1.5 .* (Args.RpBrightParams(1) .* exp(-Args.RpBrightParams(2) .* RpMags(FlagRpBright)) + Args.RpBrightParams(3)) ...
        );
    end

    DistThresholdPerStar = max(DistThresholdPerStar, 0);

    % Read astrometric quantities once
    Parallax = getColOrNaN(StarCat, Args.ColParallaxGAIA);
    ParallaxErr = getColOrNaN(StarCat, Args.ColParallaxErrGAIA);
    Pmra = getColOrNaN(StarCat, Args.ColPmraGAIA);
    PmraErr = getColOrNaN(StarCat, Args.ColPmraErrGAIA);
    Pmdec = getColOrNaN(StarCat, Args.ColPmdecGAIA);
    PmdecErr = getColOrNaN(StarCat, Args.ColPmdecErrGAIA);

    InQsoCand = getColOrFalse(StarCat, Args.ColInQsoCandGAIA);
    InGalaxyCand = getColOrFalse(StarCat, Args.ColInGalaxyCandGAIA);

    % Split catalog
    MeanDist = mean(max(DistThresholdPerStar, 3));
    StdDist  = std(max(DistThresholdPerStar, 3));
    CatSepDist = MeanDist + 3 .* StdDist;

    IsClose = DistThresholdPerStar <= CatSepDist;
    IsFar   = ~IsClose;

    StarCatClose = StarCat.selectRows(IsClose);
    StarCatFar   = StarCat.selectRows(IsFar);

    DistThresholdClose0 = DistThresholdPerStar(IsClose);
    DistThresholdFar0   = DistThresholdPerStar(IsFar);

    AstrometricCompClose = AstrometricComp(IsClose);
    AstrometricCompFar   = AstrometricComp(IsFar);

    BpMagsClose = BpMags(IsClose);
    RpMagsClose = RpMags(IsClose);
    BpMagsFar   = BpMags(IsFar);
    RpMagsFar   = RpMags(IsFar);

    ParallaxClose = Parallax(IsClose);
    ParallaxErrClose = ParallaxErr(IsClose);
    PmraClose = Pmra(IsClose);
    PmraErrClose = PmraErr(IsClose);
    PmdecClose = Pmdec(IsClose);
    PmdecErrClose = PmdecErr(IsClose);

    ParallaxFar = Parallax(IsFar);
    ParallaxErrFar = ParallaxErr(IsFar);
    PmraFar = Pmra(IsFar);
    PmraErrFar = PmraErr(IsFar);
    PmdecFar = Pmdec(IsFar);
    PmdecErrFar = PmdecErr(IsFar);

    InQsoCandClose = InQsoCand(IsClose);
    InGalaxyCandClose = InGalaxyCand(IsClose);

    InQsoCandFar = InQsoCand(IsFar);
    InGalaxyCandFar = InGalaxyCand(IsFar);

    if ~isempty(StarCatClose.Catalog)
        [LonClose, LatClose] = StarCatClose.getLonLat('rad');
    else
        LonClose = [];
        LatClose = [];
    end

    if ~isempty(StarCatFar.Catalog)
        [LonFar, LatFar] = StarCatFar.getLonLat('rad');
    else
        LonFar = [];
        LatFar = [];
    end

    Nobj = numel(ACObj);

    for Iobj = 1:Nobj

        CatSize = size(ACObj(Iobj).Catalog, 1);
        if CatSize < 1
            continue
        end

        [RA, Dec] = ACObj(Iobj).getLonLat('rad');

        PointLimit = 3;
        if ismember(class(ObjArr(Iobj)), {'AstroImage','AstroZOGY'})
            PointLimit = ObjArr(Iobj).PSFData.fwhm .* Args.PixelScale .* 1.2739;

            if ACObj(Iobj).isColumn('N_X2') && ACObj(Iobj).isColumn('N_Y2')
                N_X2 = ACObj(Iobj).getCol('N_X2');
                N_Y2 = ACObj(Iobj).getCol('N_Y2');
                PoorPSF = (median(N_X2, 'omitnan') > 1.2) || (median(N_Y2, 'omitnan') > 1.2);
                if PoorPSF
                    PointLimit = PointLimit .* 5/3;
                end
            end
        end

        MatchesAll = zeros(CatSize, 1);
        DistancesAll = NaN(CatSize, 1);
        MatchedBpAll = NaN(CatSize, 1);
        MatchedRpAll = NaN(CatSize, 1);
        ProbAll = NaN(CatSize, 1);

        % Close subset
        if ~isempty(LonClose)
            DistThresholdClose = max(DistThresholdClose0, PointLimit + AstrometricCompClose);
            RoughRadiusClose = max(DistThresholdClose);

            [Mclose, Dclose, BPclose, RPclose, Pclose] = matchStarSubset( ...
                LonClose, LatClose, DistThresholdClose, BpMagsClose, RpMagsClose, ...
                ParallaxClose, ParallaxErrClose, PmraClose, PmraErrClose, PmdecClose, PmdecErrClose, ...
                InQsoCandClose, InGalaxyCandClose, ...
                RA, Dec, RoughRadiusClose, Rad2Arcsec, Arcsec2Rad, ...
                Args.AstroWeightFloor, Args.QsoPenalty, Args.GalaxyPenalty);

            MatchesAll = MatchesAll + Mclose;
            DistancesAll = Dclose;
            MatchedBpAll = BPclose;
            MatchedRpAll = RPclose;
            ProbAll = Pclose;
        end

        % Far subset
        if ~isempty(LonFar)
            DistThresholdFar = max(DistThresholdFar0, PointLimit + AstrometricCompFar);
            RoughRadiusFar = max(DistThresholdFar);

            [Mfar, Dfar, BPfar, RPfar, Pfar] = matchStarSubset( ...
                LonFar, LatFar, DistThresholdFar, BpMagsFar, RpMagsFar, ...
                ParallaxFar, ParallaxErrFar, PmraFar, PmraErrFar, PmdecFar, PmdecErrFar, ...
                InQsoCandFar, InGalaxyCandFar, ...
                RA, Dec, RoughRadiusFar, Rad2Arcsec, Arcsec2Rad, ...
                Args.AstroWeightFloor, Args.QsoPenalty, Args.GalaxyPenalty);

            MatchesAll = MatchesAll + Mfar;

            UseFar = isnan(ProbAll) | (~isnan(Pfar) & Pfar > ProbAll);
            DistancesAll(UseFar) = Dfar(UseFar);
            MatchedBpAll(UseFar) = BPfar(UseFar);
            MatchedRpAll(UseFar) = RPfar(UseFar);
            ProbAll(UseFar) = Pfar(UseFar);
        end

        ACObj(Iobj).insertCol(MatchesAll, inf, Args.ColNmatchName);
        ACObj(Iobj).insertCol(DistancesAll, inf, Args.ColDistName);
        ACObj(Iobj).insertCol(MatchedBpAll, inf, Args.ColBpName);
        ACObj(Iobj).insertCol(MatchedRpAll, inf, Args.ColRpName);
        ACObj(Iobj).insertCol(ProbAll, inf, Args.ColProbName);
    end
end


function [Matches, Distances, MatchedBpMags, MatchedRpMags, MatchProb] = matchStarSubset( ...
    StarLon, StarLat, DistThreshold, BpMags, RpMags, ...
    Parallax, ParallaxErr, Pmra, PmraErr, Pmdec, PmdecErr, ...
    InQsoCand, InGalaxyCand, ...
    RA, Dec, RoughRadiusArcsec, Rad2Arcsec, Arcsec2Rad, ...
    AstroWeightFloor, QsoPenalty, GalaxyPenalty)

    CatSize = numel(RA);

    Matches = zeros(CatSize, 1);
    Distances = NaN(CatSize, 1);
    MatchedBpMags = NaN(CatSize, 1);
    MatchedRpMags = NaN(CatSize, 1);
    MatchProb = NaN(CatSize, 1);

    MatchRes = VO.search.search_sortedlat_multi( ...
        [StarLon, StarLat], RA, Dec, RoughRadiusArcsec .* Arcsec2Rad);

    for Isrc = 1:CatSize
        Match = MatchRes(Isrc);
        if Match.Nmatch < 1
            continue
        end

        Dist = celestial.coo.sphere_dist_fast( ...
            StarLon(Match.Ind), StarLat(Match.Ind), RA(Isrc), Dec(Isrc));
        Dist = Dist .* Rad2Arcsec;

        Thresh = DistThreshold(Match.Ind);
        FlagM = Dist < Thresh;

        Matches(Isrc) = sum(FlagM);

        if any(FlagM)
            DistGood = Dist(FlagM);
            ThreshGood = Thresh(FlagM);
            IndGood = Match.Ind(FlagM);

            % Geometry term
            Pdist = exp(-0.5 .* (DistGood ./ ThreshGood).^2);

            % Astrometric term
            Plx = Parallax(IndGood);
            PlxErr = ParallaxErr(IndGood);
            MuRa = Pmra(IndGood);
            MuRaErr = PmraErr(IndGood);
            MuDec = Pmdec(IndGood);
            MuDecErr = PmdecErr(IndGood);

            PlxSig = zeros(size(Plx));
            GoodPlx = isfinite(Plx) & isfinite(PlxErr) & (PlxErr > 0);
            PlxSig(GoodPlx) = max(0, Plx(GoodPlx) ./ PlxErr(GoodPlx));

            MuSig = zeros(size(MuRa));
            GoodMu = isfinite(MuRa) & isfinite(MuRaErr) & (MuRaErr > 0) & ...
                     isfinite(MuDec) & isfinite(MuDecErr) & (MuDecErr > 0);
            MuSig(GoodMu) = sqrt(MuRa(GoodMu).^2 + MuDec(GoodMu).^2) ./ ...
                            sqrt(MuRaErr(GoodMu).^2 + MuDecErr(GoodMu).^2);

            Pastro = 1 - exp(-0.5 .* (PlxSig.^2 + MuSig.^2));


            % Galaxy candidate punishment
            QsoFlag = logical(InQsoCand(IndGood));
            GalaxyFlag = logical(InGalaxyCand(IndGood));

            ExtraPenalty = ones(size(Pdist));
            ExtraPenalty(QsoFlag) = ExtraPenalty(QsoFlag) .* QsoPenalty;
            ExtraPenalty(GalaxyFlag) = ExtraPenalty(GalaxyFlag) .* GalaxyPenalty;

            % Combined score
            ProbGood = Pdist .* ...
                (AstroWeightFloor + (1 - AstroWeightFloor) .* Pastro) .* ...
                ExtraPenalty;

            [BestProb, iBest] = max(ProbGood);
            iStar = IndGood(iBest);

            MatchProb(Isrc) = BestProb;
            Distances(Isrc) = DistGood(iBest);
            MatchedBpMags(Isrc) = BpMags(iStar);
            MatchedRpMags(Isrc) = RpMags(iStar);
        end
    end
end

function Col = getColOrNaN(Cat, ColName)
    if Cat.isColumn(ColName)
        Col = Cat.getCol(ColName);
    else
        Col = NaN(size(Cat.Catalog, 1), 1);
    end
end

function Col = getColOrFalse(Cat, ColName)
    if Cat.isColumn(ColName)
        Col = Cat.getCol(ColName);
        Col = logical(Col);
    else
        Col = false(size(Cat.Catalog, 1), 1);
    end
end