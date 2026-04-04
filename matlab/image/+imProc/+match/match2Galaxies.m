function match2Galaxies(Obj, Args)
    %{
    Matches AstroCatalog entries to galaxies using external catalogs GLADE 
      and PGC. Appends columns with match results.
    Input  : - An AstroCatalog or AstroImage/AstroZOGY with an AstroCatalog
               property.
             * ...,key,val,...
               'GladeCatName' - Name of the GLADE catalog. Default is
                      'GLADEp'.
               'RadiusGlade' - Search radius for matching with GLADE.
                      Default is 5.
               'RadiusGladeUnits' - Units of RadiusGlade. Default is
                      'arcsec'.
               'PGCCatName' - Name of the PGC catalog. Default is 'PGC'.
               'RadiusPGC' - Initial search radius for matching with PGC.
                      Default is 300.
               'RadiusPGCUnits' - Units of RadiusPGC. Default is
                      'arcsec'.
               'DefaultGalRadiusPGC' - Default galaxy semi-major radius
                      [arcsec] used when LogD25 is NaN. Default is 10.
               'ColNmatchName' - Name of appended column with number of 
                      matches. Default is 'GAL_N'.
               'ColDistName' - Name of appended column with the angular 
                      distance to the highest-probability match. Default is
                      'GAL_DIST'.
               'ColProbName' - Name of appended column holding the
                      probability-like score of the highest-probability
                      accepted galaxy match. Default is 'GAL_PROB'.
               'MergeCols' - Bool on whether to return merged result
                      columns instead of GLADE and PGC separately. Default
                      is true.
               'PixelScale' - Pixel scale [arcsec/pix], used together with
                      the PSF FWHM to estimate the point-source size.
                      Default is 1.25.
               'ColLogD25PGC' - Name of PGC column holding LogD25.
                      Default is 'LogD25'.
               'ColLogAxisRatioPGC' - Name of PGC column holding
                      LogAxisRatio. Default is 'LogAxisRatio'.
               'ColPA1950PGC' - Name of PGC column holding PA1950 in deg.
                      Default is 'PA1950'.
               'PGCEllipseTol' - Elliptical matching tolerance in units of
                      rho. Default is 1.1.
               'GalAreaPenaltyScale' - Reference semi-major axis [arcsec]
                      for penalizing large galaxies in GAL_PROB. Default is
                      30.
    Output : - Updates the input catalog(s) in place by appending/replacing
               galaxy match columns.
    Method : - Uses a fixed-radius match for GLADE and an elliptical
               footprint match for PGC.
             - For each accepted GLADE match, computes a probability-like
               score exp(-Dist/RadiusGlade).
             - For each accepted PGC match, computes a probability-like
               score exp(-rho) multiplied by an area penalty term
               1/(1 + a/a0), where rho is the elliptical radius in the
               galaxy frame, a is the semi-major axis, and a0 is
               GalAreaPenaltyScale.
             - GAL_PROB is the highest such score among accepted GLADE and
               PGC matches.
    Author : Ruslan Konno (Feb 2024)
    %}
    
    arguments
        Obj

        Args.GladeCatName = 'GLADEp';
        Args.RadiusGlade = 5;
        Args.RadiusGladeUnits = 'arcsec';

        Args.PGCCatName = 'PGC';
        Args.RadiusPGC = 300;
        Args.RadiusPGCUnits = 'arcsec';
        Args.DefaultGalRadiusPGC = 10;

        Args.ColNmatchName = 'GAL_N';
        Args.ColDistName = 'GAL_DIST';
        Args.ColProbName = 'GAL_PROB';

        Args.MergeCols logical = true;

        Args.PixelScale = 1.25;

        Args.ColLogD25PGC = 'LogD25';
        Args.ColLogAxisRatioPGC = 'LogAxisRatio';
        Args.ColPA1950PGC = 'PA1950';

        Args.PGCEllipseTol = 1.3;
        Args.GalAreaPenaltyScale = 30;
    end

    % Normalize object type
    switch class(Obj)
        case {'AstroImage','AstroZOGY'}
            ACObj = [Obj(:).CatData];
            ObjArr = Obj(:);
        case 'AstroCatalog'
            ACObj = Obj(:);
            ObjArr = Obj(:);
        otherwise
            error('Object class not supported.');
    end

    % constants
    Rad2Arcsec = 206265;
    Arcsec2Rad = 4.84814e-6;

    Nobj = numel(ACObj);
    for Iobj = 1:Nobj

        % Skip empty catalogs
        CatSize = size(ACObj(Iobj).Catalog, 1);
        if CatSize < 1
            continue
        end

        % estimate point-source matching limit from PSF
        PointLimit = 3;
        if ismember(class(ObjArr(Iobj)), {'AstroImage','AstroZOGY'})
            PointLimit = ObjArr(Iobj).PSFData.fwhm .* Args.PixelScale .* 1.2739;
            if ACObj(Iobj).isColumn('N_X2') && ACObj(Iobj).isColumn('N_Y2')
                N_X2 = ACObj(Iobj).getCol('N_X2');
                N_Y2 = ACObj(Iobj).getCol('N_Y2');
                PoorPSF = (median(N_X2, 'omitnan') > 1.2) || ...
                          (median(N_Y2, 'omitnan') > 1.2);
                if PoorPSF
                    PointLimit = PointLimit .* 5/3;
                end
            end
        end

        % names for merged columns
        GladeDistCol = strcat(Args.ColDistName, 'GLADE');
        GladeNCol = strcat(Args.ColNmatchName, 'GLADE');

        PGCDistCol = strcat(Args.ColDistName, 'PGC');
        PGCNCol = strcat(Args.ColNmatchName, 'PGC');

        % get source coords (radians)
        RADec = ACObj(Iobj).getLonLat('rad');
        RA = RADec(:,1);
        Dec = RADec(:,2);

        % define local search radius based on footprint of the sources
        MidRA = median(RA);
        MidDec = median(Dec);

        MaxDist = max(celestial.coo.sphere_dist( ...
            RA, Dec, MidRA .* ones(CatSize,1), MidDec .* ones(CatSize,1)));
        MaxDistAngle = AstroAngle(MaxDist, 'rad');

        % GLADE rough match radius (arcsec)
        RadiusGlade = max(Args.RadiusGlade, PointLimit);
        SearchRadiusGlade = MaxDistAngle.convert(Args.RadiusGladeUnits).Angle + RadiusGlade;

        % perform GLADE cone search and quick matching
        GladeCat = catsHTM.cone_search(Args.GladeCatName, ...
                MidRA, MidDec, SearchRadiusGlade, ...
                'RadiusUnits', Args.RadiusGladeUnits, 'OutType', 'AstroCatalog');

        MatchesGlade = zeros(CatSize,1);
        DistancesGlade = NaN(CatSize,1);
        ProbGlade = NaN(CatSize,1);

        if GladeCat.sizeCatalog > 0
            GladeCat.sortrows('Dec');
            [GladeLon, GladeLat] = GladeCat.getLonLat('rad');

            MatchResGlade = VO.search.search_sortedlat_multi( ...
                [GladeLon, GladeLat], RA, Dec, -RadiusGlade .* Arcsec2Rad);
            MatchesGlade = vertcat(MatchResGlade.Nmatch);

            for Isrc = 1:CatSize
                Match = MatchResGlade(Isrc);
                if Match.Nmatch < 1
                    continue
                end

                Dist = celestial.coo.sphere_dist_fast( ...
                    GladeLon(Match.Ind), GladeLat(Match.Ind), RA(Isrc), Dec(Isrc));
                Dist = Dist .* Rad2Arcsec;

                FlagM = Dist < RadiusGlade;
                MatchesGlade(Isrc) = sum(FlagM);

                if any(FlagM)
                    DistGood = Dist(FlagM);
                    % use exponential fall-off for GLADE (short-range)
                    ProbGood = exp(-DistGood ./ RadiusGlade);

                    [BestProb, iBest] = max(ProbGood);
                    ProbGlade(Isrc) = BestProb;
                    DistancesGlade(Isrc) = DistGood(iBest);
                end
            end
        end

        % PGC rough match + refined elliptical matching
        SearchRadiusPGC = MaxDistAngle.convert(Args.RadiusPGCUnits).Angle + Args.RadiusPGC;
        PGCCat = catsHTM.cone_search(Args.PGCCatName, ...
                MidRA, MidDec, SearchRadiusPGC, ...
                'RadiusUnits', Args.RadiusPGCUnits, 'OutType', 'AstroCatalog');

        MatchesPGC = zeros(CatSize,1);
        DistancesPGC = NaN(CatSize,1);
        ProbPGC = NaN(CatSize,1);

        if PGCCat.sizeCatalog > 0
            PGCCat.sortrows('Dec');
            [PGCLon, PGCLat] = PGCCat.getLonLat();

            MatchResPGC = VO.search.search_sortedlat_multi( ...
                [PGCLon, PGCLat], RA, Dec, -Args.RadiusPGC .* Arcsec2Rad);
            MatchesPGC = vertcat(MatchResPGC.Nmatch);
        end

        % if no PGC matches at all, insert GLADE-only results and continue
        if ~any(MatchesPGC > 0)
            if Args.MergeCols
                ACObj(Iobj).insertCol(MatchesGlade, Inf, Args.ColNmatchName);
                ACObj(Iobj).insertCol(DistancesGlade, Inf, Args.ColDistName);
                ACObj(Iobj).insertCol(ProbGlade, Inf, Args.ColProbName);
            else
                ACObj(Iobj).insertCol(MatchesGlade, Inf, GladeNCol);
                ACObj(Iobj).insertCol(DistancesGlade, Inf, GladeDistCol);
                ACObj(Iobj).insertCol(MatchesPGC, Inf, PGCNCol);
                ACObj(Iobj).insertCol(DistancesPGC, Inf, PGCDistCol);
                ACObj(Iobj).insertCol(ProbGlade, Inf, strcat(Args.ColProbName, 'GLADE'));
                ACObj(Iobj).insertCol(ProbPGC, Inf, strcat(Args.ColProbName, 'PGC'));
            end
            continue
        end

        % read PGC ellipse parameters (with safe fallbacks)
        LogD25 = getColOrNaN(PGCCat, Args.ColLogD25PGC);
        LogAxisRatio = getColOrNaN(PGCCat, Args.ColLogAxisRatioPGC);
        PA1950 = getColOrNaN(PGCCat, Args.ColPA1950PGC);

        % fallback logic: if size or PA missing, treat as circular with default radius
        BadD = ~isfinite(LogD25);
        BadPA = ~isfinite(PA1950);
        Fallback = BadD | BadPA;

        % semi-major axis in arcsec (same convention as elsewhere)
        GalA = 3 .* 10.^LogD25;
        GalA(Fallback) = Args.DefaultGalRadiusPGC;
        GalA = max(GalA, PointLimit);

        % axis ratio b/a (PGC stores log10(a/b) convention)
        AxisRatio = 10.^(-LogAxisRatio);
        AxisRatio(~isfinite(AxisRatio)) = 1;
        AxisRatio = min(max(AxisRatio, 0.1), 1.0);

        % semi-minor axis
        GalB = GalA .* AxisRatio;
        GalB(Fallback) = GalA(Fallback);
        GalB = max(GalB, PointLimit);

        % position angle in radians (PA1950 from PGC; fallback 0)
        PA = PA1950 .* pi ./ 180;
        PA(~isfinite(PA)) = 0;

        CosPA = cos(PA);
        SinPA = sin(PA);

        % iterate over catalog sources and test PGC candidates
        for Isrc = 1:CatSize
            Match = MatchResPGC(Isrc);
            if Match.Nmatch < 1
                continue
            end

            Ind = Match.Ind;

            % small-angle tangent-plane offsets in arcsec
            % (use source Dec for cos scaling as before)
            dRA = (RA(Isrc) - PGCLon(Ind)) .* cos(Dec(Isrc));
            dDec = Dec(Isrc) - PGCLat(Ind);

            dx = dRA .* Rad2Arcsec;   % East offset [arcsec]
            dy = dDec .* Rad2Arcsec; % North offset [arcsec]

            % MATCHER rotation convention (source of truth):
            % xprime = dx * cosPA + dy * sinPA
            % yprime = -dx * sinPA + dy * cosPA
            % (these are coordinates along major/minor axes respectively)
            xprime = dx .* CosPA(Ind) + dy .* SinPA(Ind);
            yprime = -dx .* SinPA(Ind) + dy .* CosPA(Ind);

            % elliptical radius (dimensionless)
            rho2 = (xprime ./ GalA(Ind)).^2 + (yprime ./ GalB(Ind)).^2;
            rho = sqrt(rho2);

            % accept if within tolerance in rho units
            FlagM = rho < Args.PGCEllipseTol;
            MatchesPGC(Isrc) = sum(FlagM);

            if any(FlagM)
                DistGood = sqrt(dx(FlagM).^2 + dy(FlagM).^2);
                RhoGood = rho(FlagM);
                AGood = GalA(Ind(FlagM));  % corresponding semi-major axes

                % Area penalty to downweight large galaxies (chance alignments)
                AreaPenalty = 1 ./ (1 + AGood ./ Args.GalAreaPenaltyScale);

                % galaxy association score: exponential radial fall-off * area penalty
                ProbGood = exp(-RhoGood) .* AreaPenalty;

                [BestProb, iBest] = max(ProbGood);
                ProbPGC(Isrc) = BestProb;
                DistancesPGC(Isrc) = DistGood(iBest);
            end
        end

        % merge GLADE and PGC results (choose the best probability)
        if Args.MergeCols
            MatchesGal = MatchesGlade + MatchesPGC;

            DistancesGal = DistancesGlade;
            ProbGal = ProbGlade;

            UsePGC = isnan(ProbGal) | (~isnan(ProbPGC) & ProbPGC > ProbGal);
            DistancesGal(UsePGC) = DistancesPGC(UsePGC);
            ProbGal(UsePGC) = ProbPGC(UsePGC);

            ACObj(Iobj).insertCol(MatchesGal, Inf, Args.ColNmatchName);
            ACObj(Iobj).insertCol(DistancesGal, Inf, Args.ColDistName);
            ACObj(Iobj).insertCol(ProbGal, Inf, Args.ColProbName);
        else
            ACObj(Iobj).insertCol(MatchesGlade, Inf, GladeNCol);
            ACObj(Iobj).insertCol(DistancesGlade, Inf, GladeDistCol);
            ACObj(Iobj).insertCol(MatchesPGC, Inf, PGCNCol);
            ACObj(Iobj).insertCol(DistancesPGC, Inf, PGCDistCol);
            ACObj(Iobj).insertCol(ProbGlade, Inf, strcat(Args.ColProbName, 'GLADE'));
            ACObj(Iobj).insertCol(ProbPGC, Inf, strcat(Args.ColProbName, 'PGC'));
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