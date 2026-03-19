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
    Output : - Updates the input catalog(s) in place by appending/replacing
               galaxy match columns.
    Method : - Uses a fixed-radius match for GLADE and an elliptical
               footprint match for PGC.
             - For each accepted galaxy match, computes a probability-like
               score. For GLADE this is exp(-0.5*(Dist/RadiusGlade)^2).
               For PGC this is exp(-0.5*rho^2), where rho is the
               elliptical radius in the galaxy frame.
             - GAL_PROB is the highest such score among accepted GLADE and
               PGC matches.
    Author : Ruslan Konno (Feb 2024)
    Example: AC = AstroCatalog({rand(10,2), rand(10,2)},'ColNames',{'RA','Dec'});
             imProc.match.match2Galaxies(AC);
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
    end

    % Make sure process is run on AstroCatalog object
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

    Rad2Arcsec = 206265;
    Arcsec2Rad = 4.84814e-6;

    Nobj = numel(ACObj);
    for Iobj = 1:Nobj

        % Skip empty catalogs
        CatSize = size(ACObj(Iobj).Catalog, 1);
        if CatSize < 1
            continue
        end

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

        % Find initial rough matches for GLADE and PGC
        GladeDistCol = strcat(Args.ColDistName, 'GLADE');
        GladeNCol = strcat(Args.ColNmatchName, 'GLADE');

        PGCDistCol = strcat(Args.ColDistName, 'PGC');
        PGCNCol = strcat(Args.ColNmatchName, 'PGC');

        RADec = ACObj(Iobj).getLonLat('rad');
        RA = RADec(:,1);
        Dec = RADec(:,2);

        MidRA = median(RA);
        MidDec = median(Dec);

        MaxDist = max(celestial.coo.sphere_dist( ...
            RA, Dec, MidRA .* ones(CatSize,1), MidDec .* ones(CatSize,1)));
    
        MaxDistAngle = AstroAngle(MaxDist, 'rad');

        RadiusGlade = max(Args.RadiusGlade, PointLimit);

        SearchRadiusGlade = MaxDistAngle.convert(Args.RadiusGladeUnits).Angle ...
            + RadiusGlade;

        % Rough match is final match for GLADE
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
                    ProbGood = exp(-0.5 .* (DistGood ./ RadiusGlade).^2);

                    [BestProb, iBest] = max(ProbGood);
                    ProbGlade(Isrc) = BestProb;
                    DistancesGlade(Isrc) = DistGood(iBest);
                end
            end
        end
       
        % PGC matches will be refined for roughly matched entries
        SearchRadiusPGC = MaxDistAngle.convert(Args.RadiusPGCUnits).Angle ...
            + Args.RadiusPGC;

        PGCCat = catsHTM.cone_search(Args.PGCCatName, ...
                MidRA, MidDec, SearchRadiusPGC, ...
                'RadiusUnits', Args.RadiusPGCUnits, 'OutType', 'AstroCatalog');

        MatchesPGC = zeros(CatSize,1);
        DistancesPGC = NaN(CatSize,1);
        ProbPGC = NaN(CatSize,1);
        
        if PGCCat.sizeCatalog > 0

            PGCCat.sortrows('Dec');
    
            [PGCLon, PGCLat] = PGCCat.getLonLat('rad');
            
            MatchResPGC = VO.search.search_sortedlat_multi( ...
                [PGCLon, PGCLat], RA, Dec, -Args.RadiusPGC .* Arcsec2Rad);
    
            MatchesPGC = vertcat(MatchResPGC.Nmatch);
        end
        
        % Skip entries that have no rough PGC matches
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

        % PGC ellipse parameters
        LogD25 = getColOrNaN(PGCCat, Args.ColLogD25PGC);
        LogAxisRatio = getColOrNaN(PGCCat, Args.ColLogAxisRatioPGC);
        PA1950 = getColOrNaN(PGCCat, Args.ColPA1950PGC);

        % Semi-major axis in arcsec
        GalA = 3 .* 10.^LogD25;
        GalA(~isfinite(GalA)) = Args.DefaultGalRadiusPGC;
        GalA = max(GalA, PointLimit);

        % Axis ratio b/a
        AxisRatio = 10.^(-LogAxisRatio);
        AxisRatio(~isfinite(AxisRatio)) = 1;
        AxisRatio = min(max(AxisRatio, 0.1), 1.0);

        % Semi-minor axis
        GalB = GalA .* AxisRatio;
        GalB = max(GalB, PointLimit);

        % Position angle in radians
        PA = PA1950 .* pi ./ 180;
        PA(~isfinite(PA)) = 0;

        CosPA = cos(PA);
        SinPA = sin(PA);
        
        for Isrc = 1:CatSize

            Match = MatchResPGC(Isrc);

            if MatchesPGC(Isrc) < 1
                continue
            end

            Ind = Match.Ind;

            % Small-angle tangent-plane offsets in arcsec
            dRA = (RA(Isrc) - PGCLon(Ind)) .* cos(Dec(Isrc));
            dDec = Dec(Isrc) - PGCLat(Ind);

            dx = dRA .* Rad2Arcsec;
            dy = dDec .* Rad2Arcsec;

            % Rotate into galaxy frame
            xprime =  dx .* CosPA(Ind) + dy .* SinPA(Ind);
            yprime = -dx .* SinPA(Ind) + dy .* CosPA(Ind);

            % Elliptical radius squared
            rho2 = (xprime ./ GalA(Ind)).^2 + (yprime ./ GalB(Ind)).^2;

            FlagM = rho2 < 1;
            MatchesPGC(Isrc) = sum(FlagM);

            if any(FlagM)
                DistGood = sqrt(dx(FlagM).^2 + dy(FlagM).^2);
                Rho2Good = rho2(FlagM);

                ProbGood = exp(-0.5 .* Rho2Good);

                [BestProb, iBest] = max(ProbGood);
                ProbPGC(Isrc) = BestProb;
                DistancesPGC(Isrc) = DistGood(iBest);
            end
        end

        % If GLADE and PGC results should be merged, take the sum for
        % number of matches and the best probability / corresponding distance.
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