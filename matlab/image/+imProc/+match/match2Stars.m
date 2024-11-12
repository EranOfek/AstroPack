function match2Stars(Obj, StarCat, Args)
    %{
    Matches AstroCatalog entries to stars using external GAIA catalog. 
      Appends columns with match results.
    Input  : - An AstroCatalog or AstroImage/AstroZOGY with an AstroCatalog
               property.
             * ...,key,val,...
               'StarCat' - An AstroCatalog containing the catalog of stars.
                      If given, will be used for catalog matching. If not,
                      will calculate a catalog for each object. Deault is
                      [].
               'StarCatName' - Name of the GAIA catalog. Default is 'GAIADR3'.
               'ColNMatchName' - Name of appended column with number of matches. 
                      Default is 'StarMatches'.
               'ColDistName' - Name of appended column with the distance 
                      to the nearest match. Default is 'StarDist'.
               'ColBpMagGAIA' - Name of column holding the blue magnitude in 
                      GAIA catalog, magnitude will be used as reference. 
                      Default is 'phot_bp_mean_mag'.
               'ColAstExcessNoiseGAIA' - Name of column holding the astrometric 
                      excess noise in GAIA catalog. 
                      Default is 'astrometric_excess_noise'.
               'MaxMagGAIA' - Maximum GAIA magnitude to be considered for 
                      matching. Default is 21.
               'SearchRadius' - Initial search radius for matching with GAIA.
                      Default is 250.
               'SearchRadiusUnits' - Units of SearchRadius. Default is 'arcsec'.
               'UserSpecialBright'- Bool on whether to use parametrized
                      threshold distance for bright stars. Parametrization 
                      is a*exp(-b*x)+c. Default is true.
               'BpBrightParams' - Parameters used for threshold estimation 
                      of blue stars. Default is [2964.12, 1.03, 131.03].
               'BpBrightTresh' -  Blue magnitude threshold after which to apply 
                      bright star distance estimation. Default is 5.8.
               'RpBrightParams' - Parameters used for threshold estimation 
                      of red stars. Default is [1882.92, 1.13, 51.47].
               'RpBrightTresh' - Red magnitude threshold after which to apply 
                      bright star distance estimation. Default is 5.8.
    Author : Ruslan Konno (Feb 2024)
    Example: AC = AstroCatalog({rand(10,2), rand(10,2)},'ColNames',{'RA','Dec'});
             imProc.match.match2Stars(AC);
    %}
    
    arguments
        Obj
        StarCat

        Args.StarCatName = 'GAIADR3';
        Args.ColNmatchName = 'STAR_N';
        Args.ColDistName = 'STAR_DIST';

        Args.ColBpMagGAIA  = 'phot_bp_mean_mag';
        Args.ColRpMagGAIA  = 'phot_rp_mean_mag';
        Args.ColAstExcessNoiseGAIA = 'astrometric_excess_noise';

        Args.MaxMagGAIA  = 21;
        Args.SearchRadius = 250;
        Args.SearchRadiusUnits = 'arcsec';

        Args.UseSpecialBright logical  = true;
        Args.BpBrightParams = [2964.12, 1.03, 131.03];
        Args.BpBrightTresh = 5.8;
        Args.RpBrightParams = [1882.92, 1.13, 51.47];
        Args.RpBrightTresh = 5.8;
    end

    % Make sure process is run on AstroCatalog object
    switch class(Obj)
        case {'AstroImage','AstroZOGY'}
            ACObj = [Obj(:).CatData];
        case 'AstroCatalog'
            ACObj = Obj;
        otherwise
            warning('Object class not supported.')
    end

    Rad2Arcsec = 206265;
    Arcsec2Rad = 4.84814e-6;

    % TODO: Do an actual study on effective star size, look at brightness
    % vs 2sig, 3sig contamination radii.

    % If StarCat is given, derive distance thresholds
    % considering a star's brightness and excess noise.

    BpMags = StarCat.Table.(Args.ColBpMagGAIA);
    RpMags = StarCat.Table.(Args.ColRpMagGAIA);
    DistThresholdPerStar = max(max(1.5*(20.0-RpMags), ...
        1.5*(20.0-BpMags)), ...
        3+StarCat.Table.(Args.ColAstExcessNoiseGAIA)*0.001);

    % Estimate threshold for bright stars.
    if Args.UseSpecialBright
        DistThresholdPerStar(BpMags < Args.BpBrightTresh) = ...
            max(1.5*(Args.BpBrightParams(1).*exp(-Args.BpBrightParams(2).*...
            BpMags(BpMags<Args.BpBrightTresh))+Args.BpBrightParams(3)),...
            DistThresholdPerStar(BpMags < Args.BpBrightTresh));
        DistThresholdPerStar(RpMags < Args.RpBrightTresh) = ...
            max(1.5*(Args.RpBrightParams(1).*exp(-Args.RpBrightParams(2).*...
            RpMags(RpMags<Args.RpBrightTresh))+Args.RpBrightParams(3)),...
            DistThresholdPerStar(RpMags < Args.RpBrightTresh));
    end

    StarCat.insertCol(cast(DistThresholdPerStar,'double'), ...
            Inf, {'DistThresh'}, {''});

    % For performance, split initial catalog in two; 1) stars with smaller
    % distance thresholds (< mean+3std; majority), and 2) stars with larger
    % distance thresholds (rest, minority). This way we do not appy the
    % initial rough match with the largest threshold on all stars (very slow).

    MeanDist = mean(DistThresholdPerStar);
    StdDist = std(DistThresholdPerStar);
    CatSepDist = MeanDist + 3*StdDist;

    StarCatClose = StarCat.selectRows(DistThresholdPerStar <= CatSepDist);
    DistThresholdClose = DistThresholdPerStar(DistThresholdPerStar <= CatSepDist);
    [ObjLonClose, ObjLatClose] = StarCatClose.getLonLat('rad');

    Nobj = numel(ACObj);

    for Iobj=1:1:Nobj

        CatSize = size(ACObj(Iobj).Catalog,1);
        if CatSize < 1
            continue
        end

        RADec = ACObj(Iobj).getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);

        % Find initial rough matches
        MatchRes = VO.search.search_sortedlat_multi( ...
            [ObjLonClose, ObjLatClose], RA, Dec, CatSepDist*Arcsec2Rad);

        Matches = vertcat(MatchRes.Nmatch);
        Distances = NaN(CatSize,1);

        % Perform finer search
        for Isrc = 1:1:CatSize

            Match = MatchRes(Isrc);
            
            % Skip entries with no matches
            if Match.Nmatch < 1
                continue
            end
            
            Dist    = celestial.coo.sphere_dist_fast( ...
                ObjLonClose(Match.Ind), ObjLatClose(Match.Ind), RA(Isrc), Dec(Isrc));
            Dist = Dist * Rad2Arcsec;   

            % Flag as match if catalog entry within a star's distance
            % threshold.
            FlagM   = Dist<DistThresholdClose(Match.Ind);
    
            % Update match results with the results of the finer search
            Matches(Isrc) = sum(FlagM);
            if any(FlagM)
                Distances(Isrc) = min(Dist);
            end
    
        end

        % Update catalog with updated matches
        ACObj(Iobj).insertCol(Matches, inf, Args.ColNmatchName);
        ACObj(Iobj).insertCol(Distances, inf, Args.ColDistName);
    end


    StarCatFar = StarCat.selectRows(DistThresholdPerStar > CatSepDist);
    DistThresholdFar = DistThresholdPerStar(DistThresholdPerStar > CatSepDist);
    [ObjLonFar, ObjLatFar] = StarCatFar.getLonLat('rad');
    MaxDistThresh = max(DistThresholdFar);

    for Iobj=1:1:Nobj

        CatSize = size(ACObj(Iobj).Catalog,1);
        if CatSize < 1
            continue
        end

        RADec = ACObj(Iobj).getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);

        % Find initial rough matches
        MatchRes = VO.search.search_sortedlat_multi( ...
            [ObjLonFar, ObjLatFar], RA, Dec, MaxDistThresh*Arcsec2Rad);

        Matches = vertcat(MatchRes.Nmatch);
        Distances = NaN(CatSize,1);

        % Perform finer search
        for Isrc = 1:1:CatSize

            Match = MatchRes(Isrc);
            
            % Skip entries with no matches
            if Match.Nmatch < 1
                continue
            end
            
            Dist    = celestial.coo.sphere_dist_fast( ...
                ObjLonFar(Match.Ind), ObjLatFar(Match.Ind), RA(Isrc), Dec(Isrc));
            Dist = Dist * Rad2Arcsec;   

            % Flag as match if catalog entry within a star's distance
            % threshold.
            FlagM   = Dist<DistThresholdFar(Match.Ind);
    
            % Update match results with the results of the finer search
            Matches(Isrc) = sum(FlagM);
            if any(FlagM)
                Distances(Isrc) = min(Dist);
            end
    
        end

        MatchesClose = ACObj(Iobj).getCol(Args.ColNmatchName);
        DistancesClose = ACObj(Iobj).getCol(Args.ColDistName);

        MatchesAll = MatchesClose + Matches;
        DistancesAll = min(DistancesClose, Distances);

        % Update catalog with updated matches
        ACObj(Iobj).replaceCol(MatchesAll, Args.ColNmatchName)
        ACObj(Iobj).replaceCol(DistancesAll, Args.ColDistName);
    end


end