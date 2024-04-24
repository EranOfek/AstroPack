function match2Stars(Obj, Args)
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
               'ColMagGAIA' - Name of column holding the magnitude in GAIA 
                      catalog, magnitude will be used as reference. 
                      Default is 'phot_bp_mean_mag'.
               'ColAstExcessNoiseGAIA' - Name of column holding the astrometric 
                      excess noise in GAIA catalog. 
                      Default is 'astrometric_excess_noise'.
               'ColGalaxyCandidateGAIA' - Name of column holding a boolean 
                      value on whether source is a galaxy candidate in GAIA
                      catalog. Default is 'in_galaxy_candidates'.
               'MaxMagGAIA' - Maximum GAIA magnitude to be considered for 
                      matching. Default is 21.
               'SearchRadius' - Initial search radius for matching with GAIA.
                      Default is 10.
               'SearchRadiusUnits' - Units of SearchRadius. Default is 'arcsec'.
    Author : Ruslan Konno (Feb 2024)
    Example: AC = AstroCatalog({rand(10,2), rand(10,2)},'ColNames',{'RA','Dec'});
             imProc.match.match2Stars(AC);
    %}
    
    arguments
        Obj

        Args.StarCat = [];

        Args.StarCatName = 'GAIADR3';
        Args.ColNmatchName = 'StarMatches';
        Args.ColDistName = 'StarDist';

        Args.ColMagGAIA  = 'phot_bp_mean_mag';
        Args.ColAstExcessNoiseGAIA = 'astrometric_excess_noise';
        Args.ColGalaxyCandidateGAIA = 'in_galaxy_candidates';

        Args.MaxMagGAIA  = 21;
        Args.SearchRadius = 10;
        Args.SearchRadiusUnits = 'arcsec';
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


    Nobj = numel(ACObj);


    % TODO: Consider (more) properly effective radius due to
    % saturation
    % Perform a finer search,
    % considering a star's brightness and excess noise
    if ~isempty(Args.StarCat)
        StarCat = Args.StarCat;
        DistThresholdPerStar = max(20.0-StarCat.Table.(Args.ColMagGAIA), ...
            3+StarCat.Table.(Args.ColAstExcessNoiseGAIA));
    end
    for Iobj=1:1:Nobj

        CatSize = size(ACObj(Iobj).Catalog,1);
        if CatSize < 1
            continue
        end

        % Find initial rough matches
        imProc.match.match_catsHTM(ACObj(Iobj), Args.StarCatName,...
            'ColDistName', Args.ColDistName,...
            'ColNmatchName', Args.ColNmatchName, ...
            'Radius', Args.SearchRadius, 'RadiusUnits', Args.SearchRadiusUnits);

        Matches = ACObj(Iobj).getCol(Args.ColNmatchName);
        Distances = ACObj(Iobj).getCol(Args.ColDistName);

        % Skip catalog if no matches
        if ~any(Matches>0)
            continue
        end

        RADec = ACObj(Iobj).getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);

        if isempty(Args.StarCat)
            
            % Get catalog for whole image.

            MidRA = median(RA);
            MidDec = median(Dec);
            MaxDist = max(celestial.coo.sphere_dist(RA, Dec,...
                MidRA*ones(CatSize,1), MidDec*ones(CatSize,1)));
            MaxDistAngle = AstroAngle(MaxDist, 'rad');
            SearchRadius = MaxDistAngle.convert(Args.SearchRadiusUnits)...
                + Args.SearchRadius;

            StarCat = catsHTM.cone_search(Args.StarCatName, ...
                MidRA, MidDec, SearchRadius, 'Con', {{Args.ColGalaxyCandidateGAIA, @(x) ~(x)}},...
                'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
            DistThresholdPerStar = max(20.0-StarCat.Table.(Args.ColMagGAIA), ...
                3+StarCat.Table.(Args.ColAstExcessNoiseGAIA));
        end

        for Itran = 1:1:CatSize

            % Skip entries with no matches
            if Matches(Itran) < 1
                continue
            end
            
            Dist    = StarCat.sphere_dist(RA(Itran), Dec(Itran), 'rad', 'arcsec');
   
            % Flag as match if catalog entry within a star's distance
            % threshold.
            FlagM   = Dist<DistThresholdPerStar;
    
            % Update match results with the results of the finer search
            Matches(Itran) = sum(FlagM);
            if ~any(FlagM)
                Distances(Itran) = NaN;
            end
    
        end

        % Update catalog with updated matches
        ACObj(Iobj).replaceCol(Matches, Args.ColNmatchName);
        ACObj(Iobj).replaceCol(Distances, Args.ColDistName);
    end

end