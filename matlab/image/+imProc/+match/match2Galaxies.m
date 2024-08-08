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
               'ColNmatchName' - Name of appended column with number of 
                      matches. Default is 'GalaxyMatches'.
               'ColDistName' - Name of appended column with the angular 
                      distance to the closest match. Default is
                      'GalaxyDist'.
               'MergeCols' - Bool on whether to return merged result
                      columns instead of GLADE and PGC separately. Default
                      is true.
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
        Args.RadiusPGC = 1;
        Args.RadiusPGCUnits = 'arcsec';

        Args.ColNmatchName = 'GAL_N';
        Args.ColDistName = 'GAL_DIST';

        Args.MergeCols logical = true;

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

    Arcsec2Rad = 4.84814e-6;

    Nobj = numel(ACObj);
    for Iobj=1:1:Nobj

        % Skip empty catalogs
        CatSize = size(ACObj(Iobj).Catalog,1);
        if CatSize < 1
            continue
        end

        % Find initial rough matches for GLADE and PGC
        GladeDistCol = strcat(Args.ColDistName,'GLADE');
        GladeNCol = strcat(Args.ColNmatchName,'GLADE');

        PGCDistCol = strcat(Args.ColDistName,'PGC');
        PGCNCol = strcat(Args.ColNmatchName,'PGC');   

        RADec = ACObj(Iobj).getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);

        MidRA = median(RA);
        MidDec = median(Dec);

        MaxDist = max(celestial.coo.sphere_dist(RA, Dec,...
            MidRA*ones(CatSize,1), MidDec*ones(CatSize,1)));
    
        MaxDistAngle = AstroAngle(MaxDist, 'rad');

        SearchRadiusGlade = MaxDistAngle.convert(Args.RadiusGladeUnits).Angle...
            + Args.RadiusGlade;

        % Rough match is final match for GLADE
        GladeCat = catsHTM.cone_search(Args.GladeCatName, ...
                MidRA, MidDec, SearchRadiusGlade, ...
                'RadiusUnits',Args.RadiusGladeUnits, 'OutType','AstroCatalog');

        GladeCat.sortrows('Dec');

        [GladeLon, GladeLat] = GladeCat.getLonLat('rad');

        MatchResGlade = VO.search.search_sortedlat_multi( ...
            [GladeLon, GladeLat], RA, Dec, -Args.RadiusGlade*Arcsec2Rad);

        MatchesGlade = vertcat(MatchResGlade.Nmatch);
        DistancesGlade = NaN(CatSize,1);

        if any(MatchesGlade > 0)
            DistancesGlade(MatchesGlade > 0) = arrayfun(@(a)min(a.Dist), ...
                MatchResGlade(MatchesGlade > 0));
        end
       
        % PGC matches will be refined for roughly matched entries
        % this considers extension of galaxies

        SearchRadiusPGC = MaxDistAngle.convert(Args.RadiusPGCUnits).Angle...
            + Args.RadiusPGC;
        PGCCat = catsHTM.cone_search(Args.PGCCatName, ...
                MidRA, MidDec, SearchRadiusPGC, ...
                'RadiusUnits',Args.RadiusPGCUnits, 'OutType','AstroCatalog');

        PGCCat.sortrows('Dec');

        [PGCLon, PGCLat] = PGCCat.getLonLat('rad');
        
        MatchResPGC = VO.search.search_sortedlat_multi( ...
            [PGCLon, PGCLat], RA, Dec, -Args.RadiusPGC*Arcsec2Rad);

        MatchesPGC = vertcat(MatchResPGC.Nmatch);
        DistancesPGC = NaN(CatSize,1);
        
        if any(MatchesPGC > 0)
            DistancesPGC(MatchesPGC > 0) = arrayfun(@(a)min(a.Dist), ...
                MatchResPGC(MatchesPGC > 0));
        end
        
        % Skip entries that have no rough PGC matches
        if ~any(MatchesPGC>0)
            if Args.MergeCols
                ACObj(Iobj).insertCol(MatchesGlade, Inf, Args.ColNmatchName);
                ACObj(Iobj).insertCol(DistancesGlade, Inf, Args.ColDistName);
            else
                ACObj(Iobj).insertCol(MatchesGlade, Inf, GladeNCol);
                ACObj(Iobj).insertCol(DistancesGlade, Inf, GladeDistCol);
                ACObj(Iobj).insertCol(MatchesPGC, Inf, PGCNCol);
                ACObj(Iobj).insertCol(DistancesPGC, Inf, PGCDistCol);
            end
            continue
        end

        for Itran = 1:1:CatSize

            if MatchesPGC(Itran) < 1
                continue
            end
    
            % Cone search for each rough PGC match.
            CatPGC = catsHTM.cone_search(Args.PGCCatName, ...
                RA(Itran), Dec(Itran), Args.RadiusPGC, ...
                'RadiusUnits',Args.RadiusPGCUnits, 'OutType','AstroCatalog');

            % Compare the distance to galaxy size
            if ~CatPGC.isemptyCatalog
                Dist    = CatPGC.sphere_dist(...
                    RA(Itran), Dec(Itran), 'rad', 'arcsec');
                
                GalRadius = 3.*10.^(CatPGC.Table.LogD25);

                % Update matches
                % If there are no matches, update nearest distance to NaN.
                MatchesPGC(Itran) = sum(Dist<GalRadius);
                if isempty(Dist) || ~any(Dist<GalRadius)
                    DistancesPGC(Itran) = NaN;
                end
            end 

        end

        % If GLADE and PGC results should be merged, take the sum for
        % number of matches and the minimum between matched distances.
        if Args.MergeCols
            MatchesGal = MatchesGlade + MatchesPGC;
            DistancesGal = min(DistancesGlade, DistancesPGC);
            ACObj(Iobj).insertCol(MatchesGal, Inf, Args.ColNmatchName);
            ACObj(Iobj).insertCol(DistancesGal, Inf, Args.ColDistName);
        else
            ACObj(Iobj).insertCol(MatchesGlade, Inf, GladeNCol);
            ACObj(Iobj).insertCol(DistancesGlade, Inf, GladeDistCol);
            ACObj(Iobj).insertCol(MatchesPGC, Inf, PGCNCol);
            ACObj(Iobj).insertCol(DistancesPGC, Inf, PGCDistCol);
        end        

    end

end