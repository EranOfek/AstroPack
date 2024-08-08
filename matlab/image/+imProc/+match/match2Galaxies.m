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
        Args.RadiusPGC = 300;
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
            disp('hello2');
        otherwise
            warning('Object class not supported.')
    end

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

        % Rough match is final match for GLADE
        imProc.match.match_catsHTM(ACObj(Iobj), Args.GladeCatName,...
            'ColDistName', GladeDistCol,...
            'ColNmatchName', GladeNCol, ...
            'Radius', Args.RadiusGlade, 'RadiusUnits', Args.RadiusGladeUnits);

        % PGC matches will be refined for roughly matched entries
        % this considers extension of galaxies
        imProc.match.match_catsHTM(ACObj(Iobj), Args.PGCCatName,...
            'ColDistName', PGCDistCol,...
            'ColNmatchName', PGCNCol, ...
            'Radius', Args.RadiusPGC, 'RadiusUnits', Args.RadiusPGCUnits);
        
        Matches = ACObj(Iobj).getCol(PGCNCol);
        Distances = ACObj(Iobj).getCol(PGCDistCol);

        % Skip entries that have no rough PGC matches
        if ~any(Matches>0)
            if Args.MergeCols
                ACObj(Iobj).deleteCol({PGCNCol,PGCDistCol});
                ACObj(Iobj).replaceColNames({GladeNCol,GladeDistCol},...
                    {Args.ColNmatchName,Args.ColDistName});
            end
            continue
        end

        RADec = ACObj(Iobj).getLonLat('rad');

        RA = RADec(:,1);
        Dec = RADec(:,2);

        for Itran = 1:1:CatSize

            if Matches(Itran) < 1
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
                Matches(Itran) = sum(Dist<GalRadius);
                if isempty(Dist) || ~any(Dist<GalRadius)
                    Distances(Itran) = NaN;
                end
            end 

        end

        % Replace catalog entries with refined matches.
        ACObj(Iobj).replaceCol(Matches, PGCNCol);
        ACObj(Iobj).replaceCol(Distances, PGCDistCol);

        % If GLADE and PGC results should be merged, take the sum for
        % number of matches and the minimum between matched distances.
        if Args.MergeCols
            MatchCol = ACObj(Iobj).getCol(GladeNCol)+...
                ACObj(Iobj).getCol(PGCNCol);
            ACObj(Iobj).insertCol(MatchCol, GladeNCol, Args.ColNmatchName);
            ACObj(Iobj).deleteCol(GladeNCol);
            ACObj(Iobj).deleteCol(PGCNCol);
            
            DistCol = min(ACObj(Iobj).getCol(GladeDistCol),...
                ACObj(Iobj).getCol(PGCDistCol));
            ACObj(Iobj).insertCol(DistCol, GladeDistCol, Args.ColDistName);
            ACObj(Iobj).deleteCol(PGCDistCol);
            ACObj(Iobj).deleteCol(GladeDistCol);
        end

    end

end