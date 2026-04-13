function [Result1, Result2] = matchInd(Obj1, Obj2, Args)
    % Match catalogs by coordinates and return matched indices.
    % Package: imProc.match
    % Description: Match sources stored in AstroCatalog/AstroImage/
    %              AstroDiff/AstroZOGY objects and return, for each source,
    %              the index of the nearest matched source within a search
    %              radius. Matching can be performed either in spherical
    %              coordinates (e.g., RA/Dec) or in planar coordinates
    %              (e.g., X/Y).
    %              The function uses the fast MEX binary-search functions:
    %              imUtil.match.mex.matchCatalogs
    %              imUtil.match.mex.matchCatalogsXY
    %
    % Input  : - Obj1 : An AstroCatalog/AstroImage/AstroDiff/AstroZOGY
    %                   object array. Each element must contain a source
    %                   catalog.
    %          - Obj2 : A **sorted** AstroCatalog/AstroImage/AstroDiff/AstroZOGY
    %                   object array. Each element must contain a source
    %                   catalog.
    %                   If numel(Obj1)==numel(Obj2), catalogs are matched
    %                   element by element.
    %                   If one of the inputs is scalar, then that object is
    %                   matched against all elements of the other input.
    %          * ...,key,val,...
    %            'SearchRadius'      - Search radius. If IsSpherical is
    %                                  false, then this is in planar units
    %                                  (typically pixels). If IsSpherical
    %                                  is true, then this is in the angular
    %                                  units specified by
    %                                  SearchRadiusUnits.
    %                                  Default is 1.5.
    %            'SearchRadiusUnits' - Units of SearchRadius when
    %                                  IsSpherical=true.
    %                                  Default is 'arcsec'.
    %            'IsSpherical'       - Logical indicating whether to use
    %                                  spherical coordinates (true) or
    %                                  planar coordinates (false).
    %                                  Default is true.
    %            'Units'             - Optional override for the spherical
    %                                  coordinate units of both catalogs.
    %                                  For example: 'rad' or 'deg'.
    %                                  If empty, units are taken from the
    %                                  catalog metadata.
    %                                  Default is [].
    %            'ColSphere1'        - Two-element cell array containing the
    %                                  spherical coordinate column names in
    %                                  Obj1.
    %                                  Default is {'RA','Dec'}.
    %            'ColSphere2'        - Two-element cell array containing the
    %                                  spherical coordinate column names in
    %                                  Obj2.
    %                                  Default is {'RA','Dec'}.
    %            'ColPlanar1'        - Two-element cell array containing the
    %                                  planar coordinate column names in
    %                                  Obj1.
    %                                  Default is {'X','Y'}.
    %            'ColPlanar2'        - Two-element cell array containing the
    %                                  planar coordinate column names in
    %                                  Obj2.
    %                                  Default is {'X','Y'}.
    %            'Sort1'             - Sort catalog 1 before matching.
    %                                  Default is false.
    %            'Sort2'             - Sort catalog 2 before matching.
    %                                  Default is false.
    %            'SortCol'           - Column index in 'ColSPhere#'
    %                                  'ColPlanar#' by which to sort.
    %                                  Default is 2.
    %            'TestSort2'         - Test that catalog 2 is sorted as
    %                                  required by the MEX matcher.
    %                                  Default is false.
    %
    % Output : - Result1 : Structure array of length max(numel(Obj1),
    %                      numel(Obj2)). Each element contains the matching
    %                      results for Obj1 sources against Obj2:
    %                      .Ind    - Column vector of indices of the matched
    %                                sources in Obj2, one entry per row in
    %                                the corresponding Obj1 catalog.
    %                      .Dist   - Column vector of distances to the
    %                                matched sources.
    %                      .Nmatch - Column vector containing the number of
    %                                matches found within the search radius
    %                                for each Obj1 source.
    %          - Result2 : Same as Result1, but for the reverse direction
    %                      (Obj2 matched against Obj1). Returned only if a
    %                      second output argument is requested.
    %
    % Notes  : - For best performance, catalog 2 is typically expected to
    %            be sorted by Dec (spherical mode) or Y (planar mode),
    %            unless the MEX code is instructed to test sorting.
    %          - In spherical mode, coordinates are converted internally to
    %            radians before calling the MEX matcher.
    %          - The exact convention for unmatched sources and the units of
    %            Dist depend on the underlying MEX implementation and should
    %            be kept consistent with imUtil.match.mex.matchCatalogs and
    %            imUtil.match.mex.matchCatalogsXY.
    %
    % Author : Eran Ofek (2026 Apr)
    % Example: [Result1, Result2] = imProc.match.matchInd(Obj1, Obj2);

    arguments
        Obj1
        Obj2
        Args.SearchRadius      = 1.5;
        Args.SearchRadiusUnits = 'arcsec';
        Args.IsSpherical       = true;
        Args.Units             = []; % char array
        Args.ColSphere1        = {'RA','Dec'};
        Args.ColSphere2        = {'RA','Dec'};
        Args.ColPlanar1        = {'X','Y'};
        Args.ColPlanar2        = {'X','Y'};
        Args.Sort1             = false;
        Args.Sort2             = false;
        Args.SortCol           = 2;
        Args.TestSort2         = false;  % test that list 2 is sorted
    end
    RAD = 180./pi;

    Nobj1 = numel(Obj1);
    Nobj2 = numel(Obj2);
    Nmax  = max(Nobj1, Nobj2);

    if ~(Nobj1==Nobj2 || Nobj1==1 || Nobj2==1)
        error('Obj1 and Obj2 must have equal number of elements, or one input must be scalar.');
    end

    if Args.IsSpherical
        % convert to radians
        SearchRadius = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius);
    else
        SearchRadius = Args.SearchRadius;
    end

    Result1 = struct('Ind',cell(Nmax,1), 'Dist',cell(Nmax,1), 'Nmatch',cell(Nmax,1));
    if nargout>1
        Result2 = struct('Ind',cell(Nmax,1), 'Dist',cell(Nmax,1), 'Nmatch',cell(Nmax,1));
    end
    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);

        % get catalogs : AstroCatalog / AstroImage / ...
        Cat1  = Obj1(Iobj1).getCatData;
        Cat2  = Obj2(Iobj2).getCatData;

       
        if Args.IsSpherical
            % sort
            if Args.Sort1
                Cat1 = Cat1.sortrows(Args.ColSphere1{Args.SortCol});
            end
            if Args.Sort2
                Cat2 = Cat2.sortrows(Args.ColSphere2{Args.SortCol});
            end

            % get columns
            [Coo1, Units1] = Cat1.getCol(Args.ColSphere1);
            [Coo2, Units2] = Cat2.getCol(Args.ColSphere2);
            if isempty(Args.Units)
                % get units from table
                Units1 = Units1{1};
                Units2 = Units2{1};
            else
                Units1 = Args.Units;
                Units2 = Args.Units;
            end
            % convert to radians
            if strcmp(Units1, 'deg')
                Coo1 = Coo1./RAD;
            end
            if strcmp(Units2, 'deg')
                Coo2 = Coo2./RAD;
            end

            % assumption: Coo2 is sorted by Dec
            if nargout<2
                [Result1(Imax).Ind, Result1(Imax).Dist, Result1(Imax).Nmatch] = imUtil.match.mex.matchCatalogs(Coo1(:,1), Coo1(:,2), Coo2(:,1), Coo2(:,2), SearchRadius, false, [], [], Args.TestSort2);
            else
                % cross indexing
                [Result1(Imax).Ind, Result1(Imax).Dist, Result1(Imax).Nmatch, ...
                 Result2(Imax).Ind, Result2(Imax).Dist, Result2(Imax).Nmatch] = imUtil.match.mex.matchCatalogs(Coo1(:,1), Coo1(:,2), ...
                                                                                                               Coo2(:,1), Coo2(:,2), ...
                                                                                                               SearchRadius, false, [], [], Args.TestSort2);
                
            end


        else % if Args.IsSpherical
            % sort
            if Args.Sort1
                Cat1 = Cat1.sortrows(Args.ColPlanar1{Args.SortCol});
            end
            if Args.Sort2
                Cat2 = Cat2.sortrows(Args.ColPlanar2{Args.SortCol});
            end

            [Coo1] = Cat1.getCol(Args.ColPlanar1);
            [Coo2] = Cat2.getCol(Args.ColPlanar2);

            % assumption: Coo2 is sorted by Dec
            if nargout<2
                [Result1(Imax).Ind, Result1(Imax).Dist, Result1(Imax).Nmatch] = imUtil.match.mex.matchCatalogsXY(Coo1(:,1), Coo1(:,2), Coo2(:,1), Coo2(:,2), SearchRadius, false, [], [], Args.TestSort2);
            else
                % cross indexing
                [Result1(Imax).Ind, Result1(Imax).Dist, Result1(Imax).Nmatch, ...
                 Result2(Imax).Ind, Result2(Imax).Dist, Result2(Imax).Nmatch] = imUtil.match.mex.matchCatalogsXY(Coo1(:,1), Coo1(:,2), ...
                                                                                                               Coo2(:,1), Coo2(:,2), ...
                                                                                                               SearchRadius, false, [], [], Args.TestSort2);
                
            end


        end % if Args.IsSpherical

    end

end
