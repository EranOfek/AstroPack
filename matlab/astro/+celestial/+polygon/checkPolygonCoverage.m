function [Sel, AllCovered, CoverageAll] = checkPolygonCoverage(RefLon, RefLat, PolysLon, PolysLat, Args)
    % Check whether a set of crop polygons provides sufficient sky coverage of a reference polygon
    %   Two algorithms are available (Args.Algo):
    %     'raster' - rasterize the reference polygon and the union of the overlapping crops, and
    %            compare the two rasters. Tolerant of mutually overlapping input crops.
    %     'area'   - sum the exact spherical intersection area of each crop with the reference
    %            polygon (celestial.polygon.areaPolyIntersection), and compare the sum to the
    %            reference polygon's area. The input crop polygons must already be mutually
    %            non-overlapping (e.g., per-crop unique-region corners) for the sum not to
    %            double-count shared sky area; any residual mutual overlap between the input
    %            crops is silently excluded from the coverage sum rather than double-counted.
    % Input  : - Vector of the reference polygon (P0) vertex longitudes [deg].
    %          - Vector of the reference polygon (P0) vertex latitudes [deg].
    %          - Matrix of crop-polygon vertex longitudes [deg], one polygon per column.
    %          - Matrix of crop-polygon vertex latitudes [deg], one polygon per column.
    %          * ...,key,val,...
    %            'Algo' - 'raster' (def.) | 'area', see above.
    %            'MinCoverage' - minimum fractional coverage of the reference polygon required.
    %                   Default is 0.999.
    %            'RasterResolution' - polygon rasterization step, in arcsec ('raster' algo only).
    %                   Default is 3.
    %            'Raster0' - a precomputed raster of the reference polygon (e.g., from
    %                   celestial.healpix.pixCoversPolygon), reused as-is if supplied ('raster' algo
    %                   only); if empty, rasterized here from RefLon/RefLat. Default is [].
    % Output : - Logical column vector selecting the input crop-polygon columns to keep: for 'raster',
    %            the crops overlapping the reference polygon at all; for 'area', the crops with a
    %            positive intersection area with the reference polygon.
    %          - AllCovered: true if the kept crops cover at least MinCoverage of the reference polygon.
    %          - CoverageAll: the achieved fractional coverage of the reference polygon.
    % Author : A.M. Krassilchtchikov (2026 Jul)
    % Example: [Sel, AllCovered, CoverageAll] = celestial.polygon.checkPolygonCoverage(P0(:,1), P0(:,2), CropsLon, CropsLat, 'Algo','area');
    arguments
        RefLon
        RefLat
        PolysLon
        PolysLat
        Args.Algo             = 'raster';
        Args.MinCoverage      = 0.999;
        Args.RasterResolution = 3;
        Args.Raster0          = [];
    end

    switch lower(Args.Algo)
        case 'raster'
            % deselect crops that do not overlap with the reference region at all;
            % a single vectorized call replaces rasterizing every candidate crop just to test overlap
            % NB: PolysLon/PolysLat must not be empty here -- isSpherePolyIntersect_mex crashes (rather
            % than erroring) on an empty candidate set
            Sel = celestial.polygon.isSpherePolyIntersect(RefLon(:), RefLat(:), double(PolysLon), double(PolysLat));

            if isempty(Args.Raster0)
                Raster0 = celestial.healpix.mex.rasterize_polygon([RefLon(:), RefLat(:)], Args.RasterResolution, 'arcsec');
            else
                Raster0 = Args.Raster0;
            end

            % the surviving, overlapping crops are rasterized and their union compared to Raster0,
            % since the total coverage of several possibly mutually-overlapping crops cannot be
            % obtained from their individual overlap areas with the reference region alone
            RasterC = [];
            for Icrop = find(Sel(:)).'
                CropPoly = double([PolysLon(:,Icrop), PolysLat(:,Icrop)]);
                Raster   = celestial.healpix.mex.rasterize_polygon(CropPoly, Args.RasterResolution, 'arcsec');
                RasterC  = [RasterC; Raster(~ismember(Raster,RasterC))];
            end

            CoverageAll = sum(ismember(Raster0, RasterC))/numel(Raster0);

        case 'area'
            % sum the exact intersection areas of the (assumed mutually non-overlapping) crop
            % polygons with the reference polygon, and compare the sum to the reference polygon's area
            [Area, AreaRefPoly] = celestial.polygon.areaPolyIntersection(RefLon, RefLat, PolysLon, PolysLat, 'CooUnits','deg');
            Sel = Area(:) > 0;
            CoverageAll = sum(Area(Sel))/AreaRefPoly;

        otherwise
            error('checkPolygonCoverage:UnknownAlgo', 'Unknown Args.Algo: %s', Args.Algo);
    end

    AllCovered = CoverageAll >= Args.MinCoverage;
end
