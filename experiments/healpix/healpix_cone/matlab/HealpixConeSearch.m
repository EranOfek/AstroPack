% ***************************************************************************
% Project     : AstroPack
% Filename    : HealpixConeSearch.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Cone-search -> ClickHouse SQL generator using HEALPix NESTED ordering.
%               MATLAB port of healpix_cone_search.py (Python is source of truth).
% ***************************************************************************
classdef HealpixConeSearch
    % HealpixConeSearch  Cone-search -> ClickHouse SQL generator (NESTED ordering).
    %
    %   Assumptions
    %   -----------
    %   * Sky positions stored as HEALPix NESTED pixel index at **level 16**
    %     (NSide = 2**16 = 65 536, pixel size ~ 3.2 arcsec).
    %   * The healpix column in ClickHouse is the 64-bit unsigned integer pixel id.
    %
    %   Python -> MATLAB name mapping (for easy adaptation when Python changes):
    %   -----------------------------------------------------------------------
    %   healpix_cone_search.py          -> HealpixConeSearch.m
    %   cone_to_pixel_ranges            -> coneToPixelRanges
    %   cone_search_sql                 -> coneSearchSql
    %   cone_search_sql_full            -> coneSearchSqlFull
    %   _best_nside_for_radius          -> bestNsideForRadius
    %   _pixels_to_ranges               -> pixelsToRanges
    %   _direction_cosines              -> directionCosines
    %   Algo                            -> Algo
    %   PixelRanges                     -> PixelRanges
    %   get_backend                     -> Backend.getBackend
    %
    %   Usage
    %   -----
    %       [Sql, Post] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
    %           'proc_src', 'upix_high', Algo.CONE);
    %       disp(Sql);
    %       disp(Post);   % optional exact-distance post-filter fragment

    properties (Constant)
        HEALPIX_LEVEL_CAT = 16
        NSIDE_CAT         = 2^16   % 65 536  (pixel size ~ 3.2 arcsec)
        MAX_PIX_ID        = int64(12) * int64(2^16)^2 - 1   % 51 539 607 551 — requires UInt64
    end

    methods (Static)

        function Nside = bestNsideForRadius(RadiusDeg, Mode)
            % bestNsideForRadius  Choose the best NSide (power of 2) for a given search radius.
            %
            %   Three modes (all cap at NSIDE_CAT):
            %
            %   "conservative"  [DEFAULT - Eran's preference]
            %       pixel_size ~ 1/NSide [rad]. We want pixel_size >= radius, so:
            %           NSide <= 1/radius_rad
            %       Result: deliberately coarse - pixel bigger than cone, so centre+
            %       neighbours is guaranteed to fully envelope the search area.
            %
            %   "area"
            %       Match pixel area to cone area:
            %           pixel area = pi/(3*NSide^2), cone area = pi*r^2
            %           Set equal -> NSide = 1/(sqrt(3)*r)
            %
            %   "circumradius"
            %       pixel circumradius ~ sqrt(3)/NSide [rad]. Tightest coverage:
            %           NSide <= sqrt(3)/radius_rad
            %
            %   Input  : - Search radius in degrees (> 0).
            %          - Mode: "conservative" | "area" | "circumradius".
            %   Output : - NSide value (power of 2, 1 ... NSIDE_CAT).

            arguments
                RadiusDeg (1,1) double {mustBePositive}
                Mode (1,1) string = "conservative"
            end

            RadiusRad = deg2rad(RadiusDeg);

            switch Mode
                case "conservative"
                    % pixel edge ~ 1/NSide rad; want edge >= radius -> NSide <= 1/radius
                    Ideal = 1.0 / RadiusRad;
                case "area"
                    % equate HEALPix pixel area (pi/3N^2) with cone area (pi*r^2) -> N = 1/(sqrt(3)*r)
                    Ideal = 1.0 / (sqrt(3.0) * RadiusRad);
                case "circumradius"
                    % pixel circumradius ~ sqrt(3)/NSide; want circumradius >= radius -> NSide <= sqrt(3)/r
                    Ideal = sqrt(3.0) / RadiusRad;
                otherwise
                    error('HealpixConeSearch:UnknownMode', ...
                        'Unknown mode ''%s''. Choose ''conservative'', ''area'', or ''circumradius''.', Mode);
            end

            % Round *down* to the nearest power of 2 so the pixel is never smaller than
            % the cone (which would leave gaps between neighbours)
            Level = floor(log2(Ideal));
            Level = max(Level, 0);               % guard against radius > 1 radian (huge)
            Nside = min(2^Level, HealpixConeSearch.NSIDE_CAT);  % never exceed the catalogue resolution
            Nside = int32(Nside);
        end

        function Ranges = pixelsToRanges(Pixels, NSideSearch)
            % pixelsToRanges  Expand low-NSide nested pixels to ranges at NSIDE_CAT, then merge.
            %
            %   In NESTED ordering every child of a parent pixel occupies a contiguous
            %   block of IDs:
            %       lo = parent_pix * nchild
            %       hi = lo + nchild - 1
            %   where nchild = (NSIDE_CAT / nside_search) ** 2.

            % Each low-resolution pixel contains (factor x factor) high-res pixels.
            % This works because NESTED ordering preserves spatial locality by
            % construction — a pixel's children always form a contiguous ID block.
            Factor = HealpixConeSearch.NSIDE_CAT / NSideSearch;   % integer, power of 2
            Nchild = Factor * Factor;                             % fine pixels per coarse pixel

            Pixels = unique(int64(Pixels(:)));   % deduplicate + sort

            % Compute the first and last fine-pixel ID for each coarse pixel
            LoArr = Pixels * Nchild;
            HiArr = LoArr + Nchild - 1;

            % Sort by lo so we can do a single linear-time merge pass
            [LoArr, Order] = sort(LoArr);
            HiArr = HiArr(Order);

            Merged = zeros(0, 2, 'int64');
            CurLo = LoArr(1);
            CurHi = HiArr(1);

            for I = 2:numel(LoArr)
                Lo = LoArr(I);
                Hi = HiArr(I);
                % Adjacent ranges differ by 1 (contiguous); overlapping ranges have lo <= cur_hi.
                % Both cases collapse into a single wider range.
                if Lo <= CurHi + 1        % contiguous or overlapping -> merge
                    CurHi = max(CurHi, Hi);
                else
                    Merged(end+1, :) = [CurLo, CurHi]; %#ok<AGROW>
                    CurLo = Lo;
                    CurHi = Hi;
                end
            end
            Merged(end+1, :) = [CurLo, CurHi];   % flush the last open range

            Ranges = Merged;
        end

        function Pr = coneToPixelRanges(RaDeg, DecDeg, RadiusDeg, AlgoVal, NSideMode)
            % coneToPixelRanges  Convert a cone search (ra, dec, radius) to pixel ranges at level 16.
            %
            %   Input  : - Right ascension in degrees [0, 360).
            %          - Declination in degrees [-90, 90].
            %          - Search radius in degrees > 0.
            %          - Algo.CONE (fewer ranges) or Algo.NEIGHBOR (always <= 9).
            %          - NSide selection strategy:
            %              "conservative" (default, Eran's preference) - coarser,
            %                  never miss anything.
            %              "area"         - coarsest, matches cone area to pixel area.
            %              "circumradius" - finest, tightest coverage.
            %   Output : - PixelRanges object with .Ranges list of [lo, hi] rows.

            arguments
                RaDeg (1,1) double
                DecDeg (1,1) double
                RadiusDeg (1,1) double
                AlgoVal (1,1) Algo = Algo.CONE
                NSideMode (1,1) string = "conservative"
            end

            if RaDeg < 0.0 || RaDeg >= 360.0
                error('HealpixConeSearch:InvalidRa', ...
                    'ra_deg must be in [0, 360), got %g', RaDeg);
            end
            if DecDeg < -90.0 || DecDeg > 90.0
                error('HealpixConeSearch:InvalidDec', ...
                    'dec_deg must be in [-90, 90], got %g', DecDeg);
            end
            if RadiusDeg <= 0
                error('HealpixConeSearch:InvalidRadius', ...
                    'radius_deg must be > 0, got %g', RadiusDeg);
            end

            Backend = Backend.getBackend();
            % Choose a coarse NSide whose pixel size matches (or exceeds) the search radius.
            % Searching at this coarser resolution first dramatically reduces the number
            % of pixels we need to expand into fine-level ranges.
            NSideSearch = HealpixConeSearch.bestNsideForRadius(RadiusDeg, NSideMode);

            if AlgoVal == Algo.NEIGHBOR
                % Identify the central coarse pixel, then grab its 8 neighbours.
                % The 3x3 block is guaranteed to contain the full cone when nside_mode
                % is "conservative" (pixel size >= cone radius).
                CenterPix = Backend.ang2pixNested(NSideSearch, RaDeg, DecDeg);
                PixList = Backend.neighboursNested(NSideSearch, CenterPix);

            elseif AlgoVal == Algo.CONE
                % Ask the backend to find all coarse pixels whose centres are inside
                % the search cone.  This is tighter than the 3x3 NEIGHBOR block,
                % especially for small radii near pixel boundaries.
                PixList = Backend.queryDiscNested(NSideSearch, RaDeg, DecDeg, RadiusDeg);
                if isempty(PixList)
                    % The cone is smaller than a single coarse pixel — no centres fall
                    % inside it.  Fall back to the one pixel that contains the point.
                    CenterPix = Backend.ang2pixNested(NSideSearch, RaDeg, DecDeg);
                    PixList = int64(CenterPix);
                end
            else
                error('HealpixConeSearch:UnknownAlgo', 'Unknown algo: %s', char(AlgoVal));
            end

            % Expand each coarse pixel into a contiguous block of fine-level IDs,
            % then merge adjacent blocks to minimise the number of SQL BETWEEN clauses.
            Ranges = HealpixConeSearch.pixelsToRanges(PixList, NSideSearch);

            Pr = PixelRanges(Ranges, NSideSearch, AlgoVal, numel(PixList));
        end

        function [Cx, Cy, Cz] = directionCosines(RaDeg, DecDeg)
            % directionCosines  Unit vector from sphere centre toward (RA, Dec).
            % Used in the dot-product post-filter: cos(angle) = cx*CX + cy*CY + cz*CZ.
            Ra  = deg2rad(RaDeg);
            Dec = deg2rad(DecDeg);
            Cx  = cos(Dec) * cos(Ra);
            Cy  = cos(Dec) * sin(Ra);
            Cz  = sin(Dec);
        end

        function [Sql, Pf] = coneSearchSql(RaDeg, DecDeg, RadiusDeg, Table, Column, Args)
            % coneSearchSql  Generate a ClickHouse SELECT statement for a cone search.
            %
            %   Input  : - ra_deg, dec_deg, radius_deg : cone centre and radius (degrees).
            %          - table, column : ClickHouse table and healpix column name.
            %          * ...,key,val,...
            %            'Algo'            - Algo.CONE or Algo.NEIGHBOR.
            %            'ExtraColumns'    - columns to select (default "*").
            %            'PostFilter'      - if true, also return a post-filter fragment.
            %            'PostFilterMode'  - "cosine" | "greatcircle".
            %            'CxCol','CyCol','CzCol' - direction-cosine column names (cosine mode).
            %            'RaCol','DecCol'  - RA/Dec column names (greatcircle mode).
            %   Output : - (sql_ranges_only, post_filter_fragment_or_empty)
            %
            %   sql_ranges_only      : complete SELECT using only healpix range filters.
            %   post_filter_fragment : WHERE clause fragment for exact distance check.

            arguments
                RaDeg (1,1) double
                DecDeg (1,1) double
                RadiusDeg (1,1) double {mustBePositive}
                Table (1,1) string
                Column (1,1) string
                Args.Algo (1,1) Algo = Algo.CONE
                Args.ExtraColumns (1,1) string = "*"
                Args.PostFilter (1,1) logical = true
                Args.PostFilterMode (1,1) string = "cosine"
                Args.CxCol (1,1) string = "cx"
                Args.CyCol (1,1) string = "cy"
                Args.CzCol (1,1) string = "cz"
                Args.RaCol (1,1) string = "ra"
                Args.DecCol (1,1) string = "dec"
            end

            Pr = HealpixConeSearch.coneToPixelRanges(RaDeg, DecDeg, RadiusDeg, Args.Algo);

            % Build one "col BETWEEN lo AND hi" clause per pixel range.
            % SQL BETWEEN is inclusive on both ends, matching our [lo, hi] convention.
            RangeClauses = cell(Pr.NRanges, 1);
            for I = 1:Pr.NRanges
                Lo = Pr.Ranges(I, 1);
                Hi = Pr.Ranges(I, 2);
                RangeClauses{I} = sprintf('(%s BETWEEN %d AND %d)', Column, Lo, Hi);
            end
            % Join multiple ranges with OR — ClickHouse will evaluate these efficiently
            % against any sorted / index-structured MergeTree table.
            WhereHealpix = strjoin(RangeClauses, newline + "   OR ");

            Sql = sprintf(['SELECT %s\n' ...
                           'FROM %s\n' ...
                           'WHERE (\n' ...
                           '   %s\n' ...
                           ')'], Args.ExtraColumns, Table, WhereHealpix);

            % --- post-filter fragment ---
            % The healpix ranges over-approximate the cone (pixel granularity).
            % A post-filter removes the false positives and delivers an exact result.
            Pf = '';
            if Args.PostFilter
                RRad = deg2rad(RadiusDeg);

                switch Args.PostFilterMode
                    case "cosine"
                        % Dot-product test: point P is inside cone C if
                        %   P · C >= cos(radius)
                        % All three values are precomputed in MATLAB; ClickHouse only does
                        % arithmetic — no trig — which is faster than greatCircleAngle().
                        [Cx, Cy, Cz] = HealpixConeSearch.directionCosines(RaDeg, DecDeg);
                        CosR = cos(RRad);
                        Pf = sprintf([ ...
                            '-- Exact cone post-filter (dot product, no trig at query time)\n' ...
                            '-- Add this to the WHERE clause of the healpix range query:\n' ...
                            'AND (%s * %.17g + %s * %.17g + %s * %.17g >= %.17g)'], ...
                            Args.CxCol, Cx, Args.CyCol, Cy, Args.CzCol, Cz, CosR);

                    case "greatcircle"
                        % ClickHouse built-in great-circle distance function.
                        % More readable than the cosine form but requires a trig call per row.
                        Pf = sprintf([ ...
                            '-- Exact cone post-filter (ClickHouse greatCircleAngle)\n' ...
                            '-- Add this to the WHERE clause of the healpix range query:\n' ...
                            'AND (greatCircleAngle(%s, %s, %.10g, %.10g) <= %.10g)'], ...
                            Args.RaCol, Args.DecCol, RaDeg, DecDeg, RadiusDeg);

                    otherwise
                        error('HealpixConeSearch:UnknownPostFilterMode', ...
                            'Unknown post_filter_mode: %s', Args.PostFilterMode);
                end
            end
        end

        function SqlFull = coneSearchSqlFull(RaDeg, DecDeg, RadiusDeg, Table, Column, Args)
            % coneSearchSqlFull  Return a single SQL string with both healpix ranges AND post-filter.

            arguments
                RaDeg (1,1) double
                DecDeg (1,1) double
                RadiusDeg (1,1) double {mustBePositive}
                Table (1,1) string
                Column (1,1) string
                Args.Algo (1,1) Algo = Algo.CONE
                Args.ExtraColumns (1,1) string = "*"
                Args.PostFilterMode (1,1) string = "cosine"
                Args.CxCol (1,1) string = "cx"
                Args.CyCol (1,1) string = "cy"
                Args.CzCol (1,1) string = "cz"
                Args.RaCol (1,1) string = "ra"
                Args.DecCol (1,1) string = "dec"
            end

            [Sql, Pf] = HealpixConeSearch.coneSearchSql(RaDeg, DecDeg, RadiusDeg, ...
                Table, Column, ...
                'Algo', Args.Algo, ...
                'ExtraColumns', Args.ExtraColumns, ...
                'PostFilter', true, ...
                'PostFilterMode', Args.PostFilterMode, ...
                'CxCol', Args.CxCol, 'CyCol', Args.CyCol, 'CzCol', Args.CzCol, ...
                'RaCol', Args.RaCol, 'DecCol', Args.DecCol);

            % Strip the human-readable comment lines from pf, keeping only the AND
            % clause so the result is a valid self-contained SQL statement.
            AndLine = '';
            if ~isempty(Pf)
                Lines = splitlines(Pf);
                AndLines = Lines(startsWith(strtrim(Lines), 'AND'));
                AndLine = strjoin(AndLines, newline);
            end
            SqlFull = strtrim(Sql) + newline + AndLine;
        end
    end
end
