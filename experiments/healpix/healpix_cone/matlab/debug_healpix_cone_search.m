% ***************************************************************************
% Project     : AstroPack
% Filename    : debug_healpix_cone_search.m
% Author      : Chen Tishler
% Created     : 17/06/2026
% Modified    : 17/06/2026
% Description : Debug helpers for HealpixConeSearch.m (mirrors debug_healpix_cone_search.py).
% ***************************************************************************
function debug_healpix_cone_search()
    % debug_healpix_cone_search  Master debug — calls all debug_* functions.
    %
    %   Run:
    %       debug_healpix_cone_search()
    %
    %   Debug functions:
    %       debugBestNside()                  % best nside for radius
    %       debugPixelRangesNeighbor()        % NEIGHBOR algo
    %       debugPixelRangesCone()            % CONE algo
    %       debugCompareAlgos()               % NEIGHBOR vs CONE comparison
    %       debugSqlOutput()                  % SQL output
    %       debugEdgeCases()                  % edge cases
    %       debugPixelIdBounds()              % pixel ID bounds check

    B = Backend.getBackend();
    fprintf('\nBackend: %s\n', B.Name);
    debugBestNside();
    debugPixelRangesNeighbor();
    debugPixelRangesCone();
    debugCompareAlgos();
    debugSqlOutput();
    debugEdgeCases();
    debugPixelIdBounds();
end

% ============================================================================
%                                 Debug Functions
% ============================================================================

function debugBestNside()
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: bestNsideForRadius() — all three modes\n');
    fprintf('%s\n', repmat('=', 1, 60));

    Cases = {
        'tiny  0.001°',  0.001;
        'small 0.1°',    0.1;
        '1 arcmin',      1/60;
        'typical 1°',    1.0;
        'large 5°',      5.0;
        'huge  45°',    45.0;
    };
    Modes = ["conservative", "area", "circumradius"];

    Header = sprintf('  %-16s', 'radius');
    for M = Modes
        Header = Header + sprintf('%20s', 'NSide_' + M);
    end
    fprintf('%s\n', Header);
    fprintf('  %s\n', repmat('-', 1, 16 + 20 * numel(Modes)));

    for I = 1:size(Cases, 1)
        Label = Cases{I, 1};
        R = Cases{I, 2};
        Row = sprintf('  %-16s', Label);
        for M = Modes
            Ns = HealpixConeSearch.bestNsideForRadius(R, M);
            % pixel circumradius in degrees: sqrt(3)/NSide radians converted to degrees.
            % This is the angular distance from pixel centre to corner — the worst-case
            % separation between a point and its pixel boundary.
            PixDeg = rad2deg(sqrt(3) / Ns);
            Row = Row + sprintf('  %6d (%.3f°)', Ns, PixDeg);
        end
        fprintf('%s\n', Row);
    end

    fprintf('\n');
    fprintf('  NOTE: ''conservative'' = Eran''s preferred formula (1/r)\n');
    fprintf('        ''area''         = area-matching (1/(sqrt(3)*r))  — coarsest\n');
    fprintf('        ''circumradius'' = circumradius  (sqrt(3)/r)      — finest\n');
end

function debugPixelRangesNeighbor()
    % NEIGHBOR algo: find the coarse pixel that contains the point, grab its
    % 8 spatial neighbours, expand all 9 to fine-level ID ranges.
    % Always produces at most 9 ranges regardless of radius or position.
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: NEIGHBOR algo — RA=254 Dec=64 R=1°\n');
    fprintf('%s\n', repmat('=', 1, 60));
    Pr = HealpixConeSearch.coneToPixelRanges(254.0, 64.0, 1.0, Algo.NEIGHBOR);
    disp(Pr);
end

function debugPixelRangesCone()
    % CONE algo: find all coarse pixels whose centres fall inside the search
    % cone, then expand to fine-level ranges.  Typically returns fewer ranges
    % than NEIGHBOR because pixels outside the cone are excluded.
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: CONE algo — RA=254 Dec=64 R=1°\n');
    fprintf('%s\n', repmat('=', 1, 60));
    Pr = HealpixConeSearch.coneToPixelRanges(254.0, 64.0, 1.0, Algo.CONE);
    disp(Pr);
end

function debugCompareAlgos()
    % Side-by-side comparison of NEIGHBOR vs CONE across representative sky
    % positions and radii.  CONE should always return a smaller TotalPix count
    % (fewer false positives) at the cost of a slightly more complex range list.
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: NEIGHBOR vs CONE comparison\n');
    fprintf('%s\n', repmat('=', 1, 60));

    TestCases = {
        0.0,   0.0,  1.0,  'equator';
        254.0, 64.0, 1.0,  'Sasha example';
        180.0, 89.0, 0.5,  'near north pole';
        180.0,-89.0, 0.5,  'near south pole';
        0.0,   0.0,  0.01, 'tiny radius';
        45.0,  30.0, 5.0,  'large radius';
    };

    fprintf('  %-20s %-10s %7s %10s %8s %10s\n', ...
        'Case', 'Algo', 'NSideS', '#SearchPix', '#Ranges', 'TotalPix');
    fprintf('  %s\n', repmat('-', 1, 70));

    for I = 1:size(TestCases, 1)
        Ra = TestCases{I, 1};
        Dec = TestCases{I, 2};
        R = TestCases{I, 3};
        Label = TestCases{I, 4};

        for AlgoVal = [Algo.NEIGHBOR, Algo.CONE]
            Pr = HealpixConeSearch.coneToPixelRanges(Ra, Dec, R, AlgoVal);
            % TotalPix = total number of fine-level pixels covered across all ranges.
            % Lower is better: fewer pixels means fewer false-positive catalogue rows
            % that the post-filter must discard.
            Total = sum(Pr.Ranges(:, 2) - Pr.Ranges(:, 1) + 1);
            fprintf('  %-20s %-10s %7d %10d %8d %10d\n', ...
                Label, char(AlgoVal), Pr.NSideSearch, Pr.NSearchPixels, ...
                Pr.NRanges, Total);
        end
        fprintf('\n');
    end
end

function debugSqlOutput()
    % Show the full ClickHouse SQL produced by coneSearchSql() for both algos
    % and both post-filter modes.  The SQL has two layers:
    %   1. Healpix BETWEEN ranges  — fast index scan, may include false positives
    %   2. Post-filter AND clause  — exact distance check to remove false positives
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: SQL output — RA=254 Dec=64 R=1°\n');
    fprintf('%s\n', repmat('=', 1, 60));

    for AlgoVal = [Algo.CONE, Algo.NEIGHBOR]
        fprintf('\n--- algo=%s ---\n', char(AlgoVal));
        [Sql, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
            'proc_src', 'upix_high', ...
            'Algo', AlgoVal, ...
            'PostFilter', true, ...
            % cosine mode: pre-computes direction cosines in MATLAB, so ClickHouse
            % only evaluates a dot product (3 muls + 2 adds) — no trig per row.
            'PostFilterMode', 'cosine');
        fprintf('%s\n', Sql);
        fprintf('%s\n', Pf);
    end

    fprintf('\n--- greatcircle post-filter ---\n');
    [Sql, Pf] = HealpixConeSearch.coneSearchSql(254.0, 64.0, 1.0, ...
        'proc_src', 'upix_high', ...
        'PostFilter', true, ...
        % greatcircle mode: uses the ClickHouse built-in greatCircleAngle() function.
        % More readable and avoids needing cx/cy/cz columns in the table, but
        % requires a trig call per row inside ClickHouse.
        'PostFilterMode', 'greatcircle');
    fprintf('%s\n', Sql);
    fprintf('%s\n', Pf);
end

function debugEdgeCases()
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: edge cases\n');
    fprintf('%s\n', repmat('=', 1, 60));

    % RA wrap-around near 0°/360°
    % Both points are 0.5° from the wrap boundary.  The HEALPix pixel layout
    % handles the wrap transparently, so both should produce the same range count.
    fprintf('\n  RA near 0/360 boundary:\n');
    Pr1 = HealpixConeSearch.coneToPixelRanges(0.5,  0.0, 1.0, Algo.CONE);
    Pr2 = HealpixConeSearch.coneToPixelRanges(359.5, 0.0, 1.0, Algo.CONE);
    fprintf('    RA=0.5°  -> %d ranges\n', Pr1.NRanges);
    fprintf('    RA=359.5°-> %d ranges\n', Pr2.NRanges);

    % North pole — pixels converge toward the pole, so a 1° cone there covers
    % fewer unique pixels than the same cone at the equator.
    fprintf('\n  North pole (Dec=90):\n');
    Pr = HealpixConeSearch.coneToPixelRanges(0.0, 90.0, 1.0, Algo.CONE);
    fprintf('    ->%d ranges, %d search pixels\n', Pr.NRanges, Pr.NSearchPixels);

    % Sub-pixel radius: the cone is smaller than a single coarse pixel.
    % query_disc returns empty -> fallback to the single containing pixel.
    % nside_search will be very large (up to 32768) to minimise over-coverage.
    fprintf('\n  Sub-pixel radius (0.001°):\n');
    Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 0.001, Algo.CONE);
    fprintf('    ->%d ranges, nside_search=%d\n', Pr.NRanges, Pr.NSideSearch);

    % Large radius: coarse NSide (e.g. 8) means each range covers many fine pixels.
    % Fewer search pixels but each range is very wide.
    fprintf('\n  Large radius (10°):\n');
    Pr = HealpixConeSearch.coneToPixelRanges(45.0, 30.0, 10.0, Algo.CONE);
    fprintf('    ->%d ranges, %d search pixels\n', Pr.NRanges, Pr.NSearchPixels);
end

function debugPixelIdBounds()
    % Verify that the maximum possible pixel ID at NSIDE_CAT exceeds UInt32 but
    % fits comfortably in UInt64.  The ClickHouse column type must be UInt64.
    fprintf('\n%s\n', repmat('=', 1, 60));
    fprintf('DEBUG: pixel ID bounds check\n');
    fprintf('%s\n', repmat('=', 1, 60));

    MaxPixId = HealpixConeSearch.MAX_PIX_ID;
    NSideCat = HealpixConeSearch.NSIDE_CAT;

    fprintf('  NSIDE_CAT    = %d\n', NSideCat);
    fprintf('  MAX_PIX_ID   = %s  (%.3e)\n', ...
        insertCommas(double(MaxPixId)), double(MaxPixId));
    fprintf('  fits UInt32? : %d\n', MaxPixId <= int64(2^32 - 1));   % False — ~51.5B > 4.3B
    fprintf('  fits UInt64? : %d\n', MaxPixId <= int64(2^64 - 1));   % True

    % Spot-check a real cone to make sure no range exceeds MAX_PIX_ID
    Pr = HealpixConeSearch.coneToPixelRanges(0.0, 0.0, 1.0, Algo.CONE);
    AllIds = [Pr.Ranges(:, 1); Pr.Ranges(:, 2)];
    fprintf('  max id in sample ranges = %s\n', insertCommas(max(AllIds)));
    fprintf('  all within bounds?       %d\n', all(AllIds >= 0 & AllIds <= MaxPixId));
end

function S = insertCommas(N)
    % Format integer with comma thousands separators (for display only).
    S = sprintf('%.0f', N);
    if numel(S) > 3
        Parts = {};
        Rem = S;
        while numel(Rem) > 3
            Parts = [{Rem(end-2:end)}, Parts]; %#ok<AGROW>
            Rem = Rem(1:end-3);
        end
        Parts = [{Rem}, Parts];
        S = strjoin(Parts, ',');
    end
end
