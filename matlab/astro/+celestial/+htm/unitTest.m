function Result = unitTest()
    % unitTest for celestial.htm
    % Example: celestial.htm.unitTest
    
    
	%io.msgStyle(LogLevel.Test, '@start', 'test started');
    
    
    [H,L]=celestial.htm.htm_build(5);
    if size(L(5).ptr)~=2048
        error('Error in celestial.htm.htm_build');
    end
    
    %io.msgStyle(LogLevel.Test, '@passed', 'test passed');
    
    %--------------------------------------------------------------
    % Test in_halfspace: Crit=1 (strict >) vs Crit=2 (>=)
    % Verify on ~1M random points that:
    %   1) Crit=2 includes all points that Crit=1 includes
    %   2) Crit=2 additionally includes boundary points (dot==0)
    %   3) No numerical issues on large random samples
    %--------------------------------------------------------------
    fprintf('Testing in_halfspace Crit=1 vs Crit=2 on 10M random points...\n');

    Npts = 10000000;
    % Random unit vectors on the sphere
    RandLong = 2*pi * rand(Npts, 1);
    RandLat  = asin(2*rand(Npts, 1) - 1);
    [CD1, CD2, CD3] = celestial.coo.coo2cosined(RandLong, RandLat);
    R = [CD1, CD2, CD3];

    % Test with several pole directions and C=0 (equatorial halfspace)
    TestPoles = [1 0 0; 0 1 0; 0 0 1; ...
                 1 1 0; 1 0 1; 0 1 1];
    TestPoles = TestPoles ./ sqrt(sum(TestPoles.^2, 2));

    for Ip = 1:size(TestPoles, 1)
        N = TestPoles(Ip, :);
        FlagStrict = celestial.htm.in_halfspace(R, N, 0, 1, 1);
        FlagGe     = celestial.htm.in_halfspace(R, N, 0, 1, 2);

        % Crit=2 must include everything Crit=1 includes
        if any(FlagStrict(:) & ~FlagGe(:))
            error('in_halfspace: Crit=2 excludes points that Crit=1 includes (pole %d)', Ip);
        end

        % Crit=2 should include >= Crit=1
        if sum(FlagGe(:)) < sum(FlagStrict(:))
            error('in_halfspace: Crit=2 returns fewer points than Crit=1 (pole %d)', Ip);
        end
    end
    fprintf('  in_halfspace random poles: PASSED\n');

    % Test boundary points exactly on the plane (dot product == 0)
    % Points on the equator (Dec=0) should be IN halfspace with N=[0,0,1], C=0, Crit=2
    % and OUT with Crit=1
    Nbound = 10000;
    BoundLong = linspace(0, 2*pi, Nbound)';
    BoundLat  = zeros(Nbound, 1);  % Dec = 0 exactly
    [BCD1, BCD2, BCD3] = celestial.coo.coo2cosined(BoundLong, BoundLat);
    Rbound = [BCD1, BCD2, BCD3];
    Npole  = [0 0 1];  % north pole normal — boundary is the equator

    FlagStrict = celestial.htm.in_halfspace(Rbound, Npole, 0, 1, 1);
    FlagGe     = celestial.htm.in_halfspace(Rbound, Npole, 0, 1, 2);

    if any(FlagStrict(:))
        error('in_halfspace: Crit=1 should NOT include equatorial points for N=[0,0,1], C=0');
    end
    if ~all(FlagGe(:))
        error('in_halfspace: Crit=2 should include ALL equatorial points for N=[0,0,1], C=0');
    end
    fprintf('  in_halfspace equatorial boundary: PASSED\n');

    % Test in_polysphere: sources at Dec=0 should be found in HTM cells
    % whose edge passes through the equator (Crit=2)
    fprintf('Testing in_polysphere with Dec=0 boundary sources...\n');

    % Build HTM level 3 and pick a north root cell (cell 1)
    % Its equatorial edge has Dec=0
    [HTM3, ~] = celestial.htm.htm_build(3);

    % Find leaf cells that have a vertex at Dec=0
    Nhtm3 = numel(HTM3);
    BoundaryCells = [];
    for Ih = 1:Nhtm3
        if isempty(HTM3(Ih).son) && ~isempty(HTM3(Ih).coo)
            % Leaf cell — check if any vertex has Lat near 0
            VertLat = HTM3(Ih).coo(:,2);
            if any(abs(VertLat) < 1e-14)
                BoundaryCells = [BoundaryCells; Ih];
            end
        end
    end

    if isempty(BoundaryCells)
        error('in_polysphere: could not find boundary cells at Dec=0 in HTM level 3');
    end

    % For each boundary cell, test that a point at Dec=0 with RA inside
    % the cell is classified as inside (Crit=2)
    Nfound = 0;
    for Ic = 1:numel(BoundaryCells)
        Coo = HTM3(BoundaryCells(Ic)).coo;  % 3x2 [Long, Lat]
        % Find vertices at the equator
        EqMask = abs(Coo(:,2)) < 1e-14;
        if sum(EqMask) >= 2
            % Two vertices at Dec=0 — midpoint RA is on equatorial edge
            EqLongs = Coo(EqMask, 1);
            MidRA = mean(EqLongs);
            TestPos = [MidRA, 0];  % Dec=0 exactly
            Flag = celestial.htm.in_polysphere(TestPos, Coo, 2);
            if ~Flag
                error('in_polysphere: Dec=0 point at RA=%.4f excluded from cell %d with Crit=2', ...
                    MidRA, BoundaryCells(Ic));
            end
            Nfound = Nfound + 1;
        end
    end
    fprintf('  in_polysphere Dec=0 boundary: PASSED (%d cells tested)\n', Nfound);

    fprintf('All celestial.htm tests passed.\n');

	Result = true;
end
