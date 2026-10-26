function [Cat, Info] = buildSourceCatalog(Args)
    % The source catalog of a DUT set: Gaia DR3 joined with GALEX AIS, U for one SED, synthetic sources in the GALEX gaps
    %   The arguments are the fields of Cfg.Catalog of ultrasat.dut.config:
    %   C = ultrasat.simconfig.toArgs(Cfg.Catalog); ultrasat.dut.buildSourceCatalog(C{:}).
    %   1. Gaia and GALEX (NUV given) in the cone Radius around (RA0, Dec0).
    %   2. Join, MatchRadius: each Gaia star goes to its nearest GALEX source
    %      within MatchRadius; of the stars of one GALEX source (a blend) the
    %      brightest in G (the brightest predicted U: one SED) gets its NUV.
    %   3. U (AB, the r = 0 filter): from NUV, U = NUV + (U - NUV), for the
    %      star that gets the NUV if e_NUV <= NUVMaxErr and NUV >= NUVMin;
    %      else from G, U = G(Vega) + (U - G(Vega)); GALEX sources with no Gaia
    %      star within MatchRadius: from NUV as is (no error cut), with the
    %      U - NUV of GalexOnlySED (they are mostly galaxies and QSOs, not
    %      stars of the SED). The colors are synthetic photometry of the SEDs.
    %   4. GALEX gaps: GapCell cells of the tangent plane at (RA0, Dec0), only
    %      cells inside the cone. The local level of a cell is the median count
    %      of the cells with GALEX sources within GapRefRadius; a cell with a
    %      count below GapFrac of it is a gap. Each gap cell gets a Poisson
    %      number (mean: the mean GALEX-only count of the same cells, minus
    %      its own GALEX-only count; the mean, as the density varies by tens
    %      of percent between GALEX tiles) of synthetic
    %      GALEX-only sources at uniform positions in the cell, each a copy (U,
    %      NUV, e_NUV, FUV, e_FUV) of a random GALEX-only source of the non-gap
    %      cells within GapRefRadius, the radius grown by 0.5 deg until there
    %      are GapRefMinN of them. Gaia stars in the gaps keep U from G.
    %   5. The sources with U <= MaxU.
    %   Columns: RA, Dec [deg], U [AB mag], USource (1 NUV, 2 G, 3 NUV_only,
    %   4 synthetic), G, BP, RP [Vega mag], Plx, ErrPlx [mas], NUV, e_NUV,
    %   FUV, e_FUV [AB mag], MatchDist [arcsec] (to the GALEX source whose
    %   NUV is used), Nblend (Gaia stars of that GALEX source), SynthParent
    %   (row of the copied source; 0 if not synthetic), InGap (1 in a gap
    %   cell). NaN where a source has no such value.
    %   Writes OutFile (a FITS binary table) and <OutFile>_maps.mat: the cell
    %   grid, the GALEX counts (all and GALEX-only), their local levels, the
    %   gap mask, the synthetic counts and the 90th percentile of NUV per cell.
    %   Gap cells with no covered cell within GapRefRadius (Level NaN) are
    %   not filled; their number is reported (Info.NUnfilled).
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] the field center. Default is 254, 64.
    %            'Radius' - [deg] the cone. Default is 15.
    %            'MaxU' - Default is 22.5.
    %            'SED' - one SED for all sources: 'BB<Teff>' (a blackbody, e.g.
    %                   'BB20000') or a Pickles template name (e.g. 'G2V').
    %                   Default is 'G2V'.
    %            'MatchRadius' - [arcsec] Default is 3.
    %            'NUVMaxErr' - Default is 0.2.
    %            'NUVMin' - Default is 15.
    %            'GapCell' - [deg] Default is 0.1.
    %            'GapFrac' - Default is 0.3.
    %            'GapRefRadius' - [deg] Default is 3.
    %            'GapRefMinN' - Default is 500.
    %            'Extinction' - only false (no extinction) is implemented.
    %            'GaiaCatalog', 'GalexCatalog' - catsHTM names. Default is
    %                   'GAIADR3', 'GALEXAIS'.
    %            'GABminusVega' - Default is 0.1136.
    %            'GalexOnlySED' - the SED of U - NUV for the GALEX-only sources
    %                   (and their synthetic copies), as 'SED'. Default is 'BB20000'.
    %            'Seed' - of the gap fill. Default is 1.
    %            'OutFile' - '' (default): nothing is written.
    %            'Header' - N x 3 cell {key, value, comment} added to the table
    %                   header (e.g. ultrasat.simconfig.headerRows). Default is {}.
    %            'Verbose' - Default is true.
    % Output : - Cat: a table of the sources (the columns above).
    %          - Info: counts, the colors, the maps and the files.
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          C = ultrasat.simconfig.toArgs(Cfg.Catalog);
    %          [Cat, Info] = ultrasat.dut.buildSourceCatalog(C{:});

    arguments
        Args.RA0 (1,1) double = 254
        Args.Dec0 (1,1) double = 64
        Args.Radius (1,1) double {mustBePositive} = 15
        Args.MaxU (1,1) double = 22.5
        Args.SED (1,:) char = 'G2V'
        Args.MatchRadius (1,1) double {mustBePositive} = 3
        Args.NUVMaxErr (1,1) double = 0.2
        Args.NUVMin (1,1) double = 15
        Args.GapCell (1,1) double {mustBePositive} = 0.1
        Args.GapFrac (1,1) double = 0.3
        Args.GapRefRadius (1,1) double {mustBePositive} = 3
        Args.GapRefMinN (1,1) double {mustBePositive} = 500
        Args.Extinction (1,1) logical = false
        Args.GaiaCatalog (1,:) char = 'GAIADR3'
        Args.GalexCatalog (1,:) char = 'GALEXAIS'
        Args.GABminusVega (1,1) double = 0.1136
        Args.GalexOnlySED (1,:) char = 'BB20000'
        Args.Seed (1,1) double = 1
        Args.OutFile char = ''
        Args.Header cell = {}
        Args.Verbose (1,1) logical = true
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    if Args.Extinction
        error('dut:buildSourceCatalog:Extinction', 'Extinction is not implemented yet: use Extinction = false');
    end
    RngSaved   = rng;
    rng(Args.Seed);
    RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    T0 = tic;

    % 1. the colors of the SED
    [UmG, UmNUV] = sedColors(Args.SED, Args.GABminusVega);
    [~, UmNUVOnly] = sedColors(Args.GalexOnlySED, Args.GABminusVega);
    Say('SED %s: U - G(Vega) = %.3f, U - NUV = %.3f; GALEX-only (%s): U - NUV = %.3f', Args.SED, UmG, UmNUV, ...
        Args.GalexOnlySED, UmNUVOnly);

    % 2. the catalogs in the cone
    Rad = Args.Radius * 3600;
    [Ga, ~] = catsHTM.cone_search(Args.GaiaCatalog, deg2rad(Args.RA0), deg2rad(Args.Dec0), Rad, ...
                                  'Columns', {'RA','Dec','phot_g_mean_mag','phot_bp_mean_mag','phot_rp_mean_mag','Plx','ErrPlx'});
    Ga = Ga(isfinite(Ga(:,3)), :);              % U from G needs G
    Ga(:,1:2) = rad2deg(Ga(:,1:2));
    [Gx, ~] = catsHTM.cone_search(Args.GalexCatalog, deg2rad(Args.RA0), deg2rad(Args.Dec0), Rad, ...
                                  'Columns', {'RA','Dec','NUVmag','e_NUVmag','FUVmag','e_FUVmag'});
    Gx = Gx(isfinite(Gx(:,3)), :);              % U from NUV needs NUV
    Gx(:,1:2) = rad2deg(Gx(:,1:2));
    Nga = size(Ga,1);
    Ngx = size(Gx,1);
    Say('Gaia %d, GALEX %d in the %.1f deg cone (%.0f s)', Nga, Ngx, Args.Radius, toc(T0));

    % 3. the join
    XYZa = unitVec(Ga(:,1), Ga(:,2));
    XYZx = unitVec(Gx(:,1), Gx(:,2));
    [Jx, Dax] = knnsearch(XYZx, XYZa);          % the nearest GALEX source of each Gaia star
    [~,  Dxa] = knnsearch(XYZa, XYZx);          % the nearest Gaia star of each GALEX source
    Dax = chord2arcsec(Dax);
    Dxa = chord2arcsec(Dxa);
    Near = find(Dax <= Args.MatchRadius);       % Gaia stars that belong to a GALEX source
    [~, Ord] = sortrows([Jx(Near) Ga(Near,3)]); % by GALEX source, then by G
    Near = Near(Ord);
    First  = [true; diff(Jx(Near)) ~= 0];       % the brightest star of each GALEX source
    Win    = Near(First);
    Nblend = accumarray(Jx(Near), 1, [Ngx 1]);
    Jw     = Jx(Win);
    Good   = Gx(Jw,4) <= Args.NUVMaxErr & Gx(Jw,3) >= Args.NUVMin;
    UseNUV = Win(Good);                         % Gaia rows that take the NUV
    Jnuv   = Jw(Good);

    Nc = 17;                                    % the output columns
    A  = nan(Nga, Nc);
    A(:,[1 2 5 6 7 8 9]) = Ga(:,[1 2 3 4 5 6 7]);
    A(:,3)  = Ga(:,3) + UmG;
    A(:,4)  = 2;
    A(UseNUV,3)     = Gx(Jnuv,3) + UmNUV;
    A(UseNUV,4)     = 1;
    A(UseNUV,10:13) = Gx(Jnuv,3:6);
    A(UseNUV,14)    = Dax(UseNUV);
    A(UseNUV,15)    = Nblend(Jnuv);
    A(:,16) = 0;

    Only = find(Dxa > Args.MatchRadius);        % GALEX sources with no Gaia star
    B = nan(numel(Only), Nc);
    B(:,1:2)   = Gx(Only,1:2);
    B(:,3)     = Gx(Only,3) + UmNUVOnly;
    B(:,4)     = 3;
    B(:,10:13) = Gx(Only,3:6);
    B(:,16)    = 0;
    Cat0 = [A; B];
    Say('join: NUV %d, G %d (of them in blends %d), GALEX-only %d (%.0f s)', numel(UseNUV), Nga - numel(UseNUV), ...
        numel(Near) - numel(Win), numel(Only), toc(T0));

    % 4. the GALEX gaps
    [Maps, Cell0, CellX] = galexMaps(Gx, Cat0, Args);
    [Syn, Maps] = fillGaps(Cat0, Cell0, CellX, Maps, Args);
    Cat0 = [Cat0; Syn];
    InGap = false(size(Cat0,1), 1);
    Ok = Cell0 > 0;
    InGap(Ok) = Maps.Gap(Cell0(Ok));
    InGap(end-size(Syn,1)+1:end) = true;
    Cat0(:,17) = InGap;
    Say('gaps: %d of %d cells (%.1f%%), %d synthetic sources (%.0f s)', sum(Maps.Gap(:)), sum(Maps.Inside(:)), ...
        100*mean(Maps.Gap(Maps.Inside)), size(Syn,1), toc(T0));

    % 5. the U cut, keeping the parent rows of the synthetic sources
    Keep = Cat0(:,3) <= Args.MaxU;
    NewRow = cumsum(Keep);
    IsSyn = Cat0(:,16) > 0;
    Cat0(IsSyn,16) = NewRow(Cat0(IsSyn,16));
    Cat0 = Cat0(Keep,:);

    Names = {'RA','Dec','U','USource','G','BP','RP','Plx','ErrPlx','NUV','e_NUV','FUV','e_FUV', ...
             'MatchDist','Nblend','SynthParent','InGap'};
    Units = {'deg','deg','mag','','mag','mag','mag','mas','mas','mag','mag','mag','mag','arcsec','','',''};
    Cat = array2table(Cat0, 'VariableNames', Names);
    Cat.Properties.VariableUnits = Units;

    Src = Cat.USource;
    Info = struct('N', height(Cat), 'NNUV', sum(Src == 1), 'NG', sum(Src == 2), 'NNUVOnly', sum(Src == 3), ...
                  'NSynthetic', sum(Src == 4), 'UmG_Vega', UmG, 'UmNUV', UmNUV, 'UmNUVOnly', UmNUVOnly, 'Maps', Maps, ...
                  'GapFraction', mean(Maps.Gap(Maps.Inside)), ...
                  'NUnfilled', sum(Maps.Inside(:) & isnan(Maps.Level(:))), 'Time', toc(T0), 'Files', {{}});
    if Info.NUnfilled > 0
        warning('dut:buildSourceCatalog:Unfilled', '%d cells have no GALEX coverage within %.1f deg and are not filled', ...
                Info.NUnfilled, Args.GapRefRadius);
    end
    Say('U <= %.2f: %d sources: NUV %d, G %d, NUV_only %d, synthetic %d', Args.MaxU, Info.N, Info.NNUV, Info.NG, ...
        Info.NNUVOnly, Info.NSynthetic);

    if ~isempty(Args.OutFile)
        Info.Files = writeCatalog(Cat, Maps, Info, Args);
        Say('written: %s', strjoin(Info.Files, ', '));
    end
end

function [UmG, UmNUV] = sedColors(SED, GABminusVega)
    % U - G(Vega) and U - NUV of the SED (U: AB, the r = 0 ULTRASAT filter)
    I = Installer;
    UP = io.files.load1(fullfile(I.getDataDir('ULTRASAT_Properties'), 'P90_UP_test_60_ZP_Var_Cern_21.mat'), 'UP');
    if startsWith(upper(SED), 'BB')
        Teff = str2double(SED(3:end));
        W = (1000:5:11000)';
        S = [W reshape(astro.spec.black_body(Teff, W), [], 1)];
    else
        S = io.files.load2(fullfile(I.getDataDir('PicklesStellarSpec'), ['uk' lower(SED) '.mat']));
        S = S(:,1:2);
    end
    U     = astro.spec.synthetic_phot(S, UP.U_AstFilt(1), 'R1', 'AB');
    UmG   = U - astro.spec.synthetic_phot(S, 'GAIA', 'G', 'AB') + GABminusVega;
    UmNUV = U - astro.spec.synthetic_phot(S, 'GALEX', 'NUV', 'AB');
end

function XYZ = unitVec(RA, Dec)
    % unit vectors of positions in deg
    XYZ = [cosd(Dec).*cosd(RA), cosd(Dec).*sind(RA), sind(Dec)];
end

function A = chord2arcsec(D)
    % the angle of a chord of the unit sphere, in arcsec
    A = rad2deg(2 .* asin(D ./ 2)) .* 3600;
end

function [X, Y] = toPlane(RA, Dec, Args)
    % tangent-plane coordinates at the field center [deg at the center]
    [X, Y] = celestial.proj.pr_gnomonic(deg2rad(RA), deg2rad(Dec), 1, deg2rad([Args.RA0 Args.Dec0]));
    X = rad2deg(X);
    Y = rad2deg(Y);
end

function [Maps, Cell0, CellX] = galexMaps(Gx, Cat0, Args)
    % the cell grid, the GALEX counts, the local level and the gaps
    Half  = rad2deg(tan(deg2rad(Args.Radius)));                   % plane half-size
    Ncell = ceil(2*Half / Args.GapCell);
    Edge  = -Ncell*Args.GapCell/2 + (0:Ncell)*Args.GapCell;
    Cen   = (Edge(1:end-1) + Edge(2:end)) / 2;
    [Xc, Yc] = meshgrid(Cen, Cen);                                % rows = Y, columns = X
    Far   = hypot(abs(Xc) + Args.GapCell/2, abs(Yc) + Args.GapCell/2);   % the farthest corner
    Inside = rad2deg(atan(deg2rad(Far))) <= Args.Radius;          % the cell is in the cone

    CellX = cellIndex(Gx(:,1), Gx(:,2), Edge, Ncell, Args);       % the cell of each GALEX source
    Count = reshape(accumarray(CellX(CellX > 0), 1, [Ncell^2 1]), Ncell, Ncell);
    Count(~Inside) = NaN;
    Cell0 = cellIndex(Cat0(:,1), Cat0(:,2), Edge, Ncell, Args);   % the cell of each joined source
    IsOnly = Cat0(:,4) == 3 & Cell0 > 0;
    CountOnly = reshape(accumarray(Cell0(IsOnly), 1, [Ncell^2 1]), Ncell, Ncell);
    CountOnly(~Inside) = NaN;

    % the local level: the median count of the cells with GALEX sources within GapRefRadius
    R    = ceil(Args.GapRefRadius / Args.GapCell);
    [Dx, Dy] = meshgrid(-R:R);
    Disk = hypot(Dx, Dy) <= Args.GapRefRadius / Args.GapCell;
    Dx = Dx(Disk);
    Dy = Dy(Disk);
    Level = nan(Ncell);
    LevelOnly = nan(Ncell);
    Idx = find(Inside);
    for K = 1:numel(Idx)
        [Iy, Ix] = ind2sub([Ncell Ncell], Idx(K));
        Jy = Iy + Dy;
        Jx = Ix + Dx;
        V  = Jy >= 1 & Jy <= Ncell & Jx >= 1 & Jx <= Ncell;
        J  = sub2ind([Ncell Ncell], Jy(V), Jx(V));
        C  = Count(J);
        Covered = C > 0;
        if any(Covered)
            Level(Idx(K))     = median(C(Covered));
            LevelOnly(Idx(K)) = mean(CountOnly(J(Covered)));
        end
    end
    Gap = Inside & Count < Args.GapFrac .* Level;

    % the NUV depth: the 90th percentile of NUV per cell
    NUV90 = nan(Ncell);
    Ok = CellX > 0;
    P90 = accumarray(CellX(Ok), Gx(Ok,3), [Ncell^2 1], @(V) prctile(V, 90), NaN);
    NUV90(:) = P90;
    NUV90(~Inside) = NaN;

    Maps = struct('Edge', Edge, 'Center', Cen, 'RA0', Args.RA0, 'Dec0', Args.Dec0, 'Inside', Inside, ...
                  'Count', Count, 'Level', Level, 'CountOnly', CountOnly, 'LevelOnly', LevelOnly, ...
                  'Gap', Gap, 'NUV90', NUV90, ...
                  'Note', 'gnomonic plane at (RA0, Dec0) [deg]; rows = Y, columns = X; cell index = sub2ind(size, row, column)');
end

function C = cellIndex(RA, Dec, Edge, Ncell, Args)
    % the linear cell index of each position (0 outside the grid)
    [X, Y] = toPlane(RA, Dec, Args);
    Ix = discretize(X, Edge);
    Iy = discretize(Y, Edge);
    C  = zeros(numel(RA), 1);
    Ok = ~isnan(Ix) & ~isnan(Iy);
    C(Ok) = sub2ind([Ncell Ncell], Iy(Ok), Ix(Ok));
end

function [Syn, Maps] = fillGaps(Cat0, Cell0, CellX, Maps, Args) %#ok<INUSL>
    % synthetic GALEX-only sources in the gap cells, copies of the GALEX-only sources around
    Ncell = numel(Maps.Center);
    % the reference pool: GALEX-only sources in non-gap cells inside the cone
    Pool = find(Cat0(:,4) == 3 & Cell0 > 0);
    Pool = Pool(Maps.Inside(Cell0(Pool)) & ~Maps.Gap(Cell0(Pool)));
    [PoolCell, Ord] = sort(Cell0(Pool));
    Pool  = Pool(Ord);
    Npool = accumarray(PoolCell, 1, [Ncell^2 1]);
    Start = [0; cumsum(Npool)];                 % pool rows of cell c: Pool(Start(c)+1 : Start(c+1))

    GapIdx = find(Maps.Gap);
    Nfill  = poissrnd(max(Maps.LevelOnly(GapIdx) - Maps.CountOnly(GapIdx), 0));
    Maps.Nfill  = zeros(Ncell);
    Maps.Nfill(GapIdx) = Nfill;
    Maps.RefRadius = nan(Ncell);
    Parent = zeros(sum(Nfill), 1);
    CellOf = zeros(sum(Nfill), 1);
    Pos = 0;
    for K = 1:numel(GapIdx)
        N = Nfill(K);
        if N > 0
            [Iy, Ix] = ind2sub([Ncell Ncell], GapIdx(K));
            R = Args.GapRefRadius;
            Cand = poolCells(Iy, Ix, R, Ncell, Args.GapCell);
            while sum(Npool(Cand)) < Args.GapRefMinN && R < Args.Radius
                R = R + 0.5;
                Cand = poolCells(Iy, Ix, R, Ncell, Args.GapCell);
            end
            Maps.RefRadius(GapIdx(K)) = R;
            W  = Npool(Cand);
            Cand = Cand(W > 0);
            W  = W(W > 0);
            Ic = discretize(rand(N, 1) .* sum(W), [0; cumsum(W)]);   % a cell, weighted by its pool
            Ic(isnan(Ic)) = numel(W);
            Row = Start(Cand(Ic)) + max(ceil(rand(N, 1) .* W(Ic)), 1);   % a source of that cell
            Parent(Pos+1:Pos+N) = Pool(Row);
            CellOf(Pos+1:Pos+N) = GapIdx(K);
            Pos = Pos + N;
        end
    end

    % uniform positions in the gap cells
    [Iy, Ix] = ind2sub([Ncell Ncell], CellOf);
    X = Maps.Edge(Ix)' + rand(numel(CellOf), 1) .* Args.GapCell;
    Y = Maps.Edge(Iy)' + rand(numel(CellOf), 1) .* Args.GapCell;
    [RA, Dec] = celestial.proj.pr_ignomonic(deg2rad(X), deg2rad(Y), deg2rad([Args.RA0 Args.Dec0]));
    Syn = nan(numel(Parent), size(Cat0, 2));
    Syn(:,1)     = mod(rad2deg(RA), 360);
    Syn(:,2)     = rad2deg(Dec);
    Syn(:,3)     = Cat0(Parent,3);
    Syn(:,4)     = 4;
    Syn(:,10:13) = Cat0(Parent,10:13);
    Syn(:,16)    = Parent;
end

function C = poolCells(Iy, Ix, R, Ncell, Cell)
    % the linear indices of the cells within R deg of cell (Iy, Ix)
    N = ceil(R / Cell);
    [Dx, Dy] = meshgrid(-N:N);
    In = hypot(Dx, Dy) <= R / Cell;
    Jy = Iy + Dy(In);
    Jx = Ix + Dx(In);
    V  = Jy >= 1 & Jy <= Ncell & Jx >= 1 & Jx <= Ncell;
    C  = sub2ind([Ncell Ncell], Jy(V), Jx(V));
end

function Files = writeCatalog(Cat, Maps, Info, Args)
    % the FITS table and the maps
    Dir = fileparts(Args.OutFile);
    if ~isempty(Dir) && ~isfolder(Dir)
        mkdir(Dir);
    end
    H = { ...
        'SED',      Args.SED,       'the SED of all sources'; ...
        'UMG_VEGA', Info.UmG_Vega,  'U - G(Vega) of the SED'; ...
        'UMNUV',    Info.UmNUV,     'U - NUV of the SED'; ...
        'GOSED',    Args.GalexOnlySED, 'the SED of U - NUV of the GALEX-only sources'; ...
        'UMNUVGO',  Info.UmNUVOnly, 'U - NUV of the GALEX-only sources'; ...
        'UFILTER',  'ULTRASAT r=0', 'U: AB, the ULTRASAT filter at the field center'; ...
        'MAXU',     Args.MaxU,      'U cut'; ...
        'CONE_RA',  Args.RA0,       '[deg] cone center'; ...
        'CONE_DEC', Args.Dec0,      '[deg] cone center'; ...
        'CONE_R',   Args.Radius,    '[deg] cone radius'; ...
        'MATCHRAD', Args.MatchRadius, '[arcsec] Gaia x GALEX match radius'; ...
        'GAPFRAC',  Info.GapFraction, 'fraction of the cone cells that are GALEX gaps'; ...
        'USRC1',    'NUV',          'USource 1: U from NUV (Gaia star with GALEX)'; ...
        'USRC2',    'G',            'USource 2: U from Gaia G'; ...
        'USRC3',    'NUV_only',     'USource 3: GALEX source with no Gaia star'; ...
        'USRC4',    'synthetic',    'USource 4: synthetic, in a GALEX gap'; ...
        'NSRC',     int32(Info.N),  'number of sources'};
    H = [H; Args.Header];
    if isfile(Args.OutFile)
        delete(Args.OutFile);
    end
    FITS.writeTable1(AstroTable(Cat), Args.OutFile, 'ColUnits', Cat.Properties.VariableUnits, ...
                     'OverWrite', true, 'ExtName', 'SOURCES');
    % the keywords go into the table HDU itself (writeTable1 'Header' adds a separate image HDU)
    Fptr = matlab.io.fits.openFile(Args.OutFile, 'readwrite');
    Closer = onCleanup(@() matlab.io.fits.closeFile(Fptr));
    matlab.io.fits.movAbsHDU(Fptr, 2);
    for K = 1:size(H, 1)
        matlab.io.fits.writeKey(Fptr, H{K,1}, H{K,2}, H{K,3});
    end
    clear Closer
    MapFile = regexprep(Args.OutFile, '\.fits$', '_maps.mat');
    save(MapFile, 'Maps', '-v7.3');
    Files = {Args.OutFile, MapFile};
end
