function [Truth, Files] = simulateTile(Args)
    % One tile image of a DUT set and its truth catalog: the catalog sources and the transients through usim
    %   Called by ultrasat.dut.simulateSet, for the references as the SimFun
    %   of ultrasat.simulateRefSet (which passes RA0, Dec0, Tile,
    %   PlaneRotation, Exposure, DateObs, OutDir, OutName and Seed).
    %   1. The sources of the tile: the rows of the source catalog of the
    %      set (Cfg.Catalog.OutFile) on the tile, with their U and SED
    %      (Cfg.Catalog.SED for the stars, GalexOnlySED for the GALEX-only
    %      and synthetic sources), and the transients (X, Y, U; a
    %      TransientTeff blackbody). All enter usim as observed U in the
    %      r = 0 filter, without extinction: the catalog rows as a catalog
    %      with U ('Catalog' with ColNames.U of simulateCatalogField), the
    %      transients as its 'ExtraSrc'.
    %   2. ultrasat.usim with Cfg.Usim, the AstroFileName of the image
    %      (ultrasat.dut.productName) and the header keywords of
    %      ultrasat.simconfig.headerRows.
    %   3. The truth catalog (Product Cat, Version ...-0): per source RA,
    %      Dec, X, Y, R (the radius from the tile's inner corner [deg], as
    %      usim measures it), U (truth), MagU (usim's magnitude in the
    %      radial filter zone), CPS [e-/s], Flux = CPS x exposure [e-]
    %      (before the flat field and the vignetting), SNR (usim CrudeSNR),
    %      AboveLimit (SNR >= Cfg.Truth.SNLimit), USource (of the catalog;
    %      0 for a transient), CatRow (row in the source catalog; 0 for a
    %      transient), TransientID (0 for a catalog source).
    %   Mode 'base' (the star field of the new visits): no transients, the
    %   noiseless source image is saved (usim SaveSrcImage) as
    %   <Paths.New>/NewBase_tile<T>_SrcImage.mat with the truth of its
    %   sources in NewBase_tile<T>_truth.mat; no image file is written.
    %   Mode 'image' with 'Stars' false and 'AddSrcImage': a new visit; the
    %   truth catalog is the base truth plus the transients.
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0', 'Tile', 'PlaneRotation', 'Exposure', 'DateObs',
    %            'Seed' - as in ultrasat.simulateCatalogField / ultrasat.usim.
    %            'OutDir', 'OutName' - accepted from simulateRefSet and not used
    %                   (the names come from productName).
    %            'ConfigFile' - the saved configuration (ultrasat.simconfig.write).
    %            'Kind' - 'ref' (default) or 'new'.
    %            'Pointing', 'Image', 'Epoch' - of a reference image. Default is 1.
    %            'Visit' - the number of a new visit. Default is 0.
    %            'Mode' - 'image' (default) or 'base'.
    %            'Stars' - include the catalog sources. Default is true.
    %            'Transients' - [] or a table X, Y [pixels], U [mag].
    %            'AddSrcImage' - the saved source image of the base (usim).
    %            'TruthBase' - the truth .mat of the base, joined to the truth.
    % Output : - Truth: the truth table.
    %          - Files: struct with .Image, .Truth (and .SrcImage, .TruthBase).
    % Author : D. Kovaleva (Oct 2026)
    % Example: see ultrasat.dut.simulateSet

    arguments
        Args.RA0 (1,1) double
        Args.Dec0 (1,1) double
        Args.Tile (1,1) char = 'B'
        Args.PlaneRotation (1,1) double = 0
        Args.Exposure (1,2) double = [1 300]
        Args.DateObs (1,:) char = '2028-01-01T00:00:00'
        Args.Seed = []
        Args.OutDir = ''
        Args.OutName = ''
        Args.ConfigFile (1,:) char
        Args.Kind (1,:) char {mustBeMember(Args.Kind, {'ref', 'new'})} = 'ref'
        Args.Pointing (1,1) double = 1
        Args.Image (1,1) double = 1
        Args.Epoch (1,1) double = 1
        Args.Visit (1,1) double = 0
        Args.Mode (1,:) char {mustBeMember(Args.Mode, {'image', 'base'})} = 'image'
        Args.Stars (1,1) logical = true
        Args.Transients = []
        Args.AddSrcImage = []
        Args.TruthBase = ''
    end

    Cfg  = cachedConfig(Args.ConfigFile);
    Tile = upper(Args.Tile);
    [~, ~, G] = ultrasat.simulateCatalogField('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', Tile, ...
                                              'PlaneRotation', Args.PlaneRotation, 'GeometryOnly', true, 'Verbose', false);
    WCS = G.WCS;

    % 1. the sources: catalog rows on the tile and the transients, as ExtraSrc
    E = table();
    if Args.Stars
        Cat = cachedCatalog(Cfg.Catalog.OutFile);
        [X, Y] = WCS.sky2xy(Cat.RA, Cat.Dec);
        In = onTile(X, Y, G);
        Row = find(In);
        [Tpl, Teff] = sedSpec(Cfg.Catalog.SED, numel(Row));
        Only = Cat.USource(Row) >= 3;
        [TplO, TeffO] = sedSpec(Cfg.Catalog.GalexOnlySED, sum(Only));
        Tpl(Only) = TplO;
        Teff(Only) = TeffO;
        E = table(Cat.RA(Row), Cat.Dec(Row), X(Row), Y(Row), Cat.U(Row), Teff, Tpl, Cat.USource(Row), Row, ...
                  zeros(numel(Row), 1), 'VariableNames', {'RA','Dec','X','Y','U','Teff','Template','USource','CatRow','TransientID'});
    end
    if ~isempty(Args.Transients)
        Tr = Args.Transients;
        [TRA, TDec] = WCS.xy2sky(Tr.X, Tr.Y);
        N  = height(Tr);
        Et = table(TRA(:), TDec(:), Tr.X(:), Tr.Y(:), Tr.U(:), repmat(Cfg.New.TransientTeff, N, 1), strings(N, 1), ...
                   zeros(N, 1), zeros(N, 1), (1:N)', 'VariableNames', E0names());
        E = [E; Et];
    end
    Ns = height(E);

    % 2. usim
    Run   = {'Pointing', Args.Pointing, 'Image', Args.Image};
    [ImgFile, FN] = ultrasat.dut.productName(Cfg, Args.Kind, Tile, Args.DateObs, Run{:});
    TruthFile     = ultrasat.dut.productName(Cfg, Args.Kind, Tile, Args.DateObs, Run{:}, 'Product', 'truth');
    if strcmp(Args.Kind, 'ref')
        Keys = {'SIMKIND', 'ref', 'reference image'; 'SIMPNT', Args.Pointing, 'aimpoint'; ...
                'SIMEPOCH', Args.Epoch, 'roll epoch'; 'SIMIMAGE', Args.Image, 'image of the epoch'};
        Dir  = Cfg.Paths.Ref;
    else
        Keys = {'SIMKIND', 'new', 'new visit'; 'SIMVISIT', Args.Visit, 'new visit number'; ...
                'SIMNTRAN', sum(E.TransientID > 0), 'transients in the image'};
        Dir  = Cfg.Paths.New;
    end
    Rows = ultrasat.simconfig.headerRows(Cfg, Keys);
    if ~isfolder(Dir)
        mkdir(Dir);
    end
    UsimArgs = [ultrasat.simconfig.toArgs(Cfg.Usim), {'DateObs', Args.DateObs, 'OutDir', Dir}];
    if ~isempty(Args.Seed)
        UsimArgs = [UsimArgs, {'Seed', Args.Seed}];
    end
    Files = struct('Image', ImgFile, 'Truth', TruthFile);
    if strcmp(Args.Mode, 'base')
        UsimArgs = [UsimArgs, {'OutName', 'NewBase', 'SaveSrcImage', true, 'OutType', 'AstroImage'}];
        Files = struct('Image', '', 'Truth', '', ...
                       'SrcImage', fullfile(Dir, sprintf('NewBase_tile%s_SrcImage.mat', Tile)), ...
                       'TruthBase', fullfile(Dir, sprintf('NewBase_tile%s_truth.mat', Tile)));
    else
        UsimArgs = [UsimArgs, {'FileName', FN, 'ExtraHeader', Rows}];
    end
    if ~isempty(Args.AddSrcImage)
        UsimArgs = [UsimArgs, {'AddSrcImage', Args.AddSrcImage}];
    end
    SceneArgs = ultrasat.simconfig.toArgs(Cfg.Scene);
    IsTr = E.TransientID > 0;
    Cols = {'RA','Dec','U','Teff','Template'};
    [Sim, Src] = ultrasat.simulateCatalogField('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', Tile, ...
                     'PlaneRotation', Args.PlaneRotation, 'Exposure', Args.Exposure, SceneArgs{:}, ...
                     'Catalog', E(~IsTr, Cols), 'ColNames', struct('RA', 'RA', 'Dec', 'Dec', 'U', 'U'), ...
                     'ExtraSrc', E(IsTr, Cols), 'UsimArgs', UsimArgs, 'Verbose', false);
    if height(Src) ~= Ns
        error('dut:simulateTile:Sources', '%d sources given, %d simulated', Ns, height(Src));
    end

    % 3. the truth catalog
    Truth = truthTable(E, Sim, Tile, G, prod(Args.Exposure), Cfg.Truth.SNLimit);
    if ~isempty(Args.TruthBase)
        B = io.files.load2(Args.TruthBase, 'Truth');
        Truth = [B; Truth];
    end
    TruthKeys = [Rows; {'SNLIMIT', Cfg.Truth.SNLimit, 'AboveLimit: SNR >= SNLIMIT'; ...
                        'EXPTIME', prod(Args.Exposure), '[s] exposure'; ...
                        'DATEOBS', Args.DateObs, 'exposure start (UTC)'}];
    if strcmp(Args.Mode, 'base')
        save(Files.TruthBase, 'Truth');
    else
        ultrasat.dut.writeTable(Truth, TruthFile, TruthKeys, 'TRUTH');
    end
end

function N = E0names()
    % the columns of the source rows
    N = {'RA','Dec','X','Y','U','Teff','Template','USource','CatRow','TransientID'};
end

function In = onTile(X, Y, G)
    % on the tile for both simulateCatalogField (0.1 < X < NX) and usim (0.5 <= X < NX + 0.5)
    In = X >= 0.5 & Y >= 0.5 & X < G.NX & Y < G.NY;
end

function [Tpl, Teff] = sedSpec(SED, N)
    % the simulateCatalogField ExtraSrc spectrum of an SED name: a blackbody 'BB<Teff>' or a Pickles template
    if startsWith(upper(SED), 'BB')
        Tpl  = strings(N, 1);
        Teff = repmat(str2double(SED(3:end)), N, 1);
    else
        Tpl  = repmat(string(lower(SED)), N, 1);
        Teff = NaN(N, 1);
    end
end

function T = truthTable(E, Sim, Tile, G, ExpTime, SNLimit)
    % the truth columns, matched to the usim output catalog by position
    C  = Sim.CatData;
    CX = C.getCol('X');
    CY = C.getCol('Y');
    [J, D] = knnsearch([CX CY], [E.X E.Y]);
    if any(D > 0.01)
        error('dut:simulateTile:Match', '%d sources not found in the usim catalog', sum(D > 0.01));
    end
    PixSizeDeg = (9.5e-3 / 360) * 180/pi;
    [X0, Y0] = innerCorner(Tile, G.NX, G.NY);
    CPS  = C.getCol('Counts/s');
    SNR  = C.getCol('SNR');
    MagU = C.getCol('MagU');
    T = table(E.RA, E.Dec, E.X, E.Y, hypot(E.X - X0, E.Y - Y0) .* PixSizeDeg, E.U, MagU(J), ...
              CPS(J), CPS(J) .* ExpTime, SNR(J), double(SNR(J) >= SNLimit), E.USource, E.CatRow, E.TransientID, ...
              'VariableNames', {'RA','Dec','X','Y','R','U','MagU','CPS','Flux','SNR','AboveLimit','USource','CatRow','TransientID'});
    T.Properties.VariableUnits = {'deg','deg','pix','pix','deg','mag','mag','e-/s','e-','','','','',''};
end

function [X0, Y0] = innerCorner(Tile, NX, NY)
    % the tile corner usim measures the radius from (as in ultrasat.usim; keep in sync)
    switch Tile
        case 'A'
            X0 = NX + 0.5; Y0 = 0.5;
        case 'B'
            X0 = 0.5;      Y0 = 0.5;
        case 'C'
            X0 = 0.5;      Y0 = NY + 0.5;
        otherwise
            X0 = NX + 0.5; Y0 = NY + 0.5;
    end
end

function Cfg = cachedConfig(File)
    % the saved configuration, read once per MATLAB process (each parallel worker)
    persistent F C
    if isempty(F) || ~strcmp(F, File)
        C = ultrasat.simconfig.read(File);
        F = File;
    end
    Cfg = C;
end

function Cat = cachedCatalog(File)
    % the source catalog, read once per MATLAB process (each parallel worker)
    persistent F T
    if isempty(F) || ~strcmp(F, File)
        T = FITS.readTable1(File);
        if ~istable(T)
            T = array2table(T.Catalog, 'VariableNames', T.ColNames);
        end
        F = File;
    end
    Cat = T;
end
