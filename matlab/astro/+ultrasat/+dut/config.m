function Cfg = config(Args)
    % The configuration of a DUT simulation set (transient detection after the All-Sky Survey): the only source of its parameters
    %   Every parameter of the set is an argument here, with the design
    %   value as its default; the drivers take their arguments from the
    %   output and set nothing themselves. A call with no arguments gives
    %   the design set DUT-G2V-1.
    %   The output has one sub-structure per stage. The field names of a
    %   stage are the argument names of the function it is for, so it goes
    %   there as is, e.g. C = ultrasat.simconfig.toArgs(Cfg.Usim) and
    %   ultrasat.usim(..., C{:}):
    %     .Meta    - the set: Task, SED, ConfigN, Version, Field, RA0, Dec0, T0, Seed
    %     .Paths   - Root, Set, Catalog, Ref, New, Config directories
    %     .Naming  - the AstroFileName fields of the products
    %     .Catalog - the source catalog (Gaia DR3 x GALEX, gap fill)
    %     .Scene   - for ultrasat.simulateCatalogField
    %     .Ref     - for ultrasat.simulateRefSet (the reference images)
    %     .New     - the new visits with the transients
    %     .Usim    - for ultrasat.usim (all images)
    %     .Truth   - the per-image truth catalogs
    %     .Header  - the simulation keywords of the image headers
    %   Only the parameters the design fixes are set; every other argument
    %   of usim, simulateCatalogField and simulateRefSet keeps its own
    %   default, and ultrasat.simconfig.write records those defaults next
    %   to the configuration. Run-time options (Parallel, Nworkers,
    %   Verbose, ...) are not configuration: they go to the drivers.
    %   Seeds: from Seed, Catalog.Seed = 1e6*Seed + 1; Ref.Seed = 1e6*Seed
    %   + 1000 (image k of the set gets Ref.Seed + k); New.Seed = 1e6*Seed
    %   + 1e5 (visit v gets New.Seed + v). The streams do not overlap.
    % Input  : * ...,key,val,...
    %            --- the set
    %            'SED' - the spectrum of all catalog sources: 'G2V' (default)
    %                   or 'BB20000' (a 20000 K blackbody).
    %            'ConfigN' - the configuration number within the SED; a
    %                   changed configuration needs a new number. Default is 1.
    %            'Field' - the field name. Default is 'N3'.
    %            'RA0', 'Dec0' - [deg] the field center. Default is 254, 64.
    %            'T0' - the time origin of the set (UTC). Default is
    %                   '2028-01-01T00:00:00'.
    %            'Seed' - the seed of the set (see above). Default is 1.
    %            'Root' - the root of all simulations; the set is in
    %                   <Root>/DUT/<Version>. Default '' is the environment
    %                   variable ULTRASAT_SIM_ROOT, or ./ULTRASAT_sim if unset.
    %            --- the source catalog
    %            'CatRadius' - [deg] the radius of the catalog cone around the
    %                   field center. Default is 15.
    %            'MaxU' - the faintest U (AB, r = 0 filter) kept. Default is 22.5.
    %            'MatchRadius' - [arcsec] the Gaia x GALEX match radius. Default is 3.
    %            'NUVMaxErr' - the largest e_NUV for a U from NUV. Default is 0.2.
    %            'NUVMin' - the brightest NUV used (saturation). Default is 15.
    %            'GapCell' - [deg] the cell of the GALEX footprint. Default is 0.1.
    %            'GapFrac' - a cell with a GALEX density below this fraction
    %                   of the local median is a gap. Default is 0.3.
    %            'GapRefRadius' - [deg] the radius of the reference sources of
    %                   a gap. Default is 3.
    %            'GapRefMinN' - the radius grows until this many reference
    %                   sources. Default is 500.
    %            'Extinction' - false (default): no extinction.
    %            'CatVersion' - the version of the catalog file. Default is 1.
    %            'VersionImage', 'VersionTruth' - the last part of the
    %                   AstroFileName Version: <Version>-<VersionImage> for the
    %                   images (and the pipeline products made from them),
    %                   <Version>-<VersionTruth> for the truth catalogs, which
    %                   keep the Product 'Cat'. Default is 1 and 0.
    %            --- the reference images
    %            'DitherLeg' - [deg] the aimpoints, as ultrasat.tools.ditherGrid
    %                   'Leg'. Default is 2.
    %            'DitherPattern' - as ultrasat.tools.ditherGrid. Default is '2x2'.
    %            'Rolls' - [deg] the roll of each epoch; [] (default): Nepoch
    %                   log-uniform values from 1 to RollMax, the first set
    %                   to 0 (0, 3.56, 12.65, 45 for 4 epochs).
    %            'Nepoch' - Default is 4.
    %            'RollMax' - [deg] Default is 45.
    %            'RollRate' - [deg/day] the epochs are Rolls/RollRate days
    %                   after T0. Default is 1.
    %            'Nimage' - consecutive images per epoch and aimpoint. Default is 3.
    %            'ExpTime' - [s] one image (one exposure). Default is 300.
    %            'Tiles' - the reference tiles. Default is 'ABCD'.
    %            'PointingJitter' - [arcsec] between the images of the
    %                   references. Default is 1.
    %            --- the new visits
    %            'NewTile' - Default is 'B'.
    %            'NewRoll' - [deg] Default is 5.
    %            'Nvisit' - the number of new visits. Default is 100.
    %            'Ntransient' - transients per visit. Default is 10.
    %            'MagBright', 'MagFaint', 'Nmag' - the transient magnitudes:
    %                   m_k = MagFaint - (MagFaint - MagBright)*((Nmag - k)/(Nmag - 1))^2,
    %                   k = 1..Nmag. Default is 17, 23, 10.
    %            'RadiusRange' - [deg] the transient radii, from the tile's
    %                   inner corner. Default is [0.3 10].
    %            'TransientTeff' - [K] the blackbody of the transients.
    %                   Default is 20000.
    %            'NewStartDay' - [day] the first new visit after T0. Default is 90.
    %            'BoresightTol' - [arcsec] the new-visit boresight (New.RA0,
    %                   New.Dec0) is solved once, so that the center of
    %                   NewTile at NewRoll falls on the field center within
    %                   this. Default is 0.01.
    %            --- usim, truth, header
    %            'Jitter' - usim Jitter. Default is true.
    %            'JitterPar' - usim JitterPar. Default is [2 2 10].
    %            'Vignetting' - usim Vignetting. Default is 'ELOP'.
    %            'Zodi' - [e-/pix/s] usim Zodi, uniform. Default is 0.09.
    %            'AddCRStreaks' - usim AddCRStreaks. Default is false.
    %            'UsimArgs' - cell of further key,val for usim, added to
    %                   Cfg.Usim (and so to the record). Default is {}.
    %            'SceneArgs' - the same for ultrasat.simulateCatalogField,
    %                   added to Cfg.Scene. Default is {}.
    %            'SNLimit' - the above-the-limit flag of the truth catalogs:
    %                   predicted S/N >= SNLimit. Default is 5.
    %            'SimKeys' - write the simulation keywords in the image
    %                   headers. Default is true.
    % Output : - Cfg: the configuration (see above).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;                          % DUT-G2V-1
    %          Cfg = ultrasat.dut.config('SED','BB20000');          % DUT-BB20000-1
    %          Cfg = ultrasat.dut.config('ConfigN',2,'NewRoll',6);  % DUT-G2V-2
    %          C   = ultrasat.simconfig.toArgs(Cfg.Usim);

    arguments
        % the set
        Args.SED (1,:) char {mustBeMember(Args.SED, {'G2V', 'BB20000'})} = 'G2V'
        Args.ConfigN (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.Field (1,:) char = 'N3'
        Args.RA0 (1,1) double = 254
        Args.Dec0 (1,1) double = 64
        Args.T0 (1,:) char = '2028-01-01T00:00:00'
        Args.Seed (1,1) double {mustBeNonnegative, mustBeInteger} = 1
        Args.Root char = ''
        % the source catalog
        Args.CatRadius (1,1) double = 15
        Args.MaxU (1,1) double = 22.5
        Args.MatchRadius (1,1) double = 3
        Args.NUVMaxErr (1,1) double = 0.2
        Args.NUVMin (1,1) double = 15
        Args.GapCell (1,1) double = 0.1
        Args.GapFrac (1,1) double = 0.3
        Args.GapRefRadius (1,1) double = 3
        Args.GapRefMinN (1,1) double = 500
        Args.Extinction (1,1) logical = false
        Args.CatVersion (1,1) double {mustBePositive, mustBeInteger} = 1
        Args.VersionImage (1,1) double {mustBeNonnegative, mustBeInteger} = 1
        Args.VersionTruth (1,1) double {mustBeNonnegative, mustBeInteger} = 0
        % the reference images
        Args.DitherLeg (1,1) double = 2
        Args.DitherPattern (1,:) char = '2x2'
        Args.Rolls double = []
        Args.Nepoch (1,1) double {mustBePositive, mustBeInteger} = 4
        Args.RollMax (1,1) double = 45
        Args.RollRate (1,1) double {mustBePositive} = 1
        Args.Nimage (1,1) double {mustBePositive, mustBeInteger} = 3
        Args.ExpTime (1,1) double {mustBePositive} = 300
        Args.Tiles (1,:) char = 'ABCD'
        Args.PointingJitter double = 1
        % the new visits
        Args.NewTile (1,1) char = 'B'
        Args.NewRoll (1,1) double = 5
        Args.Nvisit (1,1) double {mustBePositive, mustBeInteger} = 100
        Args.Ntransient (1,1) double {mustBePositive, mustBeInteger} = 10
        Args.MagBright (1,1) double = 17
        Args.MagFaint (1,1) double = 23
        Args.Nmag (1,1) double {mustBeInteger, mustBeGreaterThan(Args.Nmag, 1)} = 10
        Args.RadiusRange (1,2) double = [0.3 10]
        Args.TransientTeff (1,1) double = 20000
        Args.NewStartDay (1,1) double = 90
        Args.BoresightTol (1,1) double {mustBePositive} = 0.01
        % usim, truth, header
        Args.Jitter (1,1) logical = true
        Args.JitterPar (1,3) double = [2 2 10]
        Args.Vignetting (1,:) char = 'ELOP'
        Args.Zodi (1,1) double = 0.09
        Args.AddCRStreaks (1,1) logical = false
        Args.UsimArgs cell = {}
        Args.SceneArgs cell = {}
        Args.SNLimit (1,1) double = 5
        Args.SimKeys (1,1) logical = true
    end

    Fmt     = 'yyyy-MM-dd''T''HH:mm:ss';
    T0      = datetime(Args.T0, 'InputFormat', Fmt);
    Version = sprintf('DUT-%s-%d', Args.SED, Args.ConfigN);
    Base    = 1e6 * Args.Seed;

    % the set
    Cfg.Meta = struct('Task', 'DUT', 'SED', Args.SED, 'ConfigN', Args.ConfigN, 'Version', Version, ...
                      'Field', Args.Field, 'RA0', Args.RA0, 'Dec0', Args.Dec0, 'T0', Args.T0, 'Seed', Args.Seed);

    % directories
    Root = Args.Root;
    if isempty(Root)
        Root = getenv('ULTRASAT_SIM_ROOT');
    end
    if isempty(Root)
        Root = fullfile(pwd, 'ULTRASAT_sim');
        warning('dut:config:NoRoot', 'ULTRASAT_SIM_ROOT is not set: the set goes under %s', Root);
    end
    Set = fullfile(Root, 'DUT', Version);
    Cfg.Paths = struct('Root', Root, 'Set', Set, 'Catalog', fullfile(Set, 'catalog'), ...
                       'Ref', fullfile(Set, 'ref'), 'New', fullfile(Set, 'new'), 'Config', fullfile(Set, 'config'));

    % the reference aimpoints, with the pattern simulateRefSet uses
    Grid0 = table(Args.RA0, Args.Dec0, 1, 'VariableNames', {'RA', 'Dec', 'id'});
    Grid  = ultrasat.tools.ditherGrid(Grid0, 'Leg', Args.DitherLeg, 'Pattern', Args.DitherPattern);
    Pnt   = [Grid.RA(:) Grid.Dec(:)];
    Np    = size(Pnt, 1);

    % names: AstroFileName fields (FieldID N3.1.. per aimpoint, N3.0 the new visits)
    Cfg.Naming = struct('ProjName', "USAT", 'Filter', "UMAG", ...
                        'FieldIDRef', string(Args.Field) + "." + string(1:Np), ...
                        'FieldIDNew', string(Args.Field) + ".0", ...
                        'CropID', 0, 'Type', "sci", 'LevelRef', "ref", 'LevelNew', "proc", ...
                        'ProductImage', "Image", 'ProductTruth', "Cat", ...
                        'Version', string(Version) + "-" + Args.VersionImage, ...
                        'VersionTruth', string(Version) + "-" + Args.VersionTruth, 'FileType', "fits", ...
                        'SourceCatFile', sprintf('DUT_%s_SourceCat_%s_v%d.fits', Args.Field, Args.SED, Args.CatVersion));

    % the source catalog
    Cfg.Catalog = struct('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Radius', Args.CatRadius, 'MaxU', Args.MaxU, ...
                         'SED', Args.SED, 'MatchRadius', Args.MatchRadius, 'NUVMaxErr', Args.NUVMaxErr, ...
                         'NUVMin', Args.NUVMin, 'GapCell', Args.GapCell, 'GapFrac', Args.GapFrac, ...
                         'GapRefRadius', Args.GapRefRadius, 'GapRefMinN', Args.GapRefMinN, ...
                         'Extinction', Args.Extinction, 'Seed', Base + 1, ...
                         'OutFile', fullfile(Cfg.Paths.Catalog, Cfg.Naming.SourceCatFile));

    % the scene: no extinction, the catalog's own U cut
    Cfg.Scene = struct('Ebv', 0, 'MaxU', Args.MaxU);
    if Args.Extinction
        Cfg.Scene = rmfield(Cfg.Scene, 'Ebv');   % the simulateCatalogField map
    end
    Cfg.Scene = addArgs(Cfg.Scene, Args.SceneArgs, 'ultrasat.simulateCatalogField');

    % the references: epochs = rolls, each aimpoint after the previous one
    Rolls = Args.Rolls;
    if isempty(Rolls)
        Rolls = logspace(0, log10(Args.RollMax), Args.Nepoch);
        Rolls(1) = 0;
    end
    Rolls = Rolls(:)';
    Nr    = numel(Rolls);
    [Ie, Ip] = ndgrid(1:Nr, 1:Np);     % visit v = (Ip - 1)*Nr + Ie, as in simulateRefSet
    Start = T0 + days(Rolls(Ie(:)) ./ Args.RollRate)' + seconds((Ip(:) - 1) .* Args.Nimage .* Args.ExpTime);
    Cfg.Ref = struct('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Pointings', Pnt, 'Rolls', Rolls, 'RollJitter', 0, ...
                     'PointingJitter', Args.PointingJitter, 'Tiles', Args.Tiles, 'Nimage', Args.Nimage, ...
                     'Exposure', [1 Args.ExpTime], 'DateObs', string(dateshift(Start, 'start', 'second', 'nearest'), Fmt)', ...
                     'ImageInterval', Args.ExpTime, 'OutDir', Cfg.Paths.Ref, 'Seed', Base + 1000);

    % the new visits
    K      = 1:Args.Nmag;
    MagGrid = Args.MagFaint - (Args.MagFaint - Args.MagBright) .* ((Args.Nmag - K) ./ (Args.Nmag - 1)).^2;
    VisitT = T0 + days(Args.NewStartDay) + seconds(Args.ExpTime .* (0:Args.Nvisit-1));
    [BRA, BDec, Off] = boresight(Args.RA0, Args.Dec0, Args.NewTile, Args.NewRoll, Args.BoresightTol);
    Cfg.New = struct('RA0', BRA, 'Dec0', BDec, 'Tile', Args.NewTile, 'Roll', Args.NewRoll, ...
                     'TileCenter', [Args.RA0 Args.Dec0], 'TileCenterOffset', Off, ...
                     'Nvisit', Args.Nvisit, 'Ntransient', Args.Ntransient, 'MagGrid', MagGrid, ...
                     'RadiusRange', Args.RadiusRange, 'TransientTeff', Args.TransientTeff, ...
                     'Exposure', [1 Args.ExpTime], 'DateObs', string(VisitT, Fmt), 'PointingJitter', 0, ...
                     'OutDir', Cfg.Paths.New, 'Seed', Base + 1e5);

    % usim: the values the design fixes; the rest are usim's defaults
    Cfg.Usim = struct('Jitter', Args.Jitter, 'JitterPar', Args.JitterPar, 'Vignetting', Args.Vignetting, ...
                      'Zodi', Args.Zodi, 'AddCRStreaks', Args.AddCRStreaks, 'NoisePoisson', true);
    Cfg.Usim = addArgs(Cfg.Usim, Args.UsimArgs, 'ultrasat.usim');

    Cfg.Truth  = struct('SNLimit', Args.SNLimit);
    Cfg.Header = struct('SimKeys', Args.SimKeys);
end

function S = addArgs(S, C, FunName)
    % add key,val to a stage, checking the keys against the arguments of its function
    if mod(numel(C), 2) ~= 0
        error('dut:config:Args', 'The further arguments of %s must be key,val pairs', FunName);
    end
    Keys = C(1:2:end);
    if ~isempty(Keys) && (exist('metafunction', 'builtin') ~= 0 || exist('metafunction', 'file') ~= 0)
        Known = fieldnames(ultrasat.simconfig.funDefaults(FunName));
        Bad   = setdiff(Keys, Known);
        if ~isempty(Bad)
            error('dut:config:UnknownArg', '%s has no argument %s', FunName, strjoin(Bad, ', '));
        end
    end
    for I = 1:numel(Keys)
        S.(Keys{I}) = C{2*I};
    end
end

function [RA, Dec, Off] = boresight(RAc, Decc, Tile, Roll, Tol)
    % the boresight that puts the center of Tile at (RAc, Decc) at the roll Roll
    %   Fixed-point iteration: the boresight is moved by the offset of the
    %   tile center from the target, with the tile geometry of usim
    %   (ultrasat.simulateCatalogField 'GeometryOnly').
    RA   = RAc;
    Dec  = Decc;
    It   = 0;
    Move = true;
    while Move
        [~, ~, G] = ultrasat.simulateCatalogField('RA0', RA, 'Dec0', Dec, 'Tile', Tile, 'PlaneRotation', Roll, ...
                                                  'GeometryOnly', true, 'Verbose', false);
        [CRA, CDec] = G.WCS.xy2sky((G.NX + 1)/2, (G.NY + 1)/2);
        [D, PA]     = celestial.coo.sphere_dist(CRA, CDec, RAc, Decc, 'deg');
        Off         = rad2deg(D) * 3600;     % [arcsec] of the current boresight
        It          = It + 1;
        Move        = Off > Tol && It < 20;
        if Move
            [Dec, RA] = reckon(Dec, RA, rad2deg(D), rad2deg(PA));
            RA        = mod(RA, 360);
        end
    end
    if Off > Tol
        error('dut:config:Boresight', 'The new-visit boresight did not converge: the tile center is %.3g arcsec off', Off);
    end
end
