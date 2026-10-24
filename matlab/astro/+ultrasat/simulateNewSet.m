function [Runs, Truth] = simulateNewSet(Args)
    % Simulate a set of ULTRASAT "new" images: produced as a reference set, with transients added and fluxes of some stars changed
    %   The images are made exactly as by ultrasat.simulateRefSet (the same
    %   pointings, rolls, tiles, images and times arguments, passed on
    %   unchanged), and each image also gets the transients and the
    %   magnitude changes that fall on its tile. For a set at one visit
    %   (e.g. ~100 images at one reference position), give one pointing and
    %   one roll: 'Pointings',[RA Dec],'Rolls',Roll.
    %   1. Transients: Ntransient pseudo-stars added to the catalog of the
    %      field: uniform on the sky within FieldRadius of (RA0, Dec0), only
    %      at positions that fall on a tile in at least one image; peak U in
    %      TransientMagRange with the distribution TransientMagDist; a
    %      blackbody (Teff uniform in TransientTeff) or a Pickles template
    %      (uniform choice from TransientTemplates) spectrum.
    %   2. Variable stars: Nvariable catalog stars of the field (the tiles at
    %      (RA0, Dec0), roll VariableRoll) with U_pred in VariableMagRange,
    %      each with a change dMag uniform in VariableDeltaMag.
    %   3. Light curves: TransientLC and VariableLC give the magnitude at
    %      time T [s] since the start of the first image of the set.
    %      [PLACEHOLDER] The default is constant: transients at the peak U,
    %      variables at U_pred + dMag in all images.
    %   4. Truth table: every object that falls on the tile of every image,
    %      written to <OutDir>/<OutName>_truth.csv.
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] the field center (the dither center of
    %                   ultrasat.simulateRefSet). Default is the Kepler field,
    %                   291, 44.5.
    %            'Pointings', 'DitherLeg', 'DitherPattern', 'DitherNgrid',
    %            'IncludeCenter', 'Rolls', 'Nroll', 'RollStart', 'RollStep',
    %            'RollJitter', 'Tiles', 'Nimage', 'Exposure', 'DateObs',
    %            'ImageInterval', 'VisitInterval' - as in
    %                   ultrasat.simulateRefSet; [] (default) takes its default.
    %            'FieldRadius' - [deg] radius of the field for the transients;
    %                   [] (default) is the largest distance of a tile corner
    %                   from (RA0, Dec0) over all pointings and rolls.
    %            'Ntransient' - transients in the field. Default is 40.
    %            'TransientMagRange' - [min max] of the peak U (observed AB).
    %                   Default is [18 22].
    %            'TransientMagDist' - distribution of the peak U in the range:
    %                   'uniform' (default) or 'powerlaw' (dN/dU proportional
    %                   to 10^(TransientMagSlope*U)).
    %            'TransientMagSlope' - slope of 'powerlaw'. Default is 0.6
    %                   (Euclidean counts).
    %            'TransientSpec' - 'bb' (default): blackbody with
    %                   TransientTeff; or 'pickles': a template drawn
    %                   uniformly from TransientTemplates.
    %            'TransientTeff' - [K] blackbody temperature: a value, or
    %                   [min max] for a uniform draw. Default is 20000.
    %            'TransientTemplates' - Pickles names for 'pickles'. Default is
    %                   ["a0v" "f5v" "g2v" "k0v" "m0v"].
    %            'MaxTries' - number of batches of Ntransient candidate
    %                   positions drawn before giving up on finding Ntransient
    %                   covered positions. Default is 100.
    %            'TransientLC' - [] (default, constant); or a function handle
    %                   U = f(U0, T, Par): U0 a vector of peak magnitudes, T the
    %                   time [s] since the start of the first image.
    %            'TransientLCPar' - the Par of TransientLC. Default is [].
    %            'Nvariable' - stars in the field whose flux is changed.
    %                   Default is 40.
    %            'VariableMagRange' - [min max] of U_pred of the chosen stars.
    %                   Default is [16 21].
    %            'VariableDeltaMag' - [min max] of the change dMag (positive:
    %                   fainter). Default is [-1 1].
    %            'VariableRoll' - [deg] roll of the tiles at (RA0, Dec0) from
    %                   whose catalog the variable stars are chosen. Default is 0.
    %            'VariableLC' - [] (default, constant); or a function handle
    %                   dMag = f(dMag0, T, Par), as TransientLC.
    %            'VariableLCPar' - the Par of VariableLC. Default is [].
    %            'SimArgs' - cell of further key,val for
    %                   ultrasat.simulateCatalogField (the same for the catalog
    %                   step and the images; 'UsimArgs' only for the images).
    %                   Default is {}.
    %            'OutDir' - Default is '.'.
    %            'OutName' - root of the file names. Default is 'New'.
    %            'Seed' - [] (default) draws one at random; or an integer: seeds
    %                   the draws (and the roll offsets), and image k gets
    %                   Seed + k, as in ultrasat.simulateRefSet. The Seed used
    %                   is in the run table. Use a Seed different from
    %                   that of the reference set, so that the noise of the
    %                   new and reference images is independent.
    %            'Parallel', 'Nworkers', 'NumThreads', 'SkipExisting',
    %            'StopOnError' - as in ultrasat.simulateRefSet.
    %            'Simulate' - false returns the run list and the truth table
    %                   only (the catalog step still runs). Default is true.
    %            'Verbose' - Default is true.
    % Output : - The run table of ultrasat.simulateRefSet.
    %          - Truth table, one row per object per image in which it falls
    %            on the tile: OutName, Pointing, RollIndex, Tile, Image,
    %            DateObs, T [s], ID, Kind ("transient" or "variable"), RA, Dec,
    %            X, Y [tile pixels], U0 (peak U of a transient, U_pred of a
    %            variable star), U (the magnitude in this image), Teff and
    %            Template (transients; Template "" for a blackbody).
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Runs, Truth] = ultrasat.simulateNewSet('Tiles','B','Nroll',2,'Nimage',1,'Simulate',false);
    %          % DP1: as the reference set, other seed
    %          [Runs, Truth] = ultrasat.simulateNewSet('SimArgs',{'MaxG',17},'OutDir','/Data/new','Seed',1000, ...
    %                              'Parallel',true,'Nworkers',12);
    %          % DUT: 100 images at one reference visit
    %          [Runs, Truth] = ultrasat.simulateNewSet('Pointings',[293.03 45.90],'Rolls',36.5,'Nimage',100, ...
    %                              'SimArgs',{'MaxG',17},'OutDir','/Data/dut_new','Seed',2000);

    arguments
        Args.RA0                   = 291;
        Args.Dec0                  = 44.5;
        Args.Pointings             = [];
        Args.DitherLeg             = [];
        Args.DitherPattern         = [];
        Args.DitherNgrid           = [];
        Args.IncludeCenter         = [];
        Args.Rolls                 = [];
        Args.Nroll                 = [];
        Args.RollStart             = [];
        Args.RollStep              = [];
        Args.RollJitter            = [];
        Args.Tiles                 = [];
        Args.Nimage                = [];
        Args.Exposure              = [];
        Args.DateObs               = [];
        Args.ImageInterval         = [];
        Args.VisitInterval         = [];
        Args.FieldRadius           = [];
        Args.Ntransient            = 40;
        Args.TransientMagRange     = [18 22];
        Args.TransientMagDist      = 'uniform';
        Args.TransientMagSlope     = 0.6;
        Args.TransientSpec         = 'bb';
        Args.TransientTeff         = 20000;
        Args.TransientTemplates    = ["a0v" "f5v" "g2v" "k0v" "m0v"];
        Args.MaxTries              = 100;
        Args.TransientLC           = [];
        Args.TransientLCPar        = [];
        Args.Nvariable             = 40;
        Args.VariableMagRange      = [16 21];
        Args.VariableDeltaMag      = [-1 1];
        Args.VariableRoll          = 0;
        Args.VariableLC            = [];
        Args.VariableLCPar         = [];
        Args.SimArgs cell          = {};
        Args.OutDir                = '.';
        Args.OutName               = 'New';
        Args.Seed                  = [];
        Args.Parallel logical      = false;
        Args.Nworkers              = [];
        Args.NumThreads            = [];
        Args.SkipExisting logical  = true;
        Args.StopOnError logical   = false;
        Args.Simulate logical      = true;
        Args.Verbose logical       = true;
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    % one seed for the draws here and for both simulateRefSet calls (the run list
    % and the simulation must have the same roll offsets)
    if isempty(Args.Seed)
        Args.Seed = randi(2^31 - 1 - 1e6);
    end
    RngSaved   = rng;   % the caller's generator state is restored on exit
    rng(Args.Seed);
    RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    TransientLC = Args.TransientLC;
    if isempty(TransientLC)
        TransientLC = @(U0, T, Par) U0;                 % [PLACEHOLDER] constant
    end
    VariableLC = Args.VariableLC;
    if isempty(VariableLC)
        VariableLC = @(dMag0, T, Par) dMag0;            % [PLACEHOLDER] constant
    end

    % the geometry arguments given here go to simulateRefSet as they are
    GeomNames = {'Pointings', 'DitherLeg', 'DitherPattern', 'DitherNgrid', 'IncludeCenter', 'Rolls', 'Nroll', ...
                 'RollStart', 'RollStep', 'RollJitter', 'Tiles', 'Nimage', 'Exposure', 'DateObs', ...
                 'ImageInterval', 'VisitInterval'};
    Given    = GeomNames(cellfun(@(F) ~isempty(Args.(F)), GeomNames));
    GeomArgs = [Given; cellfun(@(F) Args.(F), Given, 'UniformOutput', false)];
    RefArgs  = [{'RA0', Args.RA0, 'Dec0', Args.Dec0}, GeomArgs(:)', ...
                {'SimArgs', Args.SimArgs, 'OutDir', Args.OutDir, 'OutName', Args.OutName, 'Seed', Args.Seed, ...
                 'Parallel', Args.Parallel, 'Nworkers', Args.Nworkers, 'NumThreads', Args.NumThreads, ...
                 'SkipExisting', Args.SkipExisting, 'StopOnError', Args.StopOnError, 'Verbose', Args.Verbose}];
    Runs = ultrasat.simulateRefSet(RefArgs{:}, 'Simulate', false);

    % the catalog step takes SimArgs without the usim arguments
    CatArgs = Args.SimArgs;
    Iu = find(strcmpi(CatArgs(1:2:end), 'UsimArgs'), 1);
    if ~isempty(Iu)
        CatArgs(2*Iu-1:2*Iu) = [];
    end

    % the tile geometry of every (pointing, roll, tile) of the set
    Key  = compose('%d_%d_%s', Runs.Pointing, Runs.RollIndex, Runs.Tile);
    [UKey, Iu1] = unique(Key, 'stable');
    Geo  = containers.Map();
    Rmax = 0;
    for K = 1:numel(UKey)
        R = Runs(Iu1(K), :);
        [~, ~, Info] = ultrasat.simulateCatalogField('RA0', R.RA0, 'Dec0', R.Dec0, 'Tile', char(R.Tile), ...
                                                  'PlaneRotation', R.Roll, 'GeometryOnly', true, 'Verbose', false);
        Geo(UKey{K}) = Info;
        [CRA, CDec]  = Info.WCS.xy2sky([0.5 Info.NX+0.5 0.5 Info.NX+0.5], [0.5 0.5 Info.NY+0.5 Info.NY+0.5]);
        Rmax = max([Rmax; rad2deg(celestial.coo.sphere_dist(deg2rad(Args.RA0), deg2rad(Args.Dec0), ...
                                                            deg2rad(CRA(:)), deg2rad(CDec(:))))]);
    end
    FieldRadius = Args.FieldRadius;
    if isempty(FieldRadius)
        FieldRadius = Rmax;
    end

    % 1. transients: uniform on the sphere within FieldRadius, kept where some image covers them
    Nt    = Args.Ntransient;
    TRA   = zeros(0, 1);
    TDec  = zeros(0, 1);
    Ntry  = 0;
    Infos = values(Geo);
    while numel(TRA) < Nt && Ntry < Args.MaxTries
        Ntry = Ntry + 1;
        Dist = acosd(1 - (1 - cosd(FieldRadius)) .* rand(Nt, 1));
        Az   = 360 .* rand(Nt, 1);
        [CDec, CRA] = reckon(Args.Dec0, Args.RA0, Dist, Az);
        CRA  = mod(CRA, 360);
        Cov  = false(Nt, 1);
        for K = 1:numel(Infos)
            [X, Y] = Infos{K}.WCS.sky2xy(CRA, CDec);
            Cov = Cov | (X(:) > 0.1 & Y(:) > 0.1 & X(:) < Infos{K}.NX & Y(:) < Infos{K}.NY);
        end
        TRA  = [TRA; CRA(Cov)];   %#ok<AGROW>
        TDec = [TDec; CDec(Cov)]; %#ok<AGROW>
    end
    if numel(TRA) < Nt
        error('simulateNewSet: %d of %d transient positions covered after %d tries', numel(TRA), Nt, Ntry);
    end
    TRA  = TRA(1:Nt);
    TDec = TDec(1:Nt);
    Lim  = Args.TransientMagRange;
    switch lower(Args.TransientMagDist)
        case 'uniform'
            U0 = Lim(1) + diff(Lim) .* rand(Nt, 1);
        case 'powerlaw'
            A  = Args.TransientMagSlope;
            U0 = log10(10.^(A*Lim(1)) + (10.^(A*Lim(2)) - 10.^(A*Lim(1))) .* rand(Nt, 1)) ./ A;
        otherwise
            error('simulateNewSet: TransientMagDist must be ''uniform'' or ''powerlaw''');
    end
    TeffR = Args.TransientTeff([1 end]);
    Teff  = TeffR(1) + diff(TeffR) .* rand(Nt, 1);
    Tmpl  = strings(Nt, 1);
    switch lower(Args.TransientSpec)
        case 'bb'
        case 'pickles'
            List = string(Args.TransientTemplates);
            Tmpl = List(randi(numel(List), Nt, 1));
            Tmpl = Tmpl(:);
            Teff = NaN(Nt, 1);
        otherwise
            error('simulateNewSet: TransientSpec must be ''bb'' or ''pickles''');
    end
    Obj  = table("T" + (1:Nt)', repmat("transient", Nt, 1), TRA, TDec, U0, Teff, Tmpl, NaN(Nt, 1), ...
                 'VariableNames', {'ID', 'Kind', 'RA', 'Dec', 'U0', 'Teff', 'Template', 'dMag0'});

    % 2. variable stars from the catalog of the field
    Tiles = unique(Runs.Tile, 'stable');
    Cat   = cell(numel(Tiles), 1);
    for It = 1:numel(Tiles)
        [~, Src] = ultrasat.simulateCatalogField('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', char(Tiles(It)), ...
                                              'PlaneRotation', Args.VariableRoll, CatArgs{:}, ...
                                              'Simulate', false, 'Verbose', false);
        Cat{It} = Src(:, {'RA', 'Dec', 'U_pred'});
    end
    Cat  = vertcat(Cat{:});
    Cand = find(Cat.U_pred >= Args.VariableMagRange(1) & Cat.U_pred <= Args.VariableMagRange(2));
    Nv   = min(Args.Nvariable, numel(Cand));
    if Nv < Args.Nvariable
        Say('only %d stars in VariableMagRange, %d asked', numel(Cand), Args.Nvariable);
    end
    Iv   = Cand(randperm(numel(Cand), Nv));
    dM   = Args.VariableDeltaMag(1) + diff(Args.VariableDeltaMag) .* rand(Nv, 1);
    Obj  = [Obj; table("V" + (1:Nv)', repmat("variable", Nv, 1), Cat.RA(Iv), Cat.Dec(Iv), Cat.U_pred(Iv), ...
                       NaN(Nv, 1), strings(Nv, 1), dM, ...
                       'VariableNames', {'ID', 'Kind', 'RA', 'Dec', 'U0', 'Teff', 'Template', 'dMag0'})];
    Say('%d transients within %.2f deg (all on a tile in some image), %d variable stars', Nt, FieldRadius, Nv);

    % 3.-4. the objects on the tile of every image, and their magnitudes
    Fmt   = 'yyyy-MM-dd''T''HH:mm:ss';
    T0    = datetime(Runs.DateObs(1), 'InputFormat', Fmt);
    Tsec  = seconds(datetime(Runs.DateObs, 'InputFormat', Fmt) - T0);
    Itr   = Obj.Kind == "transient";
    Parts = cell(height(Runs), 1);
    for K = 1:height(Runs)
        Info   = Geo(Key{K});
        [X, Y] = Info.WCS.sky2xy(Obj.RA, Obj.Dec);
        On     = X(:) > 0.1 & Y(:) > 0.1 & X(:) < Info.NX & Y(:) < Info.NY;   % the usim FOV test
        U      = NaN(height(Obj), 1);
        U(Itr)  = TransientLC(Obj.U0(Itr), Tsec(K), Args.TransientLCPar);
        U(~Itr) = Obj.U0(~Itr) + VariableLC(Obj.dMag0(~Itr), Tsec(K), Args.VariableLCPar);
        N = sum(On);
        Parts{K} = [table(repmat(Runs.OutName(K), N, 1), repmat(Runs.Pointing(K), N, 1), repmat(Runs.RollIndex(K), N, 1), ...
                          repmat(Runs.Tile(K), N, 1), repmat(Runs.Image(K), N, 1), repmat(Runs.DateObs(K), N, 1), ...
                          repmat(Tsec(K), N, 1), ...
                          'VariableNames', {'OutName', 'Pointing', 'RollIndex', 'Tile', 'Image', 'DateObs', 'T'}), ...
                    Obj(On, {'ID', 'Kind', 'RA', 'Dec'}), ...
                    table(X(On), Y(On), Obj.U0(On), U(On), Obj.Teff(On), Obj.Template(On), ...
                          'VariableNames', {'X', 'Y', 'U0', 'U', 'Teff', 'Template'})];
    end
    Truth = vertcat(Parts{:});
    if ~isfolder(Args.OutDir)
        mkdir(Args.OutDir);
    end
    TruthFile = fullfile(Args.OutDir, sprintf('%s_truth.csv', Args.OutName));
    writetable(Truth, TruthFile);
    Say('truth: %d rows in %s', height(Truth), TruthFile);

    % 5. simulate: each run gets the transients and magnitude changes on its tile
    if Args.Simulate
        RunArgs = @(R) runSources(Truth(Truth.OutName == R.OutName & Truth.Tile == R.Tile, :));
        Runs = ultrasat.simulateRefSet(RefArgs{:}, 'RunArgs', RunArgs);
    end
end

function C = runSources(T)
    % ExtraSrc (transients) and MagChange (variable stars) of one image
    Tr = T(T.Kind == "transient", :);
    Va = T(T.Kind == "variable", :);
    C  = {'ExtraSrc',  table(Tr.X, Tr.Y, Tr.U, Tr.Teff, Tr.Template, 'VariableNames', {'X', 'Y', 'U', 'Teff', 'Template'}), ...
          'MagChange', table(Va.RA, Va.Dec, Va.U - Va.U0, 'VariableNames', {'RA', 'Dec', 'dMag'})};
end
