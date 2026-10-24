function [Runs, Truth] = simulateNewSet(Args)
    % Simulate a set of ULTRASAT "new" images at one visit: transients added and fluxes of some stars changed
    %   1. The catalog of each tile at the visit (RA0, Dec0, PlaneRotation),
    %      from ultrasat.simulateGaiaField without simulation.
    %   2. Transients per tile: Ntransient at uniform random pixel positions
    %      (TransientMargin from the tile edges), peak U uniform in
    %      TransientMagRange, blackbody Teff uniform in TransientTeff.
    %   3. Variable stars per tile: Nvariable catalog stars with U_pred in
    %      VariableMagRange, each with a change dMag uniform in
    %      VariableDeltaMag.
    %   4. Light curves: TransientLC and VariableLC give the magnitude of
    %      each object at time T [s] since the start of the first image.
    %      [PLACEHOLDER] The default is constant: transients at the peak U,
    %      variables at U_pred + dMag in all images.
    %   5. Nimage images per tile with ultrasat.simulateRefSet (one pointing,
    %      one roll), each with its own transients and magnitude changes,
    %      and a truth table of every object in every image, written to
    %      <OutDir>/<OutName>_truth.csv.
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] the aimpoint of the visit. Default is the
    %                   Kepler field, 291, 44.5.
    %            'PlaneRotation' - [deg] camera roll of the visit. Default is 0.
    %                   To repeat a reference visit, use its RA0, Dec0 and
    %                   Roll from the ultrasat.simulateRefSet run table.
    %            'Tiles' - Default is 'ABCD'.
    %            'Nimage' - number of images per tile. Default is 100.
    %            'Exposure' - [number duration(s)] of each image. Default is [1 300].
    %            'DateObs' - start (UTC) of the first image, 'yyyy-mm-ddTHH:MM:SS'.
    %                   Default is '2026-07-01T00:00:00'.
    %            'ImageInterval' - [s] between consecutive images; [] (default)
    %                   is Exposure(1)*Exposure(2).
    %            'Ntransient' - transients per tile. Default is 10.
    %            'TransientMagRange' - [min max] of the peak U (observed AB).
    %                   Default is [18 22].
    %            'TransientTeff' - [K] blackbody temperature: a value, or
    %                   [min max] for a uniform draw. Default is 20000.
    %            'TransientMargin' - [pix] distance from the tile edges.
    %                   Default is 50.
    %            'TransientLC' - [] (default, constant); or a function handle
    %                   U = f(U0, T, Par): U0 a vector of peak magnitudes, T the
    %                   time [s] since the start of the first image.
    %            'TransientLCPar' - the Par of TransientLC. Default is [].
    %            'Nvariable' - stars per tile whose flux is changed. Default is 10.
    %            'VariableMagRange' - [min max] of U_pred of the chosen stars.
    %                   Default is [16 21].
    %            'VariableDeltaMag' - [min max] of the change dMag (positive:
    %                   fainter). Default is [-1 1].
    %            'VariableLC' - [] (default, constant); or a function handle
    %                   dMag = f(dMag0, T, Par), as TransientLC.
    %            'VariableLCPar' - the Par of VariableLC. Default is [].
    %            'SimArgs' - cell of further key,val for
    %                   ultrasat.simulateGaiaField (the same for the catalog
    %                   step and the images; 'UsimArgs' only for the images).
    %                   Default is {}.
    %            'OutDir' - Default is '.'.
    %            'OutName' - root of the file names. Default is 'New'.
    %            'Seed' - [] (default); or an integer: seeds the draws, and
    %                   image k gets Seed + k (as in ultrasat.simulateRefSet).
    %                   Use a Seed different from that of the reference set,
    %                   so that the noise of the new and reference images
    %                   is independent.
    %            'Parallel', 'Nworkers', 'NumThreads', 'SkipExisting',
    %            'StopOnError' - as in ultrasat.simulateRefSet.
    %            'Simulate' - false returns the run list and the truth table
    %                   only (the catalog step still runs). Default is true.
    %            'Verbose' - Default is true.
    % Output : - The run table of ultrasat.simulateRefSet.
    %          - Truth table, one row per object per image: OutName, Tile,
    %            Image, DateObs, T [s], ID, Kind ("transient" or "variable"),
    %            RA, Dec, X, Y, U0 (peak U of a transient, U_pred of a
    %            variable star), U (the magnitude in this image), Teff
    %            (transients).
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Runs, Truth] = ultrasat.simulateNewSet('Tiles','B','Nimage',3,'Simulate',false);
    %          [Runs, Truth] = ultrasat.simulateNewSet('PlaneRotation',36.5,'SimArgs',{'MaxG',17}, ...
    %                              'OutDir','/Data/new','Seed',1000,'Parallel',true,'Nworkers',12);

    arguments
        Args.RA0                   = 291;
        Args.Dec0                  = 44.5;
        Args.PlaneRotation         = 0;
        Args.Tiles                 = 'ABCD';
        Args.Nimage                = 100;
        Args.Exposure              = [1 300];
        Args.DateObs               = '2026-07-01T00:00:00';
        Args.ImageInterval         = [];
        Args.Ntransient            = 10;
        Args.TransientMagRange     = [18 22];
        Args.TransientTeff         = 20000;
        Args.TransientMargin       = 50;
        Args.TransientLC           = [];
        Args.TransientLCPar        = [];
        Args.Nvariable             = 10;
        Args.VariableMagRange      = [16 21];
        Args.VariableDeltaMag      = [-1 1];
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
    if ~isempty(Args.Seed)   % repeatable draws; the caller's generator state is restored on exit
        RngSaved   = rng;
        rng(Args.Seed);
        RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    end
    TransientLC = Args.TransientLC;
    if isempty(TransientLC)
        TransientLC = @(U0, T, Par) U0;                 % [PLACEHOLDER] constant
    end
    VariableLC = Args.VariableLC;
    if isempty(VariableLC)
        VariableLC = @(dMag0, T, Par) dMag0;            % [PLACEHOLDER] constant
    end

    % the catalog step takes SimArgs without the usim arguments
    CatArgs = Args.SimArgs;
    Iu = find(strcmpi(CatArgs(1:2:end), 'UsimArgs'), 1);
    if ~isempty(Iu)
        CatArgs(2*Iu-1:2*Iu) = [];
    end

    % 1.-3. per tile: catalog, transients, variable stars
    Tiles = cellstr(Args.Tiles(:));
    Obj   = cell(numel(Tiles), 1);
    for It = 1:numel(Tiles)
        [~, Src, Info] = ultrasat.simulateGaiaField('RA0', Args.RA0, 'Dec0', Args.Dec0, 'Tile', Tiles{It}, ...
                              'PlaneRotation', Args.PlaneRotation, 'Exposure', Args.Exposure, ...
                              CatArgs{:}, 'Simulate', false, 'Verbose', false);
        Nt = Args.Ntransient;
        M  = Args.TransientMargin;
        X  = M + 0.5 + (Info.NX - 2*M) .* rand(Nt, 1);
        Y  = M + 0.5 + (Info.NY - 2*M) .* rand(Nt, 1);
        [RA, Dec] = Info.WCS.xy2sky(X, Y);
        U0   = Args.TransientMagRange(1) + diff(Args.TransientMagRange) .* rand(Nt, 1);
        TeffR = Args.TransientTeff([1 end]);
        Teff = TeffR(1) + diff(TeffR) .* rand(Nt, 1);
        Tr = table("T" + (1:Nt)', repmat("transient", Nt, 1), RA(:), Dec(:), X, Y, U0, Teff, ...
                   'VariableNames', {'ID', 'Kind', 'RA', 'Dec', 'X', 'Y', 'U0', 'Teff'});

        Cand = find(Src.U_pred >= Args.VariableMagRange(1) & Src.U_pred <= Args.VariableMagRange(2));
        Nv   = min(Args.Nvariable, numel(Cand));
        if Nv < Args.Nvariable
            Say('tile %s: only %d stars in VariableMagRange, %d asked', Tiles{It}, numel(Cand), Args.Nvariable);
        end
        Iv   = Cand(randperm(numel(Cand), Nv));
        dM   = Args.VariableDeltaMag(1) + diff(Args.VariableDeltaMag) .* rand(Nv, 1);
        Va = table("V" + (1:Nv)', repmat("variable", Nv, 1), Src.RA(Iv), Src.Dec(Iv), Src.X(Iv), Src.Y(Iv), ...
                   Src.U_pred(Iv), NaN(Nv, 1), dM, ...
                   'VariableNames', {'ID', 'Kind', 'RA', 'Dec', 'X', 'Y', 'U0', 'Teff', 'dMag0'});
        Tr.dMag0 = NaN(Nt, 1);
        Obj{It}  = [Tr; Va];
        Say('tile %s: %d transients, %d variable stars', Tiles{It}, Nt, Nv);
    end
    ObjMap = containers.Map(Tiles, Obj);

    % 4. the runs, and the magnitude of every object in every image
    RefArgs = {'Pointings', [Args.RA0 Args.Dec0], 'Rolls', Args.PlaneRotation, 'Tiles', Args.Tiles, ...
               'Nimage', Args.Nimage, 'Exposure', Args.Exposure, 'DateObs', Args.DateObs, ...
               'ImageInterval', Args.ImageInterval, 'SimArgs', Args.SimArgs, ...
               'OutDir', Args.OutDir, 'OutName', Args.OutName, 'Seed', Args.Seed, ...
               'Parallel', Args.Parallel, 'Nworkers', Args.Nworkers, 'NumThreads', Args.NumThreads, ...
               'SkipExisting', Args.SkipExisting, 'StopOnError', Args.StopOnError, 'Verbose', Args.Verbose};
    Runs = ultrasat.simulateRefSet(RefArgs{:}, 'Simulate', false);
    Fmt  = 'yyyy-MM-dd''T''HH:mm:ss';
    T0   = datetime(Runs.DateObs(1), 'InputFormat', Fmt);
    Tsec = seconds(datetime(Runs.DateObs, 'InputFormat', Fmt) - T0);

    Parts = cell(height(Runs), 1);
    for K = 1:height(Runs)
        O = ObjMap(char(Runs.Tile(K)));
        U = O.U0;
        Itr = O.Kind == "transient";
        U(Itr)  = TransientLC(O.U0(Itr), Tsec(K), Args.TransientLCPar);
        U(~Itr) = O.U0(~Itr) + VariableLC(O.dMag0(~Itr), Tsec(K), Args.VariableLCPar);
        N = height(O);
        Parts{K} = [table(repmat(Runs.OutName(K), N, 1), repmat(Runs.Tile(K), N, 1), repmat(Runs.Image(K), N, 1), ...
                          repmat(Runs.DateObs(K), N, 1), repmat(Tsec(K), N, 1), ...
                          'VariableNames', {'OutName', 'Tile', 'Image', 'DateObs', 'T'}), ...
                    O(:, {'ID', 'Kind', 'RA', 'Dec', 'X', 'Y', 'U0'}), table(U, O.Teff, 'VariableNames', {'U', 'Teff'})];
    end
    Truth = vertcat(Parts{:});
    if ~isfolder(Args.OutDir)
        mkdir(Args.OutDir);
    end
    TruthFile = fullfile(Args.OutDir, sprintf('%s_truth.csv', Args.OutName));
    writetable(Truth, TruthFile);
    Say('truth: %d rows in %s', height(Truth), TruthFile);

    % 5. simulate: each run gets the transients and magnitude changes of its image
    if Args.Simulate
        RunArgs = @(R) runSources(Truth(Truth.OutName == R.OutName & Truth.Tile == R.Tile, :));
        Runs = ultrasat.simulateRefSet(RefArgs{:}, 'RunArgs', RunArgs);
    end
end

function C = runSources(T)
    % ExtraSrc (transients) and MagChange (variable stars) of one image
    Tr = T(T.Kind == "transient", :);
    Va = T(T.Kind == "variable", :);
    C  = {'ExtraSrc',  table(Tr.X, Tr.Y, Tr.U, Tr.Teff, 'VariableNames', {'X', 'Y', 'U', 'Teff'}), ...
          'MagChange', table(Va.RA, Va.Dec, Va.U - Va.U0, 'VariableNames', {'RA', 'Dec', 'dMag'})};
end
