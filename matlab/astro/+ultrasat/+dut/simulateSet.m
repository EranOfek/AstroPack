function Out = simulateSet(Cfg, Args)
    % Simulate a DUT set: the configuration record, the source catalog, the reference images and the new visits
    %   All parameters come from Cfg (ultrasat.dut.config); the arguments
    %   here are run-time options only.
    %   1. The configuration is saved in Cfg.Paths.Config
    %      (ultrasat.simconfig.write: a changed configuration under a saved
    %      version is refused).
    %   2. The source catalog (ultrasat.dut.buildSourceCatalog), unless its
    %      file exists.
    %   3. 'ref': the reference images by ultrasat.simulateRefSet with
    %      Cfg.Ref, each tile by ultrasat.dut.simulateTile (an image and its
    %      truth catalog); the run log is <Paths.Ref>/Ref_runs.csv.
    %   4. 'new': the star field of the new tile is rendered once (Mode
    %      'base'); then Cfg.New.Nvisit visits, each with Ntransient
    %      transients added to that source image and its own noise (Seed:
    %      New.Seed + visit). Transients of a visit: the radii (from the
    %      tile's inner corner) evenly spaced over RadiusRange, each at a
    %      random azimuth among the positions on the tile, the magnitudes
    %      MagGrid in a random order. The run log is <Paths.New>/New_runs.csv.
    %   SkipExisting: an image whose truth catalog exists (it is written
    %   last) is not made again, so an interrupted set can be resumed.
    % Input  : - Cfg: the configuration (ultrasat.dut.config).
    %          * ...,key,val,...
    %            'Stage' - 'all' (default), 'catalog', 'ref' or 'new'.
    %            'Visits' - [] (default): all; or a vector of visit numbers.
    %            'SkipExisting' - Default is true.
    %            'Parallel', 'Nworkers', 'NumThreads' - for the references,
    %                   as in ultrasat.simulateRefSet. Default false, [], [].
    %            'Simulate' - false returns the run lists only. Default is true.
    %            'Verbose' - Default is true.
    % Output : - Out: struct with .ConfigFile, .Catalog (file), .RefRuns,
    %               .NewRuns (tables).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          Out = ultrasat.dut.simulateSet(Cfg, 'Stage', 'new', 'Visits', 1:2);

    arguments
        Cfg (1,1) struct
        Args.Stage (1,:) char {mustBeMember(Args.Stage, {'all', 'catalog', 'ref', 'new'})} = 'all'
        Args.Visits double = []
        Args.SkipExisting (1,1) logical = true
        Args.Parallel (1,1) logical = false
        Args.Nworkers = []
        Args.NumThreads = []
        Args.Simulate (1,1) logical = true
        Args.Verbose (1,1) logical = true
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    Do  = @(S) strcmp(Args.Stage, 'all') || strcmp(Args.Stage, S);

    % 1. the configuration record
    [CfgFile, St, Cfg] = ultrasat.simconfig.write(Cfg, Cfg.Paths.Config);
    Say('%s: configuration %s (%s)', Cfg.Meta.Version, CfgFile, St);
    Out = struct('ConfigFile', CfgFile, 'Catalog', Cfg.Catalog.OutFile, 'RefRuns', table(), 'NewRuns', table());

    % 2. the source catalog
    if Args.Simulate && ~isfile(Cfg.Catalog.OutFile)
        C = ultrasat.simconfig.toArgs(Cfg.Catalog, {'Header', ultrasat.simconfig.headerRows(Cfg), 'Verbose', Args.Verbose});
        ultrasat.dut.buildSourceCatalog(C{:});
    end

    % 3. the references
    if Do('ref')
        C = ultrasat.simconfig.toArgs(Cfg.Ref);
        Out.RefRuns = ultrasat.simulateRefSet(C{:}, 'OutName', 'Ref', 'SimFun', @ultrasat.dut.simulateTile, ...
            'SimArgs', {'ConfigFile', CfgFile, 'Kind', 'ref'}, ...
            'RunArgs', @(R) {'Pointing', R.Pointing, 'Image', R.Image, 'Epoch', R.RollIndex}, ...
            'OutFileFun', @(R) ultrasat.dut.productName(Cfg, 'ref', char(R.Tile), char(R.DateObs), ...
                                   'Pointing', R.Pointing, 'Image', R.Image, 'Product', 'truth'), ...
            'SkipExisting', Args.SkipExisting, 'Parallel', Args.Parallel, 'Nworkers', Args.Nworkers, ...
            'NumThreads', Args.NumThreads, 'Simulate', Args.Simulate, 'Verbose', Args.Verbose, 'StopOnError', false);
    end

    % 4. the new visits
    if Do('new')
        Out.NewRuns = newVisits(Cfg, CfgFile, Args, Say);
    end
end

function Runs = newVisits(Cfg, CfgFile, Args, Say)
    % the base star field, then the visits with transients
    N = Cfg.New;
    Visits = Args.Visits;
    if isempty(Visits)
        Visits = 1:N.Nvisit;
    end
    Visits = Visits(:);
    Geo = {'RA0', N.RA0, 'Dec0', N.Dec0, 'Tile', N.Tile, 'PlaneRotation', N.Roll, 'Exposure', N.Exposure, ...
           'ConfigFile', CfgFile, 'Kind', 'new'};
    Nv   = numel(Visits);
    Runs = table(Visits, strings(Nv,1), nan(Nv,1), repmat("planned", Nv, 1), nan(Nv,1), strings(Nv,1), ...
                 'VariableNames', {'Visit', 'DateObs', 'Seed', 'Status', 'Duration', 'Message'});
    Runs.DateObs = N.DateObs(Visits)';
    Runs.Seed    = N.Seed + Visits;
    if Args.Simulate
        Dir     = Cfg.Paths.New;
        LogFile = fullfile(Dir, 'New_runs.csv');
        SrcFile = fullfile(Dir, sprintf('NewBase_tile%s_SrcImage.mat', N.Tile));
        BaseTr  = fullfile(Dir, sprintf('NewBase_tile%s_truth.mat', N.Tile));
        if ~isfile(SrcFile) || ~isfile(BaseTr)
            T0 = tic;
            ultrasat.dut.simulateTile(Geo{:}, 'DateObs', char(N.DateObs(1)), 'Seed', N.Seed, 'Mode', 'base');
            Say('new: the star field of tile %s rendered in %.0f s', N.Tile, toc(T0));
        end
        for K = 1:Nv
            V = Visits(K);
            TruthFile = ultrasat.dut.productName(Cfg, 'new', N.Tile, char(Runs.DateObs(K)), 'Product', 'truth');
            if Args.SkipExisting && isfile(TruthFile)
                Runs.Status(K) = "skipped";
            else
                T0 = tic;
                try
                    Tr = transients(Cfg, Runs.Seed(K));
                    ultrasat.dut.simulateTile(Geo{:}, 'DateObs', char(Runs.DateObs(K)), 'Seed', Runs.Seed(K), ...
                        'Visit', V, 'Stars', false, 'Transients', Tr, 'AddSrcImage', SrcFile, 'TruthBase', BaseTr);
                    Runs.Status(K) = "done";
                catch ME
                    Runs.Status(K)  = "failed";
                    Runs.Message(K) = string(ME.message);
                end
                Runs.Duration(K) = toc(T0);
                Say('new visit %d (%d/%d): %s in %.0f s %s', V, K, Nv, Runs.Status(K), Runs.Duration(K), Runs.Message(K));
            end
            writetable(Runs, LogFile);
        end
        Say('new: done %d, skipped %d, failed %d; log in %s', sum(Runs.Status == "done"), ...
            sum(Runs.Status == "skipped"), sum(Runs.Status == "failed"), LogFile);
    end
end

function Tr = transients(Cfg, Seed)
    % the transients of one visit: radii evenly over RadiusRange, random azimuths on the tile, magnitudes in random order
    N = Cfg.New;
    RngSaved   = rng;
    rng(Seed);
    RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    NX = 4738;
    PixSizeDeg = (9.5e-3 / 360) * 180/pi;      % as in ultrasat.usim
    Side = NX - 1;                             % pixels from the inner corner (0.5) to the far edge, inside the tile
    Rk  = linspace(N.RadiusRange(1), N.RadiusRange(2), N.Ntransient)' ./ PixSizeDeg;   % [pix]
    if any(Rk > Side * sqrt(2))
        error('dut:simulateSet:Radius', 'RadiusRange reaches beyond the tile corner (%.2f deg)', Side*sqrt(2)*PixSizeDeg);
    end
    Lo  = acos(min(1, Side ./ Rk));            % the azimuths [rad] at which radius Rk stays on the tile
    Hi  = asin(min(1, Side ./ Rk));
    Phi = Lo + rand(N.Ntransient, 1) .* (Hi - Lo);
    Mag = N.MagGrid(:);
    Mag = Mag(randperm(numel(Mag)));
    Mag = Mag(1 + mod(0:N.Ntransient-1, numel(Mag)))';
    switch upper(N.Tile)                       % the inner corner and the directions into the tile
        case 'A'
            X0 = NX + 0.5; Y0 = 0.5; Sx = -1; Sy = 1;
        case 'B'
            X0 = 0.5; Y0 = 0.5; Sx = 1; Sy = 1;
        case 'C'
            X0 = 0.5; Y0 = NX + 0.5; Sx = 1; Sy = -1;
        otherwise
            X0 = NX + 0.5; Y0 = NX + 0.5; Sx = -1; Sy = -1;
    end
    Tr = table(X0 + Sx .* Rk .* cos(Phi), Y0 + Sy .* Rk .* sin(Phi), Mag(:), Rk .* PixSizeDeg, ...
               'VariableNames', {'X', 'Y', 'U', 'R'});
end
