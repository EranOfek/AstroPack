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
    %   4. 'new': the Npool star images of the new tile (Cfg.New.Pool: the
    %      boresight with a pointing offset each) are rendered once (Mode
    %      'base': NewBase_Pkk_tile<T>_SrcImage.mat and _StarCat.fits); then
    %      Cfg.New.Nvisit visits of Nexp exposures. A visit has Ntransient
    %      transients and Nexp different star images of the pool, drawn with
    %      New.Seed + visit; each exposure adds the transients to its star
    %      image with its own noise. Transients of a visit: one per MagGrid
    %      value (TransientID k has MagGrid(k)), the radius (from the tile's
    %      inner corner) uniform over RadiusRange, the azimuth uniform among
    %      the positions on the tile at least EdgeMargin inside it, redrawn
    %      until no source of Cfg.New.ExcludeCatalogs and no other transient
    %      of the visit is within ExcludeRadius; the positions are drawn at
    %      the nominal boresight and kept in RA, Dec, so each exposure sees
    %      them shifted by its pointing offset. All transients are listed in
    %      <Paths.New>/Transients.csv, the runs (one row per exposure) in
    %      <Paths.New>/New_runs.csv.
    %   SkipExisting: an image whose truth catalog exists (it is written
    %   last) is not made again, so an interrupted set can be resumed.
    % Input  : - Cfg: the configuration (ultrasat.dut.config).
    %          * ...,key,val,...
    %            'Stage' - 'all' (default), 'catalog', 'ref' or 'new'.
    %            'Visits' - [] (default): all; or a vector of visit numbers.
    %            'SkipExisting' - Default is true.
    %            'Parallel', 'Nworkers', 'NumThreads' - the references, the
    %                   star images and the new exposures on a pool of workers,
    %                   as in ultrasat.simulateRefSet. Default false, [], [].
    %            'Simulate' - false returns the run lists only. Default is true.
    %            'Verbose' - Default is true.
    % Output : - Out: struct with .ConfigFile, .Catalog (file), .RefRuns,
    %               .NewRuns (tables).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Cfg = ultrasat.dut.config;
    %          Out = ultrasat.dut.simulateSet(Cfg, 'Stage', 'new', 'Visits', 1:2);
    %          Out = ultrasat.dut.simulateSet(Cfg, 'Stage', 'new', 'Simulate', false);  % the run list

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

    % 2. the source catalog, and the catalogs of the other SEDs whose sources the transients avoid
    if Args.Simulate
        Files = [{Cfg.Catalog.OutFile}, Cfg.New.ExcludeCatalogs(:)'];
        SEDs  = [{Cfg.Catalog.SED}, Cfg.New.ExcludeSED(:)'];
        [Files, Iu] = unique(Files, 'stable');
        SEDs  = SEDs(Iu);
        for K = 1:numel(Files)
            if ~isfile(Files{K})
                Cc = Cfg.Catalog;
                Cc.SED = SEDs{K};
                Cc.OutFile = Files{K};
                C = ultrasat.simconfig.toArgs(Cc, {'Header', ultrasat.simconfig.headerRows(Cfg), 'Verbose', Args.Verbose});
                ultrasat.dut.buildSourceCatalog(C{:});
            end
        end
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
    % the star images of the pool, then the exposures of the visits
    N = Cfg.New;
    Visits = Args.Visits;
    if isempty(Visits)
        Visits = 1:N.Nvisit;
    end
    Visits = Visits(:);
    Nv  = numel(Visits);
    Ne  = N.Nexp;
    Np  = numel(N.Pool.RA0);
    Fmt = 'yyyy-MM-dd''T''HH:mm:ss';
    Dir = Cfg.Paths.New;
    Geo = {'Tile', N.Tile, 'PlaneRotation', N.Roll, 'Exposure', N.Exposure, 'ConfigFile', CfgFile, 'Kind', 'new'};
    BaseName = arrayfun(@(K) sprintf('NewBase_P%02d', K), (1:Np)', 'UniformOutput', false);
    SrcFile  = fullfile(Dir, strcat(BaseName, sprintf('_tile%s_SrcImage.mat', N.Tile)));
    StarName = strcat(BaseName, sprintf('_tile%s_StarCat.fits', N.Tile));
    StarFile = fullfile(Dir, StarName);

    % the run list: one row per exposure
    [Iv, Ie] = ndgrid(1:Nv, 1:Ne);
    Iv = reshape(Iv', [], 1);
    Ie = reshape(Ie', [], 1);
    PoolOf = zeros(Nv, Ne);
    for K = 1:Nv
        PoolOf(K,:) = drawVisit(N, Visits(K));
    end
    P    = PoolOf(sub2ind([Nv Ne], Iv, Ie));
    Date = datetime(N.DateObs(Visits(Iv)), 'InputFormat', Fmt)' + seconds((Ie - 1) .* prod(N.Exposure));
    Nr   = numel(Iv);
    Runs = table(Visits(Iv), Ie, P, N.Pool.RA0(P), N.Pool.Dec0(P), N.Pool.dRA(P), N.Pool.dDec(P), string(Date, Fmt), ...
                 N.Seed + 2e5 + Ne .* (Visits(Iv) - 1) + Ie, repmat("planned", Nr, 1), nan(Nr, 1), strings(Nr, 1), ...
                 'VariableNames', {'Visit', 'Exp', 'Pool', 'RA0', 'Dec0', 'dRA', 'dDec', 'DateObs', 'Seed', ...
                                   'Status', 'Duration', 'Message'});
    if Args.Simulate
        if ~isfolder(Dir)
            mkdir(Dir);
        end
        LogFile = fullfile(Dir, 'New_runs.csv');
        Pool = [];
        if Args.Parallel
            Pool = workerPool(Args.Nworkers);
            Say('parallel: %d workers', Pool.NumWorkers);
        end

        % the star images of the pool
        Todo = find(~isfile(SrcFile) | ~isfile(StarFile))';
        BaseCall = @(K) [Geo, {'RA0', N.Pool.RA0(K), 'Dec0', N.Pool.Dec0(K), 'DateObs', char(N.DateObs(1)), ...
                         'Seed', N.Pool.Seed(K), 'Mode', 'base', 'BaseName', BaseName{K}, ...
                         'ExtraKeys', poolKeys(N, K, '')}];
        Calls = arrayfun(@(K) BaseCall(K), Todo, 'UniformOutput', false);
        Res   = runCalls(Calls, Pool, Args.NumThreads);
        for J = 1:numel(Todo)
            Say('new: star image %d of %d (offset %+.2f %+.2f arcsec): %s in %.0f s %s', Todo(J), Np, ...
                N.Pool.dRA(Todo(J)), N.Pool.dDec(Todo(J)), Res.Status(J), Res.Duration(J), Res.Message(J));
        end
        if any(Res.Status == "failed")
            error('dut:simulateSet:Base', 'A star image of the pool failed: %s', strjoin(Res.Message(Res.Status == "failed"), '; '));
        end

        % the transients of the visits
        Excl = exclusionMap(Cfg);
        Tr = cell(Nv, 1);
        for K = 1:Nv
            [~, Tr{K}] = drawVisit(N, Visits(K), Excl);
        end
        Tr = vertcat(Tr{:});
        TrFile = fullfile(Dir, 'Transients.csv');
        if isfile(TrFile)                         % keep the visits of earlier calls
            Old = readtable(TrFile);
            Tr  = [Old(~ismember(Old.Visit, Visits), :); Tr];
            Tr  = sortrows(Tr, {'Visit', 'TransientID'});
        end
        writetable(Tr, TrFile);
        Say('new: %d transients of %d visits in %s', sum(ismember(Tr.Visit, Visits)), Nv, TrFile);

        % the exposures
        Exists = arrayfun(@(K) isfile(ultrasat.dut.productName(Cfg, 'new', N.Tile, char(Runs.DateObs(K)), ...
                                      'Image', Runs.Exp(K), 'Product', 'truth')), (1:Nr)');
        Runs.Status(Args.SkipExisting & Exists) = "skipped";
        Todo = find(Runs.Status ~= "skipped")';
        writetable(Runs, LogFile);
        Say('new: %d exposures to simulate, %d skipped (exist)', numel(Todo), Nr - numel(Todo));
        Calls = cell(1, numel(Todo));
        for J = 1:numel(Todo)
            K  = Todo(J);
            Pk = Runs.Pool(K);
            Ex = {'TruthBase', ''};
            Star = StarName{Pk};
            if strcmp(N.StarTruth, 'image')
                Ex   = {'TruthBase', StarFile{Pk}};
                Star = '';
            end
            Calls{J} = [Geo, {'RA0', Runs.RA0(K), 'Dec0', Runs.Dec0(K), 'DateObs', char(Runs.DateObs(K)), ...
                        'Seed', Runs.Seed(K), 'Visit', Runs.Visit(K), 'Image', Runs.Exp(K), 'Stars', false, ...
                        'Transients', Tr(Tr.Visit == Runs.Visit(K), {'RA', 'Dec', 'U', 'TransientID'}), ...
                        'AddSrcImage', SrcFile{Pk}, 'ExtraKeys', poolKeys(N, Pk, Star)}, Ex];
        end
        Runs = runCalls(Calls, Pool, Args.NumThreads, Runs, Todo, LogFile, Say);
        Say('new: done %d, skipped %d, failed %d; log in %s', sum(Runs.Status == "done"), ...
            sum(Runs.Status == "skipped"), sum(Runs.Status == "failed"), LogFile);
    end
end

function Keys = poolKeys(N, K, Star)
    % the header keywords of star image K of the pool (and of an exposure on it)
    Keys = {'SIMPOOL', K, 'star image of the pool'; ...
            'SIMDRA', N.Pool.dRA(K), '[arcsec] pointing offset in RA (on the sky)'; ...
            'SIMDDEC', N.Pool.dDec(K), '[arcsec] pointing offset in Dec'};
    if ~isempty(Star)
        Keys = [Keys; {'STARCAT', Star, 'star truth catalog (ultrasat.dut.readTruth)'}];
    end
end

function Pool = workerPool(Nworkers)
    % the current pool, or a new one
    Pool = gcp('nocreate');
    if isempty(Pool) && isempty(Nworkers)
        Pool = parpool('Processes');
    elseif isempty(Pool)
        Pool = parpool('Processes', Nworkers);
    end
end

function Runs = runCalls(Calls, Pool, NumThreads, Runs, Rows, LogFile, Say)
    % run simulateTile on each argument list (on Pool if given); record each result in Runs(Rows) and the log
    Nc = numel(Calls);
    if nargin < 4
        Runs = table(strings(Nc,1), nan(Nc,1), strings(Nc,1), 'VariableNames', {'Status', 'Duration', 'Message'});
        Rows = 1:Nc;
        LogFile = '';
        Say = @(varargin) false;
    end
    if isempty(Pool)
        for J = 1:Nc
            [St, Dur, Msg] = runSafe(Calls{J}, NumThreads);
            Runs = record(Runs, Rows(J), St, Dur, Msg, LogFile, Say, J, Nc);
        end
    else
        F = parallel.FevalFuture.empty;
        for J = 1:Nc
            F(J) = parfeval(Pool, @runSafe, 3, Calls{J}, NumThreads);
        end
        for J = 1:Nc
            [Jf, St, Dur, Msg] = fetchNext(F);
            Runs = record(Runs, Rows(Jf), St, Dur, Msg, LogFile, Say, J, Nc);
        end
    end
end

function Runs = record(Runs, K, St, Dur, Msg, LogFile, Say, J, Nc)
    % one finished run in the table and the log
    Runs.Status(K)   = St;
    Runs.Duration(K) = Dur;
    Runs.Message(K)  = Msg;
    if ~isempty(LogFile)
        writetable(Runs, LogFile);
        Say('new visit %d exposure %d (%d/%d): %s in %.0f s %s', Runs.Visit(K), Runs.Exp(K), J, Nc, St, Dur, Msg);
    end
end

function [St, Dur, Msg] = runSafe(Call, NumThreads)
    % one simulateTile call, with its status, duration and error message (also on a worker)
    if ~isempty(NumThreads)
        maxNumCompThreads(NumThreads);
    end
    T0  = tic;
    St  = "done";
    Msg = "";
    try
        ultrasat.dut.simulateTile(Call{:});
    catch ME
        St  = "failed";
        Msg = string(ME.message);
    end
    Dur = toc(T0);
end

function Excl = exclusionMap(Cfg)
    % the nominal tile geometry and the pixel positions of the sources the transients avoid
    N = Cfg.New;
    [~, ~, G] = ultrasat.simulateCatalogField('RA0', N.RA0, 'Dec0', N.Dec0, 'Tile', N.Tile, 'PlaneRotation', N.Roll, ...
                                              'GeometryOnly', true, 'Verbose', false);
    Rex = N.ExcludeRadius / (pixSizeDeg() * 3600);    % [pix]
    XY  = zeros(0, 2);
    for K = 1:numel(N.ExcludeCatalogs)
        T = FITS.readTable1(N.ExcludeCatalogs{K});
        if ~istable(T)
            T = array2table(T.Catalog, 'VariableNames', T.ColNames);
        end
        [X, Y] = G.WCS.sky2xy(T.RA, T.Dec);
        In = X > -Rex & Y > -Rex & X < G.NX + 1 + Rex & Y < G.NY + 1 + Rex;
        XY = [XY; X(In) Y(In)]; %#ok<AGROW>
    end
    XY   = unique(round(XY, 3), 'rows');
    Excl = struct('G', G, 'Rex', Rex, 'Tree', KDTreeSearcher(XY), 'Nsrc', size(XY, 1));
end

function [P, Tr] = drawVisit(N, V, Excl)
    % the star images (Nexp of the pool) and, with Excl, the transients of visit V
    RngSaved   = rng;
    rng(N.Seed + V);
    RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    P = randperm(numel(N.Pool.RA0), N.Nexp);
    if nargin < 3
        Tr = table();
    else
        G   = Excl.G;
        Pix = pixSizeDeg();
        Nt  = N.Ntransient;
        M   = N.EdgeMargin;
        Far = G.NX - M;                            % the largest offset from the inner corner [pix]
        Rr  = N.RadiusRange ./ Pix;
        if Rr(2) > hypot(Far, Far) || Rr(1) < hypot(M, M)
            error('dut:simulateSet:Radius', 'RadiusRange must be within %.2f..%.2f deg', hypot(M,M)*Pix, hypot(Far,Far)*Pix);
        end
        [X0, Y0, Sx, Sy] = innerCorner(N.Tile, G.NX);
        X = zeros(Nt, 1); Y = X; R = X;
        for K = 1:Nt
            Ok = false;
            Try = 0;
            while ~Ok
                if mod(Try, 1000) == 0                 % a new radius only if its arc is all excluded
                    R(K) = Rr(1) + rand * diff(Rr);
                    Lo = max(acos(min(1, Far / R(K))), asin(min(1, M / R(K))));
                    Hi = min(asin(min(1, Far / R(K))), acos(min(1, M / R(K))));
                end
                Phi  = Lo + rand * (Hi - Lo);
                X(K) = X0 + Sx * R(K) * cos(Phi);
                Y(K) = Y0 + Sy * R(K) * sin(Phi);
                [~, D] = knnsearch(Excl.Tree, [X(K) Y(K)]);
                Ok  = D > Excl.Rex && all(hypot(X(1:K-1) - X(K), Y(1:K-1) - Y(K)) > Excl.Rex);
                Try = Try + 1;
                if Try > 1e5
                    error('dut:simulateSet:Exclusion', 'No position found for transient %d of visit %d', K, V);
                end
            end
        end
        [RA, Dec] = G.WCS.xy2sky(X, Y);
        Mag = N.MagGrid(1 + mod(0:Nt-1, numel(N.MagGrid)))';
        Tr  = table(repmat(V, Nt, 1), (1:Nt)', RA(:), Dec(:), Mag, R .* Pix, X, Y, ...
                    'VariableNames', {'Visit', 'TransientID', 'RA', 'Dec', 'U', 'R', 'X', 'Y'});
        Tr.Properties.VariableUnits = {'', '', 'deg', 'deg', 'mag', 'deg', 'pix', 'pix'};
    end
end

function P = pixSizeDeg()
    % the pixel [deg], as in ultrasat.usim
    P = (9.5e-3 / 360) * 180/pi;
end

function [X0, Y0, Sx, Sy] = innerCorner(Tile, NX)
    % the inner corner of the tile and the directions into it (as in ultrasat.usim)
    switch upper(Tile)
        case 'A'
            X0 = NX + 0.5; Y0 = 0.5; Sx = -1; Sy = 1;
        case 'B'
            X0 = 0.5; Y0 = 0.5; Sx = 1; Sy = 1;
        case 'C'
            X0 = 0.5; Y0 = NX + 0.5; Sx = 1; Sy = -1;
        otherwise
            X0 = NX + 0.5; Y0 = NX + 0.5; Sx = -1; Sy = -1;
    end
end
