function Runs = simulateRefSet(Args)
    % Simulate a set of ULTRASAT reference images: dithered pointings x camera rolls x tiles x consecutive images
    %   1. Pointings: the dither pattern of ultrasat.tools.ditherGrid around
    %      the dither center (RA0, Dec0), with 'DitherLeg' and
    %      'DitherPattern'; optionally with the center itself; or a
    %      user-given list.
    %   2. Rolls at each pointing: RollStart + RollStep*(0:Nroll-1), each
    %      plus a uniform random offset in [0, RollJitter]; or a user-given
    %      list.
    %   3. For each pointing, roll and tile: Nimage separate images of
    %      Exposure each (independent noise), simulated by SimFun.
    %   4. Pointing jitter: each image of a visit gets its own small random
    %      offset of the aimpoint (PointingJitter), the same for its tiles;
    %      with 0 (default) all images of a visit have the same aimpoint.
    %   5. Times: each visit (one pointing and roll) starts VisitInterval
    %      after the previous one, and its images are ImageInterval apart;
    %      the 4 tiles of an image share its time (one camera).
    %   Every run is logged in Runs and written to <OutDir>/<OutName>_runs.csv
    %   after each run, so an interrupted set can be resumed with SkipExisting.
    %   With Parallel, the runs go to a pool of workers; each usim run of a
    %   dense tile needs up to ~17 GB, so choose Nworkers by the free memory.
    % Input  : * ...,key,val,...
    %            'RA0', 'Dec0' - [deg] the dither center. Default is the
    %                   Kepler field, 291, 44.5.
    %            'Pointings' - [] (default): from ultrasat.tools.ditherGrid;
    %                   or an N x 2 matrix of aimpoints [RA Dec] in deg, used
    %                   as is (the Dither* arguments are then ignored).
    %            'DitherLeg' - [deg] distance from the dither center to each
    %                   pointing, as in ultrasat.tools.ditherGrid. Default is 2.
    %            'DitherPattern' - as in ultrasat.tools.ditherGrid. Default is '2x2'.
    %            'DitherNgrid' - number of pointings per center, as in
    %                   ultrasat.tools.ditherGrid. Default is 4.
    %            'IncludeCenter' - true adds the dither center as one more
    %                   pointing. Default is false.
    %            'Rolls' - [] (default): from the Roll* arguments below; or a
    %                   vector of camera rolls [deg] (PlaneRotation), the same
    %                   at every pointing.
    %            'Nroll' - number of rolls per pointing. Default is 10.
    %            'RollStart' - [deg] the first roll. Default is 0.
    %            'RollStep' - [deg] step between rolls; [] (default) is 360/Nroll.
    %            'RollJitter' - [deg] each roll gets a uniform random offset
    %                   in [0, RollJitter], drawn separately at each pointing.
    %                   Default is 2.
    %            'PointingJitter' - [arcsec] rms of the random offset of the
    %                   aimpoint of each image, per axis (on the sky, along
    %                   RA*cos(Dec) and Dec): a scalar, or [SigmaRA SigmaDec].
    %                   Default is 0. (The paper gives a pointing stability
    %                   of < 3 arcsec, 3 sigma, over 300 s; pixels are 5.4.)
    %            'Tiles' - the tiles to simulate. Default is 'ABCD'.
    %            'Nimage' - number of consecutive images per pointing, roll
    %                   and tile. Default is 3.
    %            'Exposure' - [number duration(s)] of each image, as in
    %                   ultrasat.usim. Default is [1 300].
    %            'DateObs' - start (UTC) of the first image,
    %                   'yyyy-mm-ddTHH:MM:SS' as in ultrasat.usim (default
    %                   '2026-07-01T00:00:00'); or one start time per visit
    %                   (a string array of Npointing x Nroll, pointing-major),
    %                   then VisitInterval is not used.
    %            'ImageInterval' - [s] between the starts of consecutive images
    %                   of a visit; [] (default) is Exposure(1)*Exposure(2).
    %            'VisitInterval' - [s] between the starts of consecutive
    %                   visits. Default is 86400.
    %            'EbvMap' - the E(B-V) map for the default simulator, as in
    %                   ultrasat.simulateCatalogField: 'SFD98' (default), 'CSFD23', 'G24'.
    %            'ExtLaw' - the extinction law for the default simulator:
    %                   'CCM89' (default) or 'F99'.
    %            'Rv' - R_V of the extinction law. Default is 3.08.
    %            'SimFun' - [] (default) simulates with ultrasat.simulateCatalogField;
    %                   or a function handle called as
    %                   SimFun('RA0',RA,'Dec0',Dec,'Tile',T,'PlaneRotation',Roll,
    %                   'Exposure',Exp,'DateObs',T,'OutDir',D,'OutName',N,'Seed',S,SimArgs{:})
    %                   ('Seed' only if Seed is given; EbvMap, ExtLaw and Rv
    %                   are not passed: put them in SimArgs), which must write
    %                   <OutDir>/<OutName>_tile<T>.fits as ultrasat.usim does.
    %            'SimArgs' - cell of further key,val for the simulator (for
    %                   the default: arguments of ultrasat.simulateCatalogField,
    %                   e.g. {'MaxU',23.5}; usim arguments go in its
    %                   'UsimArgs'). Default is {}.
    %            'RunArgs' - [] (default); or a function handle R -> cell of
    %                   key,val added to SimArgs for one run, where R is that
    %                   run's row of the output table (e.g. sources that change
    %                   from image to image, see ultrasat.simulateNewSet).
    %            'OutDir' - output directory (created if missing). Default is '.'.
    %            'OutName' - root of the file names; each image is named
    %                   <OutName>_p<pointing>_r<roll>_e<image>, and usim adds
    %                   _tile<T>. Default is 'Ref'.
    %            'Seed' - [] (default): no seeding; or an integer: seeds the
    %                   roll offsets, and image k of the set gets Seed + k,
    %                   so the set is repeatable.
    %            'SkipExisting' - true skips images whose FITS file exists.
    %                   Default is true.
    %            'StopOnError' - false (default) logs a failed run and goes
    %                   on with the next one; true rethrows the error.
    %            'Parallel' - true runs the images on a pool of workers
    %                   (the current pool, or a new 'Processes' pool of
    %                   Nworkers). Default is false.
    %            'Nworkers' - number of workers of a new pool; [] (default)
    %                   is the profile default.
    %            'NumThreads' - computational threads per worker; [] (default)
    %                   leaves the worker default (1 for process workers).
    %            'Simulate' - false returns the run list only. Default is true.
    %            'Verbose' - Default is true.
    % Output : - Table of the runs: Pointing, RA0, Dec0 (the aimpoint of this
    %            image, with its pointing-jitter offset), dRA, dDec [arcsec] (the
    %            offset), RollIndex, Roll,
    %            Tile, Image, DateObs, Seed, OutName, Status ('planned',
    %            'done', 'skipped', 'failed'), Duration [s], Message (the
    %            error of a failed run).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Runs = ultrasat.simulateRefSet('Simulate',false);   % the run list only
    %          Runs = ultrasat.simulateRefSet('Tiles','B','Nroll',2,'Nimage',1,'OutDir','/tmp/ref', ...
    %                                         'SimArgs',{'MaxU',23.5},'Seed',1);
    %          Runs = ultrasat.simulateRefSet('OutDir','/Data/ref','SimArgs',{'MaxG',17},'Parallel',true,'Nworkers',12);

    arguments
        Args.RA0                  = 291;
        Args.Dec0                 = 44.5;
        Args.Pointings            = [];
        Args.DitherLeg            = 2;
        Args.DitherPattern        = '2x2';
        Args.DitherNgrid          = 4;
        Args.IncludeCenter logical = false;
        Args.Rolls                = [];
        Args.Nroll                = 10;
        Args.RollStart            = 0;
        Args.RollStep             = [];
        Args.RollJitter           = 2;
        Args.PointingJitter       = 0;
        Args.Tiles                = 'ABCD';
        Args.Nimage               = 3;
        Args.Exposure             = [1 300];
        Args.DateObs              = '2026-07-01T00:00:00';
        Args.ImageInterval        = [];
        Args.VisitInterval        = 86400;
        Args.EbvMap               = 'SFD98';
        Args.ExtLaw               = 'CCM89';
        Args.Rv                   = 3.08;
        Args.SimFun               = [];
        Args.SimArgs cell         = {};
        Args.RunArgs              = [];
        Args.OutDir               = '.';
        Args.OutName              = 'Ref';
        Args.Seed                 = [];
        Args.SkipExisting logical = true;
        Args.StopOnError logical  = false;
        Args.Parallel logical     = false;
        Args.Nworkers             = [];
        Args.NumThreads           = [];
        Args.Simulate logical     = true;
        Args.Verbose logical      = true;
    end
    Say = @(varargin) Args.Verbose && fprintf([varargin{1} '\n'], varargin{2:end});
    if ~isempty(Args.Seed)   % a repeatable set; the caller's generator state is restored on exit
        RngSaved   = rng;
        rng(Args.Seed);
        RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    end

    % 1. pointings
    if isempty(Args.Pointings)
        Grid0 = table(Args.RA0, Args.Dec0, 1, 'VariableNames', {'RA', 'Dec', 'id'});
        Grid  = ultrasat.tools.ditherGrid(Grid0, 'Leg', Args.DitherLeg, 'Ngrid', Args.DitherNgrid, ...
                                          'Pattern', Args.DitherPattern);
        Pnt   = [Grid.RA(:) Grid.Dec(:)];
    else
        Pnt   = Args.Pointings;
    end
    if Args.IncludeCenter
        Pnt = [Args.RA0 Args.Dec0; Pnt];
    end
    Np = size(Pnt, 1);

    % 2. rolls: one row per pointing
    if isempty(Args.Rolls)
        Step  = Args.RollStep;
        if isempty(Step)
            Step = 360 / Args.Nroll;
        end
        Rolls = Args.RollStart + Step .* (0:Args.Nroll-1) + Args.RollJitter .* rand(Np, Args.Nroll);
    else
        Rolls = repmat(Args.Rolls(:)', Np, 1);
    end
    Nr = size(Rolls, 2);

    % 3. the run list: pointing x roll x tile x image
    Tiles = cellstr(Args.Tiles(:));
    Nt    = numel(Tiles);
    [Ii, It, Ir, Ip] = ndgrid(1:Args.Nimage, 1:Nt, 1:Nr, 1:Np);
    Ip = Ip(:); Ir = Ir(:); It = It(:); Ii = Ii(:);
    N  = numel(Ip);
    Seed = nan(N, 1);
    if ~isempty(Args.Seed)
        Seed = Args.Seed + (1:N)';
    end
    Name = string(compose('%s_p%d_r%02d_e%d', char(Args.OutName), Ip, Ir, Ii));

    % 4. times: visit v = (pointing, roll), pointing-major
    Fmt   = 'yyyy-MM-dd''T''HH:mm:ss';
    Visit = (Ip - 1) .* Nr + Ir;
    Dt    = Args.ImageInterval;
    if isempty(Dt)
        Dt = Args.Exposure(1) * Args.Exposure(2);
    end
    DateIn = string(Args.DateObs);
    if isscalar(DateIn)
        VisitStart = datetime(DateIn, 'InputFormat', Fmt) + seconds(Args.VisitInterval .* (0:Np*Nr-1)');
    elseif numel(DateIn) == Np*Nr
        VisitStart = datetime(DateIn(:), 'InputFormat', Fmt);
    else
        error('simulateRefSet: DateObs must be one time or Npointing x Nroll = %d times', Np*Nr);
    end
    Date = string(VisitStart(Visit) + seconds(Dt .* (Ii - 1)), Fmt);

    % pointing jitter: one offset per image (visit, image), shared by its tiles
    Sig = Args.PointingJitter([1 end]);
    dRA = zeros(N, 1);
    dDec = zeros(N, 1);
    if any(Sig > 0)   % no draw at 0, so that the default sets are unchanged
        Off  = randn(Np*Nr, Args.Nimage, 2);
        Iimg = sub2ind([Np*Nr, Args.Nimage], Visit, Ii);
        dRA  = Sig(1) .* Off(Iimg);
        dDec = Sig(2) .* Off(Iimg + Np*Nr*Args.Nimage);
    end
    Dec0 = Pnt(Ip,2) + dDec ./ 3600;
    RA0  = mod(Pnt(Ip,1) + dRA ./ 3600 ./ cosd(Pnt(Ip,2)), 360);

    Runs = table(Ip, RA0, Dec0, dRA, dDec, Ir, Rolls(sub2ind(size(Rolls), Ip, Ir)), string(Tiles(It)), Ii, Date, Seed, ...
                 Name, repmat("planned", N, 1), nan(N, 1), repmat("", N, 1), ...
                 'VariableNames', {'Pointing', 'RA0', 'Dec0', 'dRA', 'dDec', 'RollIndex', 'Roll', 'Tile', 'Image', 'DateObs', 'Seed', ...
                                   'OutName', 'Status', 'Duration', 'Message'});
    Say('%d pointings x %d rolls x %d tiles x %d images = %d runs', Np, Nr, Nt, Args.Nimage, N);

    % 5. simulate
    if Args.Simulate
        if ~isfolder(Args.OutDir)
            mkdir(Args.OutDir);
        end
        LogFile = fullfile(Args.OutDir, sprintf('%s_runs.csv', Args.OutName));
        Exists  = arrayfun(@(K) isfile(fullfile(Args.OutDir, sprintf('%s_tile%s.fits', Runs.OutName(K), Runs.Tile(K)))), (1:N)');
        Skip    = Args.SkipExisting & Exists;
        Runs.Status(Skip) = "skipped";
        Todo    = find(~Skip);
        writetable(Runs, LogFile);
        Say('%d to simulate, %d skipped (exist)', numel(Todo), sum(Skip));
        if Args.Parallel
            Pool = gcp('nocreate');
            if isempty(Pool) && isempty(Args.Nworkers)
                Pool = parpool('Processes');
            elseif isempty(Pool)
                Pool = parpool('Processes', Args.Nworkers);
            end
            Say('parallel: %d workers', Pool.NumWorkers);
            F = parallel.FevalFuture.empty;
            for J = 1:numel(Todo)
                F(J) = parfeval(Pool, @runSafe, 3, Args, Runs(Todo(J),:));
            end
            for J = 1:numel(Todo)
                [Jf, St, Dur, Msg] = fetchNext(F);
                Runs = logRun(Runs, Todo(Jf), St, Dur, Msg, LogFile, Say, J, numel(Todo));
                if Args.StopOnError && St == "failed"
                    cancel(F);
                    error('simulateRefSet: %s failed: %s', Runs.OutName(Todo(Jf)), Msg);
                end
            end
        else
            for J = 1:numel(Todo)
                K = Todo(J);
                Say('[%d/%d] %s: aimpoint %.4f %+.4f, roll %.2f, tile %s, %s', J, numel(Todo), Runs.OutName(K), ...
                    Runs.RA0(K), Runs.Dec0(K), Runs.Roll(K), Runs.Tile(K), Runs.DateObs(K));
                [St, Dur, Msg] = runSafe(Args, Runs(K,:));
                Runs = logRun(Runs, K, St, Dur, Msg, LogFile, Say, J, numel(Todo));
                if Args.StopOnError && St == "failed"
                    error('simulateRefSet: %s failed: %s', Runs.OutName(K), Msg);
                end
            end
        end
        Say('done: %d, skipped: %d, failed: %d; log in %s', sum(Runs.Status == "done"), ...
            sum(Runs.Status == "skipped"), sum(Runs.Status == "failed"), LogFile);
    end
end

function Runs = logRun(Runs, K, St, Dur, Msg, LogFile, Say, J, Ntodo)
    % record one finished run and rewrite the log
    Runs.Status(K)   = St;
    Runs.Duration(K) = Dur;
    Runs.Message(K)  = Msg;
    writetable(Runs, LogFile);
    Say('  %d/%d %s tile %s: %s in %.0f s %s', J, Ntodo, Runs.OutName(K), Runs.Tile(K), St, Dur, Msg);
end

function [St, Dur, Msg] = runSafe(Args, R)
    % one image, with its status, duration and error message (also on a worker)
    if ~isempty(Args.NumThreads)
        maxNumCompThreads(Args.NumThreads);
    end
    T0  = tic;
    St  = "done";
    Msg = "";
    try
        runOne(Args, R);
    catch ME
        St  = "failed";
        Msg = string(ME.message);
    end
    Dur = toc(T0);
end

function runOne(Args, R)
    % one image: the default simulator or the user's SimFun
    Name = char(R.OutName);
    Tile = char(R.Tile);
    SimArgs = Args.SimArgs;
    if ~isempty(Args.RunArgs)
        SimArgs = [SimArgs, Args.RunArgs(R)];
    end
    if isempty(Args.SimFun)
        UsimArgs = {'OutDir', Args.OutDir, 'OutName', Name, 'DateObs', char(R.DateObs)};
        if ~isnan(R.Seed)
            UsimArgs = [UsimArgs, {'Seed', R.Seed}];
        end
        % usim arguments given in SimArgs come last, so they can override
        Extra = SimArgs;
        Iu    = find(strcmpi(Extra(1:2:end), 'UsimArgs'), 1);
        if ~isempty(Iu)
            UsimArgs = [UsimArgs, Extra{2*Iu}];
            Extra(2*Iu-1:2*Iu) = [];
        end
        ultrasat.simulateCatalogField('RA0', R.RA0, 'Dec0', R.Dec0, 'Tile', Tile, ...
                                   'PlaneRotation', R.Roll, 'Exposure', Args.Exposure, ...
                                   'EbvMap', Args.EbvMap, 'ExtLaw', Args.ExtLaw, 'Rv', Args.Rv, ...
                                   'UsimArgs', UsimArgs, Extra{:});
    else
        SeedArg = {};
        if ~isnan(R.Seed)
            SeedArg = {'Seed', R.Seed};
        end
        Args.SimFun('RA0', R.RA0, 'Dec0', R.Dec0, 'Tile', Tile, 'PlaneRotation', R.Roll, ...
                    'Exposure', Args.Exposure, 'DateObs', char(R.DateObs), 'OutDir', Args.OutDir, 'OutName', Name, ...
                    SeedArg{:}, SimArgs{:});
    end
end
