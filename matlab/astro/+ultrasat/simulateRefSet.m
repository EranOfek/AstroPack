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
    %   Every run is logged in Runs and written to <OutDir>/<OutName>_runs.csv
    %   after each run, so an interrupted set can be resumed with SkipExisting.
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
    %            'Tiles' - the tiles to simulate. Default is 'ABCD'.
    %            'Nimage' - number of consecutive images per pointing, roll
    %                   and tile. Default is 3.
    %            'Exposure' - [number duration(s)] of each image, as in
    %                   ultrasat.usim. Default is [1 300].
    %            'EbvMap' - the E(B-V) map for the default simulator, as in
    %                   ultrasat.simulateGaiaField: 'SFD98' (default), 'CSFD23', 'G24'.
    %            'ExtLaw' - the extinction law for the default simulator:
    %                   'CCM89' (default) or 'F99'.
    %            'Rv' - R_V of the extinction law. Default is 3.08.
    %            'SimFun' - [] (default) simulates with ultrasat.simulateGaiaField;
    %                   or a function handle called as
    %                   SimFun('RA0',RA,'Dec0',Dec,'Tile',T,'PlaneRotation',Roll,
    %                   'Exposure',Exp,'OutDir',D,'OutName',N,'Seed',S,SimArgs{:})
    %                   ('Seed' only if Seed is given; EbvMap, ExtLaw and Rv
    %                   are not passed: put them in SimArgs), which must write
    %                   <OutDir>/<OutName>_tile<T>.fits as ultrasat.usim does.
    %            'SimArgs' - cell of further key,val for the simulator (for
    %                   the default: arguments of ultrasat.simulateGaiaField,
    %                   e.g. {'MaxU',23.5}; usim arguments go in its
    %                   'UsimArgs'). Default is {}.
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
    %            'Simulate' - false returns the run list only. Default is true.
    %            'Verbose' - Default is true.
    % Output : - Table of the runs: Pointing, RA0, Dec0, RollIndex, Roll,
    %            Tile, Image, Seed, OutName, Status ('planned', 'done',
    %            'skipped', 'failed'), Duration [s].
    % Author : D. Kovaleva (Oct 2026)
    % Example: Runs = ultrasat.simulateRefSet('Simulate',false);   % the run list only
    %          Runs = ultrasat.simulateRefSet('Tiles','B','Nroll',2,'Nimage',1,'OutDir','/tmp/ref', ...
    %                                         'SimArgs',{'MaxU',23.5},'Seed',1);

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
        Args.Tiles                = 'ABCD';
        Args.Nimage               = 3;
        Args.Exposure             = [1 300];
        Args.EbvMap               = 'SFD98';
        Args.ExtLaw               = 'CCM89';
        Args.Rv                   = 3.08;
        Args.SimFun               = [];
        Args.SimArgs cell         = {};
        Args.OutDir               = '.';
        Args.OutName              = 'Ref';
        Args.Seed                 = [];
        Args.SkipExisting logical = true;
        Args.StopOnError logical  = false;
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
    Runs = table(Ip, Pnt(Ip,1), Pnt(Ip,2), Ir, Rolls(sub2ind(size(Rolls), Ip, Ir)), string(Tiles(It)), Ii, Seed, ...
                 Name, repmat("planned", N, 1), nan(N, 1), ...
                 'VariableNames', {'Pointing', 'RA0', 'Dec0', 'RollIndex', 'Roll', 'Tile', 'Image', 'Seed', ...
                                   'OutName', 'Status', 'Duration'});
    Say('%d pointings x %d rolls x %d tiles x %d images = %d runs', Np, Nr, Nt, Args.Nimage, N);

    % 4. simulate
    if Args.Simulate
        if ~isfolder(Args.OutDir)
            mkdir(Args.OutDir);
        end
        LogFile = fullfile(Args.OutDir, sprintf('%s_runs.csv', Args.OutName));
        for K = 1:N
            FitsFile = fullfile(Args.OutDir, sprintf('%s_tile%s.fits', Runs.OutName(K), Runs.Tile(K)));
            if Args.SkipExisting && isfile(FitsFile)
                Runs.Status(K) = "skipped";
            else
                Say('[%d/%d] %s: aimpoint %.4f %+.4f, roll %.2f, tile %s', K, N, Runs.OutName(K), ...
                    Runs.RA0(K), Runs.Dec0(K), Runs.Roll(K), Runs.Tile(K));
                T0 = tic;
                try
                    runOne(Args, Runs(K,:));
                    Runs.Status(K) = "done";
                catch ME
                    Runs.Status(K) = "failed";
                    Say('  failed: %s', ME.message);
                    if Args.StopOnError
                        writetable(Runs, LogFile);
                        rethrow(ME);
                    end
                end
                Runs.Duration(K) = toc(T0);
            end
            writetable(Runs, LogFile);
        end
        Say('done: %d, skipped: %d, failed: %d; log in %s', sum(Runs.Status == "done"), ...
            sum(Runs.Status == "skipped"), sum(Runs.Status == "failed"), LogFile);
    end
end

function runOne(Args, R)
    % one image: the default simulator or the user's SimFun
    Name = char(R.OutName);
    Tile = char(R.Tile);
    if isempty(Args.SimFun)
        UsimArgs = {'OutDir', Args.OutDir, 'OutName', Name};
        if ~isnan(R.Seed)
            UsimArgs = [UsimArgs, {'Seed', R.Seed}];
        end
        % usim arguments given in SimArgs come last, so they can override
        Extra = Args.SimArgs;
        Iu    = find(strcmpi(Extra(1:2:end), 'UsimArgs'), 1);
        if ~isempty(Iu)
            UsimArgs = [UsimArgs, Extra{2*Iu}];
            Extra(2*Iu-1:2*Iu) = [];
        end
        ultrasat.simulateGaiaField('RA0', R.RA0, 'Dec0', R.Dec0, 'Tile', Tile, ...
                                   'PlaneRotation', R.Roll, 'Exposure', Args.Exposure, ...
                                   'EbvMap', Args.EbvMap, 'ExtLaw', Args.ExtLaw, 'Rv', Args.Rv, ...
                                   'UsimArgs', UsimArgs, Extra{:});
    else
        SeedArg = {};
        if ~isnan(R.Seed)
            SeedArg = {'Seed', R.Seed};
        end
        Args.SimFun('RA0', R.RA0, 'Dec0', R.Dec0, 'Tile', Tile, 'PlaneRotation', R.Roll, ...
                    'Exposure', Args.Exposure, 'OutDir', Args.OutDir, 'OutName', Name, ...
                    SeedArg{:}, Args.SimArgs{:});
    end
end
