function Runs = simulateCalibFrames(Args)
    % Simulate ULTRASAT dark or flat calibration frames with the detector maps of ultrasat.detectorModel
    %   The frames have no sources (an empty usim catalog). They use the same
    %   per-tile detector maps (dark current, sensitivity, read-out noise) as
    %   the science frames, given by Detector, so that the master dark and
    %   flat a pipeline builds can be compared with the true maps.
    %   'dark': Nframe frames at each exposure of ExposureList; the sky terms
    %       as DarkSky.
    %   'flat': Nframe frames at each level of FlatLevel; uniform light at
    %       FlatLevel/FlatExposure e-/pix/s through the flat and, for 'lamp',
    %       the vignetting. The lamp is usim's zodiacal term set to that
    %       uniform rate, whatever zodiacal model the science frames use;
    %       Cherenkov and stray light are off. The dark current is on.
    %   The frames are run by ultrasat.simulateRefSet (one visit, the run log,
    %   SkipExisting, Seed, Parallel). With Output = 'master', the mean frame
    %   of each tile and exposure (or level) is written too. With WriteTruth,
    %   the true maps are written as FITS.
    % Input  : * ...,key,val,...
    %            'Type' - 'dark' (default) or 'flat'.
    %            'Tiles' - Default is 'ABCD'.
    %            'Nframe' - frames per tile and exposure (or level). Default is 10.
    %            'ExposureList' - [s] exposures of the dark frames. Default is 300.
    %            'DarkSky' - the sky terms in the dark frames: 'none' (default;
    %                   dark current and read-out noise only, as in the lab),
    %                   'cherenkov' (in orbit, cover closed: the Cherenkov
    %                   background on), or 'all' (the zodiacal, Cherenkov and
    %                   stray light as in the science frames).
    %            'FlatSource' - 'lamp' (default; uniform light through the
    %                   optics: flat x vignetting), 'lab' (the pixel flat only:
    %                   no vignetting), or 'sky' (no flat frames; the flat
    %                   comes from a superflat of the science frames).
    %            'FlatLevel' - [e-/pix] the lamp level per frame, before the
    %                   flat and the vignetting; a vector for a ladder.
    %                   Default is 10000.
    %            'FlatExposure' - [s] exposure of the flat frames. Default is 300.
    %            'Output' - 'raw' (default: the frames) or 'master' (also the
    %                   mean frame per tile and exposure or level, in e-,
    %                   <OutName>_<Set>_master_tile<T>.fits).
    %            'WriteTruth' - write the true maps of each tile as FITS
    %                   (<OutName>_truth_tile<T>_dark/flat/readnoise.fits).
    %                   Default is true.
    %            'Detector' - the detector maps: a file pattern with %s for
    %                   the tile, each file holding a Det structure of
    %                   ultrasat.detectorModel; [] (default) makes synthetic
    %                   maps with DetectorArgs and DetectorSeed and saves them
    %                   as <OutDir>/<OutName>_detector_tile<T>.mat, to be
    %                   given also to the science frames
    %                   (ultrasat.simulateCatalogField 'Detector').
    %            'DetectorArgs' - cell of key,val for ultrasat.detectorModel.
    %                   Default is {}.
    %            'DetectorSeed' - Seed of ultrasat.detectorModel. Default is 1.
    %            'RA0', 'Dec0', 'PlaneRotation' - the WCS written in the
    %                   headers. Default is 291, 44.5, 0.
    %            'DateObs' - start (UTC) of the first frame. Default is
    %                   '2026-07-01T00:00:00'.
    %            'UsimArgs' - cell of further key,val for ultrasat.usim
    %                   (e.g. {'ADUOutput','raw'}); they come last and can
    %                   override the settings above. Default is {}.
    %            'OutDir' - Default is '.'.
    %            'OutName' - root of the file names. Default is 'Calib'.
    %            'Seed' - [] (default): no seeding; or an integer: the frames
    %                   of the k-th exposure (level) get Seed + (k-1)*N + 1 ...
    %                   Seed + k*N, N the number of frames per exposure.
    %            'Parallel', 'Nworkers', 'NumThreads', 'SkipExisting',
    %            'StopOnError', 'Verbose' - as in ultrasat.simulateRefSet.
    %            'Simulate' - false returns the run list only (the detector
    %                   maps are still made). Default is true.
    % Output : - Table of the runs of ultrasat.simulateRefSet, plus Type, Set
    %            (e.g. "dark_300s", "flat_10000e") and Exposure [s].
    % Author : D. Kovaleva (Oct 2026)
    % Example: Runs = ultrasat.simulateCalibFrames('Type','dark','Tiles','B','Nframe',3,'OutDir','/tmp/calib','Seed',1);
    %          Runs = ultrasat.simulateCalibFrames('Type','flat','FlatLevel',[3000 10000 30000],'Output','master', ...
    %                     'Detector','/Data/det/Calib_detector_tile%s.mat','OutDir','/Data/calib','Seed',100);

    arguments
        Args.Type                  = 'dark';
        Args.Tiles                 = 'ABCD';
        Args.Nframe                = 10;
        Args.ExposureList          = 300;
        Args.DarkSky               = 'none';
        Args.FlatSource            = 'lamp';
        Args.FlatLevel             = 10000;
        Args.FlatExposure          = 300;
        Args.Output                = 'raw';
        Args.WriteTruth logical    = true;
        Args.Detector              = [];
        Args.DetectorArgs cell     = {};
        Args.DetectorSeed          = 1;
        Args.RA0                   = 291;
        Args.Dec0                  = 44.5;
        Args.PlaneRotation         = 0;
        Args.DateObs               = '2026-07-01T00:00:00';
        Args.UsimArgs cell         = {};
        Args.OutDir                = '.';
        Args.OutName               = 'Calib';
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
    Type = lower(Args.Type);
    if ~any(strcmp(Type, {'dark', 'flat'}))
        error('simulateCalibFrames: Type must be ''dark'' or ''flat''');
    end
    if ~any(strcmpi(Args.DarkSky, {'none', 'cherenkov', 'all'}))
        error('simulateCalibFrames: DarkSky must be ''none'', ''cherenkov'' or ''all''');
    end
    if ~any(strcmpi(Args.FlatSource, {'lamp', 'lab', 'sky'}))
        error('simulateCalibFrames: FlatSource must be ''lamp'', ''lab'' or ''sky''');
    end
    Runs = table();
    if strcmp(Type, 'flat') && strcmpi(Args.FlatSource, 'sky')
        Say('FlatSource = ''sky'': no flat frames (the flat comes from a superflat of the science frames)');
    else
        if ~isfolder(Args.OutDir)
            mkdir(Args.OutDir);
        end
        Tiles = cellstr(Args.Tiles(:));

        % the detector maps, one file per tile
        Det = Args.Detector;
        if isempty(Det)
            Det = fullfile(Args.OutDir, sprintf('%s_detector_tile%%s.mat', Args.OutName));
            for It = 1:numel(Tiles)
                File = sprintf(Det, Tiles{It});
                if ~isfile(File)
                    ultrasat.detectorModel('Tile', Tiles{It}, 'Seed', Args.DetectorSeed, Args.DetectorArgs{:}, 'OutFile', File);
                end
            end
            Say('detector maps: %s', Det);
        end
        if Args.WriteTruth
            writeTruth(Det, Tiles, Args.OutDir, Args.OutName);
        end

        % one set per exposure (dark) or level (flat)
        if strcmp(Type, 'dark')
            Expo = Args.ExposureList(:);
            Lamp = zeros(size(Expo));
            Set  = compose('dark_%gs', Expo);
        else
            Lamp = Args.FlatLevel(:) ./ Args.FlatExposure;   % e-/pix/s
            Expo = Args.FlatExposure .* ones(size(Lamp));
            Set  = compose('flat_%ge', Args.FlatLevel(:));
        end
        Nper  = Args.Nframe * numel(Tiles);
        Parts = cell(numel(Expo), 1);
        for Is = 1:numel(Expo)
            Seed = Args.Seed;
            if ~isempty(Seed)
                Seed = Seed + (Is - 1) * Nper;
            end
            Frame = {'CalibType', Type, 'Detector', Det, 'DarkSky', Args.DarkSky, 'FlatSource', Args.FlatSource, ...
                     'LampRate', Lamp(Is), 'UsimArgs', Args.UsimArgs};
            R = ultrasat.simulateRefSet('Pointings', [Args.RA0 Args.Dec0], 'Rolls', Args.PlaneRotation, ...
                    'Tiles', Args.Tiles, 'Nimage', Args.Nframe, 'Exposure', [1 Expo(Is)], 'DateObs', Args.DateObs, ...
                    'SimFun', @calibFrame, 'SimArgs', Frame, 'OutDir', Args.OutDir, ...
                    'OutName', sprintf('%s_%s', Args.OutName, Set{Is}), 'Seed', Seed, ...
                    'Parallel', Args.Parallel, 'Nworkers', Args.Nworkers, 'NumThreads', Args.NumThreads, ...
                    'SkipExisting', Args.SkipExisting, 'StopOnError', Args.StopOnError, ...
                    'Simulate', Args.Simulate, 'Verbose', Args.Verbose);
            R.Type     = repmat(string(Type), height(R), 1);
            R.Set      = repmat(string(Set{Is}), height(R), 1);
            R.Exposure = repmat(Expo(Is), height(R), 1);
            Parts{Is}  = R;
            if Args.Simulate && strcmpi(Args.Output, 'master')
                writeMasters(R, Args.OutDir, Args.OutName, Say);
            end
        end
        Runs = vertcat(Parts{:});
    end
end

function calibFrame(varargin)
    % one calibration frame (the SimFun of ultrasat.simulateRefSet)
    A = struct();
    for K = 1:2:numel(varargin)   % key,val pairs (struct() would expand cell values)
        A.(varargin{K}) = varargin{K+1};
    end
    Det = io.files.load2(sprintf(char(A.Detector), upper(A.Tile)), 'Det');
    C   = {'Cat', [], 'RA0', A.RA0, 'Dec0', A.Dec0, 'Tile', A.Tile, 'PlaneRotation', A.PlaneRotation, ...
           'Exposure', A.Exposure, 'DateObs', A.DateObs, 'OutDir', A.OutDir, 'OutName', A.OutName, ...
           'FlatMatrix', Det.Flat, 'DarkCurrent', Det.Dark, 'ReadNoise', Det.ReadNoise, 'Verbosity', 0};
    if isfield(A, 'Seed')
        C = [C, {'Seed', A.Seed}];
    end
    if strcmp(A.CalibType, 'dark')
        switch lower(A.DarkSky)
            case 'none'
                C = [C, {'NoiseZody', false, 'NoiseCher', false, 'NoiseStray', false}];
            case 'cherenkov'
                C = [C, {'NoiseZody', false, 'NoiseCher', true, 'NoiseStray', false}];
            case 'all'
        end
    else   % flat: the zodiacal term as a uniform lamp
        C = [C, {'NoiseZody', true, 'Zodi', A.LampRate, 'NoiseCher', false, 'NoiseStray', false}];
        if strcmpi(A.FlatSource, 'lab')
            C = [C, {'Vignetting', 'none'}];
        end
    end
    ultrasat.usim(C{:}, A.UsimArgs{:});
end

function writeTruth(Det, Tiles, OutDir, OutName)
    % the true maps of each tile as FITS
    for It = 1:numel(Tiles)
        D = io.files.load2(sprintf(char(Det), upper(Tiles{It})), 'Det');
        Maps = {'dark', D.Dark; 'flat', D.Flat; 'readnoise', D.ReadNoise};
        for K = 1:size(Maps, 1)
            M = Maps{K,2};
            if ~isscalar(M)
                FITS.write(M, sprintf('!%s', fullfile(OutDir, sprintf('%s_truth_tile%s_%s.fits', OutName, Tiles{It}, Maps{K,1}))), ...
                           'DataType', 'single', 'OverWrite', true);
            end
        end
    end
end

function writeMasters(R, OutDir, OutName, Say)
    % the mean frame of each tile of one set, from the frames that exist
    for T = unique(R.Tile)'
        Rt = R(R.Tile == T & ismember(R.Status, ["done", "skipped"]), :);
        Sum = 0;
        for K = 1:height(Rt)
            Sum = Sum + double(FITS.read1(fullfile(OutDir, sprintf('%s_tile%s.fits', Rt.OutName(K), T))));
        end
        if height(Rt) > 0
            File = fullfile(OutDir, sprintf('%s_%s_master_tile%s.fits', OutName, R.Set(1), T));
            FITS.write(single(Sum ./ height(Rt)), sprintf('!%s', File), 'DataType', 'single', 'OverWrite', true);
            Say('master %s tile %s: mean of %d frames, %s', R.Set(1), T, height(Rt), File);
        end
    end
end
