function Det = detectorModel(Args)
    % Make the fixed pixel maps of one ULTRASAT tile: dark current, pixel sensitivity (flat), read-out noise, bad pixels
    %   The maps are the "truth" of a simulated detector: the same maps are
    %   to be used in every frame of the tile (science, dark, flat), so that
    %   the calibration a pipeline derives can be compared with them. They
    %   are repeatable with Seed.
    %   Maps = 'synthetic':
    %   - Dark [e-/pix/s]: log-normal around DarkMean with the log spread
    %     DarkSpread (the mean of the map is DarkMean); a fraction
    %     HotFraction of hot pixels with rates log-uniform in HotRateRange.
    %   - Flat (mean 1): pixel-to-pixel Gaussian rms PRNU, times an optional
    %     smooth large-scale pattern of rms FlatLargeScale and correlation
    %     length FlatLargeScaleLength; dead pixels (DeadFraction) are 0.
    %     It does not contain the vignetting, which usim applies itself.
    %   - ReadNoise [e-]: ReadNoiseMean, with a Gaussian relative spread
    %     ReadNoiseSpread (0: one value for all pixels).
    %   All maps are in the image orientation (rows = Y), as the FlatMatrix
    %   argument of ultrasat.usim, e.g.
    %   ultrasat.usim(..., 'FlatMatrix', Det.Flat, 'DarkCurrent', Det.Dark, 'ReadNoise', Det.ReadNoise).
    % Input  : * ...,key,val,...
    %            'Tile' - 'A', 'B', 'C' or 'D' (recorded; with Seed it makes
    %                   the maps of each tile different). Default is 'B'.
    %            'Maps' - 'synthetic' (default); 'DESY' (maps measured on the
    %                   lab devices) is not implemented yet.
    %            'ImageSize' - [NY NX] of the maps; [] (default) is the tile
    %                   size of ultrasat.simulateCatalogField / ultrasat.usim.
    %            'DarkMean' - [e-/pix/s] mean dark current. Default is 0.04
    %                   (the ultrasat.usim default).
    %            'DarkSpread' - sigma of ln(dark current) between pixels.
    %                   Default is 0.3.
    %            'HotFraction' - fraction of hot pixels. Default is 1e-4.
    %            'HotRateRange' - [e-/pix/s] range of the hot-pixel rates
    %                   (log-uniform). Default is [0.5 5].
    %            'PRNU' - rms of the pixel-to-pixel sensitivity. Default is 0.01.
    %            'FlatLargeScale' - rms of the large-scale sensitivity pattern.
    %                   Default is 0 (none).
    %            'FlatLargeScaleLength' - [pix] its correlation length.
    %                   Default is 500.
    %            'DeadFraction' - fraction of dead pixels (Flat = 0, Dark = 0).
    %                   Default is 0.
    %            'ReadNoiseMean' - [e-] mean read-out noise. Default is sqrt(6)
    %                   (the ultrasat.usim default).
    %            'ReadNoiseSpread' - relative rms of the read-out noise
    %                   between pixels. Default is 0.
    %            'Seed' - [] (default): no seeding; or an integer, combined
    %                   with the tile so that each tile gets its own maps.
    %            'OutFile' - [] (default); or a .mat file to save Det in.
    % Output : - Structure: .Tile, .Maps, .Seed, .Dark, .Flat, .ReadNoise
    %            (single maps, rows = Y; ReadNoise is a scalar when
    %            ReadNoiseSpread = 0), .HotMask, .DeadMask (logical), .DarkMean
    %            (mean of .Dark), .Args (the arguments).
    % Author : D. Kovaleva (Oct 2026)
    % Example: Det = ultrasat.detectorModel('Tile','B','Seed',1);
    %          Det = ultrasat.detectorModel('Tile','C','PRNU',0.02,'FlatLargeScale',0.01,'HotFraction',5e-4,'Seed',1);

    arguments
        Args.Tile                 = 'B';
        Args.Maps                 = 'synthetic';
        Args.ImageSize            = [];
        Args.DarkMean             = 0.04;
        Args.DarkSpread           = 0.3;
        Args.HotFraction          = 1e-4;
        Args.HotRateRange         = [0.5 5];
        Args.PRNU                 = 0.01;
        Args.FlatLargeScale       = 0;
        Args.FlatLargeScaleLength = 500;
        Args.DeadFraction         = 0;
        Args.ReadNoiseMean        = sqrt(6);
        Args.ReadNoiseSpread      = 0;
        Args.Seed                 = [];
        Args.OutFile              = [];
    end
    if ~strcmpi(Args.Maps, 'synthetic')
        error('detectorModel: Maps = ''%s'' is not implemented; use ''synthetic''', Args.Maps);
    end
    Tiles = 'ABCD';
    It    = find(Tiles == upper(Args.Tile));
    if isempty(It)
        error('detectorModel: Tile must be A, B, C or D');
    end
    if ~isempty(Args.Seed)   % repeatable, a different stream per tile; the caller's state is restored
        RngSaved   = rng;
        rng(Args.Seed * numel(Tiles) + It - 1);
        RngRestore = onCleanup(@() rng(RngSaved)); %#ok<NASGU>
    end
    Sz = Args.ImageSize;
    if isempty(Sz)
        [~, ~, Info] = ultrasat.simulateCatalogField('Tile', Args.Tile, 'GeometryOnly', true, 'Verbose', false);
        Sz = [Info.NY Info.NX];
    end
    Npix = prod(Sz);

    % dark current: log-normal with mean DarkMean, plus hot pixels
    S    = Args.DarkSpread;
    Dark = single(Args.DarkMean .* exp(S .* randn(Sz) - S.^2 ./ 2));
    Hot  = false(Sz);
    Hot(randperm(Npix, round(Args.HotFraction * Npix))) = true;
    LogR = log10(Args.HotRateRange);
    Dark(Hot) = single(10.^(LogR(1) + diff(LogR) .* rand(nnz(Hot), 1)));

    % sensitivity: pixel-to-pixel times a smooth large-scale pattern, dead pixels 0
    Flat = single(1 + Args.PRNU .* randn(Sz));
    if Args.FlatLargeScale > 0
        L  = Args.FlatLargeScaleLength;
        Nc = ceil(Sz ./ L) + 1;                        % a coarse grid of independent values
        [Xc, Yc] = meshgrid(linspace(1, Sz(2), Nc(2)), linspace(1, Sz(1), Nc(1)));
        Coarse   = randn(Nc);
        [Xf, Yf] = meshgrid(single(1:Sz(2)), single(1:Sz(1)));
        Large    = interp2(Xc, Yc, Coarse, Xf, Yf, 'spline');
        Large    = (Large - mean(Large, 'all')) ./ std(Large, 0, 'all');
        Flat     = Flat .* single(1 + Args.FlatLargeScale .* Large);
    end
    Dead = false(Sz);
    Dead(randperm(Npix, round(Args.DeadFraction * Npix))) = true;
    Flat(Dead) = 0;
    Dark(Dead) = 0;

    % read-out noise
    if Args.ReadNoiseSpread > 0
        ReadNoise = single(max(0, Args.ReadNoiseMean .* (1 + Args.ReadNoiseSpread .* randn(Sz))));
    else
        ReadNoise = Args.ReadNoiseMean;
    end

    Det = struct('Tile', upper(Args.Tile), 'Maps', 'synthetic', 'Seed', Args.Seed, 'Dark', Dark, 'Flat', Flat, ...
                 'ReadNoise', ReadNoise, 'HotMask', Hot, 'DeadMask', Dead, 'DarkMean', mean(Dark, 'all'), ...
                 'Args', Args);
    if ~isempty(Args.OutFile)
        save(Args.OutFile, 'Det', '-v7.3');
    end
end
