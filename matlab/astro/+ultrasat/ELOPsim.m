function Result = ELOPsim(Args)
    % Build a table of ULTRASAT ELOP lab-test simulation parameters, save it to a text
    % file, and run the corresponding ultrasat.usim simulation for each row.
    %     The table lists the full factorial combination of the input parameter ranges
    %     (one row per combination), together with the output file names used for the
    %     high-gain/low-gain ADU FITS images and the raw electron-count (CT) FITS image
    %     written by the corresponding ultrasat.usim run; OutFileReg: a DS9-compatible
    %     region file (image/pixel coordinates) marking every modelled source's design
    %     geometry for that row (a circle for Template A/B, the polygon(s) for Template
    %     C/D); and SNRMin/SNRMax: the S/N measured empirically from that row's own CT
    %     image (aperture photometry, not usim's analytic CrudeSNR) across all of the
    %     row's sources -- both equal for a single-source row, and the achieved range
    %     across sources for Template B/D. The table file is re-written after each row
    %     completes, so a long run's results-so-far survive an interruption.
    % Input : * ...,key,val,...
    %         'Filter'      - cell array of filter names. Default is {'UV','VIS'}.
    %         'Temperature' - cell array of detector temperatures [K]. Default is {200,300}.
    %         'Template'    - cell array of spatial source templates. Default is {'A','B','C','D'}.
    %         'Radius'      - cell array of radial distances of the source from the tile's
    %                         inner corner. Default is {2,3,4}.
    %         'Focus'       - cell array of focus positions. Default is {1,2,3,4,5}.
    %         'Rotation'    - cell array of rotation angles [deg]. Default is {0}.
    %         'Tile'        - a single ULTRASAT tile name, common to all the rows of the
    %                         table (not part of the combinatorial grid). Default is 'B'.
    %         'OutDir'      - output directory for the table file. Default is '.'.
    %         'OutName'     - root name used to build the per-simulation output file name
    %                         template. Default is 'USim'.
    %         'TableName'   - name of the saved parameter table text file. Default is
    %                         'ELOPsim_table.csv'.
    %         'Exposure'    - [NumExposures, ExposureDuration_s], passed to usim.m as-is.
    %                         Default is [1 15].
    %         'SpecType'    - passed to usim.m's ExtSpecType. Default is 'Tab' (tabulated
    %                         input spectra).
    %         'Jitter'      - passed to usim.m's Jitter. Default is false.
    %         'Inj'         - passed to usim.m's Inj. NB: has no effect while the
    %                         simulations run in extended-object mode (ExtProfileType
    %                         non-empty), since usim.m only consults Inj on the
    %                         point-source path. Default is 'direct'.
    %         'UVSpecFile'  - a 2-column [wavelength[A], flux[erg/s/cm2/A]] text file, used
    %                         as the tabulated source spectrum for Filter = 'UV' rows.
    %         'VISSpecFile' - same, for Filter = 'VIS' rows.
    %         'ExtMag'      - trial magnitude used for the cheap CrudeSNR estimate each
    %                         row starts with (see 'TargetSNR' below); not the magnitude
    %                         the simulated source(s) actually end up at. Default is 15.
    %         'TargetSNR'   - the actual magnitude used for each row's simulation is
    %                         solved (from the ExtMag trial run's CrudeSNR, which scales
    %                         exactly with flux) so that the row's source(s) reach this
    %                         crude S/N; all sources in a row share one magnitude, solved
    %                         from the first source. Default is 50.
    %         'TemplateACircleRadius' - [arcsec] radius of the Template 'A' test source
    %                         disk, reused for each source of the Template 'B' grid.
    %                         Default is 10.
    %         'TemplateBGridM' - number of Template 'B' grid points along X. Default is 4.
    %         'TemplateBGridN' - number of Template 'B' grid points along Y. Default is 4.
    %         'TemplateBGridSpacing' - [arcsec] center-to-center spacing of the Template
    %                         'B' grid points, axis-aligned with the detector and centered
    %                         on the row's Radius-derived position. Default is 200.
    %         'TemplatePolygons' - a cell array of Nx2 [dRA, dDec] arcsec vertex-offset
    %                         lists, one polygon per cell, all relative to the row's
    %                         Radius-derived position (same axis-aligned convention as the
    %                         Template 'B' grid). Template 'C' requires exactly 1 polygon;
    %                         Template 'D' requires 1 or more. Required (no default) when
    %                         the table includes a 'C' or 'D' row.
    %         'DefocusFWHM' - [arcsec] FWHM of the default Gaussian defocus blur kernel,
    %                         applied to every source's profile for Focus ~= 1 (Focus = 1
    %                         means no blur). The same value is currently used for every
    %                         Focus level > 1 -- a placeholder pending real per-Focus-level
    %                         defocus data. Default is 5.
    %         'DefocusKernel' - an optional 2D kernel matrix that, if non-empty, replaces
    %                         the default Gaussian entirely for every Focus level > 1.
    %                         Default is [].
    % Output : - a table of simulation parameters, one row per parameter combination.
    % NB: Template 'B' is currently a regular M x N grid only; a user-supplied custom grid
    %     (a table of arcsec shifts) is a planned future addition, not yet implemented.
    %     The Focus > 1 defocus blur currently uses one shared kernel (DefocusFWHM /
    %     DefocusKernel) for all Focus levels 2-5; distinct per-level kernels are a
    %     planned future refinement once real ELOP defocus data is available.
    % Author : A. Krassilchtchikov (2026)
    % Example: T = ultrasat.ELOPsim('Template',{'A'},'Focus',{1}, ...
    %              'UVSpecFile','UV_spec.txt','VISSpecFile','VIS_spec.txt');
    arguments
        Args.Filter      = {'UV','VIS'};
        Args.Temperature = {200, 300};
        Args.Template    = {'A','B','C','D'};
        Args.Radius      = {2, 3, 4};
        Args.Focus       = {1, 2, 3, 4, 5};
        Args.Rotation    = {0};
        Args.Tile        = 'B';

        Args.OutDir      = '.';
        Args.OutName     = 'USim';
        Args.TableName   = 'ELOPsim_table.csv';

        Args.Exposure    = [1 15];
        Args.SpecType    = 'Tab';
        Args.Jitter      = false;
        Args.Inj         = 'direct';

        Args.UVSpecFile  = '';
        Args.VISSpecFile = '';
        Args.ExtMag      = 15;
        Args.TargetSNR   = 50;

        Args.TemplateACircleRadius = 10; % [arcsec]
        Args.TemplateBGridM        = 4;
        Args.TemplateBGridN        = 4;
        Args.TemplateBGridSpacing  = 200; % [arcsec]
        Args.TemplatePolygons      = {};

        Args.DefocusFWHM  = 5;  % [arcsec]
        Args.DefocusKernel = [];
    end

    NumRows = numel(Args.Filter) * numel(Args.Temperature) * numel(Args.Template) * ...
              numel(Args.Radius) * numel(Args.Focus) * numel(Args.Rotation);

    N           = zeros(NumRows,1);
    Filter      = cell(NumRows,1);
    Temperature = zeros(NumRows,1);
    Template    = cell(NumRows,1);
    Radius      = zeros(NumRows,1);
    Focus       = zeros(NumRows,1);
    Rotation    = zeros(NumRows,1);
    Tile        = cell(NumRows,1);
    OutFileHI   = cell(NumRows,1);
    OutFileLO   = cell(NumRows,1);
    OutFileCT   = cell(NumRows,1);
    OutFileReg  = cell(NumRows,1);
    SNRMin      = NaN(NumRows,1);
    SNRMax      = NaN(NumRows,1);

    % build the full factorial combination of the parameter ranges (Filter varies
    % slowest, Rotation fastest), and the corresponding output file name template
    Irow = 0;
    for Ifilt = 1:numel(Args.Filter)
        for Itemp = 1:numel(Args.Temperature)
            for Itempl = 1:numel(Args.Template)
                for Irad = 1:numel(Args.Radius)
                    for Ifoc = 1:numel(Args.Focus)
                        for Irot = 1:numel(Args.Rotation)
                            Irow = Irow + 1;

                            N(Irow)           = Irow;
                            Filter{Irow}      = Args.Filter{Ifilt};
                            Temperature(Irow) = Args.Temperature{Itemp};
                            Template{Irow}    = Args.Template{Itempl};
                            Radius(Irow)      = Args.Radius{Irad};
                            Focus(Irow)       = Args.Focus{Ifoc};
                            Rotation(Irow)    = Args.Rotation{Irot};
                            Tile{Irow}        = Args.Tile;

                            BaseName = sprintf('%s_%03d_%s_%dK_Templ%s_Rad%d_F%d_Rot%d_tile%s', ...
                                Args.OutName, Irow, Filter{Irow}, Temperature(Irow), ...
                                Template{Irow}, Radius(Irow), Focus(Irow), Rotation(Irow), Tile{Irow});

                            OutFileHI{Irow}  = sprintf('%s_HI.fits', BaseName);
                            OutFileLO{Irow}  = sprintf('%s_LO.fits', BaseName);
                            OutFileCT{Irow}  = sprintf('%s_CT.fits', BaseName);
                            OutFileReg{Irow} = sprintf('%s_REG.reg', BaseName);
                        end
                    end
                end
            end
        end
    end

    Result = table(N, Filter, Temperature, Template, Radius, Focus, Rotation, Tile, ...
        OutFileHI, OutFileLO, OutFileCT, OutFileReg, SNRMin, SNRMax);

    TableFullName = sprintf('%s%s%s', Args.OutDir, '/', Args.TableName);
    writetable(Result, TableFullName);   % SNRMin/SNRMax start as NaN, filled in and
                                          % re-written after each row below

    % run the simulations row by row
    for Irow = 1:1:NumRows

        DarkCurrent = elopDarkCurrent(Result.Temperature(Irow));

        switch Result.Filter{Irow}
            case 'UV'
                SpecFile = Args.UVSpecFile;
            case 'VIS'
                SpecFile = Args.VISSpecFile;
            otherwise
                error('ultrasat:ELOPsim:UnknownFilter', 'Unknown filter ''%s'', exiting..', Result.Filter{Irow});
        end
        if isempty(SpecFile)
            error('ultrasat:ELOPsim:NoSpecFile', ...
                'No spectrum file given for Filter = ''%s'' (see UVSpecFile/VISSpecFile), exiting..', Result.Filter{Irow});
        end
        SpecTab = readmatrix(SpecFile);            % [wavelength[A], flux[erg/s/cm2/A]]

        [CatX0, CatY0] = elopSourcePixelPos(Result.Radius(Irow), Result.Tile{Irow});

        [CatX, CatY, ExtSizeRAVec, ExtSizeDecVec, ExtProfileMatrix] = ...
            elopTemplateSources(Result.Template{Irow}, CatX0, CatY0, Args);

        NumSrc = numel(CatX);

        if Result.Focus(Irow) ~= 1
            for Ip = 1:1:NumSrc
                GridScaleArcsec = ExtSizeRAVec(Ip) / size(ExtProfileMatrix{Ip}, 2);
                Kernel = elopFocusKernel(GridScaleArcsec, size(ExtProfileMatrix{Ip}), Args);
                ExtProfileMatrix{Ip} = elopConvolveKernel(ExtProfileMatrix{Ip}, Kernel);
            end
        end

        ExtSpec = [repmat(SpecTab(:,2), 1, NumSrc), SpecTab(:,1)]; % usim.m's 'tab' convention: Nwave x (NumExt+1)

        CommonArgs = { ...
            'ExtProfileType', 'matrix', 'ExtProfileMatrix', ExtProfileMatrix, ...
            'ExtAxisRatio', 1, 'ExtPA', 0, ...
            'ExtSizeRA', ExtSizeRAVec, 'ExtSizeDec', ExtSizeDecVec, ...
            'ExtRA0', CatX, 'ExtDec0', CatY, 'ExtSkyCat', false, ...
            'ExtEbv', 0, 'ExtSpecType', Args.SpecType, 'ExtSpec', ExtSpec, ...
            'Tile', Result.Tile{Irow}, 'RotAng', Result.Rotation(Irow), ...
            'Exposure', Args.Exposure, 'Jitter', Args.Jitter, 'DarkCurrent', DarkCurrent, ...
            'NoiseZody', false, 'NoiseCher', false, 'NoiseStray', false};

        cprintf('hyper', '%s\n', sprintf('ELOPsim row %d/%d: %s', Irow, NumRows, Result.OutFileHI{Irow}));

        % cheap trial pass: get CrudeSNR at Args.ExtMag (no noise/ADU pipeline, no
        % files), then solve for the magnitude that reaches Args.TargetSNR, using the
        % first source as the shared reference for the whole row (CrudeSNR scales
        % exactly with flux, so this is an exact closed-form correction, not a guess)
        TrialMagVec = repmat(Args.ExtMag, 1, NumSrc);
        Trial = ultrasat.usim(CommonArgs{:}, 'ExtMag', TrialMagVec, 'SNROnly', true, 'OutType', 'none');
        SNRTrial = Trial.CatData.Catalog(1, strcmp(Trial.CatData.ColNames, 'SNR'));
        if ~(SNRTrial > 0) || isnan(SNRTrial)
            error('ultrasat:ELOPsim:BadTrialSNR', ...
                'Trial CrudeSNR = %g at ExtMag = %g is not usable to solve for TargetSNR, exiting..', SNRTrial, Args.ExtMag);
        end
        ExtMagRow = Args.ExtMag + 2.5 * log10(SNRTrial / Args.TargetSNR);
        ExtMagVec = repmat(ExtMagRow, 1, NumSrc);

        Sim = ultrasat.usim(CommonArgs{:}, 'ExtMag', ExtMagVec, 'OutType', 'none');

        [ImageHI, ImageLO] = elopGainImages(Sim.Image);

        FITS.write(ImageHI, sprintf('!%s/%s', Args.OutDir, Result.OutFileHI{Irow}), ...
            'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
        FITS.write(ImageLO, sprintf('!%s/%s', Args.OutDir, Result.OutFileLO{Irow}), ...
            'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
        FITS.write(Sim.Image, sprintf('!%s/%s', Args.OutDir, Result.OutFileCT{Irow}), ...
            'DataType', 'single', 'Append', false, 'OverWrite', true, 'WriteTime', true);

        % a DS9-compatible region file marking every modelled source's own design
        % geometry (a circle for Template A/B, the user-supplied polygon(s) for
        % Template C/D) in image (pixel) coordinates -- the FITS files above carry no
        % WCS. Uses the nominal/design shape, not the PSF/focus-blurred apparent extent.
        elopWriteRegionFile(sprintf('%s/%s', Args.OutDir, Result.OutFileReg{Irow}), ...
            Result.Template{Irow}, CatX, CatY, CatX0, CatY0, Args);

        % empirically measure the S/N of every source directly from the simulated
        % counts image (aperture photometry; background/noise from a fixed patch chosen
        % to be as far as possible from every source in the row, not a local annulus --
        % this lets the aperture itself grow up to half the nearest-neighbor spacing
        % instead of a quarter, much closer to true curve-of-growth convergence for
        % tightly-packed rows -- not usim's own analytic CrudeSNR, which is
        % ~Args.TargetSNR by construction for the reference source and so wouldn't be
        % an independent check of what's actually visible in the image)
        [BackPerPix, NoisePerPix] = elopReferenceBackground(Sim.Image, CatX, CatY);
        SNRVec = zeros(1, NumSrc);
        for Ip = 1:1:NumSrc
            if NumSrc > 1
                OtherDist = sqrt((CatX([1:Ip-1, Ip+1:end]) - CatX(Ip)).^2 + ...
                                  (CatY([1:Ip-1, Ip+1:end]) - CatY(Ip)).^2);
                MaxApertureRadius = min(OtherDist) / 2;   % keep the aperture clear of the
            else                                          % nearest neighbor's own aperture
                MaxApertureRadius = Inf;
            end
            SNRVec(Ip) = elopMeasureSNR(Sim.Image, CatX(Ip), CatY(Ip), MaxApertureRadius, ...
                BackPerPix, NoisePerPix);
        end
        Result.SNRMin(Irow) = min(SNRVec);
        Result.SNRMax(Irow) = max(SNRVec);

        writetable(Result, TableFullName);   % keep the table up to date row by row, so a
                                              % long run's results-so-far survive an
                                              % interruption

    end

end

function [CatX, CatY] = elopSourcePixelPos(Radius, Tile)
    % Pixel position at angular distance Radius [deg] from the tile's inner corner,
    % along the diagonal into the tile's FOV, matching usim.m's own radial-distance
    % convention and per-tile inner-corner geometry.
    % NB: the physical constants and per-tile geometry below must be kept in sync with
    % usim.m's own values (FocalLength, PixelSizeMm, ImageSizeX/Y, and the Tile switch).
    RAD = 180 / pi;
    FocalLength = 360;      % [mm]
    PixelSizeMm = 9.5e-3;   % [mm]
    PixSizeDeg  = (PixelSizeMm / FocalLength) * RAD;  % [deg/pix]
    ImageSizeX  = 4738;     % [pix]
    ImageSizeY  = 4738;     % [pix]

    switch Tile
        case 'A'
            X0 = ImageSizeX + 0.5; Y0 = 0.5;              Theta = 135;
        case 'B'
            X0 = 0.5;              Y0 = 0.5;              Theta = 45;
        case 'C'
            X0 = 0.5;              Y0 = ImageSizeY + 0.5; Theta = -45;
        case 'D'
            X0 = ImageSizeX + 0.5; Y0 = ImageSizeY + 0.5; Theta = 225;
        otherwise
            error('ultrasat:ELOPsim:InvalidTile', 'Invalid tile name, exiting..');
    end

    PixOffset = Radius / PixSizeDeg;
    CatX = X0 + PixOffset * cosd(Theta);
    CatY = Y0 + PixOffset * sind(Theta);
end

function [CatX, CatY, ExtSizeRAVec, ExtSizeDecVec, ExtProfileMatrix] = elopTemplateSources(Template, CatX0, CatY0, Args)
    % Per-source ExtRA0/ExtDec0 (pixel, see ExtSkyCat=false)/ExtSizeRA/ExtSizeDec/
    % ExtProfileMatrix for a given ELOP test Template, centered at (CatX0, CatY0) (the
    % row's Radius-derived position). Template 'A' is a single circular disk source.
    % Template 'B' is a GridM x GridN raster grid of disk sources, axis-aligned with the
    % detector. Template 'C'/'D' are one, resp. one or more, user-supplied polygons
    % (Args.TemplatePolygons), each becoming its own extended source.
    switch Template
        case 'A'
            ExtSize = 2 * Args.TemplateACircleRadius;   % [arcsec] bounding-box size of the disk
            CatX = CatX0;
            CatY = CatY0;
            ExtSizeRAVec  = ExtSize;
            ExtSizeDecVec = ExtSize;
            ExtProfileMatrix = {elopCircleMask(101)};

        case 'B'
            [CatX, CatY] = elopGridPositions(CatX0, CatY0, ...
                Args.TemplateBGridM, Args.TemplateBGridN, Args.TemplateBGridSpacing);
            NumSrc = numel(CatX);
            ExtSize = 2 * Args.TemplateACircleRadius;   % [arcsec] bounding-box size of each disk
            ExtSizeRAVec     = repmat(ExtSize, 1, NumSrc);
            ExtSizeDecVec    = repmat(ExtSize, 1, NumSrc);
            ExtProfileMatrix = repmat({elopCircleMask(101)}, 1, NumSrc);

        case {'C', 'D'}
            Polygons = Args.TemplatePolygons;
            if isempty(Polygons)
                error('ultrasat:ELOPsim:NoPolygons', ...
                    'Template ''%s'' requires Args.TemplatePolygons to be set, exiting..', Template);
            end
            if strcmp(Template, 'C') && numel(Polygons) ~= 1
                error('ultrasat:ELOPsim:PolygonCountMismatch', ...
                    'Template ''C'' requires exactly 1 polygon in Args.TemplatePolygons (got %d), exiting..', numel(Polygons));
            end
            NumSrc = numel(Polygons);
            CatX = zeros(1, NumSrc); CatY = zeros(1, NumSrc);
            ExtSizeRAVec = zeros(1, NumSrc); ExtSizeDecVec = zeros(1, NumSrc);
            ExtProfileMatrix = cell(1, NumSrc);
            for Ip = 1:1:NumSrc
                [CatX(Ip), CatY(Ip), ExtSizeRAVec(Ip), ExtSizeDecVec(Ip), ExtProfileMatrix{Ip}] = ...
                    elopPolygonSource(Polygons{Ip}, CatX0, CatY0, 101);
            end

        otherwise
            error('ultrasat:ELOPsim:TemplateNotImplemented', ...
                'Template ''%s'' is not yet implemented (only ''A'', ''B'', ''C'', ''D'' are supported), exiting..', Template);
    end
end

function [CatX, CatY] = elopGridPositions(CatX0, CatY0, GridM, GridN, GridSpacingArcsec)
    % Pixel positions of a GridM x GridN raster grid of sources, axis-aligned with the
    % detector, centered at (CatX0, CatY0), with the given center-to-center spacing.
    PixSizeArcsec = elopPixSizeArcsec();
    PixSpacing = GridSpacingArcsec / PixSizeArcsec;

    OffM = ((1:GridM) - (GridM + 1) / 2) * PixSpacing;
    OffN = ((1:GridN) - (GridN + 1) / 2) * PixSpacing;
    [OffX, OffY] = meshgrid(OffM, OffN);

    CatX = CatX0 + OffX(:)';
    CatY = CatY0 + OffY(:)';
end

function [CatX, CatY, ExtSizeRA, ExtSizeDec, Mask] = elopPolygonSource(Vertices, CatX0, CatY0, NPix)
    % A single polygon source: Vertices is an Nx2 [dRA, dDec] arcsec vertex-offset list,
    % relative to the shared center (CatX0, CatY0), axis-aligned with the detector.
    % Returns the source's own center (its vertex bounding-box center, which need not
    % coincide with (CatX0, CatY0) for an asymmetric polygon), its bounding-box size in
    % arcsec, and an NPix x NPix mask (1 inside the polygon, 0 outside) spanning that
    % bounding box, via inpolygon (as used elsewhere in AstroPack, e.g.
    % imUtil.sources.polygonFlux), rather than the Image Processing Toolbox's poly2mask.
    PixSizeArcsec = elopPixSizeArcsec();
    VertPix = Vertices / PixSizeArcsec;   % Nx2 pixel offsets from (CatX0, CatY0)

    MinX = min(VertPix(:,1)); MaxX = max(VertPix(:,1));
    MinY = min(VertPix(:,2)); MaxY = max(VertPix(:,2));

    % pad the smaller dimension symmetrically so the bounding box (and hence the
    % resulting profile stamp) is square: imUtil.art.addSources computes a single
    % injection-window radius from the stamp's row count and applies it to both axes,
    % so a non-square stamp silently produces a wrong (or out-of-bounds) column window.
    % The polygon itself is unaffected -- inpolygon still rasterizes against the true,
    % unpadded vertices, so the extra grid cells are correctly masked to 0 (an empty
    % margin around the real shape), and the padding is symmetric so the source's
    % center position doesn't shift.
    Width  = MaxX - MinX;
    Height = MaxY - MinY;
    if Width > Height
        Pad = (Width - Height) / 2;
        MinY = MinY - Pad; MaxY = MaxY + Pad;
    elseif Height > Width
        Pad = (Height - Width) / 2;
        MinX = MinX - Pad; MaxX = MaxX + Pad;
    end

    CatX = CatX0 + (MinX + MaxX) / 2;
    CatY = CatY0 + (MinY + MaxY) / 2;

    ExtSizeRA  = (MaxX - MinX) * PixSizeArcsec;   % [arcsec]
    ExtSizeDec = (MaxY - MinY) * PixSizeArcsec;   % [arcsec]

    VecX = linspace(MinX, MaxX, NPix);
    VecY = linspace(MinY, MaxY, NPix);
    [GridX, GridY] = meshgrid(VecX, VecY);
    Mask = double(inpolygon(GridX, GridY, VertPix(:,1), VertPix(:,2)));
end

function PixSizeArcsec = elopPixSizeArcsec()
    % [arcsec/pix] the ULTRASAT pixel scale, matching usim.m's own PixSizeDeg constants.
    RAD = 180 / pi;
    FocalLength = 360;      % [mm]
    PixelSizeMm = 9.5e-3;   % [mm]
    PixSizeArcsec = (PixelSizeMm / FocalLength) * RAD * 3600;
end

function Kernel = elopFocusKernel(GridScaleArcsec, MaskSize, Args)
    % The defocus blur kernel for a profile grid at GridScaleArcsec [arcsec/cell].
    % Args.DefocusKernel, if non-empty, is used as-is (an arbitrary user-supplied
    % kernel, already at the mask's own pixel grid scale). Otherwise a default Gaussian
    % kernel is built with FWHM = Args.DefocusFWHM [arcsec], sized MaskSize.
    if ~isempty(Args.DefocusKernel)
        Kernel = Args.DefocusKernel;
    else
        FWHMPix = Args.DefocusFWHM / GridScaleArcsec;
        Kernel = elopGaussianKernel(FWHMPix, MaskSize);
    end
end

function Kernel = elopGaussianKernel(FWHMPix, MaskSize)
    % A normalized 2D Gaussian kernel, MaskSize(1) x MaskSize(2), with the given FWHM
    % [pix].
    Sigma = FWHMPix / (2 * sqrt(2 * log(2)));
    VecX  = (1:MaskSize(2)) - (MaskSize(2) + 1) / 2;
    VecY  = (1:MaskSize(1)) - (MaskSize(1) + 1) / 2;
    [X, Y] = meshgrid(VecX, VecY);
    Kernel = exp( -(X.^2 + Y.^2) / (2 * Sigma^2) );
    Kernel = Kernel / sum(Kernel, 'all');
end

function Mask = elopConvolveKernel(Mask, Kernel)
    % Convolve a Template profile mask with an arbitrary 2D kernel (e.g. a defocus blur
    % kernel), renormalizing the kernel to unit sum first.
    Kernel = Kernel / sum(Kernel, 'all');
    Mask = conv2(Mask, Kernel, 'same');
end

function SNR = elopMeasureSNR(Image, CatX, CatY, MaxApertureRadius, BackPerPix, NoisePerPix)
    % Empirically measure the S/N of a source at pixel position (CatX, CatY) directly
    % from Image, with the aperture radius itself determined empirically via a curve of
    % growth, rather than assumed from the source's nominal profile size -- a small
    % profile can still be much larger in the actual image once PSF blur dominates, and
    % a fixed formula misses that. BackPerPix/NoisePerPix (from elopReferenceBackground)
    % are the background level and its empirical pixel-to-pixel scatter, measured once
    % per row from a source-free patch, not a local annulus. MaxApertureRadius caps the
    % growth (e.g. to stay clear of a neighboring source in a multi-source row); pass
    % Inf for an isolated source.
    %
    % NB: the curve of growth is evaluated at every trial radius (not stopped at the
    % first quiet step) and the aperture is set to the LAST radius whose increment over
    % the previous radius is statistically significant (> 3x the expected per-step
    % noise) -- real curves can have a quiet, still-rising-later stretch before the true
    % signal (e.g. near the profile core, before the PSF wings pick up most of the
    % flux), where a naive first-quiet-step-wins rule stops too early. That radius's own
    % Flux already includes the significant jump's full contribution, so no extra margin
    % step is added -- the trial radii are log-spaced, and stepping one further (e.g.
    % 18 -> 27) nearly triples the aperture area for no additional signal, needlessly
    % raising the noise.
    RadiiTrial = [3 5 8 12 18 27 40 60 90 130];
    RadiiTrial = RadiiTrial(RadiiTrial <= MaxApertureRadius);
    if isempty(RadiiTrial)
        RadiiTrial = min(3, MaxApertureRadius);
    end

    SignificanceRatio = 3;
    PrevFlux = 0;
    LastSignificant = 1;
    for Ri = 1:1:numel(RadiiTrial)
        [Flux, NAperture] = elopApertureFlux(Image, CatX, CatY, RadiiTrial(Ri), BackPerPix);
        ExpectedNoise = sqrt(max(NAperture,1)) * NoisePerPix;
        if (Flux - PrevFlux) > SignificanceRatio * ExpectedNoise
            LastSignificant = Ri;
        end
        PrevFlux = Flux;
    end
    RConverged = RadiiTrial(LastSignificant);

    [Signal, NAperture] = elopApertureFlux(Image, CatX, CatY, RConverged, BackPerPix);
    Noise = sqrt(NAperture) * NoisePerPix;

    SNR = Signal / Noise;
end

function [Flux, NAperture] = elopApertureFlux(Image, CatX, CatY, R, BackPerPix)
    % Background-subtracted flux in a circular aperture of radius R centered at
    % (CatX, CatY), given the per-pixel background level BackPerPix.
    [SizeY, SizeX] = size(Image);
    Box = ceil(R) + 2;
    XRange = max(1, round(CatX) - Box):min(SizeX, round(CatX) + Box);
    YRange = max(1, round(CatY) - Box):min(SizeY, round(CatY) + Box);
    [GridX, GridY] = meshgrid(XRange, YRange);
    Dist = sqrt((GridX - CatX).^2 + (GridY - CatY).^2);
    Sub = Image(YRange, XRange);

    ApertureMask = Dist <= R;
    NAperture = sum(ApertureMask, 'all');
    Flux = sum(Sub(ApertureMask), 'all') - NAperture * BackPerPix;
end

function [BackPerPix, NoisePerPix] = elopReferenceBackground(Image, CatXAll, CatYAll)
    % Background level and its empirical pixel-to-pixel scatter, measured from a fixed
    % 100x100 patch near whichever of the image's 4 corners is farthest from every
    % source in CatXAll/CatYAll -- avoids source contamination without needing an
    % annulus around each individual source, so aperture growth in elopMeasureSNR isn't
    % constrained by needing room for a local annulus too.
    [SizeY, SizeX] = size(Image);
    Margin = 60; PatchSize = 100;
    Corners = [ Margin,               Margin; ...
                SizeX-Margin-PatchSize, Margin; ...
                Margin,               SizeY-Margin-PatchSize; ...
                SizeX-Margin-PatchSize, SizeY-Margin-PatchSize];

    MinDist = zeros(4,1);
    for Ic = 1:1:4
        CX = Corners(Ic,1) + PatchSize/2;
        CY = Corners(Ic,2) + PatchSize/2;
        MinDist(Ic) = min(sqrt((CatXAll - CX).^2 + (CatYAll - CY).^2));
    end
    [~, Best] = max(MinDist);

    XRange = Corners(Best,1):(Corners(Best,1) + PatchSize - 1);
    YRange = Corners(Best,2):(Corners(Best,2) + PatchSize - 1);
    Patch = Image(YRange, XRange);

    % NB: mean, not median -- the patch is already guaranteed source-free (chosen to be
    % far from every source), so there's no outlier to be robust against, and at these
    % low background counts (Poisson, right-skewed) the median measurably underestimates
    % the true mean; that small per-pixel bias, multiplied by a large aperture area in
    % elopMeasureSNR's curve of growth, was inflating the enclosed flux without bound
    BackPerPix  = mean(Patch, 'all');
    NoisePerPix = std(Patch, 0, 'all');
end

function elopWriteRegionFile(FileName, Template, CatX, CatY, CatX0, CatY0, Args)
    % Write a DS9-compatible region file (via DS9_new.regionWrite) marking every
    % modelled source in a row, in image (pixel) coordinates. Template 'A'/'B': a
    % circle per source, of the nominal Args.TemplateACircleRadius. Template 'C'/'D':
    % the user-supplied polygon(s) (Args.TemplatePolygons), reconstructed to absolute
    % pixel vertices from the row's shared center (CatX0, CatY0) -- the same convention
    % elopPolygonSource uses -- rather than each polygon's own bounding-box center
    % (CatX/CatY), since the vertices are defined relative to the shared center.
    PixSizeArcsec = elopPixSizeArcsec();
    switch Template
        case {'A', 'B'}
            RadiusPix = Args.TemplateACircleRadius / PixSizeArcsec;
            DS9_new.regionWrite([CatX(:), CatY(:)], 'FileName', FileName, 'Coo', 'image', ...
                'Marker', 'circle', 'Size', RadiusPix, 'Color', 'green', ...
                'PrintIndividualProp', false);

        case {'C', 'D'}
            for Ip = 1:1:numel(Args.TemplatePolygons)
                Vertices = Args.TemplatePolygons{Ip};
                VertX = CatX0 + Vertices(:,1) / PixSizeArcsec;
                VertY = CatY0 + Vertices(:,2) / PixSizeArcsec;
                DS9_new.regionWrite([VertX(:), VertY(:)], 'FileName', FileName, 'Coo', 'image', ...
                    'Marker', 'polygon', 'Color', 'green', 'Append', Ip > 1, ...
                    'PrintIndividualProp', false);
            end

        otherwise
            error('ultrasat:ELOPsim:TemplateNotImplemented', ...
                'Template ''%s'' is not yet implemented (only ''A'', ''B'', ''C'', ''D'' are supported), exiting..', Template);
    end
end

function Mask = elopCircleMask(NPix)
    % A normalized circular disk mask (1 inside, 0 outside), NPix x NPix.
    Vec       = linspace(-1, 1, NPix);
    [X, Y]    = meshgrid(Vec, Vec);
    Mask = double(sqrt(X.^2 + Y.^2) <= 1);
end

function DarkCurrent = elopDarkCurrent(Temperature)
    % [e-/pix/s] dark current rate for the two calibrated ELOP test temperatures.
    switch Temperature
        case 200
            DarkCurrent = 0.03;
        case 300
            DarkCurrent = 20;
        otherwise
            error('ultrasat:ELOPsim:NoDarkCurrent', ...
                'No dark current calibration available for Temperature = %d K, exiting..', Temperature);
    end
end

function [ImageHI, ImageLO] = elopGainImages(Image)
    % Encode a counts (e-) image into two separate ADU images, as if the whole frame
    % had been read out entirely through the high-gain, resp. low-gain, amplifier chain
    % -- unlike usim.m's own ADU output, which mixes both gains pixel-by-pixel by signal
    % level. Reproduces usim.m's own e-/ADU conversion (Back/E2ADU section) and reuses
    % ultrasat.e2ADU for the packing step, without modifying usim.m.
    % NB: E2ADUhigh/E2ADUlow must be kept in sync with the values hardcoded in usim.m.
    % Input  : - a counts (e-) image, as returned in usim.m's output AstroImage.Image.
    % Output : - the all-high-gain ADU image.
    %          - the all-low-gain ADU image.
    % Author : A. Krassilchtchikov (2026)
    % Example: [ImHI, ImLO] = ultrasat.ELOPsim>elopGainImages(Sim.Image);
    E2ADUhigh = 1.185;  % usim.m's Back/E2ADU section
    E2ADUlow  = 0.074;  % usim.m's Back/E2ADU section

    GainHI = max(Image .* E2ADUhigh, 1);   % ultrasat.e2ADU requires Count >= 1
    GainLO = max(Image .* E2ADUlow,  1);

    ImageHI = ultrasat.e2ADU(GainHI, zeros(size(Image)));  % LowGain flag = 0 (high gain)
    ImageLO = ultrasat.e2ADU(GainLO, ones(size(Image)));   % LowGain flag = 1 (low gain)
end
