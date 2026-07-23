function Result = ELOPsim(Args)
    % Build a table of ULTRASAT ELOP lab-test simulation parameters, save it to a text
    % file, and run the corresponding ultrasat.usim simulation for each row.
    %     The table lists the full factorial combination of the input parameter ranges
    %     (one row per combination), together with the output file names used for the
    %     high-gain/low-gain FITS images written by the corresponding ultrasat.usim run.
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
    %         'ExtMag'      - magnitude of the simulated test source(s). Placeholder
    %                         default, not yet confirmed against a real ELOP test value.
    %                         Default is 15.
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
    % Output : - a table of simulation parameters, one row per parameter combination.
    % NB: the simulations themselves are currently only implemented for Focus = 1; any
    %     other Focus value in the table raises an explicit error, until the focus blur
    %     kernels are implemented (see the Example below). Template 'B' is currently a
    %     regular M x N grid only; a user-supplied custom grid (a table of arcsec shifts)
    %     is a planned future addition, not yet implemented.
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

        Args.TemplateACircleRadius = 10; % [arcsec]
        Args.TemplateBGridM        = 4;
        Args.TemplateBGridN        = 4;
        Args.TemplateBGridSpacing  = 200; % [arcsec]
        Args.TemplatePolygons      = {};
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

                            OutFileHI{Irow} = sprintf('%s_HI.fits', BaseName);
                            OutFileLO{Irow} = sprintf('%s_LO.fits', BaseName);
                        end
                    end
                end
            end
        end
    end

    Result = table(N, Filter, Temperature, Template, Radius, Focus, Rotation, Tile, OutFileHI, OutFileLO);

    TableFullName = sprintf('%s%s%s', Args.OutDir, '/', Args.TableName);
    writetable(Result, TableFullName);

    % run the simulations row by row (see the NB above: only Focus = 1 is currently
    % implemented; any other row raises an explicit error)
    for Irow = 1:1:NumRows

        if Result.Focus(Irow) ~= 1
            error('ultrasat:ELOPsim:FocusNotImplemented', ...
                'Focus = %d is not yet implemented (only Focus = 1 is supported), exiting..', Result.Focus(Irow));
        end

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

        ExtMagVec = repmat(Args.ExtMag, 1, NumSrc);
        ExtSpec   = [repmat(SpecTab(:,2), 1, NumSrc), SpecTab(:,1)]; % usim.m's 'tab' convention: Nwave x (NumExt+1)

        cprintf('hyper', '%s\n', sprintf('ELOPsim row %d/%d: %s', Irow, NumRows, Result.OutFileHI{Irow}));

        Sim = ultrasat.usim( ...
            'ExtProfileType', 'matrix', 'ExtProfileMatrix', ExtProfileMatrix, ...
            'ExtAxisRatio', 1, 'ExtPA', 0, ...
            'ExtSizeRA', ExtSizeRAVec, 'ExtSizeDec', ExtSizeDecVec, ...
            'ExtRA0', CatX, 'ExtDec0', CatY, 'ExtSkyCat', false, ...
            'ExtMag', ExtMagVec, 'ExtEbv', 0, ...
            'ExtSpecType', Args.SpecType, 'ExtSpec', ExtSpec, ...
            'Tile', Result.Tile{Irow}, 'RotAng', Result.Rotation(Irow), ...
            'Exposure', Args.Exposure, 'Jitter', Args.Jitter, 'DarkCurrent', DarkCurrent, ...
            'NoiseZody', false, 'NoiseCher', false, 'NoiseStray', false, ...
            'OutType', 'none');

        [ImageHI, ImageLO] = elopGainImages(Sim.Image);

        FITS.write(ImageHI, sprintf('!%s/%s', Args.OutDir, Result.OutFileHI{Irow}), ...
            'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
        FITS.write(ImageLO, sprintf('!%s/%s', Args.OutDir, Result.OutFileLO{Irow}), ...
            'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);

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
