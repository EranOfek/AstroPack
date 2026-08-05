function Result = ELOPsim(Args)
    % Build a table of ULTRASAT ELOP lab-test simulation parameters, save it to a text
    % file, and run the corresponding ultrasat.usim simulation for each row.
    %     The table lists the full factorial combination of the input parameter ranges
    %     (one row per combination), together with the output file names for that row's
    %     products (which of these are actually written, and non-empty in the table,
    %     depends on Args.OutMode): OutFileHI/OutFileLO, the 'raw'-mode all-high-gain/
    %     all-low-gain ADU FITS images; OutFileADU/OutFileGain, the 'production'-mode
    %     single gain-selected ADU FITS image and its per-pixel gain-selection map;
    %     OutFileCT, the raw electron-count FITS image (always written); OutFileReg, a
    %     DS9-compatible region file (image/pixel coordinates) marking every modelled
    %     source's design geometry for that row (a circle for Template A/B, the
    %     polygon(s) for Template C/D, always written); and SNRMin/SNRMax, the S/N
    %     measured empirically from that row's own CT image (aperture photometry, not
    %     usim's analytic CrudeSNR) across all of the row's sources -- both equal for a
    %     single-source row, and the achieved range across sources for Template B/D. The
    %     table file is re-written after each row completes, so a long run's
    %     results-so-far survive an interruption.
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
    %         'OutMode'     - which ADU-level product(s) to write per row: 'raw' (the
    %                         camera's raw-readout mode: separate all-high-gain and
    %                         all-low-gain ADU images, OutFileHI/OutFileLO), 'production'
    %                         (the camera's production-readout mode: a single per-pixel
    %                         gain-selected ADU image plus a separate per-pixel
    %                         gain-selection map, OutFileADU/OutFileGain -- both derived
    %                         from usim.m's own gain-selection threshold/constants,
    %                         reproduced locally rather than by modifying usim.m), or
    %                         'both' (write all four). The raw electron-count image
    %                         (OutFileCT) and region file (OutFileReg) are always written
    %                         regardless of OutMode. Default is 'production'.
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
    %         'TemplateADiametersMm' - [mm] vector of the Template 'A' in-line circular
    %                         source disk diameters (left to right); each a physical mask
    %                         size, converted to arcsec via size2ang. Default is
    %                         [0.2 0.4 0.55].
    %         'TemplateAGapsMm' - [mm] vector (length numel(TemplateADiametersMm)-1) of
    %                         the gaps between consecutive Template 'A' sources, converted
    %                         via size2ang. The source line is centered on the row's
    %                         Radius-derived position. Default is [17 17].
    %         'TemplateARotation' - [deg] rotation of the Template 'A' source line about
    %                         its center (0 = along detector X). Distinct from Args.Rotation
    %                         (the whole-frame RotAng). Default is 0.
    %         'TemplateBUpperDiameterMm' / 'TemplateBLowerDiameterMm' - [mm] circle
    %                         diameter of every source in the upper, resp. lower, set of
    %                         Template 'B' (converted via size2ang). Defaults 0.18, 0.57.
    %         'TemplateBUpperColGapsMm' / 'TemplateBUpperRowGapsMm' - [mm] gaps between
    %                         consecutive columns (left->right), resp. rows (top->bottom),
    %                         of the upper set; the set is (numel(ColGaps)+1) x
    %                         (numel(RowGaps)+1) sources. Default [5.145 5.290 5.435] each.
    %         'TemplateBLowerColGapsMm' / 'TemplateBLowerRowGapsMm' - as above, for the
    %                         lower set. Default [5.435 5.290 5.145] each.
    %         'SetShift' - [mm] [horizontal, vertical] offset from the upper set's
    %                         upper-left source to the lower set's upper-left source
    %                         (converted via size2ang). Default [25 25].
    %         'TemplateBRotation' - [deg] rotation of the whole Template 'B' two-set
    %                         pattern about its centroid (0 = detector-aligned: columns
    %                         +X, rows top->bottom -Y). Default is 0.
    %         'TemplatePolygonsMm' - a 1x1 cell array holding a single Nx2 [dRA, dDec]
    %                         vertex-offset list [mm] -- Template 'C''s reticle shape.
    %                         Converted to arcsec via size2ang, shifted so its area-
    %                         centroid sits at the row's Radius-derived position, and
    %                         rotated by TemplateCRotation. Default is the T-4666-1754-00
    %                         calibration-target pentagon.
    %         'TemplateCRotation' - [deg] rotation of the Template 'C' polygon about its
    %                         centroid (CCW, 0 = as drawn: dRA->+X, dDec->+Y). Default 0.
    %         'TemplateDPolygonsMm' - a cell array of Nx2 [dRA, dDec] vertex-offset lists
    %                         [mm] -- Template 'D''s 5 polygon shapes (T-4666-1751-00:
    %                         2 rhombi, a triangle, and 2 arrows), positioned relative to
    %                         the substrate center. Converted via size2ang and rotated by
    %                         TemplateDRotation. Default is the T-4666-1751-00 target.
    %         'TemplateDHoleMm' - [dRA, dDec, diameter] [mm] of Template 'D''s circular
    %                         hole -- a 6th (disk) source. Default is [0 21.998 0.10].
    %         'TemplateDRotation' - [deg] rotation of the whole Template 'D' assembly
    %                         about the substrate-center anchor (CCW, 0 = as drawn).
    %                         Default is 0.
    %         'DefocusKernelShape' - 'tophat' (uniform disk, hard cutoff) or 'topcosine'
    %                         (cosine-tapered disk, smoothly reaching 0 at the same
    %                         diameter instead of an abrupt edge). Applied to every
    %                         source's profile for Focus ~= 1 (Focus = 1 means no blur).
    %                         Default is 'tophat'.
    %         'DefocusDiameterMicron' - a 4-element vector, the blur kernel diameter
    %                         [micron] for Focus = 2, 3, 4, 5 respectively, converted to
    %                         arcsec via D_arcsec = D_micron*(1000*1.1*330)/206265.
    %                         Default is [10 18 45 90].
    %         'DefocusKernel' - an optional 2D kernel matrix that, if non-empty, replaces
    %                         DefocusKernelShape/DefocusDiameterMicron entirely for every
    %                         Focus level > 1. Default is [].
    %         'ImRes'       - ELOPsim's own profile-building/blur-convolution grid
    %                         resolution: every source profile (circle/polygon) is built
    %                         and blurred at 1/ImRes of a detector pixel, then resampled
    %                         to UsimImRes before being handed to usim.m. NOT the same as
    %                         usim.m's own Args.ImRes (see UsimImRes below). Default is 10.
    %         'UsimImRes'   - passed through to usim.m's own Args.ImRes and
    %                         ExtOversampling; the resolution ELOPsim's own ImRes-grid
    %                         profile is resampled to before being handed to usim.m.
    %                         Default is 5.
    % Output : - a table of simulation parameters, one row per parameter combination.
    % NB: Template 'B' is currently a regular M x N grid only; a user-supplied custom grid
    %     (a table of arcsec shifts) is a planned future addition, not yet implemented.
    %     The magnitude used for Focus > 1 rows is not solved independently -- it reuses
    %     the value solved at Focus = 1 for the same Filter/Temperature/Template/Radius/
    %     Rotation/Tile combination (Focus = 1 must therefore be included in Args.Focus
    %     and be processed before the other Focus values for that combination).
    % Author : A. Krassilchtchikov (2026)
    % Example: % run the full default grid (all Filter/Temperature/Template/Radius/
    %          % Focus/Rotation combinations, currently 240 rows)
    %          T = ultrasat.ELOPsim('OutDir','.','OutName','USim', ...
    %              'UVSpecFile','UV_spec.txt','VISSpecFile','VIS_spec.txt');
    %          % or restrict the grid to a subset:
    %          T = ultrasat.ELOPsim('Template',{'A'},'Focus',{1}, ...
    %              'UVSpecFile','UV_spec.txt','VISSpecFile','VIS_spec.txt');
    arguments
        Args.Filter      = {'UV','VIS'};
        Args.Temperature = {200, 300};
        Args.Template    = {'A','B','C','D'};
        Args.Radius      = {1, 4, 6};
        Args.Focus       = {1, 2, 3, 4, 5};
        Args.Rotation    = {0};
        Args.Tile        = 'B';

        Args.OutDir      = '.';
        Args.OutName     = 'USim';
        Args.TableName   = 'ELOPsim_table.csv';
        Args.OutMode     = 'raw'; % 'raw' (HI/LO), 'production' (ADU/GAIN), or 'both'

        Args.Exposure    = [1 15];
        Args.SpecType    = 'Tab';
        Args.Jitter      = false;
        Args.Inj         = 'direct';

        Args.UVSpecFile  = '';
        Args.VISSpecFile = '';
        Args.ExtMag      = 14;
        Args.TargetSNR   = 100;

        Args.TemplateADiametersMm  = [0.2 0.4 0.55]; % [mm] per-source disk diameters, left->right
        Args.TemplateAGapsMm       = [17 17];        % [mm] gaps between consecutive sources
        Args.TemplateARotation     = 0;              % [deg] rotation of the source line about its center

        Args.TemplateBUpperDiameterMm = 0.18;               % [mm] upper set circle diameter
        Args.TemplateBUpperColGapsMm  = [5.145 5.290 5.435]; % [mm] upper set column gaps (L->R)
        Args.TemplateBUpperRowGapsMm  = [5.145 5.290 5.435]; % [mm] upper set row gaps (top->bottom)
        Args.TemplateBLowerDiameterMm = 0.57;               % [mm] lower set circle diameter
        Args.TemplateBLowerColGapsMm  = [5.435 5.290 5.145]; % [mm] lower set column gaps (L->R)
        Args.TemplateBLowerRowGapsMm  = [5.435 5.290 5.145]; % [mm] lower set row gaps (top->bottom)
        Args.SetShift                 = [25 25];             % [mm] [horizontal, vertical] upper->lower set
        Args.TemplateBRotation        = 0;                   % [deg] whole-template rotation about the centroid

        Args.TemplatePolygonsMm    = { [0        0;       ...   % [mm] Template 'C' reticle shape
                                        40.635   0;       ...   % (T-4666-1754-00 calibration target),
                                        37.365  37.365;   ...   % [dRA, dDec] vertex offsets, as drawn
                                        37.365  40.635;   ...
                                        0       37.365] };
        Args.TemplateCRotation     = 0;    % [deg] whole-polygon rotation about its centroid

        % Template 'D' (T-4666-1751-00): 5 polygons + 1 circular hole, [mm] [dRA, dDec]
        % vertices relative to the substrate center (the anchor)
        Args.TemplateDPolygonsMm   = { ...
            [-18.500  21.973; -21.121  16.734; -18.500  11.499; -15.882  16.734];             ... % rhombus (top-left)
            [ 18.501  21.973;  15.883  16.734;  18.501  11.499;  21.119  16.734];             ... % rhombus (top-right)
            [ -5.820   5.999;  -0.003  -5.640;   5.818   5.999];                              ... % triangle (center)
            [-18.729 -12.093; -18.729 -13.546; -10.001 -13.546; -10.001 -21.113;             ... % left arrow
             -12.912 -21.113; -12.912 -16.457; -18.729 -16.457; -18.729 -17.910; -21.640 -15.003]; ...
            [ 16.546  -7.503;  16.546 -18.870;  15.089 -18.870;  18.000 -21.780;             ... % down arrow
              20.910 -18.870;  19.453 -18.870;  19.453  -7.503] };
        Args.TemplateDHoleMm       = [0 21.998 0.10];  % [mm] hole [dRA, dDec, diameter]
        Args.TemplateDRotation     = 0;                % [deg] whole-assembly rotation about the anchor

        Args.DefocusKernelShape    = 'topcosine'; % 'tophat' or 'topcosine'
        Args.DefocusDiameterMicron = [10 18 45 90]; % Focus = 2,3,4,5 respectively
        Args.DefocusKernel = [];

        Args.ImRes     = 10; % ELOPsim's own profile-building/blur-convolution grid
                             % resolution: 1/ImRes of a detector pixel (NOT usim.m's own
                             % Args.ImRes, see UsimImRes below)
        Args.UsimImRes = 5;  % passed through to usim.m's own Args.ImRes and
                             % ExtOversampling; the blurred profile is resampled from
                             % this function's own Args.ImRes grid down (or up) to this
                             % resolution before being handed to usim.m, replicating
                             % usim.m's exact Nx/Ny formula so its own internal resize
                             % of ExtProfileMatrix becomes a no-op
    end

    if ~ismember(Args.OutMode, {'raw','production','both'})
        error('ultrasat:ELOPsim:InvalidOutMode', ...
            'Args.OutMode must be ''raw'', ''production'', or ''both'' (got ''%s''), exiting..', Args.OutMode);
    end
    WriteRaw        = any(strcmp(Args.OutMode, {'raw','both'}));
    WriteProduction = any(strcmp(Args.OutMode, {'production','both'}));

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
    OutFileHI   = repmat({''}, NumRows, 1);
    OutFileLO   = repmat({''}, NumRows, 1);
    OutFileADU  = repmat({''}, NumRows, 1);
    OutFileGain = repmat({''}, NumRows, 1);
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

                            if WriteRaw
                                OutFileHI{Irow} = sprintf('%s_HI.fits', BaseName);
                                OutFileLO{Irow} = sprintf('%s_LO.fits', BaseName);
                            end
                            if WriteProduction
                                OutFileADU{Irow}  = sprintf('%s_ADU.fits', BaseName);
                                OutFileGain{Irow} = sprintf('%s_GAIN.fits', BaseName);
                            end
                            OutFileCT{Irow}  = sprintf('%s_CT.fits', BaseName);
                            OutFileReg{Irow} = sprintf('%s_REG.reg', BaseName);
                        end
                    end
                end
            end
        end
    end

    Result = table(N, Filter, Temperature, Template, Radius, Focus, Rotation, Tile, ...
        OutFileHI, OutFileLO, OutFileADU, OutFileGain, OutFileCT, OutFileReg, SNRMin, SNRMax);

    TableFullName = sprintf('%s%s%s', Args.OutDir, '/', Args.TableName);
    writetable(Result, TableFullName);   % SNRMin/SNRMax start as NaN, filled in and
                                          % re-written after each row below

    % magnitude solved at Focus = 1, keyed by the row's other parameters (Filter,
    % Temperature, Template, Radius, Rotation, Tile), reused for Focus > 1 rows of the
    % same combination instead of solving independently (see the NB above)
    FocusMagCache = containers.Map('KeyType', 'char', 'ValueType', 'double');

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
                if isempty(Args.DefocusKernel)
                    % grow the source's own canvas (zero-padded, centered -- valid since
                    % it's already zero outside the source's compact footprint) so it's
                    % large enough to hold the blur kernel at its full physical diameter;
                    % otherwise the kernel disk saturates into a uniform full-canvas smear
                    % once its diameter exceeds the canvas (all such foci become visually
                    % identical), and conv2's implicit zero-padding at the canvas edge
                    % discards the flux that should have spread beyond it.
                    DiameterArcsec = elopDefocusDiameterArcsec(Result.Focus(Irow), Args);
                    [ExtProfileMatrix{Ip}, ExtSizeRAVec(Ip)] = elopPadCanvasForBlur( ...
                        ExtProfileMatrix{Ip}, ExtSizeRAVec(Ip), DiameterArcsec, Args.ImRes);
                    ExtSizeDecVec(Ip) = ExtSizeRAVec(Ip);
                end
                GridScaleArcsec = ExtSizeRAVec(Ip) / size(ExtProfileMatrix{Ip}, 2);
                Kernel = elopFocusKernel(Result.Focus(Irow), GridScaleArcsec, size(ExtProfileMatrix{Ip}), Args);
                ExtProfileMatrix{Ip} = elopConvolveKernel(ExtProfileMatrix{Ip}, Kernel);
            end
        end

        % profiles above were built (and, if Focus > 1, blurred) on this function's own
        % Args.ImRes grid; resample each one now onto the grid usim.m's own Args.ImRes =
        % Args.UsimImRes expects, so usim.m's internal resize of ExtProfileMatrix becomes
        % a no-op rather than a second, undocumented interpolation step.
        for Ip = 1:1:NumSrc
            ExtProfileMatrix{Ip} = elopResampleToUsimGrid(ExtProfileMatrix{Ip}, ...
                ExtSizeRAVec(Ip), ExtSizeDecVec(Ip), Args.UsimImRes);
        end

        ExtSpec = [repmat(SpecTab(:,2), 1, NumSrc), SpecTab(:,1)]; % usim.m's 'tab' convention: Nwave x (NumExt+1)

        CommonArgs = { ...
            'ImRes', Args.UsimImRes, 'ExtOversampling', Args.UsimImRes, ...
            'ExtProfileType', 'matrix', 'ExtProfileMatrix', ExtProfileMatrix, ...
            'ExtAxisRatio', 1, 'ExtPA', 0, ...
            'ExtSizeRA', ExtSizeRAVec, 'ExtSizeDec', ExtSizeDecVec, ...
            'ExtRA0', CatX, 'ExtDec0', CatY, 'ExtSkyCat', false, ...
            'ExtEbv', 0, 'ExtSpecType', Args.SpecType, 'ExtSpec', ExtSpec, ...
            'Tile', Result.Tile{Irow}, 'RotAng', Result.Rotation(Irow), ...
            'Exposure', Args.Exposure, 'Jitter', Args.Jitter, 'DarkCurrent', DarkCurrent, ...
            'NoiseZody', false, 'NoiseCher', false, 'NoiseStray', false};

        cprintf('hyper', '%s\n', sprintf('ELOPsim row %d/%d: %s', Irow, NumRows, Result.OutFileCT{Irow}));

        MagKey = sprintf('%s|%d|%s|%g|%g|%s', Result.Filter{Irow}, Result.Temperature(Irow), ...
            Result.Template{Irow}, Result.Radius(Irow), Result.Rotation(Irow), Result.Tile{Irow});

        if Result.Focus(Irow) == 1
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
            FocusMagCache(MagKey) = ExtMagRow;
        else
            % reuse the magnitude solved at Focus = 1 for this same combination, rather
            % than solving independently -- Focus doesn't change the source's total
            % flux, so this keeps brightness fixed while only the blur varies,
            % showing how defocus alone degrades the measured S/N
            if ~isKey(FocusMagCache, MagKey)
                error('ultrasat:ELOPsim:NoFocus1Magnitude', ...
                    'Row %d (Focus = %d) needs the magnitude solved at Focus = 1 for the same Filter/Temperature/Template/Radius/Rotation/Tile combination, but that row has not been processed yet (Focus = 1 must be included in Args.Focus and come before other Focus values), exiting..', ...
                    Irow, Result.Focus(Irow));
            end
            ExtMagRow = FocusMagCache(MagKey);
        end
        ExtMagVec = repmat(ExtMagRow, 1, NumSrc);

        Sim = ultrasat.usim(CommonArgs{:}, 'ExtMag', ExtMagVec, 'OutType', 'none');

        if WriteRaw
            [ImageHI, ImageLO] = elopGainImages(Sim.Image);
            FITS.write(ImageHI, sprintf('!%s/%s', Args.OutDir, Result.OutFileHI{Irow}), ...
                'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
            FITS.write(ImageLO, sprintf('!%s/%s', Args.OutDir, Result.OutFileLO{Irow}), ...
                'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
        end
        if WriteProduction
            [ImageADU, ImageGain] = elopProductionImages(Sim.Image);
            FITS.write(ImageADU, sprintf('!%s/%s', Args.OutDir, Result.OutFileADU{Irow}), ...
                'DataType', 'int16', 'Append', false, 'OverWrite', true, 'WriteTime', true);
            FITS.write(ImageGain, sprintf('!%s/%s', Args.OutDir, Result.OutFileGain{Irow}), ...
                'DataType', 'int8', 'Append', false, 'OverWrite', true, 'WriteTime', true);
        end
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

function Angle = size2ang(SizeMm)
    % Convert a physical source size [mm], as specified on the ELOP lab-test mask, into
    % the corresponding on-sky angular size [arcsec]. The lab-test optics image the mask
    % onto the sky with magnification Magnification = 1.1*330/22000, and the conversion
    % follows Angle = 206265*Magnification*Size/(1.1*330). Works element-wise, so a
    % scalar, a vector, or an Nx2 polygon vertex-offset list are all handled uniformly.
    % Input  : - Physical source size(s) [mm].
    % Output : - Angular size(s) [arcsec].
    % Author : A. Krassilchtchikov (Jul 2026)
    % Example: Ang = ultrasat.ELOPsim>size2ang(0.55);
    ArcsecInRad = 206265;
    Magnification = 1.1 * 330 / 22000;
    Angle = ArcsecInRad * Magnification * SizeMm / (1.1 * 330);
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
    % row's Radius-derived position). Template 'A' is a line of circular disk sources
    % (per-source diameters Args.TemplateADiametersMm, consecutive gaps
    % Args.TemplateAGapsMm), rotated by Args.TemplateARotation about its center.
    % Template 'B' is two sets (upper/lower) of disk sources with per-set diameters and
    % non-uniform column/row gaps, the lower set offset by Args.SetShift and the whole
    % pattern rotated by Args.TemplateBRotation about its centroid. Template 'C' is a
    % single polygon (Args.TemplatePolygonsMm [mm], area-centroid anchored, rotated by
    % Args.TemplateCRotation). Template 'D' is 5 fixed polygons plus a circular hole
    % (T-4666-1751-00), positioned relative to the substrate center (Args.TemplateDPolygonsMm
    % / Args.TemplateDHoleMm) and rotated by Args.TemplateDRotation about that anchor.
    switch Template
        case 'A'
            % a line of circular disk sources: per-source diameters [mm] and consecutive
            % gaps [mm] are converted to arcsec, the line is centered on (CatX0, CatY0)
            % and rotated by Args.TemplateARotation [deg] about that center (0 = along X).
            DiamArcsec = size2ang(Args.TemplateADiametersMm);   % [arcsec] disk diameters
            GapArcsec  = size2ang(Args.TemplateAGapsMm);        % [arcsec] consecutive gaps
            NumSrc = numel(DiamArcsec);
            if numel(GapArcsec) ~= NumSrc - 1
                error('ultrasat:ELOPsim:TemplateAGapCount', ...
                    'Args.TemplateAGapsMm must have numel(TemplateADiametersMm)-1 = %d elements (got %d), exiting..', ...
                    NumSrc - 1, numel(GapArcsec));
            end

            AlongLine = [0, cumsum(GapArcsec)];   % [arcsec] cumulative positions along the line
            AlongLine = AlongLine - (min(AlongLine) + max(AlongLine)) / 2;   % center on the anchor

            PixSizeArcsec = elopPixSizeArcsec();
            CatX = CatX0 + AlongLine * cosd(Args.TemplateARotation) / PixSizeArcsec;
            CatY = CatY0 + AlongLine * sind(Args.TemplateARotation) / PixSizeArcsec;

            ExtSizeRAVec  = DiamArcsec;   % [arcsec] bounding-box size = disk diameter
            ExtSizeDecVec = DiamArcsec;
            ExtProfileMatrix = cell(1, NumSrc);
            for Ip = 1:1:NumSrc
                ExtProfileMatrix{Ip} = elopCircleMask(elopGridSamples(DiamArcsec(Ip), Args.ImRes));
            end

        case 'B'
            % two sets (upper/lower) of disk sources: per-set diameters and non-uniform
            % column/row gaps [mm], the lower set offset from the upper by Args.SetShift
            % [mm], the whole pattern centered on its centroid at (CatX0, CatY0) and
            % rotated by Args.TemplateBRotation [deg] about it.
            [Uup, Vup] = elopSetPositions(Args.TemplateBUpperColGapsMm, Args.TemplateBUpperRowGapsMm);
            [Ulo, Vlo] = elopSetPositions(Args.TemplateBLowerColGapsMm, Args.TemplateBLowerRowGapsMm);
            Ulo = Ulo + Args.SetShift(1);   % lower set's upper-left offset from the upper set's
            Vlo = Vlo + Args.SetShift(2);
            U = [Uup, Ulo];   % [mm] rightward from the shared upper-left
            V = [Vup, Vlo];   % [mm] downward  from the shared upper-left

            % center on the centroid (the anchor) and map to detector axes
            % (variant 1: rightward -> +X, downward -> -Y)
            DX =  (U - mean(U));
            DY = -(V - mean(V));

            % rotate the whole pattern about the centroid, then mm -> arcsec -> pixels
            Theta = Args.TemplateBRotation;
            DXr = DX * cosd(Theta) - DY * sind(Theta);
            DYr = DX * sind(Theta) + DY * cosd(Theta);
            PixSizeArcsec = elopPixSizeArcsec();
            CatX = CatX0 + size2ang(DXr) / PixSizeArcsec;
            CatY = CatY0 + size2ang(DYr) / PixSizeArcsec;

            DiamArcsec = size2ang(elopTemplateBDiametersMm(Args));   % per-source disk diameters
            NumSrc = numel(CatX);
            ExtSizeRAVec  = DiamArcsec;
            ExtSizeDecVec = DiamArcsec;
            ExtProfileMatrix = cell(1, NumSrc);
            for Ip = 1:1:NumSrc
                ExtProfileMatrix{Ip} = elopCircleMask(elopGridSamples(DiamArcsec(Ip), Args.ImRes));
            end

        case 'C'
            Polygons = elopTemplateCPolygon(Args);
            NumSrc = numel(Polygons);
            CatX = zeros(1, NumSrc); CatY = zeros(1, NumSrc);
            ExtSizeRAVec = zeros(1, NumSrc); ExtSizeDecVec = zeros(1, NumSrc);
            ExtProfileMatrix = cell(1, NumSrc);
            for Ip = 1:1:NumSrc
                [CatX(Ip), CatY(Ip), ExtSizeRAVec(Ip), ExtSizeDecVec(Ip), ExtProfileMatrix{Ip}] = ...
                    elopPolygonSource(Polygons{Ip}, CatX0, CatY0, Args.ImRes);
            end

        case 'D'
            % 5 polygons + 1 circular hole (T-4666-1751-00), positioned in [mm] relative
            % to the substrate center (the anchor at (CatX0, CatY0)), converted via
            % size2ang and rotated as a whole by Args.TemplateDRotation about the anchor.
            Polygons = elopTemplateDPolygons(Args);
            NumPoly  = numel(Polygons);
            NumSrc   = NumPoly + 1;   % + the circular hole disk source
            CatX = zeros(1, NumSrc); CatY = zeros(1, NumSrc);
            ExtSizeRAVec = zeros(1, NumSrc); ExtSizeDecVec = zeros(1, NumSrc);
            ExtProfileMatrix = cell(1, NumSrc);
            for Ip = 1:1:NumPoly
                [CatX(Ip), CatY(Ip), ExtSizeRAVec(Ip), ExtSizeDecVec(Ip), ExtProfileMatrix{Ip}] = ...
                    elopPolygonSource(Polygons{Ip}, CatX0, CatY0, Args.ImRes);
            end
            % the hole: a small circular disk source at its own rotated position
            [HoleX, HoleY, HoleDiamArcsec] = elopTemplateDHole(Args);
            PixSizeArcsec = elopPixSizeArcsec();
            CatX(NumSrc) = CatX0 + HoleX / PixSizeArcsec;
            CatY(NumSrc) = CatY0 + HoleY / PixSizeArcsec;
            ExtSizeRAVec(NumSrc)  = HoleDiamArcsec;
            ExtSizeDecVec(NumSrc) = HoleDiamArcsec;
            ExtProfileMatrix{NumSrc} = elopCircleMask(elopGridSamples(HoleDiamArcsec, Args.ImRes));

        otherwise
            error('ultrasat:ELOPsim:TemplateNotImplemented', ...
                'Template ''%s'' is not yet implemented (only ''A'', ''B'', ''C'', ''D'' are supported), exiting..', Template);
    end
end

function [U, V] = elopSetPositions(ColGapsMm, RowGapsMm)
    % Local (u,v) positions [mm] of one Template 'B' set of sources: u runs rightward
    % from the set's upper-left source (cumulative column gaps), v runs downward
    % (cumulative row gaps). The set is (numel(ColGaps)+1) x (numel(RowGaps)+1) sources.
    % Returned as row vectors in a fixed column-major order so the upper and lower sets
    % concatenate consistently with elopTemplateBDiametersMm.
    ColPos = [0, cumsum(ColGapsMm(:)')];   % 1 x Ncol
    RowPos = [0, cumsum(RowGapsMm(:)')];   % 1 x Nrow
    [GridU, GridV] = meshgrid(ColPos, RowPos);
    U = GridU(:)';
    V = GridV(:)';
end

function DiamMm = elopTemplateBDiametersMm(Args)
    % Per-source disk diameter [mm] for Template 'B', in the same upper-then-lower,
    % column-major order elopSetPositions produces, so it aligns with CatX/CatY.
    NumUpper = (numel(Args.TemplateBUpperColGapsMm) + 1) * (numel(Args.TemplateBUpperRowGapsMm) + 1);
    NumLower = (numel(Args.TemplateBLowerColGapsMm) + 1) * (numel(Args.TemplateBLowerRowGapsMm) + 1);
    DiamMm = [repmat(Args.TemplateBUpperDiameterMm, 1, NumUpper), ...
              repmat(Args.TemplateBLowerDiameterMm, 1, NumLower)];
end

function Polygons = elopTemplateCPolygon(Args)
    % Template 'C''s single polygon as a 1x1 cell of an Nx2 [dRA, dDec] arcsec vertex
    % list, relative to the row's Radius-derived position: Args.TemplatePolygonsMm's
    % [mm] vertices converted via size2ang, shifted so the polygon's area-centroid sits
    % at (0,0) -- i.e. at (CatX0, CatY0), the centroid anchor -- and rotated by
    % Args.TemplateCRotation [deg] (CCW) about that centroid.
    if numel(Args.TemplatePolygonsMm) ~= 1
        error('ultrasat:ELOPsim:PolygonCountMismatch', ...
            'Args.TemplatePolygonsMm must hold exactly 1 polygon -- Template ''C'' shape (got %d), exiting..', ...
            numel(Args.TemplatePolygonsMm));
    end
    Base = size2ang(Args.TemplatePolygonsMm{1});     % Nx2 [dRA, dDec] arcsec
    Base = Base - elopPolygonAreaCentroid(Base);
    Theta = Args.TemplateCRotation;
    Rot   = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
    Polygons = { (Rot * Base.').' };                 % rotate CCW about the centroid
end

function Polygons = elopTemplateDPolygons(Args)
    % Template 'D''s 5 polygon shapes (T-4666-1751-00) as a cell of Nx2 [dRA, dDec]
    % arcsec vertex lists relative to the substrate center (the anchor): each polygon's
    % [mm] vertices (Args.TemplateDPolygonsMm) are converted via size2ang and rotated by
    % Args.TemplateDRotation [deg] (CCW) about the anchor, preserving their layout (unlike
    % Template 'C', the shapes are NOT re-centered -- their positions carry the design).
    Theta = Args.TemplateDRotation;
    Rot   = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
    Polygons = cell(1, numel(Args.TemplateDPolygonsMm));
    for Ip = 1:1:numel(Args.TemplateDPolygonsMm)
        Polygons{Ip} = (Rot * size2ang(Args.TemplateDPolygonsMm{Ip}).').';
    end
end

function [HoleX, HoleY, HoleDiamArcsec] = elopTemplateDHole(Args)
    % Template 'D''s circular hole: its [x, y] position [mm] (Args.TemplateDHoleMm(1:2),
    % relative to the substrate center) converted to arcsec via size2ang and rotated by
    % Args.TemplateDRotation about the anchor, plus its diameter (Args.TemplateDHoleMm(3)).
    Theta = Args.TemplateDRotation;
    Rot   = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
    Pos   = (Rot * size2ang(Args.TemplateDHoleMm(1:2)).').';
    HoleX = Pos(1);
    HoleY = Pos(2);
    HoleDiamArcsec = size2ang(Args.TemplateDHoleMm(3));
end

function Cxy = elopPolygonAreaCentroid(V)
    % Area-centroid [x, y] of a simple polygon with Nx2 vertices V (any winding), via the
    % standard shoelace formula -- used to anchor Template 'C'/'D' on the polygon's own
    % area-centroid rather than its bounding-box center.
    X = V(:,1); Y = V(:,2);
    X2 = [X(2:end); X(1)]; Y2 = [Y(2:end); Y(1)];
    Cross = X .* Y2 - X2 .* Y;
    Area = sum(Cross) / 2;
    Cxy  = [sum((X + X2) .* Cross), sum((Y + Y2) .* Cross)] / (6 * Area);
end

function [CatX, CatY, ExtSizeRA, ExtSizeDec, Mask] = elopPolygonSource(Vertices, CatX0, CatY0, ImRes)
    % A single polygon source: Vertices is an Nx2 [dRA, dDec] arcsec vertex-offset list,
    % relative to the shared center (CatX0, CatY0), axis-aligned with the detector.
    % Returns the source's own center (its vertex bounding-box center, which need not
    % coincide with (CatX0, CatY0) for an asymmetric polygon), its bounding-box size in
    % arcsec, and a mask (1 inside the polygon, 0 outside) spanning that bounding box,
    % sampled at a resolution of 1/ImRes of a detector pixel (see elopGridSamples), via
    % inpolygon (as used elsewhere in AstroPack, e.g. imUtil.sources.polygonFlux),
    % rather than the Image Processing Toolbox's poly2mask.
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

    % the bounding box was padded square above, so ExtSizeRA == ExtSizeDec and a
    % single NPix (via elopGridSamples) spans both axes.
    NPix = elopGridSamples(ExtSizeRA, ImRes);

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

function NPix = elopGridSamples(ExtSizeArcsec, ImRes)
    % Number of grid samples spanning ExtSizeArcsec at a resolution of 1/ImRes of a
    % detector pixel (same ceil/floor convention as usim.m's own Nx/Ny, without the
    % odd-forcing needed only for the final grid handed to usim.m -- see
    % elopResampleToUsimGrid).
    Grain = elopPixSizeArcsec() / ImRes;
    NPix  = max(3, ceil(ExtSizeArcsec / Grain));
end

function Profile = elopResampleToUsimGrid(Profile, ExtSizeRA, ExtSizeDec, UsimImRes)
    % Resample a profile (already built/blurred at this function's own Args.ImRes
    % resolution) onto the grid usim.m's extended-object mode will independently expect,
    % replicating its exact Nx/Ny formula (Grain = PixSizeArcsec/UsimImRes; Nx/Ny =
    % max(3,ceil(ExtSize/Grain)), forced odd) so the array handed to usim.m already
    % matches the size it computes for itself, and its own internal resize (imresize to
    % [Ny Nx]) becomes a no-op.
    Nx = elopGridSamples(ExtSizeRA,  UsimImRes);
    Ny = elopGridSamples(ExtSizeDec, UsimImRes);
    Nx = Nx + 1 - mod(Nx, 2);
    Ny = Ny + 1 - mod(Ny, 2);
    Profile = imresize(Profile, [Ny Nx], 'bilinear');
end

function DiameterArcsec = elopDefocusDiameterArcsec(Focus, Args)
    % The physical defocus blur disk diameter [arcsec] for Focus (2-5), converted from
    % Args.DefocusDiameterMicron(Focus-1) [micron] via
    % D_arcsec = D_micron*(1000*1.1*330)/206265.
    DiameterMicron = Args.DefocusDiameterMicron(Focus - 1);
    DiameterArcsec = DiameterMicron * (1000 * 1.1 * 330) / 206265;
end

function [PaddedMask, NewExtSize] = elopPadCanvasForBlur(Mask, ExtSize, DiameterArcsec, ImRes)
    % Enlarge Mask's square canvas (zero-padded, centered -- valid since Mask is already
    % zero outside its own compact footprint) so it comfortably contains the blur kernel
    % of diameter DiameterArcsec, avoiding the kernel-disk saturating into a uniform
    % full-canvas smear (once its diameter exceeds the canvas, R <= Radius becomes true
    % everywhere) and the resulting loss of flux that conv2's implicit zero-padding
    % discards beyond the original canvas edge. The canvas only ever grows: NewExtSize =
    % max(ExtSize, DiameterArcsec*1.1), at the same Args.ImRes grid scale as Mask, then
    % forced to an odd sample count so the kernel's center falls exactly on a grid cell
    % (matching elopResampleToUsimGrid's own odd-forcing for the same reason).
    Grain = elopPixSizeArcsec() / ImRes;
    NewNPix = max(3, ceil(max(ExtSize, DiameterArcsec * 1.1) / Grain));
    NewNPix = NewNPix + 1 - mod(NewNPix, 2);
    NewExtSize = NewNPix * Grain;

    OldNPix = size(Mask, 1);
    PaddedMask = zeros(NewNPix, NewNPix);
    Offset = floor((NewNPix - OldNPix) / 2);
    PaddedMask(Offset+1 : Offset+OldNPix, Offset+1 : Offset+OldNPix) = Mask;
end

function Kernel = elopFocusKernel(Focus, GridScaleArcsec, MaskSize, Args)
    % The defocus blur kernel for Focus (2-5) on a profile grid at GridScaleArcsec
    % [arcsec/cell]. Args.DefocusKernel, if non-empty, is used as-is (an arbitrary
    % user-supplied kernel, already at the mask's own pixel grid scale) for every
    % Focus level > 1. Otherwise the kernel is a disk of diameter
    % Args.DefocusDiameterMicron(Focus-1) [micron] (see elopDefocusDiameterArcsec),
    % shaped per Args.DefocusKernelShape: 'tophat' (uniform disk, hard cutoff) or
    % 'topcosine' (cosine-tapered disk, smoothly reaching 0 at the same diameter instead
    % of an abrupt edge).
    if ~isempty(Args.DefocusKernel)
        Kernel = Args.DefocusKernel;
        return
    end

    DiameterArcsec = elopDefocusDiameterArcsec(Focus, Args);
    DiameterPix    = DiameterArcsec / GridScaleArcsec;

    switch lower(Args.DefocusKernelShape)
        case 'tophat'
            Kernel = elopTopHatKernel(DiameterPix, MaskSize);
        case 'topcosine'
            Kernel = elopTopCosineKernel(DiameterPix, MaskSize);
        otherwise
            error('ultrasat:ELOPsim:UnknownKernelShape', ...
                'Unknown Args.DefocusKernelShape ''%s'' (use ''tophat'' or ''topcosine''), exiting..', ...
                Args.DefocusKernelShape);
    end
end

function Kernel = elopTopHatKernel(DiameterPix, MaskSize)
    % A normalized uniform disk kernel, MaskSize(1) x MaskSize(2), of the given
    % diameter [pix], with a hard cutoff at the edge.
    Radius = DiameterPix / 2;
    VecX   = (1:MaskSize(2)) - (MaskSize(2) + 1) / 2;
    VecY   = (1:MaskSize(1)) - (MaskSize(1) + 1) / 2;
    [X, Y] = meshgrid(VecX, VecY);
    R = sqrt(X.^2 + Y.^2);
    Kernel = double(R <= Radius);
    Kernel = Kernel / sum(Kernel, 'all');
end

function Kernel = elopTopCosineKernel(DiameterPix, MaskSize)
    % A normalized cosine-tapered disk kernel, MaskSize(1) x MaskSize(2), of the given
    % diameter [pix]: intensity(r) = 0.5*(1+cos(pi*r/Radius)) for r <= Radius, 0 beyond
    % -- same finite support as elopTopHatKernel, but smoothly reaching 0 at the edge
    % instead of an abrupt cutoff.
    Radius = DiameterPix / 2;
    VecX   = (1:MaskSize(2)) - (MaskSize(2) + 1) / 2;
    VecY   = (1:MaskSize(1)) - (MaskSize(1) + 1) / 2;
    [X, Y] = meshgrid(VecX, VecY);
    R = sqrt(X.^2 + Y.^2);
    Kernel = zeros(MaskSize);
    InDisk = R <= Radius;
    Kernel(InDisk) = 0.5 * (1 + cos(pi * R(InDisk) / Radius));
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
    % modelled source in a row, in image (pixel) coordinates. Template 'A': one circle
    % per source at its own design diameter (Args.TemplateADiametersMm). Template 'B':
    % a circle per source at its set's diameter (upper/lower). Template 'C': the single
    % polygon from elopTemplateCPolygon. Template 'D': the 5 polygons from
    % elopTemplateDPolygons plus a circle for the hole. All reconstructed to absolute
    % pixel vertices from the row's shared center (CatX0, CatY0) -- the same convention
    % elopPolygonSource uses -- rather than each polygon's own bounding-box center
    % (CatX/CatY), since the vertices are defined relative to the shared center.
    PixSizeArcsec = elopPixSizeArcsec();
    switch Template
        case 'A'
            % one circle per source, each drawn at its own design diameter
            DiamArcsec = size2ang(Args.TemplateADiametersMm);
            RadiiPix   = (DiamArcsec / 2) / PixSizeArcsec;
            for Ip = 1:1:numel(CatX)
                DS9_new.regionWrite([CatX(Ip), CatY(Ip)], 'FileName', FileName, 'Coo', 'image', ...
                    'Marker', 'circle', 'Size', RadiiPix(Ip), 'Color', 'green', ...
                    'Append', Ip > 1, 'PrintIndividualProp', false);
            end

        case 'B'
            % one circle per source, each at its set's diameter (same upper-then-lower
            % order as CatX/CatY, via elopTemplateBDiametersMm)
            DiamArcsec = size2ang(elopTemplateBDiametersMm(Args));
            RadiiPix   = (DiamArcsec / 2) / PixSizeArcsec;
            for Ip = 1:1:numel(CatX)
                DS9_new.regionWrite([CatX(Ip), CatY(Ip)], 'FileName', FileName, 'Coo', 'image', ...
                    'Marker', 'circle', 'Size', RadiiPix(Ip), 'Color', 'green', ...
                    'Append', Ip > 1, 'PrintIndividualProp', false);
            end

        case 'C'
            elopWritePolygonRegions(FileName, elopTemplateCPolygon(Args), CatX0, CatY0, PixSizeArcsec, false);

        case 'D'
            elopWritePolygonRegions(FileName, elopTemplateDPolygons(Args), CatX0, CatY0, PixSizeArcsec, false);
            % the circular hole, as a circle region at its rotated position
            [HoleX, HoleY, HoleDiamArcsec] = elopTemplateDHole(Args);
            DS9_new.regionWrite([CatX0 + HoleX / PixSizeArcsec, CatY0 + HoleY / PixSizeArcsec], ...
                'FileName', FileName, 'Coo', 'image', 'Marker', 'circle', ...
                'Size', (HoleDiamArcsec / 2) / PixSizeArcsec, 'Color', 'green', ...
                'Append', true, 'PrintIndividualProp', false);

        otherwise
            error('ultrasat:ELOPsim:TemplateNotImplemented', ...
                'Template ''%s'' is not yet implemented (only ''A'', ''B'', ''C'', ''D'' are supported), exiting..', Template);
    end
end

function elopWritePolygonRegions(FileName, Polygons, CatX0, CatY0, PixSizeArcsec, Append0)
    % Write each polygon in Polygons (Nx2 [dRA, dDec] arcsec vertex lists, relative to
    % (CatX0, CatY0)) as a DS9 polygon region in image (pixel) coordinates. Append0 is
    % the Append flag for the first polygon (false to start a fresh file); subsequent
    % polygons always append.
    for Ip = 1:1:numel(Polygons)
        Vertices = Polygons{Ip};
        VertX = CatX0 + Vertices(:,1) / PixSizeArcsec;
        VertY = CatY0 + Vertices(:,2) / PixSizeArcsec;
        DS9_new.regionWrite([VertX(:), VertY(:)], 'FileName', FileName, 'Coo', 'image', ...
            'Marker', 'polygon', 'Color', 'green', 'Append', Append0 || Ip > 1, ...
            'PrintIndividualProp', false);
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

function [ImageADU, ImageGainMap] = elopProductionImages(Image)
    % Encode a counts (e-) image into the single per-pixel gain-selected ADU image a
    % production-mode camera readout outputs, plus a separate per-pixel gain-selection
    % map -- unlike elopGainImages' two uniform all-high-gain/all-low-gain images
    % (ELOPsim's 'raw' mode). Reproduces usim.m's own per-pixel gain-selection logic
    % (Back/E2ADU section: GainThresh/E2ADUhigh/E2ADUlow) locally and reuses
    % ultrasat.e2ADU for the packing step, without modifying usim.m's own behavior; the
    % ADU value itself is packed with IncludeGainBit = false (a pure 13-bit value), since
    % the gain selection is carried by the separate map instead of the top bit.
    % NB: GainThresh/E2ADUhigh/E2ADUlow must be kept in sync with the values hardcoded in
    % usim.m.
    % Input  : - a counts (e-) image, as returned in usim.m's output AstroImage.Image.
    % Output : - the single gain-selected ADU image (13-bit packed, no gain bit).
    %          - the per-pixel gain-selection map (0 = high gain used, 1 = low gain used).
    % Author : A. Krassilchtchikov (2026)
    % Example: [ImADU, ImGain] = ultrasat.ELOPsim>elopProductionImages(Sim.Image);
    GainThresh = 1.6e4;  % usim.m's Back/E2ADU section
    E2ADUhigh  = 1.185;  % usim.m's Back/E2ADU section
    E2ADUlow   = 0.074;  % usim.m's Back/E2ADU section

    GainMask     = Image > GainThresh;   % 0 = high gain, 1 = low gain (usim.m's own convention)
    GainSelected = max(Image .* (GainMask.*E2ADUlow + ~GainMask.*E2ADUhigh), 1);  % ultrasat.e2ADU requires Count >= 1

    ImageADU     = ultrasat.e2ADU(GainSelected, GainMask, false);   % IncludeGainBit = false
    ImageGainMap = uint8(GainMask);
end
