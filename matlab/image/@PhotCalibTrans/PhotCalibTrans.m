classdef PhotCalibTrans < Component
    % PhotCalibTrans - Transmission-based absolute photometric calibration
    % Description: Performs photometric calibration using atmospheric and instrumental
    %              transmission models. Fits multi-component transmission functions
    %              (Rayleigh, aerosol, water vapor, ozone, mirror, detector QE) to
    %              calibrator stars with known spectra (default: Gaia DR3 XP).
    %              Supports position-dependent field corrections via Tran2D polynomials.
    % Author : D. Kovaleva (Dec 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50 (transmission-based calibration)
    %
    % Constant Properties (Hidden):
    %   TransWvl  - Transmission wavelength grid [nm] (300:2:1100, 2 nm step, 401 points)
    %
    % Properties:
    %   TransModel - CompositeFun object with fitted transmission model and
    %   optimization sequence used for fitting
    %   CalibData  - Structure with calibrator data (spectra, positions, fluxes)
    %   AirMass, Zenith, ExpTime, NCoadd, Temp, Pressure, Humidity, Aperture - Observation metadata
    %
    % Example:
    %{
     % Create calibration object
     PC = PhotCalibTrans();

     % Perform calibration on AstroImage (metadata read from AI.HeaderData)
     PC.calibrate(AI);

     % Evaluate transmission and zero points
     Trans = PC.evaluateTransmission();  % Use constant wavelength grid (Obj.TransWvl)
     ZP = PC.evaluateZP();  % Uses Obj.TransWvl, Obj.ExpTime, Obj.NCoadd, Obj.Aperture

     % Apply calibration to catalog
     [MagAB, MagABErr] = PC.evaluateMag(MagInst, 'X', X, 'Y', Y, 'MagInstErr', MagErr);

     % Diagnostic plots
     PC.plotTransmission();
     PC.plotCalibrators();
     PC.plotResiduals();
    %}
    %
    % Methods:
    %   Constructor:
    %     PhotCalibTrans - Constructor for PhotCalibTrans class
    %   Core Calibration Methods:
    %     calibrate - Perform transmission-based photometric calibration using CompositeFun
    %     selectCalibrators - Select calibrators with reference spectra for photometric calibration
    %   Evaluation Methods:
    %     evaluateTransmission - Evaluate transmission at specific positions (with position-dependent corrections)
    %     evaluateZP - Evaluate photometric zero point at specific positions
    %     evaluateMag - Evaluate calibrated AB magnitudes from instrumental magnitudes
    %   Header I/O Methods:
    %     writeToHeader - Write calibration data to AstroHeader [PLACEHOLDER]
    %     readFromHeader - Read calibration data from AstroHeader [PLACEHOLDER]
    %   Catalog Operations:
    %     addMagAB - Add calibrated AB magnitude (and optionally error) columns to catalog
    %   Display/Output Methods:
    %     summary - Display photometric calibration summary
    %   Plotting Methods:
    %     plotTransmission - Plot transmission curve vs wavelength
    %     plotResiduals - Plot calibration residuals (magnitude and spatial)
    %     plotZPMap - Plot 2D map of position-dependent zero point corrections
    %     plotCalibrators - Plot observed vs predicted magnitudes for calibrators
    %     plotFitQuality - Plot RMS/Chi2 evolution across optimization stages
    %   Static Methods:
    %     fromHeader - Create PhotCalibTrans object from AstroHeader [PLACEHOLDER]

    properties

        % Calibration metadata (read from header, defaults for missing values)
        AirMass = NaN           % Airmass
        Zenith = NaN            % Zenith angle [deg]
        Temp = NaN              % Temperature [C]
        Pressure = 965          % Atmospheric pressure [mbar] (default: typical at observatory altitude)
        Humidity = NaN          % Relative humidity [%]
        Aperture = pi * (0.1397)^2  % Telescope aperture area [m^2] (default: LAST telescope)
        ExpTime = NaN           % Exposure time [s]
        NCoadd = 1              % Number of coadded images (default: single image)

        % Transmission model (empty until calibration)
        TransModel = []         % CompositeFun transmission model object containing:
                                %   Before calibration: .Funs (function list with initial parameters), .FunOperator ('*'),
                                %                        .Tran2DObj (position-dependent correction object), .UseTran2D (true/false)
                                %   After calibration:  .Funs.Par (fitted parameters), .RMS (fit RMS [mag]), .Chi2 (chi-squared), .DOF (degrees of freedom)

        % Calibrator information (empty until calibration)
        CalibData = []          % Structure with calibrator data from selectCalibrators containing:
                                %   .SpecWvl [N_wvl x 1] - Wavelength grid for calibrator spectra [nm] (e.g., 336:2:1020 for Gaia DR3 XP)
                                %   .Spec [N_calib x N_wvl] - Calibrator spectra flux (Gaia DR3 XP)
                                %   .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
                                %   .ObsData - struct with .Flux, .FluxErr, .X, .Y, .RA, .Dec (observed data)
                                %   .CalData - struct with .RA, .Dec (positions in the catalog of spectra)
                                %   .MatchDistance [N_calib x 1] - Match distances [arcsec]
                                %   .NumMatches - Total number of matched calibrators

        CalFound = false        % Flag indicating whether calibrators were found (set by selectCalibrators)

    end

    properties (Constant, Hidden)
        % Wavelength grid for transmission evaluation (2 nm step)
        TransWvl = (300:2:1100)'      % Transmission wavelength grid [nm] for model evaluation (401 points)
    end

    methods % Constructor
        function Obj = PhotCalibTrans(varargin)
            % Constructor for PhotCalibTrans class
            % Input  : * ...,key,val,...
            %            Any property name as key with corresponding value.
            %            Available arguments (all optional):
            %
            %            Observation Metadata:
            %            'AirMass' - Airmass. Default is NaN.
            %            'Zenith' - Zenith angle [deg]. Default is NaN.
            %            'Temp' - Temperature [C]. Default is NaN.
            %            'Pressure' - Atmospheric pressure [mbar]. Default is 965.
            %            'Humidity' - Relative humidity [%]. Default is NaN.
            %            'ExpTime' - Exposure time [s]. Default is NaN.
            %            'NCoadd' - Number of coadded images. Default is 1.
            %
            %            Instrument Configuration:
            %            'Aperture' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2 (LAST telescope).
            %
            %            Calibration Data (typically set by calibrate() method):
            %            'TransModel' - CompositeFun transmission model object. Default is [].
            %            'CalibData' - Structure with calibrator data. Default is [].
            %            'CalFound' - Flag indicating if calibrators were found. Default is false.
            %
            % Output : - PhotCalibTrans object
            % Author : D. Kovaleva (Dec 2025)
            % Example: % Create with default values
            %          PC = PhotCalibTrans();
            %
            %          % Create with custom pressure and aperture
            %          PC = PhotCalibTrans('Pressure', 970, 'Aperture', 0.05);
            %
            %          % Create with observation metadata
            %          PC = PhotCalibTrans('AirMass', 1.2, 'Zenith', 33.5, 'ExpTime', 20, ...
            %                              'NCoadd', 1, 'Temp', 15, 'Pressure', 965);

            % Parse name-value pairs and set properties if they exist
            for i = 1:2:length(varargin)
                if i+1 <= length(varargin)
                    propName = varargin{i};
                    if isprop(Obj, propName)
                        Obj.(propName) = varargin{i+1};
                    else
                        warning('PhotCalibTrans:UnknownProperty', ...
                            'Property "%s" does not exist and will be ignored.', propName);
                    end
                end
            end
        end
    end

    methods % Core calibration methods
        function Obj = calibrate(Obj, Cat, Args)
            % Perform transmission-based photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroImage or AstroCatalog object with observed sources
            %                  Supports arrays: processes each element independently
            %          * ...,key,val,...
            %            'MetadataSource' - Source of observation metadata. Options:
            %                   'FromCat' - Read from Cat(i).HeaderData (default)
            %                   'FromHeader' - Read from Args.Header(i) (AstroHeader array)
            %                   'FromStruct' - Read from Args.Metadata(i) (struct array, single or per-subimage)
            %                   'Default' - Use hardcoded default values (same for all subimages)
            %            'Header' - [1 x Nobj] AstroHeader array (used with MetadataSource='FromHeader'). Default is [].
            %            'Metadata' - [1 x Nobj] struct array with fields: .AirMass, .Zenith, .ExpTime, .NCoadd, .Temp, .Pressure
            %                         (used with MetadataSource='FromStruct'). Default is [].
            %            'TransFunList' - Cell array of transmission function names to use.
            %                             Default is {'Normalization',
            %                             'Rayleigh', 'Aerosol', 'Water', 'Ozone', 'UMG', 
            %                             'Mirror', 'Corrector', 'QE_SkewedGaussian', 'QE_Legendre'}.
            %            'OptSeqName' - Name of optimization sequence from StageCatalog.
            %                           Default is 'DefaultLAST' (5-stage sequence from Garrappa et al. 2025).
            %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
            %            'Tran2DType' - Type of 2D transformation for field corrections. Default is 'cheby1_4_xt'.
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.0.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'FluxIni' - Start column index for calibrator spectra flux values. Default is 7.
            %            'FluxEnd' - End column index for calibrator spectra flux values. Default is 349.
            %            'EFluxIni' - Start column index for calibrator spectra flux errors. Default is 350.
            %            'EFluxEnd' - End column index for calibrator spectra flux errors. Default is 692.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - Array [1 x Nobj] of PhotCalibTrans objects, one per subimage
            %                  Each Obj(i) contains independent calibration results for Cat(i)
            %                  Properties: .CalFound, .CalibData, .TransModel, metadata
            %                  Methods available: Obj(i).evaluateZP(), Obj(i).evaluateTransmission(), etc.
            % Author : D. Kovaleva (Dec 2025)
            % Example: % AstroImage with auto metadata (from FITS headers)
            %          PC = PhotCalibTrans();
            %          PC = PC.calibrate(AI);
            %
            %          % AstroCatalog with metadata from struct (same for all subcatalogs)
            %          Meta = struct('AirMass', 1.2, 'Zenith', 33.5, 'ExpTime', 20, ...
            %                        'NCoadd', 1, 'Temp', 15, 'Pressure', 965);
            %          PC = PhotCalibTrans();
            %          PC = PC.calibrate(Cat, 'Metadata', Meta);
            %
            %          % With external Header array (different per subcatalog)
            %          PC = PC.calibrate(Cat, 'Header', HeaderArray);
            %
            %          % Using default values
            %          PC = PC.calibrate(Cat, 'MetadataSource', 'Default');
            %
            %          % Access results
            %          [PC.CalFound]              % Get CalFound for all subimages
            %          rms_values = [PC.TransModel]; rms_values = [rms_values.RMS];

            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog

                % Metadata arguments
                Args.MetadataSource = 'FromCat'  % 'FromCat', 'FromHeader', 'FromStruct', 'Default'
                Args.Header = []                 % [1 x Nobj] AstroHeader array
                Args.Metadata = []               % [1 x Nobj] struct array (or single struct for all)

                % Calibration arguments
                Args.TransFunList = {'Normalization', 'Rayleigh', 'Aerosol', 'Ozone', 'Water', 'UMG', 'Mirror', 'Corrector', 'QE_SkewedGaussian', 'QE_Legendre'}
                Args.OptSeqName = 'DefaultLAST'
                Args.CustomOptSeq = []
                Args.Tran2DType = 'cheby1_4_xt'
                Args.SearchRadius = 1.0
                Args.MagRange = [12 16]
                Args.FluxIni = 7      % Start column index for calibrator spectra flux
                Args.FluxEnd = 349    % End column index for calibrator spectra flux
                Args.EFluxIni = 350   % Start column index for calibrator spectra flux errors
                Args.EFluxEnd = 692   % End column index for calibrator spectra flux errors
                Args.Verbose logical = true
            end

            % ====================================================================
            % STEP 0: Validate input type and auto-detect metadata source
            % ====================================================================

            % Validate input object type
            if isa(Cat, 'AstroImage')
                IsAstroImage = true;
            elseif isa(Cat, 'AstroCatalog')
                IsAstroImage = false;
            else
                error('PhotCalibTrans:calibrate:InvalidInput', ...
                    'Cat must be AstroImage or AstroCatalog object');
            end

            Nobj = numel(Cat);

            % Validate metadata source
            ValidSources = {'FromCat', 'FromHeader', 'FromStruct', 'Default'};
            if ~any(strcmp(Args.MetadataSource, ValidSources))
                error('PhotCalibTrans:calibrate:InvalidMetadataSource', ...
                    'Invalid MetadataSource: %s. Valid options: %s', ...
                    Args.MetadataSource, strjoin(ValidSources, ', '));
            end

            % Validate metadata arrays if provided
            if strcmp(Args.MetadataSource, 'FromHeader')
                if numel(Args.Header) ~= Nobj
                    error('PhotCalibTrans:calibrate:HeaderSizeMismatch', ...
                        'Header array size (%d) must match Cat size (%d)', numel(Args.Header), Nobj);
                end
            end

            if strcmp(Args.MetadataSource, 'FromStruct')
                NMeta = numel(Args.Metadata);
                if NMeta ~= 1 && NMeta ~= Nobj
                    error('PhotCalibTrans:calibrate:MetadataSizeMismatch', ...
                        'Metadata array size (%d) must be 1 (same for all) or match Cat size (%d)', NMeta, Nobj);
                end
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n');
                fprintf('Processing %d object(s)\n', Nobj);
                fprintf('Input type: %s\n', class(Cat));
                fprintf('Metadata source: %s\n\n', Args.MetadataSource);
            end

            % ====================================================================
            % STEP 1: Build TransModel structure
            % ====================================================================

            if Args.Verbose
                fprintf('Step 1: Building transmission model structure...\n');
            end

            % Load catalog
            [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();

            % Build transmission function list from catalog
            FunList = [];
            for i = 1:length(Args.TransFunList)
                FunName = Args.TransFunList{i};
                if isfield(FunCat, FunName)
                    FunList = [FunList, FunCat.(FunName)];
                else
                    error('PhotCalibTrans:calibrate:InvalidFunction', ...
                          'Transmission function %s not found in catalog', FunName);
                end
            end

            % Get optimization sequence
            if ~isempty(Args.CustomOptSeq)
                OptSeq = Args.CustomOptSeq;
            else
                if isfield(StageCat, Args.OptSeqName)
                    OptSeq = StageCat.(Args.OptSeqName);
                else
                    error('PhotCalibTrans:calibrate:InvalidOptSeq', ...
                          'Optimization sequence %s not found in catalog', Args.OptSeqName);
                end
            end

            if Args.Verbose
                fprintf('  Using %d transmission functions: %s\n', ...
                        length(Args.TransFunList), strjoin(Args.TransFunList, ', '));
                if ~isempty(Args.CustomOptSeq)
                    fprintf('  Using custom optimization sequence (%d stages)\n', numel(OptSeq));
                else
                    fprintf('  Using optimization sequence: %s (%d stages)\n', ...
                            Args.OptSeqName, numel(OptSeq));
                end
            end

            if Args.Verbose
                fprintf('  Transmission functions and optimization sequence configured\n\n');
            end

            % ====================================================================
            % STEP 1.5: Pre-allocate output array
            % ====================================================================

            % Each object will store independent calibration results for its subimage
            Obj(Nobj) = PhotCalibTrans();

            % ====================================================================
            % STEP 2: Loop over subimages/subcatalogs
            % ====================================================================

            for Iobj = 1:Nobj
                if Args.Verbose
                    fprintf('--- Processing Object %d/%d ---\n', Iobj, Nobj);
                end

                % ----------------------------------------------------------------
                % STEP 2.0: Extract metadata and catalog for this subimage
                % ----------------------------------------------------------------

                % Extract metadata based on source
                switch Args.MetadataSource
                    case 'FromCat'
                        % Extract from Cat(Iobj).HeaderData
                        if IsAstroImage
                            HeaderSource = Cat(Iobj).HeaderData;
                        else
                            % AstroCatalog case - placeholder for when AstroCatalog has HeaderData
                            % TODO: Implement HeaderData property for AstroCatalog
                            if isprop(Cat(Iobj), 'HeaderData') && ~isempty(Cat(Iobj).HeaderData)
                                HeaderSource = Cat(Iobj).HeaderData;
                            else
                                error('PhotCalibTrans:calibrate:NoHeaderData', ...
                                    'AstroCatalog object does not have HeaderData property. Use different MetadataSource.');
                            end
                        end

                        Keys = {'MNTTEMP', 'EXPTIME', 'NCOADD', 'AIRMASS', 'PRESSURE'};
                        PropNames = {'Temp', 'ExpTime', 'NCoadd', 'AirMass', 'Pressure'};
                        Res = getStructKey(HeaderSource, Keys);

                        % Map FITS keywords to property names
                        Metadata = struct();
                        for i = 1:length(Keys)
                            if isfield(Res, Keys{i})
                                Metadata.(PropNames{i}) = Res.(Keys{i});
                            end
                        end

                        % Calculate derived fields
                        if isfield(Metadata, 'AirMass')
                            Metadata.Zenith = acosd(1.0 / Metadata.AirMass);
                        end

                        if Args.Verbose
                            fprintf('Metadata from Cat(%d).HeaderData:\n', Iobj);
                        end

                    case 'FromHeader'
                        % Extract from Args.Header(Iobj)
                        Keys = {'MNTTEMP', 'EXPTIME', 'NCOADD', 'AIRMASS', 'PRESSURE'};
                        PropNames = {'Temp', 'ExpTime', 'NCoadd', 'AirMass', 'Pressure'};
                        Res = getStructKey(Args.Header(Iobj), Keys);

                        % Map to property names
                        Metadata = struct();
                        for i = 1:length(Keys)
                            if isfield(Res, Keys{i})
                                Metadata.(PropNames{i}) = Res.(Keys{i});
                            end
                        end

                        % Calculate derived fields
                        if isfield(Metadata, 'AirMass')
                            Metadata.Zenith = acosd(1.0 / Metadata.AirMass);
                        end

                        if Args.Verbose
                            fprintf('Metadata from Header array (Args.Header(%d)):\n', Iobj);
                        end

                    case 'FromStruct'
                        % Use Args.Metadata directly (single struct for all or array)
                        if numel(Args.Metadata) == 1
                            % Single struct - use for all subimages
                            Metadata = Args.Metadata;
                            if Args.Verbose
                                fprintf('Metadata from struct (same for all):\n');
                            end
                        else
                            % Array of structs - use Iobj element
                            Metadata = Args.Metadata(Iobj);
                            if Args.Verbose
                                fprintf('Metadata from struct array (Args.Metadata(%d)):\n', Iobj);
                            end
                        end

                        % Calculate derived fields if needed
                        if isfield(Metadata, 'AirMass') && ~isfield(Metadata, 'Zenith')
                            Metadata.Zenith = acosd(1.0 / Metadata.AirMass);
                        end

                    case 'Default'
                        % Use default values
                        Metadata = struct(...
                            'AirMass', 1.2, ...
                            'Zenith', 33.56, ...
                            'ExpTime', 20, ...
                            'NCoadd', 1, ...
                            'Temp', 15, ...
                            'Pressure', 965);

                        if Args.Verbose
                            fprintf('Using default metadata values:\n');
                        end
                end

                % Set properties for this object
                Obj(Iobj).setProps(Metadata);

                % Extract catalog (depends on input type)
                if IsAstroImage
                    CurrentCat = Cat(Iobj).CatData;
                else
                    CurrentCat = Cat(Iobj);
                end

                % Display metadata if verbose
                if Args.Verbose
                    fprintf('  AirMass  = %.2f\n', Obj(Iobj).AirMass);
                    fprintf('  Zenith   = %.2f deg\n', Obj(Iobj).Zenith);
                    fprintf('  ExpTime  = %.1f s\n', Obj(Iobj).ExpTime);
                    fprintf('  NCoadd   = %d\n', Obj(Iobj).NCoadd);
                    fprintf('  Temp     = %.1f C\n', Obj(Iobj).Temp);
                    fprintf('  Pressure = %.1f mbar\n', Obj(Iobj).Pressure);
                end

                % ----------------------------------------------------------------
                % STEP 2.1: Build TransModel with real metadata for this subimage
                % ----------------------------------------------------------------

                % Build MetaValues from extracted metadata (use defaults for NaN values)
                % Use class default values if metadata is missing/NaN
                Zenith_val = Obj(Iobj).Zenith;
                if isnan(Zenith_val)
                    Zenith_val = 30;  % Default zenith angle [deg]
                    if Args.Verbose
                        fprintf('  Warning: Zenith angle is NaN, using default %.1f deg\n', Zenith_val);
                    end
                end

                Pressure_val = Obj(Iobj).Pressure;
                if isnan(Pressure_val)
                    Pressure_val = 965;  % Default pressure [mbar] (from property default)
                    if Args.Verbose
                        fprintf('  Warning: Pressure is NaN, using default %.1f mbar\n', Pressure_val);
                    end
                end

                Temp_val = Obj(Iobj).Temp;
                if isnan(Temp_val)
                    Temp_val = 15;  % Default temperature [C]
                    if Args.Verbose
                        fprintf('  Warning: Temperature is NaN, using default %.1f C\n', Temp_val);
                    end
                end

                MetaValuesSubim = struct(...
                    'ZenithAngle_deg', Zenith_val, ...
                    'Pressure_mbar', Pressure_val, ...
                    'Temperature_C', Temp_val);

                % Build TransModel with real metadata
                Obj(Iobj).TransModel = tools.math.fun.CompositeFun.model(FunList, ...
                    'MetadataValues', MetaValuesSubim, ...
                    'OptimizationSequence', OptSeq, ...
                    'UseTran2D', true, ...
                    'Tran2DType', Args.Tran2DType);

                % ----------------------------------------------------------------
                % STEP 2.2: Select calibrators
                % ----------------------------------------------------------------

                if Args.Verbose
                    fprintf('Selecting calibrators...\n');
                end

                CalData = Obj(Iobj).selectCalibrators(CurrentCat, ...
                    'SearchRadius', Args.SearchRadius, ...
                    'MagRange', Args.MagRange, ...
                    'FluxIni', Args.FluxIni, ...
                    'FluxEnd', Args.FluxEnd, ...
                    'EFluxIni', Args.EFluxIni, ...
                    'EFluxEnd', Args.EFluxEnd, ...
                    'Verbose', Args.Verbose);

                % ----------------------------------------------------------------
                % STEP 2.3: Fit transmission if calibrators found
                % ----------------------------------------------------------------

                if ~Obj(Iobj).CalFound
                    if Args.Verbose
                        fprintf('  No calibrators found - skipping transmission fitting.\n\n');
                    end
                    % Object already has CalFound = false and CalibData property set by selectCalibrators
                    % TransModel is present but not fitted
                    continue;
                end

                if Args.Verbose
                    fprintf('Fitting transmission parameters...\n');
                end

                % Extract data for fitting
                Flux = CalData.ObsData.Flux;
                X = CalData.ObsData.X;
                Y = CalData.ObsData.Y;

                % Calculate effective exposure time (accounting for coadding)
                ExpTime_eff = Obj(Iobj).ExpTime / Obj(Iobj).NCoadd;

                % Setup CostArgs for TransmissionMode
                CostArgs = struct(...
                    'WeightMatrix', CalData.Spec', ...
                    'TransmissionMode', true, ...
                    'CalibWavelength', CalData.SpecWvl, ...
                    'ExpTime', ExpTime_eff, ...
                    'Aperture_area_m2', Obj(Iobj).Aperture);

                % Fit transmission parameters
                [Model, FitRes] = Obj(Iobj).TransModel.fitPar(Obj(Iobj).TransWvl, Flux, ...
                    'X', X, 'Y', Y, ...
                    'CostArgs', CostArgs, ...
                    'Verbose', Args.Verbose);

                % Store fitted model
                Obj(Iobj).TransModel = Model;

                if Args.Verbose
                    fprintf('  Number of calibrators: %d\n', size(Obj(Iobj).CalibData.Spec, 1));
                    if ~isnan(Obj(Iobj).TransModel.RMS)
                        fprintf('  RMS: %.4f mag\n', Obj(Iobj).TransModel.RMS);
                    end
                    if ~isnan(Obj(Iobj).TransModel.Chi2) && ~isnan(Obj(Iobj).TransModel.DOF) && Obj(Iobj).TransModel.DOF > 0
                        fprintf('  Chi2/DOF: %.2f / %d = %.3f\n', ...
                                Obj(Iobj).TransModel.Chi2, Obj(Iobj).TransModel.DOF, Obj(Iobj).TransModel.Chi2/Obj(Iobj).TransModel.DOF);
                    end
                end

                % ----------------------------------------------------------------
                % STEP 2.6:  Update header (AstroImage only) - TODO placeholder
                % ----------------------------------------------------------------

                if IsAstroImage
                    % TODO: Write calibration results to Cat(Iobj).HeaderData
                    % Keys: PH_ZP, PH_RMS, PH_NCAL, etc.
                end

                if Args.Verbose
                    fprintf('\n');
                end
            end

            if Args.Verbose
                fprintf('=== Calibration Complete ===\n');
                fprintf('Processed %d object(s)\n', Nobj);
                fprintf('Successful: %d\n', sum([Obj.CalFound]));
                fprintf('Failed (no calibrators): %d\n\n', sum(~[Obj.CalFound]));
            end
        end

        function CalibData = selectCalibrators(Obj, Cat, Args)
            % Select calibrators with reference spectra for photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroImage or AstroCatalog object with observed sources
            %          * ...,key,val,...
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.0.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'MinSN' - Minimum S/N for calibrators. Default is 5.
            %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
            %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
            %            'FluxColName' - Flux column name to compare with. Default is 'FLUX_APER_3'.
            %            'FluxIni' - Start column index for calibrator spectra flux values. Default is 7.
            %            'FluxEnd' - End column index for calibrator spectra flux values. Default is 349.
            %            'EFluxIni' - Start column index for calibrator spectra flux errors. Default is 350.
            %            'EFluxEnd' - End column index for calibrator spectra flux errors. Default is 692.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - CalibData - Structure with calibrator data:
            %                        .SpecWvl - Wavelength grid for calibrator spectra [nm] [N_wvl x 1]
            %                        .Spec - Calibrator reference spectra [N_calib x N_wvl]
            %                        .SpecErr - Calibrator spectra errors [N_calib x N_wvl]
            %                        .ObsData - Structure with observed catalog data (e.g., LAST):
            %                          .Flux, .FluxErr, .X, .Y, .RA, .Dec
            %                        .CalData - Structure with calibrator data:
            %                          .RA, .Dec
            %                        .MatchDistance - Matching distance [arcsec]
            %                        .NumMatches - Number of matches per source
            % Author : D. Kovaleva (Dec 2025)
            % Example: CalibData = PC.selectCalibrators(Cat);
            %          CalibData = PC.selectCalibrators(AI, 'SearchRadius', 1.5, 'MagRange', [12 16]);
            % Note: Default implementation uses Gaia DR3 XP spectra from GAIADR3spec catalog.
            %       Default telescope/instrument configuration is for LAST.

            arguments
                Obj
                Cat  % AstroImage or AstroCatalog
                Args.SearchRadius = 1.0  % arcsec
                Args.MagRange = [12 16]
                Args.MinSN = 5
                Args.MaxSN = 1000
                Args.FilterBadFlags logical = true
                Args.FluxColName = 'FLUX_APER_3'
                Args.FluxIni = 7      % Start column index for calibrator spectra flux
                Args.FluxEnd = 349    % End column index for calibrator spectra flux
                Args.EFluxIni = 350   % Start column index for calibrator spectra flux errors
                Args.EFluxEnd = 692   % End column index for calibrator spectra flux errors
                Args.Verbose logical = true
            end

            RAD = constant.RAD;  % Conversion factor

            % ====================================================================
            % STEP 1: EXTRACT CATALOG FROM INPUT OBJECT
            % ====================================================================

            if isa(Cat, 'AstroImage')
                % For AstroImage, use first element if array - %%%%% TEMPORARY
        
                if numel(Cat) > 1
                    warning('PhotCalibTrans:selectCalibrators:multipleImages', ...
                            'Multiple AstroImage elements provided. Using first element only.');
                end
                Cat = Cat(1).CatData;
            end

            % Get the catalog table
            Tab = Cat.Table;

            % ====================================================================
            % STEP 2: APPLY QUALITY FILTERS
            % ====================================================================

            % Track original indices (for mapping match results back to filtered table)
            OriginalIdx = (1:height(Tab))';

            % Filter 1: Magnitude range
            magFilterMask = true(height(Tab), 1);
            if ismember('MAG_APER_3', Tab.Properties.VariableNames)
                magFilterMask = (Tab.MAG_APER_3 >= Args.MagRange(1)) & (Tab.MAG_APER_3 <= Args.MagRange(2));
                if Args.Verbose
                    fprintf('  Magnitude filter (%g-%g): %d sources passed\n', ...
                            Args.MagRange(1), Args.MagRange(2), sum(magFilterMask));
                end
            end

            Tab = Tab(magFilterMask, :);
            OriginalIdx = OriginalIdx(magFilterMask);

            % Filter 2: Bad FLAGS (optional) - vectorized
            if Args.FilterBadFlags && ismember('FLAGS', Tab.Properties.VariableNames)
                flags = Tab.FLAGS;
                % Check for critical bad flags (vectorized bitget operations)
                isSaturated = bitget(flags, 1);
                isNaN = bitget(flags, 7);
                isNegative = bitget(flags, 11);
                isCR = bitget(flags, 15);
                isNearEdge = bitget(flags, 24);

                % Mark as bad if it has multiple problematic flags
                badFlagsMask = (isSaturated + isNaN + isNegative + isCR + isNearEdge) >= 2;
                Tab = Tab(~badFlagsMask, :);
                OriginalIdx = OriginalIdx(~badFlagsMask);

                if Args.Verbose
                    fprintf('  FLAGS filter: %d sources passed\n', height(Tab));
                end
            end

            % Filter 3: S/N range
            if ismember('SN', Tab.Properties.VariableNames)
                snMask = (Tab.SN >= Args.MinSN) & (Tab.SN <= Args.MaxSN);
                Tab = Tab(snMask, :);
                OriginalIdx = OriginalIdx(snMask);

                if Args.Verbose
                    fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                            Args.MinSN, Args.MaxSN, height(Tab));
                end
            end

            % ====================================================================
            % STEP 3: MATCH WITH CALIBRATOR CATALOG
            % ====================================================================

            % Match with calibrator catalog (default: GAIADR3spec)
            % Use filtered Tab directly - RA/Dec in degrees, pass as degrees
            if Args.Verbose
                fprintf('  Matching %d filtered sources with GAIADR3spec (radius=%.1f arcsec)...\n', ...
                        height(Tab), Args.SearchRadius);
            end

            [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', ...
                                                              'Coo', [Tab.RA/RAD, Tab.Dec/RAD], ...
                                                              'Radius', Args.SearchRadius, ...
                                                              'CooUnits', 'rad', ...
                                                              'RadiusUnits', 'arcsec');

            % Extract match information (indices are into full Cat.Table)
            calIdx_all   = ResInd.Obj2_IndInObj1;     % Index of calibrator match for each observed source
            dist_rad_all  = ResInd.Obj2_Dist;          % Distance in radians
            nmatch_all    = ResInd.Obj2_NmatchObj1;    % Number of matches

            % Find which matched sources are in our filtered list
            % idxObsMatched_Full contains indices into the FULL catalog
            idxObsMatched_Full = find(~isnan(calIdx_all));

            % Filter to keep only those that passed our quality filters
            % Check which of the matched indices are in OriginalIdx
            [~, idxInFiltered] = ismember(idxObsMatched_Full, OriginalIdx);
            keepMask = idxInFiltered > 0;  % Only keep matches that are in our filtered table

            idxObsMatched_Full = idxObsMatched_Full(keepMask);
            idxInFiltered = idxInFiltered(keepMask);  % Positions in filtered Tab

            if Args.Verbose
                fprintf('  Found %d/%d filtered sources with Gaia XP matches\n', ...
                        length(idxObsMatched_Full), height(Tab));
            end

            calIdx        = double(calIdx_all(idxObsMatched_Full));
            dist_rad       = dist_rad_all(idxObsMatched_Full);
            nmatch         = nmatch_all(idxObsMatched_Full);

            if isempty(idxObsMatched_Full)
                warning('PhotCalibTrans:selectCalibrators:NoMatches', ...
                        'No calibrator matches found within %.1f arcsec for filtered sources', Args.SearchRadius);
                CalibData = struct('SpecWvl', [], 'Spec', [], 'SpecErr', [], ...
                                   'ObsData', [], 'CalData', [], ...
                                   'MatchDistance', [], 'NumMatches', []);
                Obj.CalibData = CalibData;
                Obj.CalFound = false;
                return;
            end

            % Extract matched tables using positions in filtered table
            ObsTab = Tab(idxInFiltered, :);  % Use positions in filtered Tab
            CalArr = CatH.Catalog;  % Use Catalog (matrix) instead of Table to avoid VariableUnits validation
            CalTab = CalArr(calIdx, :);
            Nmatch = size(CalTab, 1);

            if Args.Verbose
                fprintf('  Found %d matched calibrator pairs\n', Nmatch);
            end

            % ====================================================================
            % STEP 4: EXTRACT CALIBRATOR SPECTRA AND PREPARE OUTPUT
            % ====================================================================

            % Extract calibrator spectra (CalTab is already a matrix from Catalog)
            % Convert to double (catsHTM stores Gaia data as single for memory efficiency)
            SpecFlux = double(CalTab(:, Args.FluxIni:Args.FluxEnd));      % [N x 343]
            SpecErr = double(CalTab(:, Args.EFluxIni:Args.EFluxEnd));     % [N x 343]

            % Extract coordinates
            Cal_RA = double(CalTab(:, 1)) * RAD;   % rad -> deg
            Cal_Dec = double(CalTab(:, 2)) * RAD;  % rad -> deg

            % Extract observed data
            Obs_X = ObsTab.X;
            Obs_Y = ObsTab.Y;
            Obs_RA = ObsTab.RA;
            Obs_Dec = ObsTab.Dec;

            % Extract flux and flux error
            if ismember(Args.FluxColName, ObsTab.Properties.VariableNames)
                Obs_Flux = ObsTab.(Args.FluxColName);
            else
                error('PhotCalibTrans:selectCalibrators:FluxColNotFound', ...
                      'Flux column %s not found in catalog', Args.FluxColName);
            end

            % Get flux error column name (replace FLUX with FLUXERR)
            FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
            if ismember(FluxErrColName, ObsTab.Properties.VariableNames)
                Obs_FluxErr = ObsTab.(FluxErrColName);
            else
                Obs_FluxErr = sqrt(Obs_Flux);  % Use Poisson approximation
                if Args.Verbose
                    fprintf('  Warning: %s not found, using sqrt(flux) for errors\n', FluxErrColName);
                end
            end

            % Convert distance to arcsec
            Dist_arcsec = convert.angular('rad', 'arcsec', dist_rad);

            % ====================================================================
            % STEP 5: CREATE OUTPUT STRUCTURE
            % ====================================================================

            CalibData = struct();

            % Determine wavelength grid for calibrator spectra
            % Default: Gaia DR3 XP wavelength grid (336:2:1020 nm, 343 points)
            % TODO: Add logic to read SpecWvl from catalog if different calibrator source is used
            CalibData.SpecWvl = (336:2:1020)';   % [N_wvl x 1]

            CalibData.Spec = SpecFlux;           % [N_calib x N_wvl]
            CalibData.SpecErr = SpecErr;         % [N_calib x N_wvl]

            % Observed data structure
            CalibData.ObsData = struct(...
                'Flux', Obs_Flux, ...
                'FluxErr', Obs_FluxErr, ...
                'X', Obs_X, ...
                'Y', Obs_Y, ...
                'RA', Obs_RA, ...
                'Dec', Obs_Dec);

            % Calibrator data structure
            CalibData.CalData = struct(...
                'RA', Cal_RA, ...
                'Dec', Cal_Dec);

            % Match statistics
            CalibData.MatchDistance = Dist_arcsec;  % [N x 1]
            CalibData.NumMatches = nmatch;          % [N x 1]

            % Store calibrator data in object
            Obj.CalibData = CalibData;
            Obj.CalFound = true;

            if Args.Verbose
                fprintf('Calibrator selection complete: %d matched calibrators.\n\n', Nmatch);
            end
        end
    end

    methods % Evaluation methods
        function Trans = evaluateTransmission(Obj, Args)
            % Evaluate transmission at specific positions (with position-dependent corrections)
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Lambda' - Wavelength grid [nm] [N_lambda x 1]. Default is Obj.TransWvl (constant property).
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            % Output : - Trans - Transmission values [N_pos x N_lambda] or [N_lambda x 1]
            %                    If X, Y provided: matrix where Trans(i,j) = transmission for position i at wavelength j
            %                    If X, Y empty: vector of base transmission at field center
            % Author : D. Kovaleva (Dec 2025)
            % Example: Trans = PC.evaluateTransmission();  % Transmission at field center using Obj.TransWvl
            %          Trans = PC.evaluateTransmission('Lambda', CustomLambda, 'X', X, 'Y', Y);

            arguments
                Obj
                Args.Lambda = []
                Args.X = []
                Args.Y = []
            end

            % Check if calibration was performed
            if ~Obj.CalFound
                error('PhotCalibTrans:evaluateTransmission:NoCalibration', ...
                    'No calibrators found during calibration. Cannot evaluate transmission.');
            end

            % Use default Lambda if not provided
            if isempty(Args.Lambda)
                Lambda = Obj.TransWvl;
            else
                Lambda = Args.Lambda;
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:evaluateTransmission:NoModel', ...
                    'TransModel is not available. Run calibrate() first.');
            end

            Lambda = Lambda(:);  % Ensure column vector

            % If no positions provided, evaluate at field center (no position correction)
            if isempty(Args.X) || isempty(Args.Y)
                if ~isempty(Obj.TransModel.Tran2DObj)
                    % With Tran2D: evaluate at field center (reference point)
                    Xc = Obj.TransModel.Tran2DObj.ParNX(1);
                    Yc = Obj.TransModel.Tran2DObj.ParNY(1);
                    Trans = Obj.TransModel.evaluateWithPosition(Lambda, Xc, Yc);
                else
                    % Without Tran2D: evaluate base transmission directly
                    Trans = Obj.TransModel.evaluateAllFunParInput(Lambda);
                end
                Trans = Trans(:);  % Return as column vector [N_lambda x 1]
                return;
            end

            % Evaluate transmission at specified positions
            X = Args.X(:);  % Ensure column vectors
            Y = Args.Y(:);

            if ~isempty(Obj.TransModel.Tran2DObj)
                % With Tran2D: evaluate with position-dependent corrections
                % evaluateWithPosition returns [N_pos x N_lambda]
                Trans = Obj.TransModel.evaluateWithPosition(Lambda, X, Y);
            else
                % Without Tran2D: same transmission for all positions
                % Evaluate once and replicate
                TransBase = Obj.TransModel.evaluateAllFunParInput(Lambda);
                Trans = repmat(TransBase(:)', length(X), 1);  % [N_pos x N_lambda]
            end
        end

        function ZP = evaluateZP(Obj, Args)
            % Evaluate photometric zero point at specific positions
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            % Output : - ZP - Zero point(s) [N_pos x 1] or scalar
            %                 If X, Y provided: vector with ZP for each position
            %                 If X, Y empty: scalar ZP at field center
            % Author : D. Kovaleva (Dec 2025)
            % Example: ZP = PC.evaluateZP();  % ZP at field center
            %          ZP = PC.evaluateZP('X', X, 'Y', Y);  % ZP at specific positions
            %
            % Formula: ZP = 2.5*log10(ExpTime_eff * Area * Integral(Trans * Fnu * Lambda * dLambda) / (h*c))
            % where ExpTime_eff = EXPTIME/NCOADD (effective exposure time per image)
            %       Fnu is the AB system flux density (constant for flat spectrum)
            %       Lambda is Obj.TransWvl constant property (300:2:1100 nm)

            arguments
                Obj
                Args.X = []
                Args.Y = []
            end

            % Check if calibration was performed
            if ~Obj.CalFound
                error('PhotCalibTrans:evaluateZP:NoCalibration', ...
                    'No calibrators found during calibration. Cannot evaluate zero point.');
            end

            % Use constant wavelength grid
            Lambda = Obj.TransWvl;

            % Check that calibration has been performed
            if isnan(Obj.ExpTime) || isnan(Obj.NCoadd)
                error('PhotCalibTrans:evaluateZP:NoMetadata', 'ExpTime and NCoadd must be set. Run calibrate() first.');
            end

            % Evaluate transmission at positions (or field center if X, Y empty)
            % Trans is [N_lambda x 1] if no positions, or [N_pos x N_lambda] if positions provided
            Trans = Obj.evaluateTransmission('X', Args.X, 'Y', Args.Y);

            % Create flat Fnu spectrum for AB zero-point
            Fnu = constant.Fnu('SI');  % AB system flux density [W/m^2/Hz]
            FlatSpectrum = Fnu * ones(size(Lambda));  % [N_lambda x 1]

            % Physical constants
            H = constant.h('SI');  % Planck constant [J·s]
            B = H;  % For zero-point: B = H (not H*C as in flux conversion)

            % Ensure Trans is 2D matrix for consistent handling
            if isvector(Trans)
                Trans = Trans(:)';  % Convert to row vector [1 x N_lambda]
            end
            % Now Trans is [N_pos x N_lambda]

            % Apply transmission: multiply each row by FlatSpectrum
            % FlatSpectrum is [N_lambda x 1], Trans is [N_pos x N_lambda]
            SpecTrans = Trans .* FlatSpectrum';  % [N_pos x N_lambda]

            % Multiply by Lambda for integration
            Integrand = SpecTrans ./ Lambda';  % [N_pos x N_lambda]

            % Integrate along wavelength dimension (dim=2) for each position
            A = tools.math.integral.trapzmat(Lambda(:)', Integrand, 2);  % [N_pos x 1]

            % Calculate zero-point flux for all positions
            TotalFlux_ZP = Obj.Aperture * A / B;  % [N_pos x 1]

            % Convert to magnitude
            ZP = 2.5 * log10(TotalFlux_ZP);  % [N_pos x 1]

            % If single position, return scalar
            if length(ZP) == 1
                ZP = ZP(1);
            end
        end

        function [MagAB, MagABErr] = evaluateMag(Obj, Flux, Args)
            % Evaluate calibrated AB magnitudes from observed flux
            % Input  : - Obj - PhotCalibTrans object
            %          - Flux - Observed flux values [photons] [N x 1]
            %          * ...,key,val,...
            %            'X' - X coordinates [N x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N x 1]. Default is [] (field center).
            %            'MagErr' - Magnitude errors [N x 1]. Default is [].
            % Output : - MagAB - Calibrated AB magnitudes [N x 1]
            %          - MagABErr - Calibrated AB magnitude errors [N x 1] (optional)
            % Author : D. Kovaleva (Jan 2026)
            % Example: MagAB = PC.evaluateMag(Flux);
            %          [MagAB, MagABErr] = PC.evaluateMag(Flux, 'X', X, 'Y', Y, 'MagErr', MagErr);
            % Description: Converts observed flux to calibrated AB magnitudes.
            %              MAG_AB = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              Uses evaluateZP to calculate position-dependent zero points.
            %              Errors are provided directly (e.g., from MAGERR columns).

            arguments
                Obj
                Flux                 % Observed flux [photons] [N x 1]
                Args.X = []          % X coordinates [N x 1]
                Args.Y = []          % Y coordinates [N x 1]
                Args.MagErr = []     % Magnitude errors [N x 1]
            end

            % Check if calibration was performed
            if ~Obj.CalFound
                error('PhotCalibTrans:evaluateMag:NoCalibration', ...
                    'No calibrators found during calibration. Cannot evaluate magnitudes.');
            end

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.ExpTime / Obj.NCoadd;

            % Ensure column vectors
            Flux = Flux(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP('X', Args.X, 'Y', Args.Y);

            % Calculate calibrated AB magnitudes
            % MAG_AB = -2.5*log10(FLUX/ExpTime_eff) + ZP
            MagInst = -2.5 * log10(Flux / ExpTime_eff);

            if isscalar(ZP)
                MagAB = MagInst + ZP;
            else
                MagAB = MagInst + ZP(:);
            end

            % Return magnitude errors if requested
            if nargout > 1
                if isempty(Args.MagErr)
                    % No errors provided
                    MagABErr = [];
                else
                    % Use provided magnitude errors directly
                    MagABErr = Args.MagErr(:);
                end
            end
        end
    end


    methods % Header I/O methods
        function HeaderObj = writeToHeader(Obj, HeaderObj, Args)
            % Write calibration data to AstroHeader
            % Input  : - Obj - PhotCalibTrans object
            %          - HeaderObj - AstroHeader object
            %          * ...,key,val,...
            % Output : - HeaderObj - Updated AstroHeader object
            % Author : D. Kovaleva (Dec 2025)
            % Example: Header = PC.writeToHeader(Header);

            % TODO: Implement
        end

        function Obj = readFromHeader(Obj, HeaderObj, Args)
            % Read calibration data from AstroHeader
            % Input  : - Obj - PhotCalibTrans object
            %          - HeaderObj - AstroHeader object
            %          * ...,key,val,...
            % Output : - Obj - PhotCalibTrans object with data from header
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.readFromHeader(Header);

            % TODO: Implement
        end
    end

    methods % Catalog operations
        function CatObj = addMagAB(Obj, CatObj, Args)
            % Add calibrated AB magnitude columns to catalog
            % Input  : - Obj - PhotCalibTrans object
            %          - CatObj - AstroCatalog object with flux measurements
            %          * ...,key,val,...
            %            'FluxColNames' - Flux column names to calibrate. Default is all FLUX_* columns.
            %            'ApplyPosCorrection' - Apply position-dependent corrections. Default is true.
            % Output : - CatObj - AstroCatalog with added calibrated AB magnitude columns
            %                     (e.g., FLUX_APER_3 → MAG_AB_APER_3, FLUX_PSF → MAG_AB_PSF)
            % Author : D. Kovaleva (Jan 2026)
            % Example: Cat = PC.addMagAB(Cat);
            %          Cat = PC.addMagAB(Cat, 'FluxColNames', {'FLUX_APER_3', 'FLUX_PSF'});
            % Description: Creates new columns with calibrated AB magnitudes from flux measurements.
            %              Formula: MAG_AB = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              For each FLUX_<something> column, creates MAG_AB_<something> column.
            %              Preserves original flux columns.
            %              Applies position-dependent corrections if available.

            arguments
                Obj
                CatObj
                Args.FluxColNames = []
                Args.ApplyPosCorrection logical = true
            end

            % Validate inputs
            if isempty(Obj.TransModel)
                error('PhotCalibTrans:addMagAB:NoModel', 'TransModel is not calibrated. Run calibrate() first.');
            end

            % Get catalog table
            if isprop(CatObj, 'Table')
                Tab = CatObj.Table;
            else
                error('PhotCalibTrans:addMagAB:InvalidCatalog', 'CatObj must have a Table property');
            end

            if isempty(Tab) || height(Tab) == 0
                warning('PhotCalibTrans:addMagAB:EmptyCatalog', 'Catalog is empty. No columns added.');
                return;
            end

            % Determine which flux columns to calibrate
            AllColNames = Tab.Properties.VariableNames;
            if isempty(Args.FluxColNames)
                % Find all flux columns (FLUX_*)
                FluxColNames = AllColNames(startsWith(AllColNames, 'FLUX_'));
            else
                % Use specified columns
                if ischar(Args.FluxColNames)
                    FluxColNames = {Args.FluxColNames};
                else
                    FluxColNames = Args.FluxColNames;
                end

                % Verify columns exist
                for i = 1:length(FluxColNames)
                    if ~ismember(FluxColNames{i}, AllColNames)
                        error('PhotCalibTrans:addMagAB:ColumnNotFound', ...
                              'Column %s not found in catalog', FluxColNames{i});
                    end
                end
            end

            if isempty(FluxColNames)
                warning('PhotCalibTrans:addMagAB:NoFluxCols', 'No FLUX_* columns found in catalog.');
                return;
            end

            % Extract X, Y coordinates if position corrections are requested
            X = [];
            Y = [];
            if Args.ApplyPosCorrection
                if ismember('X', AllColNames) && ismember('Y', AllColNames)
                    X = Tab.X;
                    Y = Tab.Y;
                else
                    warning('PhotCalibTrans:addMagAB:NoCoords', ...
                            'X, Y columns not found. Position corrections disabled.');
                end
            end

            % Process each flux column
            for i = 1:length(FluxColNames)
                FluxColName = FluxColNames{i};

                % Get flux values [photons]
                Flux = Tab.(FluxColName);

                % Calculate calibrated AB magnitudes from flux
                % MAG_AB = -2.5*log10(FLUX/ExpTime_eff) + ZP
                MagAB = Obj.evaluateMag(Flux, 'X', X, 'Y', Y);

                % Create new calibrated magnitude column name
                % FLUX_APER_3 → MAG_AB_APER_3
                % FLUX_PSF → MAG_AB_PSF
                NewMagColName = strrep(FluxColName, 'FLUX_', 'MAG_AB_');

                % Insert column into catalog
                CatObj = CatObj.insertCol(MagAB, Inf, {NewMagColName});
            end

            % Note: New columns were inserted using insertCol within the loop above
        end
    end

    methods % Display/Output methods
        function summary(Obj, Args)
            % Display photometric calibration summary
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : None
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.summary();

            arguments
                Obj
                Args.Verbose logical = true
            end

            if ~Args.Verbose
                return;
            end

            fprintf('\n=== PhotCalibTrans Object ===\n');
            if ~isempty(Obj.CalibData)
                fprintf('Calibrators: %d\n', size(Obj.CalibData.Spec, 1));
            else
                fprintf('Calibrators: 0\n');
            end

            if ~isempty(Obj.TransModel)
                fprintf('Transmission Model: Available\n');

                if ~isempty(Obj.TransModel.RMS)
                    fprintf('RMS: %.4f mag\n', Obj.TransModel.RMS);
                end

                if ~isempty(Obj.TransModel.Chi2) && ~isempty(Obj.TransModel.DOF)
                    fprintf('Chi2/DOF: %.2f / %d = %.3f\n', ...
                            Obj.TransModel.Chi2, Obj.TransModel.DOF, ...
                            Obj.TransModel.Chi2/Obj.TransModel.DOF);
                end
            else
                fprintf('Transmission Model: Not available\n');
            end

            if ~isnan(Obj.AirMass)
                fprintf('Airmass: %.3f\n', Obj.AirMass);
            end

            % Check for position-dependent corrections
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.Tran2DObj) && ~isempty(Obj.TransModel.Tran2DObj.ParX)
                PosParams = Obj.TransModel.Tran2DObj.ParX;
                if any(PosParams(:) ~= 0)
                    fprintf('Position-dependent Corrections: Available (max: %.4f mag)\n', max(abs(PosParams(:))));
                end
            end

            fprintf('========================\n\n');
        end
    end

    
    methods % Plotting methods
        function Fig = plotTransmission(Obj, Args)
            % Plot transmission curve vs wavelength
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotTransmission();
            % Description: Uses Obj.TransWvl (300:2:1100 nm, 401 points) for transmission evaluation.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:plotTransmission:NoModel', 'TransModel not available');
            end

            % Evaluate transmission using constant wavelength grid
            Trans = Obj.evaluateTransmission('Lambda', Obj.TransWvl);

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Plot transmission curve
            plot(Obj.TransWvl, Trans, 'LineWidth', 2);
            grid on;
            xlabel('Wavelength [nm]');
            ylabel('Transmission');
            title('Total System Transmission');
            ylim([0, max(Trans(:)) * 1.1]);

            % Add metadata to title if available
            if ~isnan(Obj.AirMass)
                title(sprintf('Total System Transmission (Airmass=%.2f)', Obj.AirMass));
            end
        end

        function Fig = plotResiduals(Obj, Args)
            % Plot calibration residuals
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Type' - Plot type: 'magnitude' (residuals vs mag),
            %                     'spatial' (2D spatial distribution), 'both'. Default is 'both'.
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle or array of handles
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotResiduals();
            %          PC.plotResiduals('Type', 'spatial');
            % Description: Plots magnitude residuals from last fit stage.
            %              Shows spatial patterns and magnitude-dependent systematics.

            arguments
                Obj
                Args.Type = 'both'
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel) || isempty(Obj.TransModel.FitResults)
                error('PhotCalibTrans:plotResiduals:NoFitResults', 'Fit results not available');
            end

            % Get residuals from last fit stage
            LastStage = Obj.TransModel.FitResults(end);
            Residuals = LastStage.Residual;  % [N_calibrators x 1] in magnitude units

            % Get calibrator data
            if isempty(Obj.CalibData) || isempty(Obj.CalibData.ObsData)
                error('PhotCalibTrans:plotResiduals:NoCalibData', 'Calibrator data not available');
            end

            X = Obj.CalibData.ObsData.X;
            Y = Obj.CalibData.ObsData.Y;
            Flux = Obj.CalibData.ObsData.Flux;
            MagInst = -2.5 * log10(Flux);  % Convert flux to instrumental magnitude

            % Determine what to plot
            switch lower(Args.Type)
                case 'magnitude'
                    Nplots = 1;
                case 'spatial'
                    Nplots = 1;
                case 'both'
                    Nplots = 2;
                otherwise
                    error('Invalid Type. Must be ''magnitude'', ''spatial'', or ''both''.');
            end

            % Create figure(s)
            if Args.NewFigure
                if Nplots == 1
                    Fig = figure;
                else
                    Fig = [figure, figure];
                end
            else
                Fig = gcf;
            end

            % Plot 1: Residuals vs Magnitude
            if strcmpi(Args.Type, 'magnitude') || strcmpi(Args.Type, 'both')
                if Nplots == 2
                    figure(Fig(1));
                end

                scatter(MagInst, Residuals, 30, 'filled', 'MarkerFaceAlpha', 0.6);
                hold on;
                yline(0, 'k--', 'LineWidth', 1.5);
                grid on;
                xlabel('Instrumental Magnitude');
                ylabel('Residual [mag]');
                title(sprintf('Calibration Residuals (RMS=%.4f mag)', Obj.TransModel.RMS));

                % Add RMS lines
                yline(Obj.TransModel.RMS, 'r--', 'RMS');
                yline(-Obj.TransModel.RMS, 'r--', 'RMS');
            end

            % Plot 2: Spatial distribution
            if strcmpi(Args.Type, 'spatial') || strcmpi(Args.Type, 'both')
                if Nplots == 2
                    figure(Fig(2));
                end

                scatter(X, Y, 50, Residuals, 'filled');
                colorbar;
                colormap(jet);
                caxis([-3*Obj.TransModel.RMS, 3*Obj.TransModel.RMS]);
                xlabel('X [pixels]');
                ylabel('Y [pixels]');
                title(sprintf('Spatial Distribution of Residuals (RMS=%.4f mag)', Obj.TransModel.RMS));
                axis equal;
                grid on;
            end
        end

        function Fig = plotZPMap(Obj, Args)
            % Plot 2D map of position-dependent zero point corrections
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'GridSize' - Grid resolution [Nx, Ny]. Default is [50, 50].
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotZPMap();
            % Description: Shows position-dependent ZP corrections across the field.
            %              Requires TransModel with Tran2D position corrections.
            %              Uses Obj.TransWvl (300:2:1100 nm, 401 points) for ZP calculation.

            arguments
                Obj
                Args.GridSize = [50, 50]
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel) || isempty(Obj.TransModel.Tran2DObj)
                error('PhotCalibTrans:plotZPMap:NoTran2D', 'Position-dependent corrections not available');
            end

            % Get field boundaries from Tran2D
            Xc = Obj.TransModel.Tran2DObj.ParNX(1);
            Yc = Obj.TransModel.Tran2DObj.ParNY(1);
            Xrange = Obj.TransModel.Tran2DObj.ParNX(2);
            Yrange = Obj.TransModel.Tran2DObj.ParNY(2);

            % Create grid
            Xmin = Xc - Xrange/2;
            Xmax = Xc + Xrange/2;
            Ymin = Yc - Yrange/2;
            Ymax = Yc + Yrange/2;

            Xvec = linspace(Xmin, Xmax, Args.GridSize(1));
            Yvec = linspace(Ymin, Ymax, Args.GridSize(2));
            [Xgrid, Ygrid] = meshgrid(Xvec, Yvec);

            % Flatten grid for evaluation
            Xflat = Xgrid(:);
            Yflat = Ygrid(:);

            % Evaluate ZP at all grid positions 
            ZP = Obj.evaluateZP('X', Xflat, 'Y', Yflat);

            % Reshape to grid
            ZPgrid = reshape(ZP, Args.GridSize(2), Args.GridSize(1));

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Plot ZP map
            imagesc(Xvec, Yvec, ZPgrid);
            colorbar;
            colormap(jet);
            xlabel('X [pixels]');
            ylabel('Y [pixels]');
            title('Zero Point Map Across Field');
            axis xy;  % Correct orientation
            axis equal tight;

            % Add calibrator positions if available
            if ~isempty(Obj.CalibData) && ~isempty(Obj.CalibData.ObsData)
                hold on;
                plot(Obj.CalibData.ObsData.X, Obj.CalibData.ObsData.Y, 'w.', 'MarkerSize', 8);
            end
        end

        function Fig = plotCalibrators(Obj, Args)
            % Plot observed vs predicted magnitudes for calibrators
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotCalibrators();
            % Description: Shows 1:1 plot of observed vs model-predicted magnitudes.
            %              Includes RMS and Chi2/DOF statistics.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel) || isempty(Obj.TransModel.FitResults)
                error('PhotCalibTrans:plotCalibrators:NoFitResults', 'Fit results not available');
            end

            % Get observed and predicted values from last fit stage
            LastStage = Obj.TransModel.FitResults(end);
            Residuals = LastStage.Residual;  % [N x 1]

            % Get observed fluxes and convert to instrumental magnitudes
            Flux_obs = Obj.CalibData.ObsData.Flux;
            MagInst_obs = -2.5 * log10(Flux_obs);

            % Predicted instrumental magnitudes
            MagInst_pred = MagInst_obs - Residuals;

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Plot 1:1 comparison
            scatter(MagInst_pred, MagInst_obs, 40, 'filled', 'MarkerFaceAlpha', 0.6);
            hold on;

            % Add 1:1 line
            lims = [min([MagInst_pred; MagInst_obs]), max([MagInst_pred; MagInst_obs])];
            plot(lims, lims, 'k--', 'LineWidth', 2);

            % Add RMS error bands
            plot(lims, lims + Obj.TransModel.RMS, 'r--', 'LineWidth', 1);
            plot(lims, lims - Obj.TransModel.RMS, 'r--', 'LineWidth', 1);

            grid on;
            xlabel('Model Predicted Magnitude');
            ylabel('Observed Magnitude');
            axis equal tight;

            % Add statistics to title
            NumCalib = size(Obj.CalibData.Spec, 1);
            if ~isempty(Obj.TransModel.Chi2) && ~isempty(Obj.TransModel.DOF)
                title(sprintf('Calibrators: N=%d, RMS=%.4f mag, Chi2/DOF=%.2f/%d=%.2f', ...
                    NumCalib, Obj.TransModel.RMS, ...
                    Obj.TransModel.Chi2, Obj.TransModel.DOF, ...
                    Obj.TransModel.Chi2/Obj.TransModel.DOF));
            else
                title(sprintf('Calibrators: N=%d, RMS=%.4f mag', ...
                    NumCalib, Obj.TransModel.RMS));
            end

            % Add legend
            legend('Calibrators', '1:1 line', 'RMS bounds', 'Location', 'best');
        end

        function Fig = plotFitQuality(Obj, Args)
            % Plot RMS/Chi2 evolution across optimization stages
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotFitQuality();
            % Description: Shows convergence of fit across optimization stages.
            %              Displays RMS, Chi2/DOF evolution, and number of calibrators.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel) || isempty(Obj.TransModel.FitResults)
                error('PhotCalibTrans:plotFitQuality:NoFitResults', 'Fit results not available');
            end

            FitResults = Obj.TransModel.FitResults;
            Nstages = length(FitResults);

            % Extract metrics from each stage
            RMS_stages = zeros(Nstages, 1);
            Chi2_stages = zeros(Nstages, 1);
            DOF_stages = zeros(Nstages, 1);

            for i = 1:Nstages
                RMS_stages(i) = FitResults(i).RMS;
                if isfield(FitResults(i), 'Chi2')
                    Chi2_stages(i) = FitResults(i).Chi2;
                end
                if isfield(FitResults(i), 'DOF')
                    DOF_stages(i) = FitResults(i).DOF;
                end
            end

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Subplot 1: RMS evolution
            subplot(2, 1, 1);
            plot(1:Nstages, RMS_stages, 'o-', 'LineWidth', 2, 'MarkerSize', 8);
            grid on;
            xlabel('Optimization Stage');
            ylabel('RMS [mag]');
            title(sprintf('Fit Convergence (N=%d calibrators)', size(Obj.CalibData.Spec, 1)));
            xticks(1:Nstages);

            % Subplot 2: Chi2/DOF evolution
            subplot(2, 1, 2);
            if any(Chi2_stages ~= 0) && any(DOF_stages ~= 0)
                Chi2PerDOF = Chi2_stages ./ DOF_stages;
                plot(1:Nstages, Chi2PerDOF, 's-', 'LineWidth', 2, 'MarkerSize', 8);
                yline(1, 'r--', 'LineWidth', 1.5);  % Ideal Chi2/DOF = 1
                ylabel('Chi2/DOF');
                legend('Fit Quality', 'Ideal (Chi2/DOF=1)', 'Location', 'best');
            else
                plot(1:Nstages, Chi2_stages, 's-', 'LineWidth', 2, 'MarkerSize', 8);
                ylabel('Chi2');
            end
            grid on;
            xlabel('Optimization Stage');
            title('Goodness of Fit Evolution');
            xticks(1:Nstages);
        end
    end

    methods (Access = private)
    end

    methods (Static)
        function Obj = fromHeader(HeaderObj, Args)
            % Create and populate PhotCalibTrans object from AstroHeader

            % TODO: Implement
            Obj = PhotCalibTrans();
        end
    end
end