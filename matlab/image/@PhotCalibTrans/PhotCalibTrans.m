classdef PhotCalibTrans < Component
    % PhotCalibTrans - Transmission-based absolute photometric calibration
    % Description: Performs photometric calibration using atmospheric and instrumental
    %              transmission models. Fits multi-component transmission functions to
    %              calibrator stars with known spectra (default: Gaia DR3 XP). 
    %              Supports position-dependent field corrections via Tran2D polynomials.
    % Author : D. Kovaleva (Dec 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50 (transmission-based calibration)
    %
    % Constant Properties (Hidden):
    %   TransWvl  - Transmission wavelength grid [Angstrom] (3000:20:11000, 20 Angstrom step, 401 points)
    %
    % Inherited Properties (Hidden, from Component):
    %   Logger    - MsgLogger object for status logging via msgLog(LogLevel, message, ...)
    %               Supports LogLevel: Error, Warning, Info, Verbose, Debug
    %
    % Properties:
    %   TransModel - CompositeFun object with fitted transmission model
    %   SpecData   - Structure with reference spectral data (calibrator spectra)
    %   SourceData - AstroCatalog with observed calibrator sources (after calibration: Used, Residuals columns)
    %   CalFound   - Flag indicating whether calibrators were found (set by selectCalibrators)
    %   Success    - Flag indicating successful calibration (set by populateSuccess)
    %   AirMass, ExpTime, NCoadd, Temp, Pressure, Humidity, Aperture - Observation metadata
    %
    % Example:
    %{
     % Create calibration object and perform calibration on AstroImage
     PC = PhotCalibTrans();
     PC = PC.calibrate(AI);  % metadata read from AI.HeaderData, Success flag set automatically

     % Check calibration success
     if PC.Success
         fprintf('Calibration successful! RMS = %.4f mag\n', PC.TransModel.RMS);
     end

     % Evaluate transmission and zero point
     Trans = PC.evaluateTransmission();
     ZP = PC.evaluateZP();

     % Add calibrated magnitudes to catalog
     Cat = PC.addMag(Cat);

     % Write results to header
     PC.photCalibTransToHeader(AI.HeaderData);

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
    %     calibrate - Perform transmission-based photometric calibration
    %     selectCalibrators - Select calibrators with reference spectra
    %     populateSuccess - Evaluate and set Success flag based on calibration quality
    %   Evaluation Methods:
    %     evaluateTransmission - Evaluate transmission at specific positions
    %     evaluateZP - Evaluate photometric zero point at specific positions
    %     evaluateMag - Evaluate calibrated magnitudes from observed flux (AB or Vega)
    %     evaluatePredictedFlux - Evaluate model-predicted flux for calibrators
    %   Pre-computation Methods:
    %     propagateCalibratorMagErr - Propagate calibrator spectral and flux errors into per-star magnitude uncertainties
    %     resampleCalibratorSpectra - Resample calibrator reference spectra onto the transmission model wavelength grid
    %   Header I/O Methods:
    %     photCalibTransToHeader - Write calibration results to AstroHeader
    %     photCalibTransFromHeader - Read calibration data from AstroHeader
    %   Catalog Operations:
    %     addMag - Add calibrated magnitude columns to catalog (AB or Vega)
    %     addZP - Add position-dependent ZP column to catalog
    %   Display/Output Methods:
    %     summary - Display photometric calibration summary
    %   Plotting Methods:
    %     plotTransmission - Plot transmission curve vs wavelength
    %     plotResiduals - Plot calibration residuals (magnitude and spatial)
    %     plotZPMap - Plot 2D map of position-dependent zero point corrections
    %     plotCalibrators - Plot observed vs predicted magnitudes for calibrators
    %     plotFitQuality - Plot RMS/Chi2 evolution across optimization stages

    properties

        % Transmission model (empty until calibration)
        TransModel = []         % CompositeFun transmission model object containing:
                                %   Before calibration: .Funs (function list with initial parameters), .FunOperator ('*'),
                                %                        .Tran2DObj (position-dependent correction object), .UseTran2D (true/false)
                                %   After calibration:  .Funs.Par (fitted parameters), .RMS (fit RMS [mag]), .Chi2 (chi-squared), .DOF (degrees of freedom)

        % Calibration metadata (read from header, defaults for missing values)
        AirMass = 1.2           % Airmass
        Temp = 15              % Temperature [C]
        Pressure = 965          % Atmospheric pressure [mbar] (default: typical at observatory altitude)
        Humidity = NaN          % Relative humidity [%]
        Aperture = pi * (0.1397)^2  % Telescope aperture area [m^2] (default: LAST telescope)
        ExpTime = 1             % Exposure time [s]
        NCoadd = 1              % Number of coadded images (default: single image)

        % Calibrator information (empty until calibration)
        SpecData = []           % Structure with reference spectral data from selectCalibrators:
                                %   .CalData - struct with .RA, .Dec (catalog positions)
                                %   .SpecWvl [N_wvl x 1] - Wavelength grid [Angstrom] (e.g., 3360:20:10200 for Gaia DR3 XP)
                                %   .Spec [N_calib x N_wvl] - Calibrator spectra flux (Gaia DR3 XP)
                                %   .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
                                %   .SpecFluxMatrix [N_TransWvl x N_calib] - Pre-computed interpolated spectra
                                %        (set by calibrate, computed by resampleCalibratorSpectra)

        SourceData = []         % AstroCatalog with observed calibrator sources from selectCalibrators:
                                %   Catalog table columns: Flux, FluxErr, X, Y, RA, Dec, MatchDistance, NumMatches
                                %   After calibration: Used, Residuals, MAG_<System>, PredictedFlux, MagErr

        CalFound = false        % Flag indicating whether calibrators were found (set by selectCalibrators)
        NoRADec = false         % Flag indicating RA/Dec columns missing (set by selectCalibrators)

        % Per-source airmass
        AirmassColName = 'AIRMASS'          % Column name for per-source airmass in catalog
        PerSourceAirmass logical = false    % Whether per-source airmass was actually used

        % Success status
        Success = false         % Flag indicating successful calibration (set by populateSuccess)

        % Fit results by stage (stored after calibration for diagnostics)
        FitResults = []         % Struct array from CompositeFun.fitPar() with per-stage results:
                                %   Single-stage: FitResults.Cost, .RMS, .Residuals, .NumObs, .NumClipped,
                                %                 .KeepMask, .ConvergedSigmaClip, .Chi2, .DOF
                                %   Multi-stage:  FitResults(i).StageName, .Method, .Cost, .RMS, .Residuals,
                                %                 .NumObs, .NumClipped, .KeepMask, .IsFieldCorrection, .Chi2, .DOF

    end

    properties
        % Wavelength grid for transmission evaluation (20 Angstrom step)
        TransWvl = (3000:20:11000)'   % Transmission wavelength grid [Angstrom] for model evaluation (401 points)
    end

    methods % Constructor
        function Obj = PhotCalibTrans(varargin)
            % Constructor for PhotCalibTrans class
            % Input  : 
            %            * ...,key,val,...
            %            Metadata describing conditions of observations:
            %            'AirMass' - Airmass. Default is 1.2.
            %            'Temp' - Temperature [C]. Default is 15.
            %            'Pressure' - Atmospheric pressure [mbar]. Default is 965.
            %            'Humidity' - Relative humidity [%]. Default is NaN.
            %            'ExpTime' - Exposure time [s]. Default is 1.
            %            'NCoadd' - Number of coadded images. Default is 1.
            %            Instrument Configuration:
            %            'Aperture' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2 (LAST telescope).
            %            Calibration Data (set by calibrate() method):
            %            'TransModel' - CompositeFun transmission model object. Default is [].
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
            %          PC = PhotCalibTrans('AirMass', 1.5, 'ExpTime', 20, ...
            %                              'NCoadd', 1, 'Temp', 15, 'Pressure', 965);

            % Parse name-value pairs and set properties if they exist
            for I = 1:2:length(varargin)
                if I+1 <= length(varargin)
                    PropName = varargin{I};
                    if isprop(Obj, PropName)
                        Obj.(PropName) = varargin{I+1};
                    else
                        warning('PhotCalibTrans:UnknownProperty', ...
                            'Property "%s" does not exist and will be ignored.', PropName);
                    end
                end
            end
        end
    end

    methods % Core calibration methods
        function Obj = calibrate(Obj, Cat, Args)
            % Perform transmission-based photometric calibration
            % Input  : - PhotCalibTrans object 
            %          - AstroImage or AstroCatalog object with observed sources 
            %                  Metadata source is determined automatically:
            %                    AstroImage: metadata from Cat.HeaderData
            %                    AstroCatalog: metadata from 'Metadata' argument (if provided)
            %          * ...,key,val,...
            %            'Metadata' - Metadata source (for AstroCatalog only). Can be:
            %                         AstroHeader object, cell array {key1, val1, ...}, or [].
            %                         Default is [].
            %            'Lambda'         - Transmission wavelength grid [Angstrom]. Default is (3000:20:11000)'.
            %            'SearchRadius'   - Gaia matching radius [arcsec]. Default is 2.
            %            'MagRange'       - Calibrator magnitude range [min max]. Default is [11.5 15.5].
            %            'FunListName'    - Transmission function list name. Default is 'DefaultLASTFunList'.
            %            'CustomFunList'  - Custom function list. Default is [].
            %            'OptSeqName'     - Optimization sequence name. Default is 'LAST_NormLin'.
            %            'CustomOptSeq'   - Custom optimization sequence. Default is [].
            %            'Tran2DType'     - Position-dependent correction type. Default is 'cheby1_4_xt'.
            %            'UseTran2D'      - Enable position-dependent correction. Default is true.
            %            'WeightingMode'  - Weighting mode. Default is 'spectral'.
            %            'FluxErrColName' - Flux error column name. Default is 'FluxErr'.
            %            'SigmaClipMethod'- Sigma clipping method. Default is 'median'.
            %            'FluxErrorNorm'  - Flux error normalization. Default is 0.5.
            %            'MagSystem' - Magnitude system ('AB' or 'Vega'). Default is 'AB'.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - PhotCalibTrans object with calibration results.
            %                  SourceData catalog includes: Used, Residuals, MAG_<System>, PredictedFlux, MagErr
            % Author : D. Kovaleva (Jan 2026)
            % Reference: Garrappa et al. 2025, A&A 699, A50.
            % Example: PC = PhotCalibTrans();
            %          PC = PC.calibrate(AI);
            %          % With custom settings:
            %          PC = PC.calibrate(AI, 'UseTran2D', false, 'SearchRadius', 3);
            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog

                % Metadata argument (for AstroCatalog only)
                Args.Metadata = []     % AstroHeader object or cell array {key1, val1, key2, val2, ...}

                % Calibration settings (individual NV pairs with defaults)
                Args.Lambda           = (3000:20:11000)'
                Args.SearchRadius     = 2
                Args.MagRange         = [11.5 15.5]
                Args.FunListName      = 'DefaultLASTFunList'
                Args.CustomFunList    = []
                Args.OptSeqName       = 'LAST_NormLin'
                Args.CustomOptSeq     = []
                Args.Tran2DType       = 'cheby1_4_xt'
                Args.UseTran2D logical = true
                Args.WeightingMode    = 'spectral'
                Args.FluxErrColName   = 'FluxErr'
                Args.SigmaClipMethod  = 'median'
                Args.FluxErrorNorm    = 0.5
                Args.AirmassColName   = 'AIRMASS'
                Args.PerSourceAirmass logical = false

                Args.MagSystem char   = 'AB'
                Args.Verbose logical  = true
            end

            % Save Metadata argument separately
            Metadata = Args.Metadata;

            % Set wavelength grid
            Obj.TransWvl = Args.Lambda(:);

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:calibrate:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n');
            end

            IsAstroImage = isa(Cat, 'AstroImage');

            % ====================================================================
            % STEP 1: Extract metadata
            % ====================================================================

            if Args.Verbose
                fprintf('Step 1: Extracting observation metadata...\n');
            end

            % Extract metadata as cell array {key1, val1, key2, val2, ...}
            if iscell(Metadata)
                % AstroCatalog with cell array: use directly
                % (Metadata already set from Args.Metadata above)
            elseif IsAstroImage || isa(Metadata, 'AstroHeader')
                % Extract from header (either Cat.HeaderData or Metadata)
                Keys = {'MNTTEMP', 'EXPTIME', 'NCOADD', 'AIRMASS', 'PRESSURE'};
                PropNames = {'Temp', 'ExpTime', 'NCoadd', 'AirMass', 'Pressure'};

                if IsAstroImage
                    Res = Cat.HeaderData.getStructKey(Keys);
                else
                    Res = Metadata.getStructKey(Keys);
                end

                % Build cell array - only include non-NaN values
                % This preserves class default properties when header values are missing or invalid
                Metadata = cell(1, 2 * length(Keys));
                Idx = 1;
                for I = 1:length(Keys)
                    if isfield(Res, Keys{I})
                        Val = Res.(Keys{I});
                        if ~isempty(Val) && isnumeric(Val) && ~any(isnan(Val))
                            Metadata{Idx} = PropNames{I};
                            Metadata{Idx+1} = Val;
                            Idx = Idx + 2;
                        end
                    end
                end
                Metadata = Metadata(1:Idx-1);  % Trim to actual size
            else
                % Empty metadata - use object defaults
                Metadata = {};
            end

            % Set properties from cell array (convert to struct for setProps)
            if ~isempty(Metadata)
                MetadataStruct = struct(Metadata{:});
                Obj.setProps(MetadataStruct);
            end

            % Extract catalog (depends on input type)
            if IsAstroImage
                CurrentCat = Cat.CatData;
            else
                CurrentCat = Cat;
            end

            % Display metadata if verbose
            if Args.Verbose
                fprintf('  AirMass  = %.2f\n', Obj.AirMass);
                fprintf('  ExpTime  = %.1f s\n', Obj.ExpTime);
                fprintf('  NCoadd   = %d\n', Obj.NCoadd);
                fprintf('  Temp     = %.1f C\n', Obj.Temp);
                fprintf('  Pressure = %.1f mbar\n', Obj.Pressure);
            end

            % ====================================================================
            % STEP 2: Build TransModel structure with observation metadata
            % ====================================================================

            if Args.Verbose
                fprintf('\nStep 2: Building transmission model structure...\n');
            end

            % Compute zenith angle from airmass: sec(z) = AirMass → z = acosd(1/AirMass)
            ZenithAngle = acosd(1 / max(Obj.AirMass, 1.0));

            % Load catalog with actual observation metadata
            [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun(...
                'ZenithAngle_deg', ZenithAngle, ...
                'Pressure_mbar', Obj.Pressure, ...
                'Temperature_C', Obj.Temp);

            % Get transmission function list and optimization sequence
            FunList = FunCat.(Args.FunListName);
            OptSeq = StageCat.(Args.OptSeqName);

            if Args.Verbose
                if ~isempty(Args.CustomFunList)
                    fprintf('  Using custom function list (%d functions)\n', length(FunList));
                else
                    fprintf('  Using function list: %s (%d functions)\n', Args.FunListName, length(FunList));
                end
                if ~isempty(Args.CustomOptSeq)
                    fprintf('  Using custom optimization sequence (%d stages)\n', numel(OptSeq));
                else
                    fprintf('  Using optimization sequence: %s (%d stages)\n', Args.OptSeqName, numel(OptSeq));
                end
                fprintf('  ZenithAngle = %.1f deg (from AirMass = %.2f)\n', ZenithAngle, Obj.AirMass);
            end

            if Args.Verbose
                fprintf('  Transmission functions and optimization sequence configured\n\n');
            end

            % ====================================================================
            % STEP 3: Build TransModel with real metadata
            % ====================================================================

            % MetaValues for CompositeFun.model (already set in FunCatalog
            % via predefSeqCompositeFun, kept here for backward compatibility)
            MetaValues = {'ZenithAngle_deg', ZenithAngle, ...
                          'Pressure_mbar', Obj.Pressure, ...
                          'Temperature_C', Obj.Temp};

            % Build TransModel 
            Obj.TransModel = tools.math.fun.CompositeFun.model(FunList, ...
                'MetadataValues', MetaValues, ...
                'OptimizationSequence', OptSeq, ...
                'UseTran2D', Args.UseTran2D, ...
                'Tran2DType', Args.Tran2DType);

            % ====================================================================
            % STEP 4: Select calibrators
            % ====================================================================

            if Args.Verbose
                fprintf('Selecting calibrators...\n');
            end

            % Select calibrators (populates Obj.SpecData, Obj.SourceData, Obj.CalFound)
            Obj.selectCalibrators(CurrentCat, ...
                'SearchRadius', Args.SearchRadius, ...
                'MagRange', Args.MagRange, ...
                'Verbose', Args.Verbose);

            % selectCalibrators populates Obj.SpecData, Obj.SourceData, and Obj.CalFound

            % Store AirmassColName on object for post-fit use by addMag/addZP
            Obj.AirmassColName = Args.AirmassColName;

            % ====================================================================
            % STEP 4b: Extract per-source airmass if requested
            % ====================================================================

            PerSourceZenithAngles = [];
            if Args.PerSourceAirmass && Obj.CalFound
                CalibColNames = Obj.SourceData.Table.Properties.VariableNames;
                if ismember(Args.AirmassColName, CalibColNames)
                    PerSourceAirmassVec = Obj.SourceData.getCol(Args.AirmassColName);
                    ValidAM = ~isnan(PerSourceAirmassVec) & PerSourceAirmassVec >= 1.0;
                    if all(ValidAM)
                        PerSourceZenithAngles = acosd(1 ./ PerSourceAirmassVec);
                        Obj.PerSourceAirmass = true;
                        if Args.Verbose
                            fprintf('  Per-source airmass: range %.3f - %.3f (from %s)\n', ...
                                min(PerSourceAirmassVec), max(PerSourceAirmassVec), Args.AirmassColName);
                        end
                    else
                        Obj.msgLog(LogLevel.Debug, ...
                            'calibrate: %d/%d calibrators have invalid airmass in %s - falling back to header airmass', ...
                            sum(~ValidAM), length(PerSourceAirmassVec), Args.AirmassColName);
                        Obj.PerSourceAirmass = false;
                    end
                else
                    Obj.msgLog(LogLevel.Debug, ...
                        'calibrate: Column %s not found in calibrator catalog - falling back to header airmass', ...
                        Args.AirmassColName);
                    Obj.PerSourceAirmass = false;
                end
            else
                Obj.PerSourceAirmass = false;
            end

            % ====================================================================
            % STEP 5: Fit transmission if calibrators found
            % ====================================================================

            if ~Obj.CalFound
                if Args.Verbose
                    fprintf('  No calibrators found - skipping transmission fitting.\n\n');
                end
                % Object already has CalFound = false
                % TransModel is present but not fitted
            else
                % Calibrators found - proceed with fitting

                if Args.Verbose
                    fprintf('Fitting transmission parameters...\n');
                end

                % Extract data for fitting from SourceData
                Flux = Obj.SourceData.getCol('Flux');
                X = Obj.SourceData.getCol('X');
                Y = Obj.SourceData.getCol('Y');

                % Extract flux errors if using flux-based weighting
                FluxErrVector = [];
                if ismember(lower(Args.WeightingMode), {'flux', 'combined'})
                    try
                        FluxErrVector = Obj.SourceData.getCol(Args.FluxErrColName);
                        if Args.Verbose
                            fprintf('  Extracted flux errors from %s column\n', Args.FluxErrColName);
                        end
                    catch
                        warning('PhotCalibTrans:NoFluxErr', ...
                            'Could not extract flux errors from %s. Falling back to spectral weighting.', ...
                            Args.FluxErrColName);
                        if strcmpi(Args.WeightingMode, 'flux')
                            Args.WeightingMode = 'none';
                        else  % 'combined'
                            Args.WeightingMode = 'spectral';
                        end
                    end
                end

                % Calculate effective exposure time (accounting for coadding)
                ExpTime_eff = Obj.ExpTime / Obj.NCoadd;

                % Pre-compute MagErr for all calibrators (expensive, do once)
                % This avoids recalculating error propagation on every costFun call
                PrecomputedMagErr = Obj.propagateCalibratorMagErr(Flux, FluxErrVector, ...
                    'WeightingMode', Args.WeightingMode, ...
                    'ExpTime', ExpTime_eff, ...
                    'FluxErrorNorm', Args.FluxErrorNorm);

                % Store pre-computed MagErr in SourceData
                if istable(Obj.SourceData.Catalog)
                    Obj.SourceData.Catalog.MagErr = PrecomputedMagErr;
                else
                    Tab = Obj.SourceData.Table;
                    Tab.MagErr = PrecomputedMagErr;
                    Obj.SourceData.Catalog = Tab;
                end

                % Pre-compute interpolated spectra matrix (expensive, do once)
                % This avoids recalculating interpolation on every costFun call
                Obj.SpecData.SpecFluxMatrix = Obj.resampleCalibratorSpectra();

                % Setup CostArgs for TransmissionMode
                % MagErr and SpecFluxMatrix pre-computed to avoid repeated calculations
                CostArgs = {...
                    'WeightMatrix', Obj.SpecData.Spec', ...
                    'PrecomputedMagErr', PrecomputedMagErr, ...
                    'PrecomputedSpecFluxMatrix', Obj.SpecData.SpecFluxMatrix, ...
                    'TransmissionMode', true, ...
                    'CalibWavelength', Obj.SpecData.SpecWvl, ...
                    'ExpTime', ExpTime_eff, ...
                    'Aperture_area_m2', Obj.Aperture, ...
                    'PerSourceZenithAngles', PerSourceZenithAngles};

                % Fit transmission parameters
                [Model, FitResult] = Obj.TransModel.fitPar(Obj.TransWvl, Flux, ...
                    'X', X, 'Y', Y, ...
                    'CostArgs', CostArgs, ...
                    'SigmaClipMethod', Args.SigmaClipMethod, ...
                    'Verbose', Args.Verbose);

                % Store fitted model and fit results
                Obj.TransModel = Model;
                Obj.FitResults = FitResult;

                % Add Used and Residuals columns to SourceData
                % Get final KeepMask and Residuals (from last stage if multi-stage)
                if isstruct(FitResult) && ~isempty(FitResult)
                    if numel(FitResult) > 1
                        % Multi-stage: use last stage result
                        FinalResult = FitResult(end);
                    else
                        FinalResult = FitResult;
                    end

                    NCalib = height(Obj.SourceData.Table);
                    Used = FinalResult.KeepMask(:);
                    Residuals = nan(NCalib, 1);
                    Residuals(Used) = FinalResult.Residuals(:);

                    % MagErr was pre-computed and stored in SourceData before fitting
                    % Keep the original pre-computed values for all calibrators

                    % Calculate calibrated magnitudes for calibrators
                    % MAG = -2.5*log10(Flux/ExpTime_eff) + ZP(X,Y)
                    MagCalib = Obj.evaluateMag(Flux, 'X', X, 'Y', Y, ...
                                               'MagSystem', Args.MagSystem);

                    % Dynamic column name: MAG_AB or MAG_VEGA
                    MagColName = ['MAG_', Args.MagSystem];

                    % Get predicted flux from FitResult (calculated by costFun during optimization)
                    PredictedFlux = nan(NCalib, 1);
                    PredictedFlux(Used) = FinalResult.PredictedFlux(:);

                    % Add columns directly to the catalog (MagErr already present from pre-computation)
                    AMPerSourceUsed = double(Obj.PerSourceAirmass) * ones(NCalib, 1);
                    if istable(Obj.SourceData.Catalog)
                        Obj.SourceData.Catalog.Used = Used;
                        Obj.SourceData.Catalog.Residuals = Residuals;
                        Obj.SourceData.Catalog.(MagColName) = MagCalib;
                        Obj.SourceData.Catalog.PredictedFlux = PredictedFlux;
                        Obj.SourceData.Catalog.AMPerSourceUsed = AMPerSourceUsed;
                    else
                        % Convert to table, add columns, convert back
                        Tab = Obj.SourceData.Table;
                        Tab.Used = Used;
                        Tab.Residuals = Residuals;
                        Tab.(MagColName) = MagCalib;
                        Tab.PredictedFlux = PredictedFlux;
                        Tab.AMPerSourceUsed = AMPerSourceUsed;
                        Obj.SourceData.Catalog = Tab;
                    end

                    % Recalculate DOF correctly for multi-stage optimization
                    % DOF = Ncalib(final) - Ntot(free params)
                    NCalibFinal = sum(Used);
                    NFreeParams = 0;

                    % Count unique free function parameters across all stages
                    if ~isempty(Obj.TransModel.OptSeq)
                        % Collect unique parameter names from all stages
                        FittedParamNames = {};
                        HasFieldCorrection = false;

                        for IStage = 1:length(Obj.TransModel.OptSeq)
                            Stage = Obj.TransModel.OptSeq(IStage);
                            if ~isempty(Stage.FreeParams)
                                for IFree = 1:length(Stage.FreeParams)
                                    ParamName = Stage.FreeParams(IFree).Parameter;
                                    if ~any(strcmp(FittedParamNames, ParamName))
                                        FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                                    end
                                end
                            else
                                % Empty FreeParams indicates field correction stage
                                if ~isempty(Obj.TransModel.Tran2DObj)
                                    HasFieldCorrection = true;
                                end
                            end
                        end

                        NFreeParams = length(FittedParamNames);

                        % Count position correction parameters if fitted
                        if HasFieldCorrection
                            NFreeParams = NFreeParams + length(Obj.TransModel.Tran2DObj.ParX);
                        end
                    else
                        % No OptSeq, use initial FitPar configuration
                        for IFun = 1:length(Obj.TransModel.Funs)
                            NFreeParams = NFreeParams + sum(Obj.TransModel.Funs(IFun).FitPar);
                        end
                    end

                    % Set DOF
                    Obj.TransModel.DOF = NCalibFinal - NFreeParams;

                    if Args.Verbose
                        fprintf('  Calibrators (final): %d\n', NCalibFinal);
                        fprintf('  Free parameters: %d\n', NFreeParams);
                        fprintf('  DOF: %d\n', Obj.TransModel.DOF);
                    end
                end

                if Args.Verbose
                    fprintf('  Calibrators (initial): %d\n', size(Obj.SpecData.Spec, 1));
                    if ~isnan(Obj.TransModel.RMS)
                        fprintf('  RMS: %.4f mag\n', Obj.TransModel.RMS);
                    end
                    if ~isnan(Obj.TransModel.Chi2) && ~isnan(Obj.TransModel.DOF) && Obj.TransModel.DOF > 0
                        fprintf('  Chi2/DOF: %.2f / %d = %.3f\n', ...
                                Obj.TransModel.Chi2, Obj.TransModel.DOF, Obj.TransModel.Chi2/Obj.TransModel.DOF);
                    end
                end

            % Evaluate success criteria
            Obj = Obj.populateSuccess('Verbose', Args.Verbose);

            if Args.Verbose
                fprintf('=== Calibration Complete ===\n');
            end
            end  % Close if ~Obj.CalFound ... else block
        end

        function Obj = selectCalibrators(Obj, Cat, Args)
            % Select calibrators with reference spectra for photometric calibration
            % Input  : - PhotCalibTrans object
            %          - AstroCatalog object with observed sources (single element)
            %          * ...,key,val,...
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 2.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [11.5 15.5].
            %            'MinSN' - Minimum S/N for calibrators. Default is 5.
            %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
            %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
            %            'FluxColName' - Flux column name to compare with. Default is 'FLUX_APER_3'.
            %            'SpFluxCol' - Spectral flux column indices [flux_start, flux_end, error_start, error_end].
            %                          Default is [7, 349, 350, 692] for Gaia DR3 XP spectra.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - PhotCalibTrans object with populated properties:
            %                  .SpecData - Structure with reference spectral data:
            %                    .CalData - struct with .RA, .Dec (catalog positions)
            %                    .SpecWvl [N_wvl x 1] - Wavelength grid [Angstrom]
            %                    .Spec [N_calib x N_wvl] - Calibrator spectra flux
            %                    .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
            %                  .SourceData - AstroCatalog with observed calibrator sources
            %                    (columns: Flux, FluxErr, X, Y, RA, Dec, MatchDistance, NumMatches)
            %                  .CalFound - true if length(SourceData) > 0
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.selectCalibrators(Cat);
            %          PC = PC.selectCalibrators(Cat, 'SearchRadius', 2, 'MagRange', [11.5 15.5]);
            %          PC = PC.selectCalibrators(Cat, 'SpFluxCol', [7, 349, 350, 692]);
            % Note: Default implementation uses Gaia DR3 XP spectra from GAIADR3spec catalog.
            %       Default telescope/instrument configuration is for LAST.
            %       Input must be single-element AstroCatalog (extracted in calibrate()).

            arguments
                Obj
                Cat  % AstroCatalog
                Args.SearchRadius = 2  % arcsec
                Args.MagRange = [11.5 15.5]
                Args.MinSN = 5
                Args.MaxSN = 1000
                Args.FilterBadFlags logical = true
                Args.FluxColName = 'FLUX_APER_3'
                Args.MagColName = 'MAG_APER_3'
                Args.SpFluxCol = [7, 349, 350, 692]  % [flux_start, flux_end, error_start, error_end]
                Args.Verbose logical = true
            end

            RAD = constant.RAD;

            % ====================================================================
            % STEP 1: VALIDATE INPUT
            % ====================================================================

            % Get the catalog table
            Tab = Cat.Table;
            Nsources_initial = height(Tab);

            % Check if RA/Dec columns exist for calibrator matching
            AllColNames = Tab.Properties.VariableNames;
            HasRADec = ismember('RA', AllColNames) && ismember('Dec', AllColNames);

            if ~HasRADec
                Obj.NoRADec = true;  % Mark that RA/Dec columns are missing
                Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Catalog missing RA/Dec columns - cannot match calibrators');
                if Args.Verbose
                    fprintf('  Warning: Catalog missing RA/Dec columns - cannot match calibrators\n');
                    fprintf('Calibrator selection complete: 0 matched calibrators.\n\n');
                end
            end

            % ====================================================================
            % STEP 2: MATCH WITH CALIBRATOR CATALOG (BEFORE FILTERING)
            % ====================================================================

            % Only proceed if RA/Dec are available
            if HasRADec
                % Match all sources with calibrator catalog (default: GAIADR3spec)
                % Filter matches afterward to avoid indexing issues
                if Args.Verbose
                    fprintf('  Matching %d sources with GAIADR3spec (radius=%.1f arcsec)...\n', ...
                            Nsources_initial, Args.SearchRadius);
                end

                [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', ...
                                                              'Radius', Args.SearchRadius, ...
                                                              'RadiusUnits', 'arcsec');

            % Extract match information (indexed to full catalog)
            CalIdxAll   = ResInd.Obj2_IndInObj1;     % Index of calibrator match for each source
            DistRadAll  = ResInd.Obj2_Dist;          % Distance in radians
            NmatchAll   = ResInd.Obj2_NmatchObj1;    % Number of matches

            % Create mask for sources that have matches
            HasMatchMask = ~isnan(CalIdxAll);

            if Args.Verbose
                fprintf('  Found %d/%d sources with Gaia XP matches\n', ...
                        sum(HasMatchMask), Nsources_initial);
            end

            % ====================================================================
            % STEP 3: APPLY QUALITY FILTERS TO MATCHED SOURCES
            % ====================================================================

            % Start with sources that have matches
            GoodMask = HasMatchMask;

            % Filter 1: Magnitude range
            if ismember(Args.MagColName, Tab.Properties.VariableNames)
                MagFilterMask = (Tab.(Args.MagColName) >= Args.MagRange(1)) & (Tab.(Args.MagColName) <= Args.MagRange(2));
                GoodMask = GoodMask & MagFilterMask;
                if Args.Verbose
                    fprintf('  Magnitude filter (%g-%g): %d sources passed\n', ...
                            Args.MagRange(1), Args.MagRange(2), sum(GoodMask));
                end
            end

            % Filter 2: Bad FLAGS (optional)
            if Args.FilterBadFlags && ismember('FLAGS', Tab.Properties.VariableNames)
                Flags = Tab.FLAGS;
                % Sanitize: NaN/Inf/non-integer flags treated as bad
                BadValue = isnan(Flags) | isinf(Flags) | Flags < 0 | Flags ~= floor(Flags);
                Flags(BadValue) = 0;
                % Check for critical bad flags (vectorized bitget operations)
                IsSaturated = bitget(Flags, 1);
                IsNaN = bitget(Flags, 7);
                IsNegative = bitget(Flags, 11);
                IsCR = bitget(Flags, 15);
                IsNearEdge = bitget(Flags, 24);

                % Mark as bad if ANY of these flags is true, or if FLAGS value was invalid
                BadFlagsMask = BadValue | IsSaturated | IsNaN | IsNegative | IsCR | IsNearEdge;
                GoodMask = GoodMask & ~BadFlagsMask;

                if Args.Verbose
                    fprintf('  FLAGS filter: %d sources passed\n', sum(GoodMask));
                end
            end

            % Filter 3: S/N range
            if ismember('SN', Tab.Properties.VariableNames)
                SNMask = (Tab.SN >= Args.MinSN) & (Tab.SN <= Args.MaxSN);
                GoodMask = GoodMask & SNMask;

                if Args.Verbose
                    fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                            Args.MinSN, Args.MaxSN, sum(GoodMask));
                end
            end

            % Filter 4: Unique matches only (exclude sources with multiple identifications)
            UniqueMatchMask = (NmatchAll == 1);
            GoodMask = GoodMask & UniqueMatchMask;

            if Args.Verbose
                fprintf('  Unique match filter: %d sources passed\n', sum(GoodMask));
            end

                % Check if any sources passed all filters
                HasGoodMatches = any(GoodMask);

                if ~HasGoodMatches && Args.Verbose
                    fprintf('  Warning: No sources passed quality filters and have calibrator matches\n');
                end
            else
                HasGoodMatches = false;
            end

            % ====================================================================
            % STEP 4: EXTRACT CALIBRATOR DATA (if matches found)
            % ====================================================================

            if HasRADec && HasGoodMatches
                % Extract matched and filtered sources
                ObsTab = Tab(GoodMask, :);                    % Filtered observed sources
                CalIdx = double(CalIdxAll(GoodMask));        % Calibrator indices
                DistRad = DistRadAll(GoodMask);            % Match distances
                Nmatch = NmatchAll(GoodMask);                % Number of matches

                CalArr = CatH.Catalog;  % Use Catalog (matrix) instead of Table
                CalTab = CalArr(CalIdx, :);  % Matched calibrators
                NmatchTotal = size(CalTab, 1);

                if Args.Verbose
                    fprintf('  Found %d matched calibrator pairs\n', NmatchTotal);
                end

                % Extract column indices from SpFluxCol
                FluxIni = Args.SpFluxCol(1);
                FluxEnd = Args.SpFluxCol(2);
                EFluxIni = Args.SpFluxCol(3);
                EFluxEnd = Args.SpFluxCol(4);

                % Extract calibrator spectra (CalTab is already a matrix from Catalog)
                % Convert to double (catsHTM stores Gaia data as single for memory efficiency)
                SpecFlux = double(CalTab(:, FluxIni:FluxEnd));      % [N x 343]
                SpecErr = double(CalTab(:, EFluxIni:EFluxEnd));     % [N x 343]

                % Extract coordinates
                Cal_RA = double(CalTab(:, 1)) * RAD;   % rad -> deg
                Cal_Dec = double(CalTab(:, 2)) * RAD;  % rad -> deg

                % Extract observed data
                Obs_X = ObsTab.X;
                Obs_Y = ObsTab.Y;
                Obs_RA = ObsTab.RA;
                Obs_Dec = ObsTab.Dec;

                % Extract flux from specified column (for fitting)
                Obs_Flux = ObsTab.(Args.FluxColName);

                % Extract per-source airmass if available
                HasAirmassCol = ismember('AIRMASS', ObsTab.Properties.VariableNames);
                if HasAirmassCol
                    Obs_Airmass = ObsTab.AIRMASS;
                end

                % Get flux error column name (replace FLUX with FLUXERR)
                FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
                if ismember(FluxErrColName, ObsTab.Properties.VariableNames)
                    Obs_FluxErr = ObsTab.(FluxErrColName);
                else
                    Obs_FluxErr = sqrt(abs(Obs_Flux));  % Use Poisson approximation
                    if Args.Verbose
                        fprintf('  Warning: %s not found, using sqrt(flux) for errors\n', FluxErrColName);
                    end
                end

                % ============================================================
                % DATA VALIDATION: Check for invalid values in calibrator data
                % Invalid calibrators will be excluded from fitting but logged
                % ============================================================
                Nsources_before = length(Obs_Flux);

                % Validate Flux
                InvalidFlux = isnan(Obs_Flux) | isinf(Obs_Flux) | (Obs_Flux <= 0);
                if any(InvalidFlux)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Flux validation: %d/%d sources have invalid Flux (NaN/Inf/<=0) - excluded from calibrators', ...
                        sum(InvalidFlux), Nsources_before);
                end

                % Validate X, Y coordinates
                InvalidXY = isnan(Obs_X) | isinf(Obs_X) | isnan(Obs_Y) | isinf(Obs_Y);
                if any(InvalidXY)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Position validation: %d/%d sources have invalid X/Y (NaN/Inf) - excluded from calibrators', ...
                        sum(InvalidXY), Nsources_before);
                end

                % Validate RA, Dec
                InvalidRADec = isnan(Obs_RA) | isinf(Obs_RA) | isnan(Obs_Dec) | isinf(Obs_Dec);
                if any(InvalidRADec)
                    Obj.msgLog(LogLevel.Debug, 'selectCalibrators: Coordinate validation: %d/%d sources have invalid RA/Dec (NaN/Inf) - excluded from calibrators', ...
                        sum(InvalidRADec), Nsources_before);
                end

                % Combined valid mask for calibrator selection
                ValidCalibMask = ~InvalidFlux & ~InvalidXY & ~InvalidRADec;
                Nvalid = sum(ValidCalibMask);

                if Nvalid < Nsources_before
                    % Keep only valid calibrators for fitting
                    Obs_X = Obs_X(ValidCalibMask);
                    Obs_Y = Obs_Y(ValidCalibMask);
                    Obs_RA = Obs_RA(ValidCalibMask);
                    Obs_Dec = Obs_Dec(ValidCalibMask);
                    Obs_Flux = Obs_Flux(ValidCalibMask);
                    Obs_FluxErr = Obs_FluxErr(ValidCalibMask);
                    DistRad = DistRad(ValidCalibMask);
                    Nmatch = Nmatch(ValidCalibMask);
                    CalIdx = CalIdx(ValidCalibMask);
                    Cal_RA = Cal_RA(ValidCalibMask);
                    Cal_Dec = Cal_Dec(ValidCalibMask);
                    SpecFlux = SpecFlux(ValidCalibMask, :);
                    SpecErr = SpecErr(ValidCalibMask, :);
                    if HasAirmassCol
                        Obs_Airmass = Obs_Airmass(ValidCalibMask);
                    end

                    if Args.Verbose
                        fprintf('  Data validation: %d/%d calibrators have valid data\n', Nvalid, Nsources_before);
                    end
                end

                NmatchTotal = Nvalid;

                % Check if any valid calibrators remain
                if NmatchTotal == 0
                    Obj.msgLog(LogLevel.Error, 'selectCalibrators: No valid calibrators remain after data validation');
                    Obj.SourceData = [];
                    Obj.SpecData = [];
                    Obj.CalFound = false;
                    return;
                end

                % Convert distance to arcsec
                DistArcsec = convert.angular('rad', 'arcsec', DistRad);

                % Populate SpecData structure with reference spectral data
                Obj.SpecData = struct();
                Obj.SpecData.CalData = struct('RA', Cal_RA, 'Dec', Cal_Dec);

                % Determine wavelength grid for calibrator spectra
                % Default: Gaia DR3 XP wavelength grid (3360:20:10200 Angstrom, 343 points)
                Obj.SpecData.SpecWvl = (3360:20:10200)';   % [N_wvl x 1]
                Obj.SpecData.Spec = SpecFlux;              % [N_calib x N_wvl]
                Obj.SpecData.SpecErr = SpecErr;            % [N_calib x N_wvl]

                % Populate SourceData as AstroCatalog with observed calibrator sources
                SourceTable = table(Obs_Flux, Obs_FluxErr, Obs_X, Obs_Y, Obs_RA, Obs_Dec, DistArcsec, Nmatch, ...
                                    'VariableNames', {'Flux', 'FluxErr', 'X', 'Y', 'RA', 'Dec', 'MatchDistance', 'NumMatches'});
                if HasAirmassCol
                    SourceTable.AIRMASS = Obs_Airmass;
                end
                Obj.SourceData = AstroCatalog(SourceTable);

                % Set CalFound flag
                Obj.CalFound = true;

                if Args.Verbose
                    fprintf('Calibrator selection complete: %d matched calibrators.\n\n', NmatchTotal);
                end
            else
                % No RA/Dec or no good matches - set failure state
                Obj.SourceData = [];
                Obj.SpecData = [];
                Obj.CalFound = false;
            end

            % Clean up temporary columns added by ee to input catalog
            if HasRADec
                CatTab = Cat.Table;
                if ismember('Dist', CatTab.Properties.VariableNames)
                    Cat = Cat.deleteCol('Dist');
                end
                if ismember('Nmatch', CatTab.Properties.VariableNames)
                    Cat = Cat.deleteCol('Nmatch');
                end
            end
        end

        function Obj = populateSuccess(Obj, Args)
            % Evaluate and set Success flag based on calibration quality criteria
            % Input  : - PhotCalibTrans object (scalar)
            %          * ...,key,val,...
            %            'NCalibMin' - Minimum number of calibrators required. Default is 30.
            %            'RMSMax' - Maximum allowed RMS [mag]. Default is 0.1.
            %            'MinCalibRetention' - Minimum fraction of calibrators retained after sigma clipping. Default is 0.8.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - PhotCalibTrans object with updated Success flag
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.populateSuccess();
            %          PC = PC.populateSuccess('NCalibMin', 50, 'RMSMax', 0.08, 'MinCalibRetention', 0.75);
            % Description: Evaluates calibration success based on four criteria:
            %              1. CalFound = true (calibrators were found)
            %              2. Number of calibrators >= NCalibMin (default: 30)
            %              3. RMS <= RMSMax (default: 0.1 mag)
            %              4. Calibrator retention >= MinCalibRetention (default: 0.8, i.e., 80% retained)
            %              Sets Obj.Success = true only if all criteria are met.

            arguments
                Obj
                Args.NCalibMin = 0   %30
                Args.RMSMax = 100    %0.1  
                Args.MinCalibRetention = 0.0 %0.5
                Args.Verbose logical = false
            end

            % Evaluate all criteria (Success remains false unless all criteria pass)
            Obj.Success = false;

            % Criterion 1+2: Check if we have sufficient calibrators (this also implies CalFound = true)
            HasEnoughCalibrators = false;
            NCalibInitial = 0;
            if ~isempty(Obj.SpecData) && ~isempty(Obj.SpecData.Spec)
                NCalibInitial = size(Obj.SpecData.Spec, 1);
                HasEnoughCalibrators = (NCalibInitial >= Args.NCalibMin);
            end

            % Criterion 3: Check if RMS is acceptable
            HasAcceptableRMS = false;
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.RMS) && ~isnan(Obj.TransModel.RMS)
                HasAcceptableRMS = (Obj.TransModel.RMS <= Args.RMSMax);
            end

            % Criterion 4: Check if sufficient calibrators survived sigma clipping
            HasAcceptableRetention = false;
            if ~isempty(Obj.SourceData) && istable(Obj.SourceData.Catalog) && ismember('Used', Obj.SourceData.Catalog.Properties.VariableNames)
                NCalibFinal = sum(Obj.SourceData.Catalog.Used);
                if NCalibInitial > 0
                    CalibRetention = NCalibFinal / NCalibInitial;
                    HasAcceptableRetention = (CalibRetention >= Args.MinCalibRetention);

                    if Args.Verbose
                        fprintf('Calibrator retention: %d/%d = %.1f%%\n', NCalibFinal, NCalibInitial, CalibRetention*100);
                    end
                end
            end

            % Set success only if all criteria are met
            if HasEnoughCalibrators && HasAcceptableRMS && HasAcceptableRetention
                Obj.Success = true;
            end
        end
    end

    methods % Evaluation methods
        function Trans = evaluateTransmission(Obj, Args)
            % Evaluate transmission at specific positions (with position-dependent corrections)
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Lambda' - Wavelength grid [Angstrom] [N_lambda x 1]. Default is Obj.TransWvl (constant property).
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            % Output : - Transmission values [N_pos x N_lambda] or [N_lambda x 1]
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

            % Use default Lambda if not provided
            if isempty(Args.Lambda)
                Lambda = Obj.TransWvl;
            else
                Lambda = Args.Lambda;
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
            % Input  : - PhotCalibTrans object.
            %          * ...,key,val,...
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            %            'PerSourceZenithAngles' - Per-source zenith angles [deg]
            %                         [N_pos x 1]. When non-empty, evaluates per-source
            %                         atmospheric transmission for each source.
            %                         Default is [] (use single fitted airmass).
            % Output : - Zero point(s) [N_pos x 1] or scalar.
            %                 If X, Y provided: vector with ZP for each position.
            %                 If X, Y empty: scalar ZP at field center.
            % Author : D. Kovaleva (Dec 2025)
            % Example: ZP = PC.evaluateZP();  % ZP at field center
            %          ZP = PC.evaluateZP('X', X, 'Y', Y);  % ZP at specific positions
            %          ZP = PC.evaluateZP('MagSystem', 'AB');

            arguments
                Obj
                Args.X = []
                Args.Y = []
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
                Args.PerSourceZenithAngles = []  % [N_pos x 1] per-source zenith angles [deg]
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:evaluateZP:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            Fnu = constant.Fnu('SI');  % AB system reference flux density [W/m^2/Hz]
            H = 6.62607015e-34;         % SI 2019 Plank constant

            % Use constant wavelength grid
            Lambda = Obj.TransWvl;

            if ~isempty(Args.PerSourceZenithAngles)
                % === Per-source airmass mode ===
                % Each source gets its own atmospheric transmission based on its zenith angle
                N_pos = length(Args.PerSourceZenithAngles);

                % Build per-source AllFunPar matrix
                AllFunPar = Obj.TransModel.getAllFunPar();
                AllNames = AllFunPar.Name;
                ZenithIdx = find(strcmp(AllNames, 'ZenithAngle_deg'));
                PerSourceParams = repmat(AllFunPar.Val(:)', N_pos, 1);  % [N_pos x N_params]
                PerSourceParams(:, ZenithIdx) = Args.PerSourceZenithAngles(:);

                % Evaluate per-source transmission: [N_wvl x N_pos]
                TransPerSource = Obj.TransModel.evaluateAllFunParInput(Lambda, PerSourceParams);

                % Create flat Fnu spectrum for AB zero-point
                FlatSpectrum = Fnu * ones(size(Lambda));  % [N_lambda x 1]

                % Apply transmission per source: [N_wvl x N_pos] .* [N_wvl x 1]
                SpecTrans = TransPerSource .* FlatSpectrum;  % [N_wvl x N_pos]

                % Divide by Lambda for integration
                Integrand = SpecTrans ./ Lambda;  % [N_wvl x N_pos]

                % Integrate along wavelength dimension (dim 1 = along rows/wavelengths)
                A = tools.math.integral.trapzmat(Lambda(:), Integrand, 1);  % [1 x N_pos]

                % Calculate per-source zero-point flux
                TotalFlux_ZP = Obj.Aperture * A / H;  % [1 x N_pos]

                % Convert to per-source magnitude ZP
                ZP = 2.5 * log10(TotalFlux_ZP);  % [1 x N_pos]
                ZP = ZP(:);  % [N_pos x 1]

                % Add position-dependent Tran2D correction if available
                if ~isempty(Args.X) && ~isempty(Args.Y) && ...
                   ~isempty(Obj.TransModel.Tran2DObj) && Obj.TransModel.UseTran2D
                    [FieldCorrectionMag, ~] = Obj.TransModel.Tran2DObj.forward(Args.X(:), Args.Y(:), false);
                    ZP = ZP - FieldCorrectionMag(:);
                end
            else
                % === Single airmass mode (original path) ===
                % Evaluate BASE transmission (without position-dependent correction)
                TransBase = Obj.TransModel.evaluateAllFunParInput(Lambda);
                TransBase = TransBase(:)';  % Row vector [1 x N_lambda]

                % Create flat Fnu spectrum for AB zero-point
                FlatSpectrum = Fnu * ones(size(Lambda));  % [N_lambda x 1]

                % Apply transmission: multiply by FlatSpectrum
                SpecTrans = TransBase .* FlatSpectrum';  % [1 x N_lambda]

                % Multiply by Lambda for integration
                Integrand = SpecTrans ./ Lambda';  % [1 x N_lambda]

                % Integrate along wavelength dimension
                A = tools.math.integral.trapzmat(Lambda(:)', Integrand, 2);  % scalar

                % Calculate base zero-point flux
                TotalFlux_ZP = Obj.Aperture * A / H;  % scalar

                % Convert to base magnitude ZP
                ZP_base = 2.5 * log10(TotalFlux_ZP);  % scalar

                % Add position-dependent correction if positions provided and Tran2D exists
                if ~isempty(Args.X) && ~isempty(Args.Y) && ...
                   ~isempty(Obj.TransModel.Tran2DObj) && Obj.TransModel.UseTran2D
                    X = Args.X(:);
                    Y = Args.Y(:);

                    % Get field correction in magnitude space from Tran2D
                    [FieldCorrectionMag, ~] = Obj.TransModel.Tran2DObj.forward(X, Y, false);
                    FieldCorrectionMag = FieldCorrectionMag(:);  % [N_pos x 1]

                    % ZP at each position = base ZP + field correction
                    ZP = ZP_base - FieldCorrectionMag;
                else
                    ZP = ZP_base;
                end
            end

            % If single position, return scalar
            if length(ZP) == 1
                ZP = ZP(1);
            end
        end

        function [Mag, MagErr] = evaluateMag(Obj, Flux, Args)
            % Evaluate calibrated magnitudes from observed flux
            % Input  : - PhotCalibTrans object.
            %          - Observed flux values [photons] [N x 1].
            %          * ...,key,val,...
            %            'X' - X coordinates [N x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N x 1]. Default is [] (field center).
            %            'MagErr' - Magnitude errors [N x 1]. Default is [].
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            % Output : - Calibrated magnitudes [N x 1].
            %          - Calibrated magnitude errors [N x 1] (optional).
            % Author : D. Kovaleva (Jan 2026)
            % Example: Mag = PC.evaluateMag(Flux);
            %          [Mag, MagErr] = PC.evaluateMag(Flux, 'X', X, 'Y', Y, 'MagErr', MagErr);
            %          Mag = PC.evaluateMag(Flux, 'MagSystem', 'AB');
            % Description: Converts observed flux to calibrated magnitudes.
            %              MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              Uses evaluateZP to calculate position-dependent zero points.
            %              Errors are provided directly (e.g., from MAGERR columns).

            arguments
                Obj
                Flux                 % Observed flux [photons] [N x 1]
                Args.X = []          % X coordinates [N x 1]
                Args.Y = []          % Y coordinates [N x 1]
                Args.MagErr = []     % Magnitude errors [N x 1]
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:evaluateMag:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.ExpTime / Obj.NCoadd;

            % Ensure column vectors
            Flux = Flux(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP('X', Args.X, 'Y', Args.Y, ...
                                'MagSystem', Args.MagSystem);
            ZP = ZP(:);  % Ensure column vector

            % Calculate calibrated magnitudes
            % MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            Mag = convert.luptitude(Flux/ExpTime_eff, 10.^(0.4.*ZP));

            % Return magnitude errors if requested
            if nargout > 1
                if isempty(Args.MagErr)
                    % No errors provided
                    MagErr = [];
                else
                    % Use provided magnitude errors directly
                    MagErr = Args.MagErr(:);
                end
            end
        end

        function PredictedFlux = evaluatePredictedFlux(Obj, Args)
            % Evaluate predicted flux for calibrators using fitted transmission model
            % Input  : - PhotCalibTrans object (must have TransModel and SpecData)
            %          * ...,key,val,...
            %            'CostArgs' - Cell array of costFun arguments. Default uses stored data.
            % Output : - Predicted photon counts [N_calib x 1]
            % Author : D. Kovaleva (Jan 2026)
            % Description: Calls costFun with stored or provided CostArgs to calculate predicted flux.
            % Example: PredictedFlux = PC.evaluatePredictedFlux();
            %          PredictedFlux = PC.evaluatePredictedFlux('CostArgs', CustomCostArgs);

            arguments
                Obj
                Args.CostArgs = []
            end

            if isempty(Obj.TransModel) || isempty(Obj.SpecData)
                error('PhotCalibTrans:evaluatePredictedFlux:NoModel', ...
                    'TransModel and SpecData must be populated');
            end

            % Get stored data
            Flux = Obj.SourceData.getCol('Flux');

            % Build default CostArgs if not provided
            if isempty(Args.CostArgs)
                X = Obj.SourceData.getCol('X');
                Y = Obj.SourceData.getCol('Y');
                ExpTime_eff = Obj.ExpTime / Obj.NCoadd;
                CostArgs = {'WeightMatrix', Obj.SpecData.Spec', 'TransmissionMode', true, ...
                            'CalibWavelength', Obj.SpecData.SpecWvl, 'ExpTime', ExpTime_eff, ...
                            'Aperture_area_m2', Obj.Aperture, 'X', X, 'Y', Y};
            else
                CostArgs = Args.CostArgs;
            end

            [~, ~, PredictedFlux] = Obj.TransModel.costFun(Obj.TransWvl, Flux, CostArgs{:});
        end

        function ParamsInfo = getMCMCParamsInfo(Obj, Args)
            % Get parameter information for MCMC sampling
            % Description: Extracts parameters that were fitted in ANY optimization
            %              stage (not just current FitPar flags). This ensures MCMC
            %              samples all physically relevant parameters.
            % Input  : - PhotCalibTrans object (must be calibrated)
            %          * ...,key,val,...
            %            'IncludeTran2D' - Include position coefficients. Default is false.
            %            'PosBounds' - Bounds for position coefficients [min, max].
            %                   Default is [-10, 10].
            % Output : - Structure with fields:
            %                   .Names - Cell array of parameter names
            %                   .Values - Current parameter values [N x 1]
            %                   .Min - Lower bounds [N x 1]
            %                   .Max - Upper bounds [N x 1]
            %                   .NumTrans - Number of transmission parameters
            %                   .NumPos - Number of position parameters
            %                   .TransIndices - Indices into Funs structure (for setFreeParamVector)
            %                   .WasFitted - Logical array indicating which were fitted
            % Author : D. Kovaleva (Jan 2026)
            % Example: Info = PC.getMCMCParamsInfo('IncludeTran2D', true);

            arguments
                Obj
                Args.IncludeTran2D logical = false
                Args.PosBounds = [-10, 10]
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:getMCMCParamsInfo:NoModel', ...
                    'TransModel is empty. Run calibration first.');
            end

            OptSeq = Obj.TransModel.OptSeq;

            % Get all parameters via getAllFunPar (consistent with optimization code)
            AllFunPar = Obj.TransModel.getAllFunPar();
            NumAllParams = length(AllFunPar.Val);

            % Collect parameters that were fitted in ANY stage
            Names = {};
            Values = [];
            MinVals = [];
            MaxVals = [];
            TransIndices = [];  % Global indices for setFreeParamVector
            WasFitted = [];

            if ~isempty(OptSeq)
                % Use OptSeq to determine fitted parameters
                % Look up parameters by name directly (same approach as optimization code)
                FittedParamNames = {};

                for IStage = 1:length(OptSeq)
                    Stage = OptSeq(IStage);
                    if ~isempty(Stage.FreeParams)
                        for IFree = 1:length(Stage.FreeParams)
                            ParamName = Stage.FreeParams(IFree).Parameter;
                            % Add to list if not already present
                            if ~any(strcmp(FittedParamNames, ParamName))
                                FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                            end
                        end
                    end
                end

                % Now find each fitted parameter in AllFunPar by name
                for IParam = 1:length(FittedParamNames)
                    ParamName = FittedParamNames{IParam};
                    % Look up parameter by name (same as CompositeFun.runTransmissionOptimization)
                    Idx = find(strcmp(AllFunPar.Name, ParamName), 1);
                    if ~isempty(Idx)
                        Names{end+1} = ParamName; %#ok<AGROW>
                        Values(end+1) = AllFunPar.Val(Idx); %#ok<AGROW>
                        MinVals(end+1) = AllFunPar.Min(Idx); %#ok<AGROW>
                        MaxVals(end+1) = AllFunPar.Max(Idx); %#ok<AGROW>
                        TransIndices(end+1) = Idx; %#ok<AGROW>
                        WasFitted(end+1) = true; %#ok<AGROW>
                    end
                end
            else
                % No OptSeq, use FitPar flags (fallback)
                for Idx = 1:NumAllParams
                    if AllFunPar.FitPar(Idx)
                        Names{end+1} = AllFunPar.Name{Idx}; %#ok<AGROW>
                        Values(end+1) = AllFunPar.Val(Idx); %#ok<AGROW>
                        MinVals(end+1) = AllFunPar.Min(Idx); %#ok<AGROW>
                        MaxVals(end+1) = AllFunPar.Max(Idx); %#ok<AGROW>
                        TransIndices(end+1) = Idx; %#ok<AGROW>
                        WasFitted(end+1) = true; %#ok<AGROW>
                    end
                end
            end

            NumTrans = length(Names);

            % Add position parameters if requested
            NumPos = 0;
            if Args.IncludeTran2D && Obj.TransModel.UseTran2D && ~isempty(Obj.TransModel.Tran2DObj)
                ParX = Obj.TransModel.Tran2DObj.ParX;
                NCoeff = length(ParX);

                for ICoeff = 1:NCoeff
                    Names{end+1} = sprintf('PosCoeff_%d', ICoeff);  %#ok<AGROW>
                    Values(end+1) = ParX(ICoeff);  %#ok<AGROW>
                    MinVals(end+1) = Args.PosBounds(1);  %#ok<AGROW>
                    MaxVals(end+1) = Args.PosBounds(2);  %#ok<AGROW>
                    % Position parameter indices are offset from transmission parameters
                    TransIndices(end+1) = NumAllParams + ICoeff;  %#ok<AGROW>
                    WasFitted(end+1) = true;  %#ok<AGROW>
                end
                NumPos = NCoeff;
            end

            % Build output structure
            ParamsInfo.Names = Names(:);
            ParamsInfo.Values = Values(:);
            ParamsInfo.Min = MinVals(:);
            ParamsInfo.Max = MaxVals(:);
            ParamsInfo.NumTrans = NumTrans;
            ParamsInfo.NumPos = NumPos;
            ParamsInfo.TransIndices = TransIndices(:);
            ParamsInfo.WasFitted = WasFitted(:);
        end

        function MagErr = propagateCalibratorMagErr(Obj, Flux, FluxErrVector, Args)
            % Propagate calibrator spectral and flux errors into per-star magnitude uncertainties
            % Description: Combines Gaia XP spectral errors (through reference
            %              transmission) and observed flux errors into a single
            %              MagErr vector, used as weights in the cost function
            %              during optimization. Called once before fitting to
            %              avoid repeated error propagation.
            % Input  : - PhotCalibTrans object (must have SpecData populated)
            %          - Observed flux values [photons] [N_calib x 1]
            %          - Relative flux errors [N_calib x 1] (can be [])
            %          * ...,key,val,...
            %            'WeightingMode' - Error sources to include:
            %                   'spectral' - Gaia XP spectral errors only (default)
            %                   'flux'     - Observed flux errors only
            %                   'combined' - Quadrature sum of both
            %                   'none'     - No weighting (returns [])
            %            'ExpTime' - Effective exposure time [s]. Default uses Obj.ExpTime/Obj.NCoadd.
            %            'RefTransmissionFun' - Function handle for reference transmission.
            %                   Default is @telescope.optics.refTransmissionLAST.
            %            'FluxErrorNorm' - Effective area scaling for synthetic flux
            %                   in error calculation [dimensionless]. Default is 0.5.
            % Output : - Per-calibrator magnitude uncertainties [N_calib x 1],
            %                     or [] if WeightingMode is 'none'
            % Author : D. Kovaleva (Jan 2026)
            % Example: MagErr = PC.propagateCalibratorMagErr(Flux, FluxErrVector, 'WeightingMode', 'spectral');

            arguments
                Obj
                Flux
                FluxErrVector = []
                Args.WeightingMode = 'spectral'
                Args.ExpTime = []
                Args.RefTransmissionFun = @telescope.optics.refTransmissionLAST
                Args.FluxErrorNorm = 0.5
            end

            % Get effective exposure time
            if isempty(Args.ExpTime)
                ExpTime_eff = Obj.ExpTime / Obj.NCoadd;
            else
                ExpTime_eff = Args.ExpTime;
            end

            % Ensure column vectors
            Flux = Flux(:);
            N_calib = length(Flux);

            % Initialize output
            MagErr = zeros(N_calib, 1);

            % Check weighting mode
            UseSpectralWeighting = ismember(lower(Args.WeightingMode), {'spectral', 'combined'});
            UseFluxWeighting = ismember(lower(Args.WeightingMode), {'flux', 'combined'});

            if ~UseSpectralWeighting && ~UseFluxWeighting
                % No weighting, return empty
                MagErr = [];
                return;
            end

            % Constants
            H = 6.62607015e-34;  % Planck constant [J*s]
            C = constant.c('SI');  % Speed of light [m/s]
            B = H * C * 1e10;  % H*C with Angstrom to m conversion

            % Get wavelength grid from TransModel or use default
            if ~isempty(Obj.TransWvl)
                SpecWvl_Integration = Obj.TransWvl(:);
            else
                SpecWvl_Integration = (3000:20:11000)';  % Default LAST grid
            end
            SpecWvl_nm = SpecWvl_Integration / 10;  % Convert to nm

            % Compute dLambda for each wavelength bin
            dLambda = diff(SpecWvl_Integration(:));
            dLambda = [dLambda(1); (dLambda(1:end-1) + dLambda(2:end)) / 2; dLambda(end)];

            % Get reference transmission (for error propagation)
            T_ref_vec = Args.RefTransmissionFun(SpecWvl_Integration);  % [N_wvl x 1]

            % Scaling factor 
            NSigma = 3;

            MagErr_spectral = [];
            MagErr_flux = [];

            % Spectral error propagation
            if UseSpectralWeighting && ~isempty(Obj.SpecData) && ~isempty(Obj.SpecData.SpecErr)
                SpecErrMatrix = Obj.SpecData.SpecErr';  % [N_wvl x N_calib]
                SpecWvl = Obj.SpecData.SpecWvl(:);

                % Interpolate spectral errors onto integration grid (same as costFun)
                N_integration = length(SpecWvl_Integration);
                SpecWvl_min = min(SpecWvl);
                SpecWvl_max = max(SpecWvl);

                MaskGaia = (SpecWvl_Integration >= SpecWvl_min) & (SpecWvl_Integration <= SpecWvl_max);
                MaskUV = (SpecWvl_Integration <= SpecWvl_min);
                MaskIR = (SpecWvl_Integration >= SpecWvl_max);

                SpecErrInterp = zeros(N_integration, N_calib);
                WvlGaiaRegion = SpecWvl_Integration(MaskGaia);

                for IObs = 1:N_calib
                    SpecErrInterp(MaskGaia, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), WvlGaiaRegion, 'linear');
                    if any(MaskUV)
                        SpecErrInterp(MaskUV, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), SpecWvl_min, 'linear');
                    end
                    if any(MaskIR)
                        SpecErrInterp(MaskIR, IObs) = interp1(SpecWvl, SpecErrMatrix(:, IObs), SpecWvl_max, 'linear');
                    end
                end

                % Error propagation: sigma_Spec * T_ref * Lambda
                T_ref = repmat(T_ref_vec(:), 1, N_calib);
                TransmittedSpectraErr = SpecErrInterp .* T_ref;  % [N_wvl x N_calib]
                TransmittedSpectraErrT = TransmittedSpectraErr';  % [N_calib x N_wvl]
                ErrIntegrand = TransmittedSpectraErrT .* SpecWvl_nm(:)';  % [N_calib x N_wvl]

                % Quadrature sum (scaled by FluxErrorNorm to match model normalization)
                Dt = ExpTime_eff;
                Ageom = Obj.Aperture;
                PredictedFlux_err = Args.FluxErrorNorm * Dt * Ageom * sqrt(sum((NSigma * ErrIntegrand .* dLambda(:)').^2, 2)) / B;
              
                % Convert to magnitude error
                MagErr_spectral = 2.5 * log10(1 + PredictedFlux_err ./ Flux);
                MagErr_spectral(isinf(MagErr_spectral)) = 100;
                MagErr_spectral(isnan(MagErr_spectral)) = 100;
            end

            % Flux error propagation (flat spectrum)
            if UseFluxWeighting && ~isempty(FluxErrVector)
                FluxErrVector = FluxErrVector(:);
                if length(FluxErrVector) == N_calib
                    % Compute bandpass factor
                    T_lambda_dlambda = T_ref_vec(:) .* SpecWvl_nm(:) .* dLambda(:);
                    BandpassNorm = sum(T_lambda_dlambda);
                    BandpassQuad = sqrt(sum(T_lambda_dlambda.^2));
                    BandpassFactor = BandpassQuad / BandpassNorm;

                    % Propagated error
                    FluxErrPropagated = NSigma * FluxErrVector .* BandpassFactor;
                    MagErr_flux = 2.5 * log10(1 + FluxErrPropagated);
                    MagErr_flux(isinf(MagErr_flux)) = 100;
                    MagErr_flux(isnan(MagErr_flux)) = 100;
                    MagErr_flux(MagErr_flux <= 0) = 100;
                end
            end

            % Combine errors based on weighting mode
            if ~isempty(MagErr_spectral) && ~isempty(MagErr_flux)
                MagErr = sqrt(MagErr_spectral.^2 + MagErr_flux.^2);
            elseif ~isempty(MagErr_spectral)
                MagErr = MagErr_spectral;
            elseif ~isempty(MagErr_flux)
                MagErr = MagErr_flux;
            else
                MagErr = [];
            end
        end

        function SpecFluxMatrix = resampleCalibratorSpectra(Obj, Args)
            % Resample calibrator reference spectra onto the transmission model wavelength grid
            % Description: Interpolates Gaia XP spectra within their native range
            %              (3360-10200 Angstrom) and extrapolates with constant
            %              boundary values outside. Called once before fitting to
            %              avoid repeated interpolation in costFun.
            % Input  : - PhotCalibTrans object (must have SpecData populated)
            %          * ...,key,val,...
            %            'TransWvl' - Transmission wavelength grid [Angstrom]. Default uses Obj.TransWvl.
            % Output : - Resampled spectra [N_TransWvl x N_calib]
            %                             on the transmission model wavelength grid
            % Author : D. Kovaleva (Feb 2026)
            % Example: SpecFluxMatrix = PC.resampleCalibratorSpectra();
            %          % Pass to costFun via CostArgs:
            %          CostArgs = {..., 'PrecomputedSpecFluxMatrix', SpecFluxMatrix, ...};

            arguments
                Obj
                Args.TransWvl = []
            end

            % Get transmission wavelength grid
            if isempty(Args.TransWvl)
                TransWvl = Obj.TransWvl(:);
            else
                TransWvl = Args.TransWvl(:);
            end

            % Check that SpecData is populated
            if isempty(Obj.SpecData) || isempty(Obj.SpecData.Spec)
                Obj.msgLog(LogLevel.Error, 'resampleCalibratorSpectra: SpecData.Spec is empty - run selectCalibrators first');
                SpecFluxMatrix = [];
                return;
            end

            % Get calibrator spectra and wavelength grid
            Spec = Obj.SpecData.Spec';  % [N_SpecWvl x N_calib] (transpose from [N_calib x N_SpecWvl])
            SpecWvl = Obj.SpecData.SpecWvl(:);

            Ninput = length(TransWvl);
            NCalib = size(Spec, 2);

            % Calibrator spectral boundaries (e.g., Gaia XP: 3360-10200 Angstrom)
            SpecWvlMin = min(SpecWvl);
            SpecWvlMax = max(SpecWvl);

            % Wavelength region masks for extrapolation
            MaskGaia = (TransWvl >= SpecWvlMin) & (TransWvl <= SpecWvlMax);
            MaskUV = (TransWvl < SpecWvlMin);
            MaskIR = (TransWvl > SpecWvlMax);
            WvlGaiaRegion = TransWvl(MaskGaia);

            % Interpolate calibrator spectra onto transmission grid (vectorized)
            SpecFluxMatrix = zeros(Ninput, NCalib);
            SpecFluxMatrix(MaskGaia, :) = interp1(SpecWvl, Spec, WvlGaiaRegion, 'linear');

            % UV/IR extrapolation: constant boundary values
            if any(MaskUV)
                EdgeValuesUV = interp1(SpecWvl, Spec, SpecWvlMin, 'linear');
                SpecFluxMatrix(MaskUV, :) = repmat(EdgeValuesUV, sum(MaskUV), 1);
            end
            if any(MaskIR)
                EdgeValuesIR = interp1(SpecWvl, Spec, SpecWvlMax, 'linear');
                SpecFluxMatrix(MaskIR, :) = repmat(EdgeValuesIR, sum(MaskIR), 1);
            end
        end
    end


    methods % Header I/O methods
        function HeaderObj = photCalibTransToHeader(Obj, HeaderObj, Args)
            % Write calibration data to AstroHeader
            % Input  : - PhotCalibTrans object
            %          - AstroHeader object
            %          * ...,key,val,...
            %            'WriteComments' - Add explanatory comments to keywords. Default is false.
            % Output : - Updated AstroHeader object with PT_* keywords
            % Author : D. Kovaleva (Jan 2026)
            % Example: Header = PC.photCalibTransToHeader(Header);
            %          Header = PC.photCalibTransToHeader(Header, 'WriteComments', true);
            % Description: Writes calibration results and fitted parameters to header.
            %              Keywords: PT_RMS, PT_CHI2, PT_DOF, PT_NCALIB, PT_SUCC,
            %                        PT_AREF, PT_SPEC,
            %                        PT_X_N, PT_X_VY, PT_X_FY (function parameters),
            %                        PT_P_N, PT_P_VY, PT_P_FY (position corrections if UseTran2D=true)

            arguments
                Obj
                HeaderObj
                Args.WriteComments logical = false
            end

            % Preallocate history comments array if requested
            if Args.WriteComments
                % Estimate max size: 8 (general) + 10*11 (functions) + 101 (position) = ~220
                HistoryComments = cell(1, 300);
                IComment = 0;
            end

            % Remove all existing PT_* keywords to ensure clean ordering
            HeaderObj = HeaderObj.deleteKey({'PT_.*'});

            % General results
            HeaderObj = HeaderObj.replaceVal('PT_RMS', Obj.TransModel.RMS);
            HeaderObj = HeaderObj.replaceVal('PT_CHI2', Obj.TransModel.Chi2);
            HeaderObj = HeaderObj.replaceVal('PT_DOF', Obj.TransModel.DOF);
            % Use final calibrator count (after sigma clipping) from last stage
            if ~isempty(Obj.FitResults)
                if numel(Obj.FitResults) > 1
                    NCalFinal = Obj.FitResults(end).NCalUsed;
                else
                    NCalFinal = Obj.FitResults.NCalUsed;
                end
            else
                NCalFinal = size(Obj.SpecData.Spec, 1);  % Fallback to initial
            end
            HeaderObj = HeaderObj.replaceVal('PT_NCALIB', NCalFinal);
            HeaderObj = HeaderObj.replaceVal('PT_SUCC', Obj.Success);
            HeaderObj = HeaderObj.replaceVal('PT_AREF', 'SMART v2.9.8');
            HeaderObj = HeaderObj.replaceVal('PT_SPEC', 'GaiaDR3');

            if Args.WriteComments
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_RMS: RMS of calibration fit [mag]';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_CHI2: Chi-squared of fit';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_DOF: Degrees of freedom';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_NCALIB: Number of calibrators';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_SUCC: Calibration success flag';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_AREF: Atmospheric model reference';
                IComment = IComment + 1; HistoryComments{IComment} = 'PT_SPEC: Spectra reference';
            end

            % Function parameters
            Funs = Obj.TransModel.Funs;
            NFuns = length(Funs);

            % Pre-compute fitted parameter names from OptSeq 
            FittedParamNames = {};
            if ~isempty(Obj.TransModel.OptSeq)
                for IStage = 1:length(Obj.TransModel.OptSeq)
                    Stage = Obj.TransModel.OptSeq(IStage);
                    if ~isempty(Stage.FreeParams)
                        for IFree = 1:length(Stage.FreeParams)
                            ParamName = Stage.FreeParams(IFree).Parameter;
                            if ~any(strcmp(FittedParamNames, ParamName))
                                FittedParamNames{end+1} = ParamName; %#ok<AGROW>
                            end
                        end
                    end
                end
            end

            for IFun = 1:NFuns
                Fun = Funs(IFun);

                % Function reference
                if IFun == 1 && strcmp(Fun.Desc, 'Normalization')
                    FunRef = '@(Lambda,Par)Par';
                else
                    FunRef = func2str(Fun.Handle);
                end
                KeyName = sprintf('PT_%d_N', IFun);
                HeaderObj = HeaderObj.replaceVal(KeyName, FunRef);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = sprintf('%s: %s function', KeyName, Fun.Desc);
                end

                % Parameters
                NPar = length(Fun.Par);
                for IPar = 1:NPar
                    % Value
                    KeyName = sprintf('PT_%d_V%d', IFun, IPar);
                    HeaderObj = HeaderObj.replaceVal(KeyName, Fun.Par(IPar));

                    % Get parameter name from ArgNames if available
                    if ~isempty(Fun.ArgNames) && IPar <= length(Fun.ArgNames)
                        ParName = Fun.ArgNames(IPar).Description;
                    else
                        ParName = sprintf('%s_Par%d', Fun.Desc, IPar);
                    end

                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: %s [%s]', KeyName, Fun.Desc, ParName);
                    end

                    % Fit flag - check if parameter name is in fitted list
                    KeyName = sprintf('PT_%d_F%d', IFun, IPar);

                    % Determine if parameter was ever freed during optimization
                    if ~isempty(Obj.TransModel.OptSeq)
                        % Check if parameter name is in the fitted list
                        WasFitted = any(strcmp(FittedParamNames, ParName));
                    else
                        % No OptSeq defined, use initial FitPar configuration
                        WasFitted = Fun.FitPar(IPar);
                    end

                    FitFlag = double(WasFitted);
                    HeaderObj = HeaderObj.replaceVal(KeyName, FitFlag);
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Fit flag (1=fitted in any stage, 0=always fixed)', KeyName);
                    end
                end
            end

            % Position-dependent corrections (only if UseTran2D = true)
            if Obj.TransModel.UseTran2D
                % Type
                HeaderObj = HeaderObj.replaceVal('PT_P_N', Obj.TransModel.NameTran2D);
                if Args.WriteComments
                    IComment = IComment + 1;
                    HistoryComments{IComment} = 'PT_P_N: Position correction type';
                end

                % Coefficients
                ParX = Obj.TransModel.Tran2DObj.ParX;
                NCoeff = length(ParX);

                for ICoeff = 1:NCoeff
                    % Value
                    KeyName = sprintf('PT_P_V%d', ICoeff);
                    HeaderObj = HeaderObj.replaceVal(KeyName, ParX(ICoeff));
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Coefficient %d of position-dependent correction', KeyName, ICoeff);
                    end

                    % Fit flag (all coefficients of position-dependent correction are fitted if UseTran2D=true)
                    KeyName = sprintf('PT_P_F%d', ICoeff);
                    HeaderObj = HeaderObj.replaceVal(KeyName, 1);
                    if Args.WriteComments
                        IComment = IComment + 1;
                        HistoryComments{IComment} = sprintf('%s: Fit flag (1=fitted, 0=fixed)', KeyName);
                    end
                end
            end

            % Write HISTORY comments at the end if requested
            if Args.WriteComments
                % Trim to actual size
                HistoryComments = HistoryComments(1:IComment);
                for I = 1:IComment
                    HeaderObj = HeaderObj.insertKey({'HISTORY', HistoryComments{I}}, Inf);
                end
            end
        end

        function Obj = photCalibTransFromHeader(Obj, HeaderObj, Args)
            % Populate PhotCalibTrans object from AstroHeader
            % Input  : - PhotCalibTrans object (existing)
            %          - HeaderObj - AstroHeader object with PT_* keywords
            %          * ...,key,val,...
            % Output : - PhotCalibTrans object populated from header
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.photCalibTransFromHeader(Header);
            % Description: Reads calibration results and fitted parameters from header.
            %              Populates existing PhotCalibTrans object with stored data.

            arguments
                Obj
                HeaderObj
                Args.Dummy = []  % Reserved for future arguments
            end

            % Create TransModel if empty
            if isempty(Obj.TransModel)
                Obj.TransModel = tools.math.fun.CompositeFun();
            end

            % General results
            if HeaderObj.isKeyExist('PT_RMS')
                Obj.TransModel.RMS = HeaderObj.getVal('PT_RMS');
            end
            if HeaderObj.isKeyExist('PT_CHI2')
                Obj.TransModel.Chi2 = HeaderObj.getVal('PT_CHI2');
            end
            if HeaderObj.isKeyExist('PT_DOF')
                Obj.TransModel.DOF = HeaderObj.getVal('PT_DOF');
            end
            if HeaderObj.isKeyExist('PT_SUCC')
                Obj.Success = HeaderObj.getVal('PT_SUCC');
            end

            % Observation metadata - read from standard FITS keywords if available
            % Override defaults only if value exists and is not NaN
            if HeaderObj.isKeyExist('AIRMASS')
                Val = HeaderObj.getVal('AIRMASS');
                if ~isnan(Val)
                    Obj.AirMass = Val;
                end
            end

            if HeaderObj.isKeyExist('MNTTEMP')
                Val = HeaderObj.getVal('MNTTEMP');
                if ~isnan(Val)
                    Obj.Temp = Val;
                end
            end

            if HeaderObj.isKeyExist('PRESSURE')
                Val = HeaderObj.getVal('PRESSURE');
                if ~isnan(Val)
                    Obj.Pressure = Val;
                end
            end

            if HeaderObj.isKeyExist('HUMIDITY')
                Val = HeaderObj.getVal('HUMIDITY');
                if ~isnan(Val)
                    Obj.Humidity = Val;
                end
            end

            if HeaderObj.isKeyExist('APERTURE')
                Val = HeaderObj.getVal('APERTURE');
                if ~isnan(Val)
                    Obj.Aperture = Val;
                end
            end

            if HeaderObj.isKeyExist('EXPTIME')
                Val = HeaderObj.getVal('EXPTIME');
                if ~isnan(Val)
                    Obj.ExpTime = Val;
                end
            end

            if HeaderObj.isKeyExist('NCOADD')
                Val = HeaderObj.getVal('NCOADD');
                if ~isnan(Val)
                    Obj.NCoadd = Val;
                end
            end

            % Function parameters - read function list
            IFun = 1;
            while true
                KeyName = sprintf('PT_%d_N', IFun);
                if ~HeaderObj.isKeyExist(KeyName)
                    break;
                end

                FunRef = HeaderObj.getVal(KeyName);

                % Initialize function entry
                Obj.TransModel.Funs(IFun).Name = IFun;
                Obj.TransModel.Funs(IFun).Desc = '';
                Obj.TransModel.Funs(IFun).Handle = str2func(FunRef);
                Obj.TransModel.Funs(IFun).Par = [];
                Obj.TransModel.Funs(IFun).FitPar = [];
                Obj.TransModel.Funs(IFun).OptionalArgs = {};
                Obj.TransModel.Funs(IFun).ArgNames = [];
                Obj.TransModel.Funs(IFun).ArgMapping = [];
                Obj.TransModel.Funs(IFun).PreCalc = [];

                % Read parameters (build arrays from scratch)
                % Preallocate for max expected parameters per function (e.g., 20)
                ParValues = zeros(1, 20);
                FitFlags = false(1, 20);
                IPar = 1;
                while true
                    KeyNameV = sprintf('PT_%d_V%d', IFun, IPar);
                    if ~HeaderObj.isKeyExist(KeyNameV)
                        break;
                    end

                    % Read parameter value
                    ParValues(IPar) = HeaderObj.getVal(KeyNameV);

                    % Read fit flag
                    KeyNameF = sprintf('PT_%d_F%d', IFun, IPar);
                    if HeaderObj.isKeyExist(KeyNameF)
                        FitFlags(IPar) = logical(HeaderObj.getVal(KeyNameF));
                    else
                        FitFlags(IPar) = false;  % Default to fixed if not specified
                    end

                    IPar = IPar + 1;
                end

                % Trim to actual size and store parameters and fit flags
                if IPar > 1
                    Obj.TransModel.Funs(IFun).Par = ParValues(1:IPar-1);
                    Obj.TransModel.Funs(IFun).FitPar = FitFlags(1:IPar-1);
                else
                    Obj.TransModel.Funs(IFun).Par = [];
                    Obj.TransModel.Funs(IFun).FitPar = [];
                end

                IFun = IFun + 1;
            end

            % Position-dependent corrections
            if HeaderObj.isKeyExist('PT_P_N')
                Tran2DType = HeaderObj.getVal('PT_P_N');
                if ~isempty(Tran2DType)
                    Obj.TransModel.UseTran2D = true;
                    Obj.TransModel.NameTran2D = Tran2DType;

                    % Create Tran2D object
                    Obj.TransModel.Tran2DObj = Tran2D(Tran2DType);

                    % Read coefficients
                    ICoeff = 1;
                    % Preallocate for max expected coefficients (e.g., 100)
                    ParX = zeros(1, 100);
                    while true
                        KeyName = sprintf('PT_P_V%d', ICoeff);
                        if ~HeaderObj.isKeyExist(KeyName)
                            break;
                        end
                        ParX(ICoeff) = HeaderObj.getVal(KeyName);
                        ICoeff = ICoeff + 1;
                    end
                    % Trim to actual size
                    if ICoeff > 1
                        ParX = ParX(1:ICoeff-1);
                    else
                        ParX = [];
                    end

                    % Set coefficients
                    if ~isempty(ParX)
                        Obj.TransModel.Tran2DObj.ParX = ParX;
                    end
                else
                    Obj.TransModel.UseTran2D = false;
                end
            else
                Obj.TransModel.UseTran2D = false;
            end

            % Set CalFound based on PT_NCALIB
            if HeaderObj.isKeyExist('PT_NCALIB')
                Val = HeaderObj.getVal('PT_NCALIB');
                if ~isnan(Val) && Val > 0
                    Obj.CalFound = true;
                else
                    Obj.CalFound = false;
                end
            end
        end
    end

    methods % Catalog operations
        function CatObj = addMag(Obj, CatObj, Args)
            % Add calibrated magnitude columns to catalog
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object with flux measurements.
            %          * ...,key,val,...
            %            'FluxColNames' - Flux column names to calibrate.
            %                             Default is all FLUX_* columns.
            %            'ApplyPosCorrection' - Apply position-dependent
            %                                   corrections. Default is true.
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            %            'AddMagErr' - Add magnitude error columns. Default is true.
            %                         Error formula: MagErr = 1.086 * FluxErr / Flux.
            %                         Requires FLUXERR_<suffix> columns in catalog.
            %                         Column naming: MAG_<System>_<suffix>_ERR.
            %            'PropagateCalibratedErr' - Propagate calibrated magnitude
            %                         errors. Default is false. Not yet implemented.
            % Output : - AstroCatalog with added calibrated magnitude columns.
            %                     Column naming: FLUX_<suffix> -> MAG_<System>_<suffix>
            %                     (e.g., FLUX_APER_3 -> MAG_AB_APER_3)
            %                     If AddMagErr=true, also: MAG_<System>_<suffix>_ERR
            %                     (e.g., MAG_AB_APER_3_ERR)
            % Author : D. Kovaleva (Jan 2026)
            % Example: Cat = PC.addMag(Cat);
            %          Cat = PC.addMag(Cat, 'FluxColNames', {'FLUX_APER_3', 'FLUX_PSF'});
            %          Cat = PC.addMag(Cat, 'MagSystem', 'AB');
            %          Cat = PC.addMag(Cat, 'AddMagErr', false);
            % Description: Creates new columns with calibrated magnitudes from flux measurements.
            %              Formula: MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP
            %              For each FLUX_<suffix> column, creates MAG_<System>_<suffix> column.
            %              If AddMagErr=true, also creates MAG_<System>_<suffix>_ERR column
            %              with error = 1.086 * FLUXERR_<suffix> / FLUX_<suffix>.
            %              Preserves original flux columns.
            %              Applies position-dependent corrections if available.

            arguments
                Obj
                CatObj
                Args.FluxColNames = []
                Args.ApplyPosCorrection logical = true
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
                Args.AddMagErr logical = true  % Add magnitude error columns
                Args.AddZP logical = false  % Also insert ZP column (avoids recomputing)
                Args.PropagateCalibratedErr logical = false  % Propagate calibrated errors (placeholder)
            end

            % Vega magnitude system placeholder — not yet implemented
            if strcmpi(Args.MagSystem, 'Vega')
                error('PhotCalibTrans:addMag:VegaNotImplemented', ...
                      'Vega magnitude system is not yet implemented.');
            end

            % Build dynamic column prefix: MAG_AB_ or MAG_VEGA_
            MagPrefix = ['MAG_', Args.MagSystem, '_'];

            % Get catalog table
            Tab = CatObj.Table;

            if isempty(Tab) || height(Tab) == 0
                warning('PhotCalibTrans:addMag:EmptyCatalog', 'Catalog is empty. No columns added.');
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
            end

            if isempty(FluxColNames)
                warning('PhotCalibTrans:addMag:NoFluxCols', 'No FLUX_* columns found in catalog.');
                return;
            end

            % Extract X, Y coordinates if position corrections requested
            X = [];
            Y = [];
            if Args.ApplyPosCorrection
                if ismember('X', AllColNames) && ismember('Y', AllColNames)
                    X = Tab.X;
                    Y = Tab.Y;
                else
                    warning('PhotCalibTrans:addMag:NoCoords', ...
                            'X, Y columns not found. Position corrections disabled.');
                end
            end

            % Compute ZP once for all flux columns
            Nrows = height(Tab);
            ZP = nan(Nrows, 1);
            ExpTime_eff = Obj.ExpTime / Obj.NCoadd;
            ValidPosMask = true(Nrows, 1);
            if ~isempty(X)
                InvalidPos = isnan(X) | isinf(X) | isnan(Y) | isinf(Y);
                if any(InvalidPos)
                    Obj.msgLog(LogLevel.Debug, 'addMag: Position validation: %d/%d sources have invalid X/Y - magnitude and ZP will be NaN', ...
                        sum(InvalidPos), Nrows);
                    ValidPosMask = ~InvalidPos;
                end
            end

            % Extract per-source zenith angles if per-source airmass was used
            PerSourceZenithAngles = [];
            if Obj.PerSourceAirmass && ~isempty(Obj.AirmassColName) && ...
               ismember(Obj.AirmassColName, AllColNames)
                Airmass = Tab.(Obj.AirmassColName);
                ValidAM = Airmass >= 1 & isfinite(Airmass);
                ValidPosMask = ValidPosMask & ValidAM;
                PerSourceZenithAngles = nan(Nrows, 1);
                PerSourceZenithAngles(ValidAM) = acosd(1 ./ Airmass(ValidAM));
            end

            if any(ValidPosMask)
                ZPArgs = {'MagSystem', Args.MagSystem};
                if ~isempty(X)
                    ZPArgs = [ZPArgs, 'X', X(ValidPosMask), 'Y', Y(ValidPosMask)];
                end
                if ~isempty(PerSourceZenithAngles)
                    ZPArgs = [ZPArgs, 'PerSourceZenithAngles', PerSourceZenithAngles(ValidPosMask)];
                end
                ZP_valid = Obj.evaluateZP(ZPArgs{:});
                ZP(ValidPosMask) = ZP_valid(:);
            end

            % Insert ZP column if requested
            if Args.AddZP
                ZPColName = [Args.MagSystem, '_ZP'];
                CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
            end

            % Process each flux column
            for I = 1:length(FluxColNames)
                FluxColName = FluxColNames{I};

                % Get flux values [photons]
                Flux = Tab.(FluxColName);

                % Calibrated magnitude using pre-computed ZP
                % MAG = -2.5*log10(FLUX/ExpTime_eff) + ZP  (via luptitude)
                Mag = convert.luptitude(Flux/ExpTime_eff, 10.^(0.4.*ZP));

                % Create new calibrated magnitude column name
                % e.g., FLUX_APER_3 -> MAG_AB_APER_3
                NewMagColName = strrep(FluxColName, 'FLUX_', MagPrefix);

                % Insert magnitude column into catalog
                CatObj = CatObj.insertCol(Mag, Inf, {NewMagColName});

                % Add magnitude error column if requested
                if Args.AddMagErr
                    % Derive corresponding flux error column name
                    % e.g., FLUX_APER_3 -> FLUXERR_APER_3
                    FluxErrColName = strrep(FluxColName, 'FLUX_', 'FLUXERR_');
                    MagErrColName = [NewMagColName, '_ERR'];

                    if ismember(FluxErrColName, AllColNames)
                        FluxErr = Tab.(FluxErrColName);
                        % MagErr = 1.086 * FluxErr / Flux  (first-order error propagation)
                        MagErr = nan(Nrows, 1);
                        ValidFlux = Flux > 0 & ~isnan(Flux) & ~isnan(FluxErr);
                        MagErr(ValidFlux) = 1.086 .* FluxErr(ValidFlux) ./ Flux(ValidFlux);
                        CatObj = CatObj.insertCol(MagErr, Inf, {MagErrColName});
                    else
                        % No flux error column found — insert NaN column
                        Obj.msgLog(LogLevel.Debug, ...
                            'addMag: Flux error column %s not found - %s set to NaN', ...
                            FluxErrColName, MagErrColName);
                        CatObj = CatObj.insertCol(nan(Nrows, 1), Inf, {MagErrColName});
                    end
                end

                % Propagate calibrated magnitude error if requested (placeholder)
                if Args.PropagateCalibratedErr
                    % TODO: call dedicated method for calibrated error propagation
                    error('PhotCalibTrans:addMag:PropagateCalibratedErrNotImplemented', ...
                          'Calibrated magnitude error propagation is not yet implemented.');
                end
            end

        end

        function CatObj = addZP(Obj, CatObj, Args)
            % Add position-dependent ZP column to catalog
            % Input  : - PhotCalibTrans object.
            %          - AstroCatalog object.
            %          * ...,key,val,...
            %            'MagSystem' - Magnitude system: 'AB' or 'Vega'.
            %                         Default is 'AB'. Vega is not yet implemented.
            % Output : - AstroCatalog with added ZP column
            %                     (AB_ZP or VEGA_ZP depending on MagSystem).
            % Author : D. Kovaleva (Jan 2026)
            % Example: Cat = PC.addZP(Cat);
            %          Cat = PC.addZP(Cat, 'MagSystem', 'AB');

            arguments
                Obj
                CatObj
                Args.MagSystem char = 'AB'  % 'AB' or 'Vega' (placeholder)
            end

            Tab = CatObj.Table;
            if isempty(Tab) || height(Tab) == 0
                Obj.msgLog(LogLevel.Debug, 'addZP: Catalog is empty. No columns added.');
                return;
            end

            Nrows = height(Tab);

            % Extract X, Y coordinates
            AllColNames = Tab.Properties.VariableNames;
            % Dynamic column name: AB_ZP or VEGA_ZP
            ZPColName = [Args.MagSystem, '_ZP'];

            if ~ismember('X', AllColNames) || ~ismember('Y', AllColNames)
                Obj.msgLog(LogLevel.Error, 'addZP: X, Y columns not found in catalog. ZP column set to NaN.');
                ZP = nan(Nrows, 1);
                CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
                return;
            end

            X = Tab.X(:);
            Y = Tab.Y(:);

            % Validate X, Y coordinates
            InvalidPos = isnan(X) | isinf(X) | isnan(Y) | isinf(Y);
            if any(InvalidPos)
                Obj.msgLog(LogLevel.Debug, 'addZP: Position validation: %d/%d sources have invalid X/Y - ZP set to NaN', ...
                    sum(InvalidPos), Nrows);
            end

            % Initialize ZP as NaN
            ZP = nan(Nrows, 1);

            % Extract per-source zenith angles if per-source airmass was used
            ValidMask = ~InvalidPos;
            PerSourceZenithAngles = [];
            AllColNames = Tab.Properties.VariableNames;
            if Obj.PerSourceAirmass && ~isempty(Obj.AirmassColName) && ...
               ismember(Obj.AirmassColName, AllColNames)
                Airmass = Tab.(Obj.AirmassColName);
                ValidAM = Airmass >= 1 & isfinite(Airmass);
                ValidMask = ValidMask & ValidAM;
                PerSourceZenithAngles = nan(Nrows, 1);
                PerSourceZenithAngles(ValidAM) = acosd(1 ./ Airmass(ValidAM));
            end

            % Evaluate ZP only for valid positions
            if any(ValidMask)
                ZPArgs = {'X', X(ValidMask), 'Y', Y(ValidMask), ...
                          'MagSystem', Args.MagSystem};
                if ~isempty(PerSourceZenithAngles)
                    ZPArgs = [ZPArgs, 'PerSourceZenithAngles', PerSourceZenithAngles(ValidMask)];
                end
                ZP_valid = Obj.evaluateZP(ZPArgs{:});
                ZP(ValidMask) = ZP_valid(:);
            end

            % Insert column
            CatObj = CatObj.insertCol(ZP, Inf, {ZPColName});
        end
    end

    methods % Display/Output methods
        function summary(Obj, Args)
            % Display photometric calibration summary
            % Input  : - PhotCalibTrans object
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
            fprintf('Success: %s\n', mat2str(Obj.Success));

            if ~isempty(Obj.SpecData)
                fprintf('Calibrators: %d (min required: %d)\n', size(Obj.SpecData.Spec, 1), Obj.NCalibMin);
            else
                fprintf('Calibrators: 0 (min required: %d)\n', Obj.NCalibMin);
            end

            if ~isempty(Obj.TransModel)
                fprintf('Transmission Model: Available\n');

                if ~isempty(Obj.TransModel.RMS)
                    fprintf('RMS: %.4f mag (max allowed: %.4f mag)\n', Obj.TransModel.RMS, Obj.RMSMax);
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
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotTransmission();
            % Description: Uses Obj.TransWvl (300:2:1100 nm, 401 points) for transmission evaluation.

            arguments
                Obj
                Args.NewFigure logical = true
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
            xlabel('Wavelength [Angstrom]');
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
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Type' - Plot type: 'magnitude' (residuals vs mag),
            %                     'spatial' (2D spatial distribution), 'both'. Default is 'both'.
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle or array of handles
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

            % Get residuals and calibrator data from SourceData
            % (Residuals and Used columns are added by calibrate() after fitting)
            Tab = Obj.SourceData.Table;
            ColNames = Tab.Properties.VariableNames;

            if ~ismember('Residuals', ColNames)
                error('PhotCalibTrans:plotResiduals:NoResiduals', ...
                      'No residuals available. Run calibrate() first.');
            end

            AllResiduals = Tab.Residuals;
            X_all = Tab.X;
            Y_all = Tab.Y;
            Flux_all = Tab.Flux;

            % Filter to used calibrators only (not sigma-clipped)
            if ismember('Used', ColNames)
                UsedMask = logical(Tab.Used);
            else
                UsedMask = true(size(AllResiduals));
            end

            Residuals = AllResiduals(UsedMask);
            X = X_all(UsedMask);
            Y = Y_all(UsedMask);
            Flux = Flux_all(UsedMask);
            MagInst = -2.5 * log10(Flux);  % Convert flux to instrumental magnitude

            % Determine what to plot
            switch lower(Args.Type)
                case 'magnitude'
                    Nplots = 1;
                case 'spatial'
                    Nplots = 1;
                case 'both'
                    Nplots = 2;
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
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'GridSize' - Grid resolution [Nx, Ny]. Default is [50, 50].
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
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
            if ~isempty(Obj.SourceData)
                hold on;
                plot(Obj.SourceData.getCol('X'), Obj.SourceData.getCol('Y'), 'w.', 'MarkerSize', 8);
            end
        end

        function Fig = plotCalibrators(Obj, Args)
            % Plot observed vs predicted magnitudes for calibrators
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
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
            Flux_obs = Obj.SourceData.getCol('Flux');
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
            Lims = [min([MagInst_pred; MagInst_obs]), max([MagInst_pred; MagInst_obs])];
            plot(Lims, Lims, 'k--', 'LineWidth', 2);

            % Add RMS error bands
            plot(Lims, Lims + Obj.TransModel.RMS, 'r--', 'LineWidth', 1);
            plot(Lims, Lims - Obj.TransModel.RMS, 'r--', 'LineWidth', 1);

            grid on;
            xlabel('Model Predicted Magnitude');
            ylabel('Observed Magnitude');
            axis equal tight;

            % Add statistics to title
            NumCalib = size(Obj.SpecData.Spec, 1);
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
            % Input  : - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotFitQuality();
            % Description: Shows convergence of fit across optimization stages.
            %              Displays RMS, Chi2/DOF evolution, and number of calibrators.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.FitResults)
                error('PhotCalibTrans:plotFitQuality:NoFitResults', ...
                      'Fit results not available. Run calibrate() first.');
            end

            FitRes = Obj.FitResults;
            Nstages = length(FitRes);

            % Extract metrics from each stage
            RMS_stages = zeros(Nstages, 1);
            Chi2_stages = zeros(Nstages, 1);
            DOF_stages = zeros(Nstages, 1);

            for I = 1:Nstages
                RMS_stages(I) = FitRes(I).RMS;
                if isfield(FitRes(I), 'Chi2')
                    Chi2_stages(I) = FitRes(I).Chi2;
                end
                if isfield(FitRes(I), 'DOF')
                    DOF_stages(I) = FitRes(I).DOF;
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
            title(sprintf('Fit Convergence (N=%d calibrators)', size(Obj.SpecData.Spec, 1)));
            xticks(1:Nstages);

            % Subplot 2: Chi2/DOF evolution
            subplot(2, 1, 2);
            if any(Chi2_stages ~= 0) && any(DOF_stages ~= 0)
                Chi2PerDOF = Chi2_stages ./ DOF_stages;
                plot(1:Nstages, Chi2PerDOF, 's-', 'LineWidth', 2, 'MarkerSize', 8);
                yline(1, 'r--', 'LineWidth', 1.5);  % Ideal Chi2/DOF = 1
                ylabel('Chi2/DOF');
                legend('Fit Quality', 'Location', 'best');
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
end
