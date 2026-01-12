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
    % Properties:
    %   TransModel - CompositeFun object with fitted transmission model and
    %   optimization sequence used for fitting
    %   SpecData   - Structure with reference spectral data (calibrator spectra)
    %   SourceData - AstroCatalog with observed calibrator sources (after calibration: Used, Residuals columns)
    %   CalFound   - Flag indicating whether calibrators were found (set by selectCalibrators)
    %   Success    - Flag indicating successful calibration (set by populateSuccess)
    %   AirMass, Zenith, ExpTime, NCoadd, Temp, Pressure, Humidity, Aperture - Observation metadata
    %
    % Example:
    %{
     % Create calibration object
     PC = PhotCalibTrans();

     % Perform calibration on AstroImage (metadata read from AI.HeaderData)
     PC.calibrate(AI);

     % Check calibration success
     PC = PC.populateSuccess();  % Evaluates success criteria and sets PC.Success flag
     if PC.Success
         fprintf('Calibration successful!\n');
     end

     % Evaluate transmission and zero points
     Trans = PC.evaluateTransmission();  % Use constant wavelength grid (Obj.TransWvl)
     ZP = PC.evaluateZP();  % Uses Obj.TransWvl, Obj.ExpTime, Obj.NCoadd, Obj.Aperture

     % Apply calibration to catalog
     [MagAB, MagABErr] = PC.evaluateMag(Flux, 'X', X, 'Y', Y, 'MagErr', MagErr);

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
    %     populateSuccess - Evaluate and set Success flag based on calibration quality criteria
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

        % Transmission model (empty until calibration)
        TransModel = []         % CompositeFun transmission model object containing:
                                %   Before calibration: .Funs (function list with initial parameters), .FunOperator ('*'),
                                %                        .Tran2DObj (position-dependent correction object), .UseTran2D (true/false)
                                %   After calibration:  .Funs.Par (fitted parameters), .RMS (fit RMS [mag]), .Chi2 (chi-squared), .DOF (degrees of freedom)

        % Calibration metadata (read from header, defaults for missing values)
        AirMass = 1.2           % Airmass
        Zenith = 30            % Zenith angle [deg], if provided, overrides AirMass
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

        SourceData = []         % AstroCatalog with observed calibrator sources from selectCalibrators:
                                %   Catalog table columns: Flux, FluxErr, X, Y, RA, Dec, MatchDistance, NumMatches
                                %   After calibration: Used (logical, non-clipped), Residuals (valid for Used)

        CalFound = false        % Flag indicating whether calibrators were found (set by selectCalibrators)

        % Success status
        Success = false         % Flag indicating successful calibration (set by populateSuccess)

    end

    properties (Constant, Hidden)
        % Wavelength grid for transmission evaluation (20 Angstrom step)
        TransWvl = (3000:20:11000)'   % Transmission wavelength grid [Angstrom] for model evaluation (401 points)
    end

    methods % Constructor
        function Obj = PhotCalibTrans(varargin)
            % Constructor for PhotCalibTrans class
            % Input  : * ...,key,val,...
            %            Metadata describing conditions of observations: 
            %            'AirMass' - Airmass. 
            %            'Zenith' - Zenith angle [deg]. 
            %            'Temp' - Temperature [C]. 
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
            % Input  : - Obj - PhotCalibTrans object (scalar)
            %          - Cat - AstroImage or AstroCatalog object with observed sources (scalar)
            %                  For multi-object processing, use external wrapper loop
            %                  Metadata source is determined automatically:
            %                    AstroImage: metadata from Cat.HeaderData
            %                    AstroCatalog: metadata from Args.Metadata (if provided),
            %                                  otherwise use object property defaults
            %          * ...,key,val,...
            %            'Metadata' - Metadata source (for AstroCatalog only). Can be:
            %                         AstroHeader object: extract metadata from header
            %                         Cell array: {key1, val1, key2, val2, ...} with metadata key-value pairs
            %                         Empty []: use object property defaults
            %                         Default is [].
            %            'FunListName' - Name of transmission function list from FunCatalog.
            %                            Default is 'DefaultLASTFunList'.
            %                            (10 functions, Garrappa et al. 2025)
            %            'CustomFunList' - Custom function list (overrides FunListName). Default is [].
            %            'OptSeqName' - Name of optimization sequence from StageCatalog.
            %                           Default is 'DefaultLASTOptSeq' (5-stage sequence, Garrappa et al. 2025).
            %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
            %            'Tran2DType' - Type of 2D transformation for field corrections. Default is 'cheby1_4_xt'.
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.5.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - PhotCalibTrans object with calibration results
            %                  Properties: .CalFound, .SpecData, .SourceData, .TransModel, metadata
            %                  Methods available: Obj.evaluateZP(), Obj.evaluateTransmission(), etc.
            % Author : D. Kovaleva (Dec 2025)
            % Example: % AstroImage with auto metadata (from FITS headers)
            %          PC = PhotCalibTrans();
            %          PC = PC.calibrate(AI);
            %
            %          % AstroCatalog with metadata from cell array
            %          PC = PhotCalibTrans();
            %          PC = PC.calibrate(Cat, 'Metadata', {'AirMass', 1.2, 'Zenith', 33.5, 'ExpTime', 20, ...
            %                                               'NCoadd', 1, 'Temp', 15, 'Pressure', 965});
            %
            %          % AstroCatalog with metadata from AstroHeader
            %          PC = PC.calibrate(Cat, 'Metadata', HeaderObj);
            %
            %          % AstroCatalog with object property defaults
            %          PC = PC.calibrate(Cat);

            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog

                % Metadata argument (for AstroCatalog only)
                Args.Metadata = []     % AstroHeader object or cell array {key1, val1, key2, val2, ...}

                % Calibration arguments
                Args.FunListName = 'DefaultLASTFunList'
                Args.CustomFunList = []
                Args.OptSeqName = 'DefaultLASTOptSeq'
                Args.CustomOptSeq = []
                Args.Tran2DType = 'cheby1_4_xt'
                Args.SearchRadius = 1.5
                Args.MagRange = [12 16]
                Args.Verbose logical = true
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n');
            end

            IsAstroImage = isa(Cat, 'AstroImage');

            % ====================================================================
            % STEP 1: Build TransModel structure
            % ====================================================================

            if Args.Verbose
                fprintf('Step 1: Building transmission model structure...\n');
            end

            % Load catalog
            [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();

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
            end

            if Args.Verbose
                fprintf('  Transmission functions and optimization sequence configured\n\n');
            end

            % ====================================================================
            % STEP 2: Extract metadata
            % ====================================================================

            % Extract metadata as cell array {key1, val1, key2, val2, ...}
            if iscell(Args.Metadata)
                % AstroCatalog with cell array: use directly
                Metadata = Args.Metadata;
            elseif IsAstroImage || isa(Args.Metadata, 'AstroHeader')
                % Extract from header (either Cat.HeaderData or Args.Metadata)
                Keys = {'MNTTEMP', 'EXPTIME', 'NCOADD', 'AIRMASS', 'PRESSURE'};
                PropNames = {'Temp', 'ExpTime', 'NCoadd', 'AirMass', 'Pressure'};

                if IsAstroImage
                    Res = getStructKey(Cat.HeaderData, Keys);
                else
                    Res = getStructKey(Args.Metadata, Keys);
                end

                % Build cell array - only include non-NaN values
                % This preserves class default properties when header values are missing or invalid
                Metadata = cell(1, 2 * length(Keys));
                idx = 1;
                for i = 1:length(Keys)
                    if isfield(Res, Keys{i})
                        val = Res.(Keys{i});
                        if ~isempty(val) && isnumeric(val) && ~any(isnan(val))
                            Metadata{idx} = PropNames{i};
                            Metadata{idx+1} = val;
                            idx = idx + 2;
                        end
                    end
                end
                Metadata = Metadata(1:idx-1);  % Trim to actual size
            else
                % Empty metadata - use object defaults
                Metadata = {};
            end

            % Set properties from cell array (convert to struct for setProps)
            if ~isempty(Metadata)
                MetadataStruct = struct(Metadata{:});
                Obj.setProps(MetadataStruct);
            end

            % Handle AirMass and Zenith dependency (Zenith takes precedence if both provided)
            hasAirMass = ~isnan(Obj.AirMass);
            hasZenith = ~isnan(Obj.Zenith);

            if hasZenith
                % Zenith provided (either alone or with AirMass) - calculate AirMass from Zenith
                Obj.AirMass = 1.0 / cosd(Obj.Zenith);
            elseif hasAirMass
                % Only AirMass provided - calculate Zenith from AirMass
                Obj.Zenith = acosd(1.0 / Obj.AirMass);
            end
            % If neither provided, both remain at their default values

            % Extract catalog (depends on input type)
            if IsAstroImage
                CurrentCat = Cat.CatData;
            else
                CurrentCat = Cat;
            end

            % Display metadata if verbose
            if Args.Verbose
                fprintf('  AirMass  = %.2f\n', Obj.AirMass);
                fprintf('  Zenith   = %.2f deg\n', Obj.Zenith);
                fprintf('  ExpTime  = %.1f s\n', Obj.ExpTime);
                fprintf('  NCoadd   = %d\n', Obj.NCoadd);
                fprintf('  Temp     = %.1f C\n', Obj.Temp);
                fprintf('  Pressure = %.1f mbar\n', Obj.Pressure);
            end

            % ====================================================================
            % STEP 3: Build TransModel with real metadata
            % ====================================================================

            % Build MetaValues from object properties (cell array format)
            % Properties contain either extracted header values or class defaults
            MetaValues = {'ZenithAngle_deg', Obj.Zenith, ...
                          'Pressure_mbar', Obj.Pressure, ...
                          'Temperature_C', Obj.Temp};

            % Build TransModel 
            Obj.TransModel = tools.math.fun.CompositeFun.model(FunList, ...
                'MetadataValues', MetaValues, ...
                'OptimizationSequence', OptSeq, ...
                'UseTran2D', true, ...
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

                % Calculate effective exposure time (accounting for coadding)
                ExpTime_eff = Obj.ExpTime / Obj.NCoadd;

                % Setup CostArgs for TransmissionMode using SpecData
                % (arguments for the costFun method of CompositeFun)
                CostArgs = {...
                    'WeightMatrix', Obj.SpecData.Spec', ...
                    'TransmissionMode', true, ...
                    'CalibWavelength', Obj.SpecData.SpecWvl, ...
                    'ExpTime', ExpTime_eff, ...
                    'Aperture_area_m2', Obj.Aperture};

                % Fit transmission parameters
                [Model, FitResult] = Obj.TransModel.fitPar(Obj.TransWvl, Flux, ...
                    'X', X, 'Y', Y, ...
                    'CostArgs', CostArgs, ...
                    'Verbose', Args.Verbose);

                % Store fitted model
                Obj.TransModel = Model;

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

                    % Add columns directly to the catalog
                    if istable(Obj.SourceData.Catalog)
                        Obj.SourceData.Catalog.Used = Used;
                        Obj.SourceData.Catalog.Residuals = Residuals;
                    else
                        % Convert to table, add columns, convert back
                        Tab = Obj.SourceData.Table;
                        Tab.Used = Used;
                        Tab.Residuals = Residuals;
                        Obj.SourceData.Catalog = Tab;
                    end
                end

                if Args.Verbose
                    fprintf('  Number of calibrators: %d\n', size(Obj.SpecData.Spec, 1));
                    if ~isnan(Obj.TransModel.RMS)
                        fprintf('  RMS: %.4f mag\n', Obj.TransModel.RMS);
                    end
                    if ~isnan(Obj.TransModel.Chi2) && ~isnan(Obj.TransModel.DOF) && Obj.TransModel.DOF > 0
                        fprintf('  Chi2/DOF: %.2f / %d = %.3f\n', ...
                                Obj.TransModel.Chi2, Obj.TransModel.DOF, Obj.TransModel.Chi2/Obj.TransModel.DOF);
                    end
                end

                % ----------------------------------------------------------------
                % TODO: Update input Object
                % ----------------------------------------------------------------

                if IsAstroImage
                    % TODO: Write calibration results to Cat.HeaderData
                    % Keys: PH_ZP, PH_RMS, PH_NCAL, etc.
                end

                if Args.Verbose
                    fprintf('\n');
                end
            end  % if ~Obj.CalFound ... else

            % Evaluate success criteria
            Obj = Obj.populateSuccess('Verbose', Args.Verbose);

            if Args.Verbose
                fprintf('=== Calibration Complete ===\n');
            end
        end

        function Obj = selectCalibrators(Obj, Cat, Args)
            % Select calibrators with reference spectra for photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroCatalog object with observed sources (single element)
            %          * ...,key,val,...
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.5.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'MinSN' - Minimum S/N for calibrators. Default is 5.
            %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
            %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
            %            'FluxColName' - Flux column name to compare with. Default is 'FLUX_APER_3'.
            %            'SpFluxCol' - Spectral flux column indices [flux_start, flux_end, error_start, error_end].
            %                          Default is [7, 349, 350, 692] for Gaia DR3 XP spectra.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - PhotCalibTrans object with populated properties:
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
            %          PC = PC.selectCalibrators(Cat, 'SearchRadius', 1.5, 'MagRange', [12 16]);
            %          PC = PC.selectCalibrators(Cat, 'SpFluxCol', [7, 349, 350, 692]);
            % Note: Default implementation uses Gaia DR3 XP spectra from GAIADR3spec catalog.
            %       Default telescope/instrument configuration is for LAST.
            %       Input must be single-element AstroCatalog (extracted in calibrate()).

            arguments
                Obj
                Cat  % AstroCatalog
                Args.SearchRadius = 1.5  % arcsec
                Args.MagRange = [12 16]
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

            % ====================================================================
            % STEP 2: MATCH WITH CALIBRATOR CATALOG (BEFORE FILTERING)
            % ====================================================================

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
            calIdx_all   = ResInd.Obj2_IndInObj1;     % Index of calibrator match for each source
            dist_rad_all  = ResInd.Obj2_Dist;          % Distance in radians
            nmatch_all    = ResInd.Obj2_NmatchObj1;    % Number of matches

            % Create mask for sources that have matches
            hasMatchMask = ~isnan(calIdx_all);

            if Args.Verbose
                fprintf('  Found %d/%d sources with Gaia XP matches\n', ...
                        sum(hasMatchMask), Nsources_initial);
            end

            % ====================================================================
            % STEP 3: APPLY QUALITY FILTERS TO MATCHED SOURCES
            % ====================================================================

            % Start with sources that have matches
            goodMask = hasMatchMask;

            % Filter 1: Magnitude range
            if ismember(Args.MagColName, Tab.Properties.VariableNames)
                magFilterMask = (Tab.(Args.MagColName) >= Args.MagRange(1)) & (Tab.(Args.MagColName) <= Args.MagRange(2));
                goodMask = goodMask & magFilterMask;
                if Args.Verbose
                    fprintf('  Magnitude filter (%g-%g): %d sources passed\n', ...
                            Args.MagRange(1), Args.MagRange(2), sum(goodMask));
                end
            end

            % Filter 2: Bad FLAGS (optional)
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
                goodMask = goodMask & ~badFlagsMask;

                if Args.Verbose
                    fprintf('  FLAGS filter: %d sources passed\n', sum(goodMask));
                end
            end

            % Filter 3: S/N range
            if ismember('SN', Tab.Properties.VariableNames)
                snMask = (Tab.SN >= Args.MinSN) & (Tab.SN <= Args.MaxSN);
                goodMask = goodMask & snMask;

                if Args.Verbose
                    fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                            Args.MinSN, Args.MaxSN, sum(goodMask));
                end
            end

            % Filter 4: Unique matches only (exclude sources with multiple identifications)
            uniqueMatchMask = (nmatch_all == 1);
            goodMask = goodMask & uniqueMatchMask;

            if Args.Verbose
                fprintf('  Unique match filter: %d sources passed\n', sum(goodMask));
            end

            % Check if any sources passed all filters
            if ~any(goodMask)
                warning('PhotCalibTrans:selectCalibrators:NoMatches', ...
                        'No sources passed quality filters and have calibrator matches');
                Obj.SourceData = [];
                Obj.SpecData = [];
                Obj.CalFound = false;
                return;
            end

            % Extract matched and filtered sources
            ObsTab = Tab(goodMask, :);                    % Filtered observed sources
            calIdx = double(calIdx_all(goodMask));        % Calibrator indices
            dist_rad = dist_rad_all(goodMask);            % Match distances
            nmatch = nmatch_all(goodMask);                % Number of matches

            CalArr = CatH.Catalog;  % Use Catalog (matrix) instead of Table
            CalTab = CalArr(calIdx, :);  % Matched calibrators
            Nmatch = size(CalTab, 1);

            if Args.Verbose
                fprintf('  Found %d matched calibrator pairs\n', Nmatch);
            end

            % ====================================================================
            % STEP 4: EXTRACT CALIBRATOR SPECTRA AND PREPARE OUTPUT
            % ====================================================================

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

            % Extract flux and flux error
            Obs_Flux = ObsTab.(Args.FluxColName);

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
            % STEP 5: POPULATE OBJECT PROPERTIES
            % ====================================================================

            % Populate SpecData structure with reference spectral data
            Obj.SpecData = struct();
            Obj.SpecData.CalData = struct('RA', Cal_RA, 'Dec', Cal_Dec);

            % Determine wavelength grid for calibrator spectra
            % Default: Gaia DR3 XP wavelength grid (3360:20:10200 Angstrom, 343 points)
            % TODO: Add logic to read SpecWvl from catalog if different calibrator source is used
            Obj.SpecData.SpecWvl = (3360:20:10200)';   % [N_wvl x 1]
            Obj.SpecData.Spec = SpecFlux;              % [N_calib x N_wvl]
            Obj.SpecData.SpecErr = SpecErr;            % [N_calib x N_wvl]

            % Populate SourceData as AstroCatalog with observed calibrator sources
            SourceTable = table(Obs_Flux, Obs_FluxErr, Obs_X, Obs_Y, Obs_RA, Obs_Dec, Dist_arcsec, nmatch, ...
                                'VariableNames', {'Flux', 'FluxErr', 'X', 'Y', 'RA', 'Dec', 'MatchDistance', 'NumMatches'});
            Obj.SourceData = AstroCatalog(SourceTable);

            % Set CalFound flag based on whether we have source data
            Obj.CalFound = ~isempty(Obj.SourceData);

            if Args.Verbose
                fprintf('Calibrator selection complete: %d matched calibrators.\n\n', Nmatch);
            end
        end

        function Obj = populateSuccess(Obj, Args)
            % Evaluate and set Success flag based on calibration quality criteria
            % Input  : - Obj - PhotCalibTrans object (scalar)
            %          * ...,key,val,...
            %            'NCalibMin' - Minimum number of calibrators required. Default is 30.
            %            'RMSMax' - Maximum allowed RMS [mag]. Default is 0.1.
            %            'Verbose' - Enable verbose output. Default is false.
            % Output : - Obj - PhotCalibTrans object with updated Success flag
            % Author : D. Kovaleva (Jan 2026)
            % Example: PC = PC.populateSuccess();
            %          PC = PC.populateSuccess('NCalibMin', 50, 'RMSMax', 0.08);
            % Description: Evaluates calibration success based on three criteria:
            %              1. CalFound = true (calibrators were found)
            %              2. Number of calibrators >= NCalibMin (default: 30)
            %              3. RMS <= RMSMax (default: 0.1 mag)
            %              Sets Obj.Success = true only if all criteria are met.

            arguments
                Obj
                Args.NCalibMin = 30
                Args.RMSMax = 0.1
                Args.Verbose logical = false
            end

            % Evaluate all criteria (Success remains false unless all criteria pass)
            Obj.Success = false;

            % Criterion 1+2: Check if we have sufficient calibrators (this also implies CalFound = true)
            HasEnoughCalibrators = false;
            if ~isempty(Obj.SpecData) && ~isempty(Obj.SpecData.Spec)
                NCalib = size(Obj.SpecData.Spec, 1);
                HasEnoughCalibrators = (NCalib >= Args.NCalibMin);
            end

            % Criterion 3: Check if RMS is acceptable
            HasAcceptableRMS = false;
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.RMS) && ~isnan(Obj.TransModel.RMS)
                HasAcceptableRMS = (Obj.TransModel.RMS <= Args.RMSMax);
            end

            % Set success only if all criteria are met
            if HasEnoughCalibrators && HasAcceptableRMS
                Obj.Success = true;
            end
        end
    end

    methods % Evaluation methods
        function Trans = evaluateTransmission(Obj, Args)
            % Evaluate transmission at specific positions (with position-dependent corrections)
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'Lambda' - Wavelength grid [Angstrom] [N_lambda x 1]. Default is Obj.TransWvl (constant property).
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
            

            arguments
                Obj
                Args.X = []
                Args.Y = []
            end

            Fnu = constant.Fnu('SI');  % AB system flux density [W/m^2/Hz]
            H = constant.h('SI');  % Planck constant [J·s]
              
            % Use constant wavelength grid
            Lambda = Obj.TransWvl;

            % Evaluate transmission at positions (or field center if X, Y empty)
            % Trans is [N_lambda x 1] if no positions, or [N_pos x N_lambda] if positions provided
            Trans = Obj.evaluateTransmission('X', Args.X, 'Y', Args.Y);

            % Create flat Fnu spectrum for AB zero-point
            FlatSpectrum = Fnu * ones(size(Lambda));  % [N_lambda x 1]

            % Physical constants
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

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.ExpTime / Obj.NCoadd;

            % Ensure column vectors
            Flux = Flux(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP('X', Args.X, 'Y', Args.Y);

            % Calculate calibrated AB magnitudes
            % MAG_AB = -2.5*log10(FLUX/ExpTime_eff) + ZP
            MagInst = convert.luptitude(Flux/ExpTime_eff,10.^(0.4.*ZP'));

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

            % Get catalog table
            Tab = CatObj.Table;

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

            % Get residuals from last fit stage
            LastStage = Obj.TransModel.FitResults(end);
            Residuals = LastStage.Residual;  % [N_calibrators x 1] in magnitude units

            % Get calibrator data from SourceData
            X = Obj.SourceData.getCol('X');
            Y = Obj.SourceData.getCol('Y');
            Flux = Obj.SourceData.getCol('Flux');
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
            title(sprintf('Fit Convergence (N=%d calibrators)', size(Obj.SpecData.Spec, 1)));
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