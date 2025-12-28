classdef PhotCalibTrans < Component
    % PhotCalibTrans - This class provides container for transmission-based absolute calibration data
    %                  and basic functionality for absolute photometric calibration.
    % Description: Stores and manages photometric calibration data.
    %              Wraps CompositeFun methods and provides header/catalog operations.
    % Author : D. Kovaleva (Dec 2025)
    % Example:
    %{
     PC = PhotCalibTrans();
     [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();
     TransFuns = [FunCat.Rayleigh, FunCat.Aerosol, FunCat.Mirror];
     Model = tools.math.fun.CompositeFun.model(TransFuns);
     PC.TransModel = Model;
     Lambda = (300:1100)';
     Trans = PC.evaluateTransmission(Lambda);

    %}
    %          PC.calibrate(CalibData, FunList, OptSeq, Metadata);
    %
    % Methods:
    %   Constructor:
    %     PhotCalibTrans - Constructor for PhotCalibTrans class
    %   Setters:
    %     reset - Reset calibration data while keeping configuration
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
    %     addMagAB - Add calibrated AB magnitude (and optionally error) columns to catalog [PLACEHOLDER]
    %   Display/Output Methods:
    %     summary - Display photometric calibration summary
    %     saveTransmission - Save base transmission to file
    %   Plotting Methods:
    %     plotTransmission - Plot transmission curve vs wavelength [PLACEHOLDER]
    %     plotResiduals - Plot calibration residuals (mag vs residual, spatial distribution) [PLACEHOLDER]
    %     plotZPMap - Plot 2D map of position-dependent zero point corrections [PLACEHOLDER]
    %     plotCalibrators - Plot observed vs predicted magnitudes for calibrators [PLACEHOLDER]
    %     plotFitQuality - Plot RMS/Chi2 evolution across optimization stages [PLACEHOLDER]
    %   Static Methods:
    %     fromHeader - Create PhotCalibTrans object from AstroHeader [PLACEHOLDER]

    properties
        % Core calibration results
        %ZP                      % Base zero point without positional correction [mag]
        %ZP_Err                  % Zero point uncertainty [mag]

        % Quality metrics
        NumCalib                % Number of calibrators used

        % Transmission model
        TransModel              % CompositeFun transmission model object with Tran2D for position-dependent corrections
        %FitResults              % Structure array with per-stage fit results

        % Calibration metadata (FITS header naming convention)
        AIRMASS                 % Airmass
        ZENITH                  % Zenith angle [deg]
        EXPTIME                 % Exposure time [s]
        TEMP                    % Temperature [C]
        PRESSURE                % Atmospheric pressure [mbar]
        HUMIDITY                % Relative humidity [%]
        APERTURE                % Telescope aperture area [m^2]

        % Calibrator information
        CalibData               % Structure with calibrator data from selectCalibrators

        % Calibration scheme configuration
        FunList                 % Built transmission function list (struct array from predefSeqCompositeFun)
        OptSeq                  % Built optimization sequence (struct from predefSeqCompositeFun)
        %SearchRadius            % Calibrator matching radius [arcsec]
        %MagRange                % Calibrator magnitude range [min max]

        % Transmission output
        %TransFile               % Filename for saved base transmission
        %TransWvl                % Wavelength grid for transmission [nm]
        %TransValues             % Base transmission values (without position correction)
    end

    methods % Constructor
        function Obj = PhotCalibTrans(Args)
            % Constructor for PhotCalibTrans class
            % Input  : * ...,key,val,...
            %            'NumCalib' - Number of calibrators. Default is 0.
            %            'TransModel' - CompositeFun transmission model. Default is [].
            %            'FunList' - Built transmission function list. Default is [].
            %            'OptSeq' - Built optimization sequence. Default is [].
            %            'AIRMASS' - Airmass. Default is NaN.
            %            'ZENITH' - Zenith angle [deg]. Default is NaN.
            %            'EXPTIME' - Exposure time [s]. Default is NaN.
            %            'TEMP' - Temperature [C]. Default is NaN.
            %            'PRESSURE' - Atmospheric pressure [mbar]. Default is NaN.
            %            'HUMIDITY' - Relative humidity [%]. Default is NaN.
            %            'APERTURE' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2 (LAST).
            % Output : - PhotCalibTrans object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans();
            %          PC = PhotCalibTrans('NumCalib', 50);

            arguments
                Args.NumCalib = 0
                Args.TransModel = []
                Args.FunList = []
                Args.OptSeq = []
                Args.AIRMASS = NaN
                Args.ZENITH = NaN
                Args.EXPTIME = NaN
                Args.TEMP = NaN
                Args.PRESSURE = NaN
                Args.HUMIDITY = NaN
                Args.APERTURE = pi * (0.1397)^2
            end

            % Call parent constructor
            Obj@Component();

            % Initialize properties from arguments
            Fields = fieldnames(Args);
            for I = 1:length(Fields)
                Obj.(Fields{I}) = Args.(Fields{I});
            end
        end
    end

    methods % Setters
        function Obj = reset(Obj)
            % Reset calibration data while keeping configuration
            % Input  : - Obj - PhotCalibTrans object
            % Output : - Obj - PhotCalibTrans object with cleared results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.reset();
            % Description: Clears calibration results (TransModel, CalibData) and
            %              observation-specific metadata (AirMass, ExpTime, etc.)
            %              while keeping calibration scheme (FunList, OptSeq, ApertureArea_m2).

            arguments
                Obj
            end

            % Clear calibration results
            Obj.TransModel = [];
            Obj.NumCalib = 0;
            Obj.CalibData = [];

            % Clear observation-specific metadata
            Obj.AIRMASS = NaN;
            Obj.ZENITH = NaN;
            Obj.EXPTIME = NaN;
            Obj.TEMP = NaN;
            Obj.PRESSURE = NaN;
            Obj.HUMIDITY = NaN;

            % Keep calibration scheme configuration:
            % - FunList (transmission function list)
            % - OptSeq (optimization sequence)
            % - APERTURE (telescope aperture)
        end
    end

    methods % Core calibration methods
        function Obj = calibrate(Obj, Cat, Metadata, Args)
            % Perform transmission-based photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroImage or AstroCatalog object with observed sources
            %          - Metadata - Structure with observation metadata:
            %                       .AIRMASS, .EXPTIME, .TEMP
            %          * ...,key,val,...
            %            'TransFunList' - Cell array of transmission function names to use.
            %                             Default is {'Normalization', 'Rayleigh', 'Aerosol', 'Water',
            %                                         'Mirror', 'Corrector', 'QE_SkewedGaussian'}.
            %            'OptSeqName' - Name of optimization sequence from StageCatalog.
            %                           Default is 'DefaultLAST' (5-stage sequence from Garrappa et al. 2025).
            %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
            %            'RebuildScheme' - Force rebuild of FunList and OptSeq even if already stored. Default is false.
            %            'WvlRange_nm' - Wavelength range for calibration [min max] [nm]. Default is [300, 1100].
            %            'Pressure_mbar' - Atmospheric pressure [mbar]. Default is 965.
            %            'Tran2DType' - Type of 2D transformation for field corrections. Default is 'cheby1_4_xt'.
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.0.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - PhotCalibTrans object with calibration results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans();
            %          PC.calibrate(AI, Metadata);
            %          PC.calibrate(Cat, Metadata, 'TransFunList', {'Rayleigh', 'Aerosol', 'Mirror', 'QE_Legendre'});

            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog
                Metadata struct
                Args.TransFunList = {'Normalization', 'Rayleigh', 'Aerosol', 'Ozone', 'Water', ' UMG', 'Mirror', 'Corrector', 'QE_SkewedGaussian', 'QE_Legendre'}
                Args.OptSeqName = 'DefaultLAST'
                Args.CustomOptSeq = []
                Args.RebuildScheme logical = false
                Args.WvlRange_nm = [300, 1100]
                Args.Pressure_mbar = 965
                Args.Tran2DType = 'cheby1_4_xt'
                Args.SearchRadius = 1.0
                Args.MagRange = [12 16]
                Args.Verbose logical = true
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n\n');
            end

            % ====================================================================
            % STEP 1: Build or reuse calibration scheme (FunList and OptSeq)
            % ====================================================================

            if Args.Verbose
                fprintf('Step 1: Preparing calibration scheme...\n');
            end

            % Determine if we need to rebuild the scheme
            % Rebuild if: (1) first call (empty), (2) RebuildScheme=true,
            %             (3) user explicitly provided new TransFunList or CustomOptSeq
            NeedRebuild = Args.RebuildScheme || isempty(Obj.FunList) || isempty(Obj.OptSeq);

            if NeedRebuild
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

                % Store in object for reuse
                Obj.FunList = FunList;
                Obj.OptSeq = OptSeq;

                if Args.Verbose
                    fprintf('  Built new calibration scheme\n');
                    fprintf('  Using %d transmission functions: %s\n', ...
                            length(Args.TransFunList), strjoin(Args.TransFunList, ', '));
                    if ~isempty(Args.CustomOptSeq)
                        fprintf('  Using custom optimization sequence (%d stages)\n', length(OptSeq));
                    else
                        fprintf('  Using optimization sequence: %s (%d stages)\n', ...
                                Args.OptSeqName, length(OptSeq.StageName));
                    end
                end
            else
                % Reuse stored scheme
                FunList = Obj.FunList;
                OptSeq = Obj.OptSeq;

                if Args.Verbose
                    fprintf('  Reusing stored calibration scheme\n');
                end
            end

            % Store metadata using setProps
            % Check required fields
            if ~isfield(Metadata, 'AIRMASS')
                error('PhotCalibTrans:calibrate:NoAirmass', 'Metadata.AIRMASS is required');
            end
            if ~isfield(Metadata, 'EXPTIME')
                error('PhotCalibTrans:calibrate:NoExpTime', 'Metadata.EXPTIME is required');
            end

            % Set defaults for optional fields
            if ~isfield(Metadata, 'TEMP')
                Metadata.TEMP = 15;  % Default temperature [C]
            end
            if ~isfield(Metadata, 'PRESSURE')
                Metadata.PRESSURE = Args.Pressure_mbar;
            end

            % Calculate derived fields
            Metadata.ZENITH = acosd(1.0 / Metadata.AIRMASS);

            % Use setProps to copy all metadata fields to object properties
            Obj.setProps(Metadata);

            % Build metadata values for transmission model
            MetaValues = struct(...
                'ZenithAngle_deg', Obj.ZENITH, ...
                'Pressure_mbar', Obj.PRESSURE, ...
                'Temperature_C', Obj.TEMP);

            % Create CompositeFun model
            Model = tools.math.fun.CompositeFun.model(FunList, ...
                'MetadataValues', MetaValues, ...
                'UseTran2D', true, ...
                'Tran2DType', Args.Tran2DType);

            % ====================================================================
            % STEP 2: Select calibrators 
            % ====================================================================

            if Args.Verbose
                fprintf('\nStep 2: Selecting calibrators...\n');
            end

            CalibData = Obj.selectCalibrators(Cat, ...
                'SearchRadius', Args.SearchRadius, ...
                'MagRange', Args.MagRange, ...
                'Verbose', Args.Verbose);

            if isempty(CalibData.Spec)
                error('PhotCalibTrans:calibrate:NoCalibrators', ...
                      'No calibrators found. Cannot proceed with calibration.');
            end

            Obj.NumCalib = size(CalibData.Spec, 1);

            % ====================================================================
            % STEP 3: Fit transmission parameters
            % ====================================================================

            if Args.Verbose
                fprintf('\nStep 3: Fitting transmission parameters...\n');
            end

            % Define two wavelength grids:
            % Lambda: Transmission wavelength grid for evaluating transmission model
            Lambda = (Args.WvlRange_nm(1):1:Args.WvlRange_nm(2))';  % e.g., 300:1100 nm

            % SpecWvl: Spectral wavelength grid where calibrator reference spectra are defined
            SpecWvl = CalibData.Lambda(:);  % Calibrator wavelengths (e.g., 336-1020 nm for Gaia XP)

            % Extract data for fitting
            Flux = CalibData.ObsData.Flux;
            X = CalibData.ObsData.X;
            Y = CalibData.ObsData.Y;

            % Setup CostArgs for TransmissionMode
            CostArgs = struct(...
                'WeightMatrix', CalibData.Spec, ...  % Calibrator reference spectra [N_cal x N_wvl]
                'TransmissionMode', true, ...
                'GaiaWavelength', SpecWvl, ...       % Wavelength grid for calibrator spectra
                'ExpTime', Obj.EXPTIME, ...
                'Aperture_area_m2', Obj.APERTURE);

            % Fit transmission parameters using multi-stage optimization
            % Lambda is used to evaluate transmission and integrate predicted fluxes
            [Model, FitRes] = Model.fitPar(Lambda, Flux, ...
                'X', X, 'Y', Y, ...
                'CostArgs', CostArgs, ...
                'OptimizationSequence', OptSeq, ...
                'Verbose', Args.Verbose);

            % ====================================================================
            % STEP 4: Store results in PhotCalibTrans object
            % ====================================================================

            if Args.Verbose
                fprintf('\nStep 4: Storing calibration results...\n');
            end

            % Store the fitted model (contains RMS, Chi2, DOF, FitResults)
            Obj.TransModel = Model;

            if Args.Verbose
                fprintf('  Calibration complete!\n');
                fprintf('  Number of calibrators: %d\n', Obj.NumCalib);

                % Access RMS, Chi2, DOF from TransModel
                if ~isempty(Obj.TransModel.RMS)
                    fprintf('  RMS: %.4f mag\n', Obj.TransModel.RMS);
                end
                if ~isempty(Obj.TransModel.Chi2) && ~isempty(Obj.TransModel.DOF)
                    fprintf('  Chi2/DOF: %.2f / %d = %.3f\n', ...
                            Obj.TransModel.Chi2, Obj.TransModel.DOF, Obj.TransModel.Chi2/Obj.TransModel.DOF);
                end
                fprintf('\n=== Calibration Complete ===\n\n');
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
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - CalibData - Structure with calibrator data:
            %                        .Spec - Calibrator reference spectra [N x WvlPoints]
            %                        .SpecErr - Calibrator spectra errors [N x WvlPoints]
            %                        .Lambda - Wavelength grid [nm]
            %                        .ObsData - Structure with observed catalog data:
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
                Args.Verbose logical = true
            end

            % Constants for calibrator spectra columns (for GAIADR3spec catalog)
            FluxIni = 7;      % Start of flux values
            FluxEnd = 349;    % End of flux values
            EFluxIni = 350;   % Start of flux errors
            EFluxEnd = 692;   % End of flux errors
  
            RAD = constant.RAD;  % Conversion factor

            % ====================================================================
            % STEP 1: EXTRACT CATALOG FROM INPUT OBJECT
            % ====================================================================

            if isa(Cat, 'AstroImage')
                % For AstroImage, use first element if array
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

            % Filter 2: Bad FLAGS (optional)
            if Args.FilterBadFlags && ismember('FLAGS', Tab.Properties.VariableNames)
                badFlagsMask = false(height(Tab), 1);
                for i = 1:height(Tab)
                    flags = Tab.FLAGS(i);
                    % Check for critical bad flags
                    isSaturated = bitget(flags, 1);
                    isNaN = bitget(flags, 7);
                    isNegative = bitget(flags, 11);
                    isCR = bitget(flags, 15);
                    isNearEdge = bitget(flags, 24);

                    % Mark as bad if it has multiple problematic flags
                    if (isSaturated + isNaN + isNegative + isCR + isNearEdge) >= 2
                        badFlagsMask(i) = true;
                    end
                end
                Tab = Tab(~badFlagsMask, :);

                if Args.Verbose
                    fprintf('  FLAGS filter: %d sources passed\n', height(Tab));
                end
            end

            % Filter 3: S/N range
            if ismember('SN', Tab.Properties.VariableNames)
                snMask = (Tab.SN >= Args.MinSN) & (Tab.SN <= Args.MaxSN);
                Tab = Tab(snMask, :);

                if Args.Verbose
                    fprintf('  S/N filter (%g-%g): %d sources passed\n', ...
                            Args.MinSN, Args.MaxSN, height(Tab));
                end
            end

            % Update Cat with filtered table
            Cat.Table = Tab;

            % ====================================================================
            % STEP 3: MATCH WITH CALIBRATOR CATALOG
            % ====================================================================

            % Match with calibrator catalog using imProc.match.match_catsHTM (default: GAIADR3spec)
            [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', ...
                                                              'Coo', [Cat.Table.RA/RAD, Cat.Table.Dec/RAD], ...
                                                              'Radius', Args.SearchRadius, ...
                                                              'CooUnits', 'rad', ...
                                                              'RadiusUnits', 'arcsec');

            % Extract match information
            calIdx_all   = ResInd.Obj2_IndInObj1;     % Index of calibrator match for each observed source
            dist_rad_all  = ResInd.Obj2_Dist;          % Distance in radians
            nmatch_all    = ResInd.Obj2_NmatchObj1;    % Number of matches

            % Keep only rows with valid calibrator index
            idxObsMatched = find(~isnan(calIdx_all));
            calIdx        = double(calIdx_all(idxObsMatched));
            dist_rad       = dist_rad_all(idxObsMatched);
            nmatch         = nmatch_all(idxObsMatched);

            if isempty(idxObsMatched)
                warning('PhotCalibTrans:selectCalibrators:NoMatches', ...
                        'No calibrator matches found within %.1f arcsec', Args.SearchRadius);
                CalibData = struct('Spec', [], 'SpecErr', [], 'Lambda', [], ...
                                   'ObsData', [], 'CalData', [], ...
                                   'MatchDistance', [], 'NumMatches', []);
                return;
            end

            % Extract matched tables
            ObsTab = Cat.Table(idxObsMatched, :);
            CalTabAll = CatH.Table;
            CalTab = CalTabAll(calIdx, :);
            Nmatch = height(CalTab);

            if Args.Verbose
                fprintf('  Found %d matched calibrator pairs\n', Nmatch);
            end

            % ====================================================================
            % STEP 4: EXTRACT CALIBRATOR SPECTRA AND PREPARE OUTPUT
            % ====================================================================

            % Extract calibrator spectra
            CalArr = table2array(CalTab);
            SpecFlux = CalArr(:, FluxIni:FluxEnd);      % [N x 343]
            SpecErr = CalArr(:, EFluxIni:EFluxEnd);     % [N x 343]

            % Get wavelength grid from catalog
            % For GAIADR3spec, wavelengths are 336-1020 nm in 343 points (non-uniform)
            Lambda = catsHTM.xp.gaia_xp_wvl();  % Get wavelength grid for Gaia XP spectra

            % Extract coordinates
            Cal_RA = CalArr(:, 1) * RAD;   % rad -> deg
            Cal_Dec = CalArr(:, 2) * RAD;  % rad -> deg

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
            CalibData.Spec = SpecFlux;           % [N x 343]
            CalibData.SpecErr = SpecErr;         % [N x 343]
            CalibData.Lambda = Lambda;           % [343 x 1] or [1 x 343]

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

            if Args.Verbose
                fprintf('Calibrator selection complete: %d matched calibrators.\n\n', Nmatch);
            end
        end
    end

    methods % Evaluation methods
        function Trans = evaluateTransmission(Obj, Lambda, Args)
            % Evaluate transmission at specific positions (with position-dependent corrections)
            % Input  : - Obj - PhotCalibTrans object
            %          - Lambda - Wavelength grid [nm] [N_lambda x 1]
            %          * ...,key,val,...
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            % Output : - Trans - Transmission values [N_pos x N_lambda] or [N_lambda x 1]
            %                    If X, Y provided: matrix where Trans(i,j) = transmission for position i at wavelength j
            %                    If X, Y empty: vector of base transmission at field center
            % Author : D. Kovaleva (Dec 2025)
            % Example: Trans = PC.evaluateTransmission(Lambda);  % Base transmission at field center
            %          Trans = PC.evaluateTransmission(Lambda, 'X', X, 'Y', Y);

            arguments
                Obj
                Lambda
                Args.X = []
                Args.Y = []
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:evaluateTransmission:NoModel', 'TransModel is not available');
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

        function ZP = evaluateZP(Obj, Lambda, Args)
            % Evaluate photometric zero point at specific positions
            % Input  : - Obj - PhotCalibTrans object
            %          - Lambda - Wavelength grid [nm] [N_lambda x 1]
            %          * ...,key,val,...
            %            'X' - X coordinates [N_pos x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N_pos x 1]. Default is [] (field center).
            %            'ExpTime' - Exposure time [s]. Default is Obj.EXPTIME.
            %            'ApertureArea_m2' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2.
            % Output : - ZP - Zero point(s) [N_pos x 1] or scalar
            %                 If X, Y provided: vector with ZP for each position
            %                 If X, Y empty: scalar ZP at field center
            % Author : D. Kovaleva (Dec 2025)
            % Example: ZP = PC.evaluateZP(Lambda);  % ZP at field center
            %          ZP = PC.evaluateZP(Lambda, 'X', X, 'Y', Y);  % ZP at specific positions
            %
            % Formula: ZP = 2.5*log10(ExpTime * Area * Integral(Trans * Fnu * Lambda * dLambda) / (h*c))
            % where Fnu is the AB system flux density (constant for flat spectrum)

            arguments
                Obj
                Lambda
                Args.X = []
                Args.Y = []
                Args.ExpTime = []
                Args.ApertureArea_m2 = pi * (0.1397)^2
            end

            Lambda = Lambda(:);  % Ensure column vector [N_lambda x 1]

            % Get exposure time from arguments or object property
            if ~isempty(Args.ExpTime)
                Obj.EXPTIME = Args.ExpTime;
            elseif isnan(Obj.EXPTIME)
                error('PhotCalibTrans:evaluateZP:NoExpTime', 'EXPTIME not available in object or arguments');
            end

            % Evaluate transmission at positions (or field center if X, Y empty)
            % Trans is [N_lambda x 1] if no positions, or [N_pos x N_lambda] if positions provided
            Trans = Obj.evaluateTransmission(Lambda, 'X', Args.X, 'Y', Args.Y);

            % Create flat Fnu spectrum for AB zero-point
            Fnu = constant.Fnu('SI');  % AB system flux density [W/m^2/Hz]
            FlatSpectrum = Fnu * ones(size(Lambda));  % [N_lambda x 1]

            % Physical constants
            H = constant.h('SI');  % Planck constant [J·s]
            C = constant.c('SI');  % Speed of light [m/s]
            B = H * C * 1e9;       % H*C with nm to m conversion

            % Ensure Trans is 2D matrix for consistent handling
            if isvector(Trans)
                Trans = Trans(:)';  % Convert to row vector [1 x N_lambda]
            end
            % Now Trans is [N_pos x N_lambda]

            % Apply transmission: multiply each row by FlatSpectrum
            % FlatSpectrum is [N_lambda x 1], Trans is [N_pos x N_lambda]
            SpecTrans = Trans .* FlatSpectrum';  % [N_pos x N_lambda]

            % Multiply by Lambda for integration
            Integrand = SpecTrans .* Lambda';  % [N_pos x N_lambda]

            % Integrate along wavelength dimension (dim=2) for each position
            A = tools.math.integral.trapzmat(Lambda(:), Integrand, 2);  % [N_pos x 1]

            % Calculate zero-point flux for all positions
            TotalFlux_ZP = Obj.EXPTIME * Args.ApertureArea_m2 * A / B;  % [N_pos x 1]

            % Convert to magnitude
            ZP = 2.5 * log10(TotalFlux_ZP);  % [N_pos x 1]

            % If single position, return scalar
            if length(ZP) == 1
                ZP = ZP(1);
            end
        end

        function [MagAB, MagABErr] = evaluateMag(Obj, Lambda, MagInst, Args)
            % Evaluate calibrated AB magnitudes from instrumental magnitudes
            % Input  : - Obj - PhotCalibTrans object
            %          - Lambda - Wavelength grid [nm]
            %          - MagInst - Instrumental magnitudes [N x 1]
            %          * ...,key,val,...
            %            'X' - X coordinates [N x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N x 1]. Default is [] (field center).
            %            'MagInstErr' - Instrumental magnitude errors [N x 1]. Default is [].
            %            'ExpTime' - Exposure time [s]. Default is Obj.EXPTIME.
            %            'ApertureArea_m2' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2.
            % Output : - MagAB - Calibrated AB magnitudes [N x 1]
            %          - MagABErr - Calibrated AB magnitude errors [N x 1] (optional)
            % Author : D. Kovaleva (Dec 2025)
            % Example: MagAB = PC.evaluateMag(Lambda, MagInst);
            %          [MagAB, MagABErr] = PC.evaluateMag(Lambda, MagInst, 'X', X, 'Y', Y, 'MagInstErr', MagErr);
            % Description: Converts instrumental magnitudes to calibrated AB magnitudes.
            %              Mag_AB = Mag_inst + ZP
            %              Uses evaluateZP to calculate position-dependent zero points.
            %              Error propagation: MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2)

            arguments
                Obj
                Lambda               % Wavelength grid [nm]
                MagInst              % Instrumental magnitudes [N x 1]
                Args.X = []          % X coordinates [N x 1]
                Args.Y = []          % Y coordinates [N x 1]
                Args.MagInstErr = [] % Instrumental magnitude errors [N x 1]
                Args.ExpTime = []
                Args.ApertureArea_m2 = pi * (0.1397)^2
            end

            % Ensure column vectors
            MagInst = MagInst(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP(Lambda, 'X', Args.X, 'Y', Args.Y, ...
                               'ExpTime', Args.ExpTime, ...
                               'ApertureArea_m2', Args.ApertureArea_m2);

            % Apply ZP to get calibrated magnitudes
            % Mag_AB = Mag_inst + ZP
            if isscalar(ZP)
                MagAB = MagInst + ZP;
            else
                MagAB = MagInst + ZP(:);
            end

            % Calculate calibrated magnitude errors if requested
            if nargout > 1
                if isempty(Args.MagInstErr)
                    % No instrumental errors provided
                    MagABErr = [];
                else
                    MagInstErr = Args.MagInstErr(:);

                    % Estimate ZP uncertainty from RMS if available
                    if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.RMS) && Obj.NumCalib > 0
                        ZP_Err = Obj.TransModel.RMS / sqrt(Obj.NumCalib);
                    else
                        ZP_Err = 0;
                    end

                    % Error propagation: MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2)
                    MagABErr = sqrt(MagInstErr.^2 + ZP_Err^2);
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
            % Add calibrated AB magnitude (and optionally error) columns to catalog
            % Input  : - Obj - PhotCalibTrans object
            %          - CatObj - AstroCatalog object with instrumental magnitudes
            %          * ...,key,val,...
            %            'MagColNames' - Instrumental magnitude column names. Default is all MAG_* columns.
            %            'NewColSuffix' - Suffix for new calibrated columns. Default is '_AB'.
            %            'ApplyPosCorrection' - Apply position-dependent corrections. Default is true.
            %            'AddErrors' - Also add calibrated magnitude error columns. Default is true.
            %            'Lambda' - Wavelength grid [nm] for ZP calculation. Default is 300:1100.
            % Output : - CatObj - AstroCatalog with added calibrated AB magnitude columns
            %                     (e.g., MAG_APER_1 → MAG_APER_1_AB)
            %                     and optionally error columns (e.g., MAGERR_APER_1 → MAGERR_APER_1_AB)
            % Author : D. Kovaleva (Dec 2025)
            % Example: Cat = PC.addMagAB(Cat);
            %          Cat = PC.addMagAB(Cat, 'NewColSuffix', '_CAL', 'AddErrors', false);
            % Description: Creates new columns with calibrated AB magnitudes = instrumental + ZP.
            %              If AddErrors=true, also creates error columns with error propagation:
            %              MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2).
            %              Preserves original instrumental magnitude and error columns.
            %              Applies position-dependent corrections if available.

            arguments
                Obj
                CatObj
                Args.MagColNames = []
                Args.NewColSuffix = '_AB'
                Args.ApplyPosCorrection logical = true
                Args.AddErrors logical = true
                Args.Lambda = (300:1100)'
            end

            % TODO: Implement
            % 1. Find all MAG_* columns (or use Args.MagColNames)
            % 2. Extract X, Y positions from catalog
            % 3. For each magnitude column:
            %    a. Get instrumental magnitudes
            %    b. Call evaluateMag to get calibrated magnitudes (and errors if AddErrors=true)
            %    c. Add new column(s) to catalog with suffix
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
            fprintf('Calibrators: %d\n', Obj.NumCalib);

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

            if ~isnan(Obj.AIRMASS)
                fprintf('Airmass: %.3f\n', Obj.AIRMASS);
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

        function saveTransmission(Obj, Filename, Args)
            % Save base transmission to file
            % Input  : - Obj - PhotCalibTrans object
            %          - Filename - Output filename
            %          * ...,key,val,...
            %            'WvlRange_nm' - Wavelength range [min max] [nm]. Default is [300, 1100].
            %            'WvlStep_nm' - Wavelength step [nm]. Default is 1.
            %            'Format' - Output format: 'ascii' or 'mat'. Default is 'ascii'.
            % Output : None (writes file)
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.saveTransmission('transmission.txt');
            %          PC.saveTransmission('trans.mat', 'Format', 'mat');

            arguments
                Obj
                Filename
                Args.WvlRange_nm = [300, 1100]
                Args.WvlStep_nm = 1
                Args.Format = 'ascii'
            end

            % Create wavelength grid and compute transmission
            Lambda = (Args.WvlRange_nm(1):Args.WvlStep_nm:Args.WvlRange_nm(2))';
            Trans = Obj.evaluateTransmission(Lambda);

            % Store in object
            Obj.TransFile = Filename;
            Obj.TransWvl = Lambda;
            Obj.TransValues = Trans;

            % Write to file
            switch lower(Args.Format)
                case 'ascii'
                    % Write as ASCII table: wavelength [nm], transmission
                    Data = [Lambda(:), Trans(:)];
                    writematrix(Data, Filename, 'Delimiter', ' ');
                case 'mat'
                    % Save as MATLAB file
                    save(Filename, 'Lambda', 'Trans');
                otherwise
                    error('PhotCalibTrans:saveTransmission:InvalidFormat', 'Format must be ''ascii'' or ''mat''');
            end
        end
    end

    methods % Plotting methods
        function Fig = plotTransmission(Obj, Args)
            % Plot transmission curve vs wavelength
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'WvlRange_nm' - Wavelength range [min max] [nm]. Default is [300, 1100].
            %            'WvlStep_nm' - Wavelength step [nm]. Default is 1.
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotTransmission();
            %          PC.plotTransmission('WvlRange_nm', [400, 900]);

            % TODO: Implement
            Fig = [];
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

            % TODO: Implement
            Fig = [];
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

            % TODO: Implement
            Fig = [];
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

            % TODO: Implement
            Fig = [];
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

            % TODO: Implement
            Fig = [];
        end
    end

    methods (Access = private)
    end

    methods (Static)
        function Obj = fromHeader(HeaderObj, Args)
            % Create PhotCalibTrans object from AstroHeader

            % TODO: Implement
            Obj = PhotCalibTrans();
        end
    end
end
