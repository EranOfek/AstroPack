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
    % Constant Properties:
    %   Lambda  - Transmission wavelength grid [nm] (300:2:1100, 401 points)
    %   SpecWvl - Calibrator spectra wavelength grid [nm] (default: Gaia DR3 XP, 336-1020, 343 points)
    %
    % Key Properties:
    %   TransModel - CompositeFun object with fitted transmission model
    %   CalibData  - Structure with calibrator data (spectra, positions, fluxes)
    %   AIRMASS, ZENITH, EXPTIME, NCOADD, TEMP, PRESSURE, HUMIDITY, APERTURE - Observation metadata
    %   FunList, OptSeq - Calibration scheme configuration (reusable across images)
    %
    % Example:
    %{
     % Create calibration object
     PC = PhotCalibTrans();  

     % Perform calibration on AstroImage (metadata read from AI.HeaderData)
     PC.calibrate(AI);

     % Evaluate transmission and zero points
     Trans = PC.evaluateTransmission(PC.Lambda);  % Use constant wavelength grid
     ZP = PC.evaluateZP();  % Uses Obj.Lambda, Obj.EXPTIME, Obj.NCOADD, Obj.APERTURE

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

    properties (Constant)
        % Wavelength grids (2 nm step)
        Lambda = (300:2:1100)'      % Transmission wavelength grid [nm] for model evaluation (401 points)
        SpecWvl = (336:2:1020)'     % Calibrator spectra wavelength grid [nm] (default: Gaia DR3 XP, 343 points)
    end

    properties
     
        % Calibration metadata (FITS header naming convention)
        AIRMASS                 % Airmass
        ZENITH                  % Zenith angle [deg]
        TEMP                    % Temperature [C]
        PRESSURE                % Atmospheric pressure [mbar]
        HUMIDITY                % Relative humidity [%]
        APERTURE                % Telescope aperture area [m^2]
        EXPTIME                 % Exposure time [s]
        NCOADD                  % Number of coadded images
        
        % Calibration scheme configuration
        FunList                 % Built transmission function list (struct array from predefSeqCompositeFun)
        OptSeq                  % Built optimization sequence (struct from predefSeqCompositeFun)
    
        % Transmission model
        TransModel              % CompositeFun transmission model object containing:
                                %   Before calibration: .Funs (function list with initial parameters), .FunOperator ('*'),
                                %                        .Tran2DObj (position-dependent correction object), .UseTran2D (true/false)
                                %   After calibration:  .Funs.Par (fitted parameters), .RMS (fit RMS [mag]), .Chi2 (chi-squared), .DOF (degrees of freedom)

        % Calibrator information
        CalibData               % Structure with calibrator data from selectCalibrators containing:
                                %   .Spec [N_calib x N_wvl] - Calibrator spectra flux (Gaia DR3 XP)
                                %   .SpecErr [N_calib x N_wvl] - Calibrator spectra flux errors
                                %   .ObsData - struct with .Flux, .FluxErr, .X, .Y, .RA, .Dec (observed data)
                                %   .CalData - struct with .RA, .Dec (positions in the catalog of spectra)
                                %   .MatchDistance [N_calib x 1] - Match distances [arcsec]
                                %   .NumMatches - Total number of matched calibrators

    end

    methods % Constructor
        function Obj = PhotCalibTrans(Args)
            % Constructor for PhotCalibTrans class
            % Input  : * ...,key,val,...
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

            arguments
                Args.TransModel = []
                Args.FunList = []
                Args.OptSeq = []
                Args.AIRMASS = NaN
                Args.ZENITH = NaN
                Args.TEMP = NaN
                Args.PRESSURE = 965     % Default atmospheric pressure [mbar]
                Args.HUMIDITY = NaN
                Args.APERTURE = pi * (0.1397)^2    % LAST telescope aperture [m^2]
                Args.EXPTIME = NaN
                Args.NCOADD = 1         % Default number of coadded images
            end

            % Call parent constructor
      %      Obj@Component();

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
            Obj.CalibData = [];

            % Clear observation-specific metadata (restore defaults)
            Obj.AIRMASS = NaN;
            Obj.ZENITH = NaN;
            Obj.EXPTIME = NaN;
            Obj.NCOADD = 1;         % Default: single image
            Obj.TEMP = NaN;
            Obj.PRESSURE = 965;     % Default atmospheric pressure [mbar]
            Obj.HUMIDITY = NaN;

            % Keep calibration scheme configuration:
            % - FunList (transmission function list)
            % - OptSeq (optimization sequence)
            % - APERTURE (telescope aperture)
        end
    end

    methods % Core calibration methods
        function Obj = calibrate(Obj, Cat, Args)
            % Perform transmission-based photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroImage or AstroCatalog object with observed sources
            %                  Metadata is read from Cat.HeaderData (FITS header)
            %          * ...,key,val,...
            %            'TransFunList' - Cell array of transmission function names to use.
            %                             Default is {'Normalization', 'Rayleigh', 'Aerosol', 'Water',
            %                                         'Mirror', 'Corrector', 'QE_SkewedGaussian'}.
            %            'OptSeqName' - Name of optimization sequence from StageCatalog.
            %                           Default is 'DefaultLAST' (5-stage sequence from Garrappa et al. 2025).
            %            'CustomOptSeq' - Custom optimization sequence (overrides OptSeqName). Default is [].
            %            'RebuildScheme' - Force rebuild of FunList and OptSeq even if already stored. Default is false.
            %            'Tran2DType' - Type of 2D transformation for field corrections. Default is 'cheby1_4_xt'.
            %            'SearchRadius' - Calibrator matching radius [arcsec]. Default is 1.0.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - Obj - PhotCalibTrans object with calibration results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans();
            %          PC.calibrate(AI);
            %          PC.calibrate(Cat, 'TransFunList', {'Rayleigh', 'Aerosol', 'Mirror'});

            arguments
                Obj
                Cat                    % AstroImage or AstroCatalog
                Args.TransFunList = {'Normalization', 'Rayleigh', 'Aerosol', 'Ozone', 'Water', 'UMG', 'Mirror', 'Corrector', 'QE_SkewedGaussian', 'QE_Legendre'}
                Args.OptSeqName = 'DefaultLAST'
                Args.CustomOptSeq = []
                Args.RebuildScheme logical = false
                Args.Tran2DType = 'cheby1_4_xt'
                Args.SearchRadius = 1.0
                Args.MagRange = [12 16]
                Args.Verbose logical = true
            end

            if Args.Verbose
                fprintf('\n=== PhotCalibTrans Calibration ===\n\n');
            end

            % ====================================================================
            % STEP 0: Extract metadata from FITS header
            % ====================================================================

            % Extract metadata from Cat.HeaderData with defaults for missing values
            Metadata = struct();
            Metadata.AIRMASS  = Cat.HeaderData.getVal('AIRMASS', 'Fill', 1.2);
            Metadata.TEMP     = Cat.HeaderData.getVal('TEMP_MNT', 'Fill', 15);    % Temperature from TEMP_MNT keyword
            Metadata.EXPTIME  = Cat.HeaderData.getVal('EXPTIME', 'Fill', 20);
            Metadata.NCOADD   = Cat.HeaderData.getVal('NCOADD', 'Fill', 1);
            Metadata.PRESSURE = Cat.HeaderData.getVal('PRESSURE', 'Fill', 965);

            % Calculate derived fields
            Metadata.ZENITH = acosd(1.0 / Metadata.AIRMASS);

            % Use setProps to copy all metadata fields to object properties
            Obj.setProps(Metadata);

            if Args.Verbose
                fprintf('Metadata from FITS header:\n');
                fprintf('  AIRMASS  = %.2f\n', Obj.AIRMASS);
                fprintf('  ZENITH   = %.2f deg\n', Obj.ZENITH);
                fprintf('  EXPTIME  = %.1f s\n', Obj.EXPTIME);
                fprintf('  NCOADD   = %d\n', Obj.NCOADD);
                fprintf('  TEMP     = %.1f C\n', Obj.TEMP);
                fprintf('  PRESSURE = %.1f mbar\n\n', Obj.PRESSURE);
            end

            % ====================================================================
            % STEP 1: Build or reuse calibration scheme (FunList and OptSeq)
            % ====================================================================

            if Args.Verbose
                fprintf('Step 1: Preparing calibration scheme...\n');
            end

            % Determine if we need to rebuild the scheme
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
                        fprintf('  Using custom optimization sequence (%d stages)\n', numel(OptSeq));
                    else
                        fprintf('  Using optimization sequence: %s (%d stages)\n', ...
                                Args.OptSeqName, numel(OptSeq));
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

            % ====================================================================
            % STEP 3: Fit transmission parameters
            % ====================================================================

            if Args.Verbose
                fprintf('\nStep 3: Fitting transmission parameters...\n');
            end

            % Use pre-computed wavelength grids from constant properties:
            % Lambda: Transmission wavelength grid for evaluating transmission model (300:2:1100 nm)
            % SpecWvl: Wavelength grid where calibrator reference spectra are defined (default: Gaia DR3 XP, 336-1020 nm)

            % Extract data for fitting
            Flux = CalibData.ObsData.Flux;
            X = CalibData.ObsData.X;
            Y = CalibData.ObsData.Y;

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.EXPTIME / Obj.NCOADD;

            % Setup CostArgs for TransmissionMode
            CostArgs = struct(...
                'WeightMatrix', CalibData.Spec', ... % Calibrator reference spectra [N_wvl x N_cal] (transposed)
                'TransmissionMode', true, ...
                'CalibWavelength', Obj.SpecWvl, ...  % Wavelength grid for calibrator spectra (default: Gaia DR3 XP)
                'ExpTime', ExpTime_eff, ...          % Effective exposure time per image [s]
                'Aperture_area_m2', Obj.APERTURE);

            % Fit transmission parameters using multi-stage optimization
            % Obj.Lambda is used to evaluate transmission and integrate predicted fluxes
            [Model, FitRes] = Model.fitPar(Obj.Lambda, Flux, ...
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

            % Store the fitted model (RMS, Chi2, DOF stored in Model properties)
            Obj.TransModel = Model;

            if Args.Verbose
                fprintf('  Calibration complete!\n');
                fprintf('  Number of calibrators: %d\n', size(Obj.CalibData.Spec, 1));

                % Access RMS, Chi2, DOF from TransModel
                if ~isnan(Obj.TransModel.RMS)
                    fprintf('  RMS: %.4f mag\n', Obj.TransModel.RMS);
                end
                if ~isnan(Obj.TransModel.Chi2) && ~isnan(Obj.TransModel.DOF) && Obj.TransModel.DOF > 0
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

            % Match with calibrator catalog using imProc.match.match_catsHTM (default: GAIADR3spec)
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
                CalibData = struct('Spec', [], 'SpecErr', [], ...
                                   'ObsData', [], 'CalData', [], ...
                                   'MatchDistance', [], 'NumMatches', []);
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
            %       Lambda is Obj.Lambda constant property (300:2:1100 nm)

            arguments
                Obj
                Args.X = []
                Args.Y = []
            end

            % Use constant wavelength grid
            Lambda = Obj.Lambda;

            % Check that calibration has been performed
            if isnan(Obj.EXPTIME) || isnan(Obj.NCOADD)
                error('PhotCalibTrans:evaluateZP:NoMetadata', 'EXPTIME and NCOADD must be set. Run calibrate() first.');
            end

            % Calculate effective exposure time (accounting for coadding)
            ExpTime_eff = Obj.EXPTIME / Obj.NCOADD;

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
            A = tools.math.integral.trapzmat(Lambda(:)', Integrand, 2);  % [N_pos x 1]

            % Calculate zero-point flux for all positions
            TotalFlux_ZP = ExpTime_eff * Obj.APERTURE * A / B;  % [N_pos x 1]

            % Convert to magnitude
            ZP = 2.5 * log10(TotalFlux_ZP);  % [N_pos x 1]

            % If single position, return scalar
            if length(ZP) == 1
                ZP = ZP(1);
            end
        end

        function [MagAB, MagABErr] = evaluateMag(Obj, MagInst, Args)
            % Evaluate calibrated AB magnitudes from instrumental magnitudes
            % Input  : - Obj - PhotCalibTrans object
            %          - MagInst - Instrumental magnitudes [N x 1]
            %          * ...,key,val,...
            %            'X' - X coordinates [N x 1]. Default is [] (field center).
            %            'Y' - Y coordinates [N x 1]. Default is [] (field center).
            %            'MagInstErr' - Instrumental magnitude errors [N x 1]. Default is [].
            % Output : - MagAB - Calibrated AB magnitudes [N x 1]
            %          - MagABErr - Calibrated AB magnitude errors [N x 1] (optional)
            % Author : D. Kovaleva (Dec 2025)
            % Example: MagAB = PC.evaluateMag(MagInst);
            %          [MagAB, MagABErr] = PC.evaluateMag(MagInst, 'X', X, 'Y', Y, 'MagInstErr', MagErr);
            % Description: Converts instrumental magnitudes to calibrated AB magnitudes.
            %              Mag_AB = Mag_inst + ZP
            %              Uses evaluateZP to calculate position-dependent zero points.
            %              Uses Obj.Lambda constant property for wavelength grid.
            %              Error propagation: MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2)

            arguments
                Obj
                MagInst              % Instrumental magnitudes [N x 1]
                Args.X = []          % X coordinates [N x 1]
                Args.Y = []          % Y coordinates [N x 1]
                Args.MagInstErr = [] % Instrumental magnitude errors [N x 1]
            end

            % Ensure column vectors
            MagInst = MagInst(:);

            % Calculate ZP at positions (or field center if X, Y empty)
            ZP = Obj.evaluateZP('X', Args.X, 'Y', Args.Y);

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
                    NumCalib = size(Obj.CalibData.Spec, 1);
                    if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.RMS) && NumCalib > 0
                        ZP_Err = Obj.TransModel.RMS / sqrt(NumCalib);
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

            % Determine which magnitude columns to calibrate
            AllColNames = Tab.Properties.VariableNames;
            if isempty(Args.MagColNames)
                % Find all magnitude columns (MAG_*)
                MagColNames = AllColNames(startsWith(AllColNames, 'MAG_'));
            else
                % Use specified columns
                if ischar(Args.MagColNames)
                    MagColNames = {Args.MagColNames};
                else
                    MagColNames = Args.MagColNames;
                end

                % Verify columns exist
                for i = 1:length(MagColNames)
                    if ~ismember(MagColNames{i}, AllColNames)
                        error('PhotCalibTrans:addMagAB:ColumnNotFound', ...
                              'Column %s not found in catalog', MagColNames{i});
                    end
                end
            end

            if isempty(MagColNames)
                warning('PhotCalibTrans:addMagAB:NoMagCols', 'No magnitude columns found in catalog.');
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

            % Process each magnitude column
            for i = 1:length(MagColNames)
                MagColName = MagColNames{i};

                % Get instrumental magnitudes
                MagInst = Tab.(MagColName);

                % Find corresponding error column
                ErrColName = strrep(MagColName, 'MAG_', 'MAGERR_');
                if ~ismember(ErrColName, AllColNames)
                    ErrColName = [];
                end

                % Get instrumental magnitude errors if available and needed
                MagInstErr = [];
                if Args.AddErrors && ~isempty(ErrColName)
                    MagInstErr = Tab.(ErrColName);
                end

                % Calculate calibrated AB magnitudes
                if Args.AddErrors && ~isempty(MagInstErr)
                    % Call evaluateMag to get both magnitude and error
                    [MagAB, MagABErr] = Obj.evaluateMag(MagInst, ...
                                                        'X', X, 'Y', Y, ...
                                                        'MagInstErr', MagInstErr);
                else
                    % Call evaluateMag to get only magnitude
                    MagAB = Obj.evaluateMag(MagInst, 'X', X, 'Y', Y);
                end

                % Create new column name and insert into catalog
                NewMagColName = [MagColName, Args.NewColSuffix];
                Tab.(NewMagColName) = MagAB;  % Also update Tab for reference
                CatObj = CatObj.insertCol(MagAB, Inf, {NewMagColName});

                % Add calibrated magnitude error column if requested and available
                if Args.AddErrors && exist('MagABErr', 'var') && ~isempty(MagABErr)
                    NewErrColName = [ErrColName, Args.NewColSuffix];
                    Tab.(NewErrColName) = MagABErr;
                    CatObj = CatObj.insertCol(MagABErr, Inf, {NewErrColName});
                end
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
    end

   %{
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
 %}
        
    methods % Plotting methods
        function Fig = plotTransmission(Obj, Args)
            % Plot transmission curve vs wavelength
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            %            'NewFigure' - Create new figure. Default is true.
            % Output : - Fig - Figure handle
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC.plotTransmission();
            % Description: Uses Obj.Lambda (300:2:1100 nm, 401 points) for transmission evaluation.

            arguments
                Obj
                Args.NewFigure logical = true
            end

            if isempty(Obj.TransModel)
                error('PhotCalibTrans:plotTransmission:NoModel', 'TransModel not available');
            end

            % Evaluate transmission using constant wavelength grid
            Trans = Obj.evaluateTransmission(Obj.Lambda);

            % Create figure
            if Args.NewFigure
                Fig = figure;
            else
                Fig = gcf;
            end

            % Plot transmission curve
            plot(Obj.Lambda, Trans, 'LineWidth', 2);
            grid on;
            xlabel('Wavelength [nm]');
            ylabel('Transmission');
            title('Total System Transmission');
            ylim([0, max(Trans(:)) * 1.1]);

            % Add metadata to title if available
            if ~isnan(Obj.AIRMASS)
                title(sprintf('Total System Transmission (Airmass=%.2f)', Obj.AIRMASS));
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
            %              Uses Obj.Lambda (300:2:1100 nm, 401 points) for ZP calculation.

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
