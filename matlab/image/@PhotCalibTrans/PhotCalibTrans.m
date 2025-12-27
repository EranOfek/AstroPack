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
    %   Core Calibration:
    %     calibrate - Perform transmission-based photometric calibration using CompositeFun
    %   Getters:
    %     isValid - Check if calibration is valid (has non-NaN zero point)
    %     getZP - Get zero point at specific positions (with optional position-dependent correction)
    %     getPosParams - Get position-dependent correction parameters from TransModel.Tran2DObj
    %   Setters :
    %     setFromFitResults - Set calibration from fit results [PLACEHOLDER]
    %     reset - Reset calibration data while keeping configuration [PLACEHOLDER]   
    %   Utility Methods:
    %     selectCalibrators - Select Gaia DR3 calibrators with XP spectra for photometric calibration
    %   Evaluation Methods:
    %     evaluateTransmission - Evaluate transmission at specific positions (with position-dependent corrections)
    %     evaluateZP - Evaluate photometric zero point at specific positions
    %     evaluateMag - Evaluate calibrated AB magnitudes from instrumental magnitudes
    %   Calculation Methods:
    %     calculateChi2DOF - Calculate Chi^2 and degrees of freedom from fit results
    %   Header I/O Methods:
    %     writeToHeader - Write calibration data to AstroHeader [PLACEHOLDER]
    %     readFromHeader - Read calibration data from AstroHeader [PLACEHOLDER]
    %   Catalog Operations:
    %     addMagAB - Add calibrated AB magnitude columns to catalog [PLACEHOLDER]
    %     addMagABErr - Add calibrated AB magnitude error columns to catalog [PLACEHOLDER]
    %   Utility / Comparison Methods:
    %     compare - Compare two PhotCalibTrans objects [PLACEHOLDER]
    %     clone - Create deep copy of PhotCalibTrans object [PLACEHOLDER]
    %   Display / Output Methods:
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
    %     fromFitPar - Create PhotCalibTrans object from CompositeFun.fitPar output

    properties
        % Core calibration results
        %ZP                      % Base zero point without positional correction [mag]
        %ZP_Err                  % Zero point uncertainty [mag]

        % Quality metrics
        RMS                     % RMS of residuals [mag]
        NumCalib                % Number of calibrators used
        Chi2                    % Chi squared
        DOF                     % Number of degrees of freedom

        % Transmission model
        TransModel              % CompositeFun transmission model object with Tran2D for position-dependent corrections
        %FitResults              % Structure array with per-stage fit results

        % Calibration metadata
        AirMass                 % Airmass
        ZenithAngle             % Zenith angle [deg]
        ExpTime                 % Exposure time [s]
        Temperature             % Temperature [C]
        Pressure                % Atmospheric pressure [mbar]
        Humidity                % Relative humidity [%]
        ApertureArea_m2         % Telescope aperture area [m^2]

        % Calibrator information
        %CalibTable              % Table with the data on calibrators

        % Configuration used
        %TransFuns               % Transmission function list (from predefSeqCompositeFun)
        %OptSeq                  % Optimization sequence (from predefSeqCompositeFun)
        %SearchRadius            % Gaia matching radius [arcsec]
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
            %            'RMS' - RMS of residuals [mag]. Default is NaN.
            %            'NumCalib' - Number of calibrators. Default is 0.
            %            'Chi2' - Chi squared value. Default is NaN.
            %            'DOF' - Degrees of freedom. Default is NaN.
            %            'TransModel' - CompositeFun transmission model. Default is [].
            %            'AirMass' - Airmass. Default is NaN.
            %            'ZenithAngle' - Zenith angle [deg]. Default is NaN.
            %            'ExpTime' - Exposure time [s]. Default is NaN.
            %            'Temperature' - Temperature [C]. Default is NaN.
            %            'Pressure' - Atmospheric pressure [mbar]. Default is NaN.
            %            'Humidity' - Relative humidity [%]. Default is NaN.
            %            'ApertureArea_m2' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2 (LAST).
            % Output : - PhotCalibTrans object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans();
            %          PC = PhotCalibTrans('RMS', 0.05, 'NumCalib', 50);

            arguments
                Args.RMS = NaN
                Args.NumCalib = 0
                Args.Chi2 = NaN
                Args.DOF = NaN
                Args.TransModel = []
                Args.AirMass = NaN
                Args.ZenithAngle = NaN
                Args.ExpTime = NaN
                Args.Temperature = NaN
                Args.Pressure = NaN
                Args.Humidity = NaN
                Args.ApertureArea_m2 = pi * (0.1397)^2
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

    methods % Core calibration
        % buildCompositeFun

        % fitTransmission(Obj, AI)

        % 
    end

    methods % utility functions
        function CalibData = selectCalibrators(Obj, Cat, Args)
            % Select Gaia DR3 calibrators with XP spectra for photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - Cat - AstroImage or AstroCatalog object with LAST sources
            %          * ...,key,val,...
            %            'SearchRadius' - Gaia matching radius [arcsec]. Default is 1.0.
            %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
            %            'MinSN' - Minimum S/N for calibrators. Default is 5.
            %            'MaxSN' - Maximum S/N for calibrators. Default is 1000.
            %            'FilterBadFlags' - Apply FLAGS quality filtering. Default is true.
            %            'FluxColName' - LAST flux column name. Default is 'FLUX_APER_3'.
            %            'Verbose' - Enable verbose output. Default is true.
            % Output : - CalibData - Structure with calibrator data:
            %                        .Spec - Gaia XP spectra [N x WvlPoints]
            %                        .SpecErr - Gaia XP spectra errors [N x WvlPoints]
            %                        .Lambda - Wavelength grid [nm]
            %                        .LASTData - Structure with LAST catalog data:
            %                          .Flux, .FluxErr, .X, .Y, .RA, .Dec
            %                        .GaiaData - Structure with Gaia data:
            %                          .RA, .Dec
            %                        .MatchDistance - Matching distance [arcsec]
            %                        .NumMatches - Number of matches per source
            % Author : D. Kovaleva (Dec 2025)
            % Example: CalibData = PC.selectCalibrators(Cat);
            %          CalibData = PC.selectCalibrators(AI, 'SearchRadius', 1.5, 'MagRange', [12 16]);
            % Reference: Based on findCalibrators_m.m

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

            % Constants for Gaia XP spectra columns
            FluxIni = 7;      % Start of flux values in GAIADR3spec
            FluxEnd = 349;    % End of flux values
            EFluxIni = 350;   % Start of flux errors
            EFluxEnd = 692;   % End of flux errors
            Npoint = FluxEnd - FluxIni + 1;

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
            if ismember('MAG_PSF', Tab.Properties.VariableNames)
                magFilterMask = (Tab.MAG_PSF >= Args.MagRange(1)) & (Tab.MAG_PSF <= Args.MagRange(2));
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
            % STEP 3: MATCH WITH GAIA XP SPECTRA
            % ====================================================================

            % Match with Gaia catalog using imProc.match.match_catsHTM
            [~, ~, ResInd, CatH] = imProc.match.match_catsHTM(Cat, 'GAIADR3spec', ...
                                                              'Coo', [Cat.Table.RA/RAD, Cat.Table.Dec/RAD], ...
                                                              'Radius', Args.SearchRadius, ...
                                                              'CooUnits', 'rad', ...
                                                              'RadiusUnits', 'arcsec');

            % Extract match information
            gaiaIdx_all   = ResInd.Obj2_IndInObj1;     % Index of Gaia match for each LAST source
            dist_rad_all  = ResInd.Obj2_Dist;          % Distance in radians
            nmatch_all    = ResInd.Obj2_NmatchObj1;    % Number of matches

            % Keep only rows with valid Gaia index
            idxLastMatched = find(~isnan(gaiaIdx_all));
            gaiaIdx        = double(gaiaIdx_all(idxLastMatched));
            dist_rad       = dist_rad_all(idxLastMatched);
            nmatch         = nmatch_all(idxLastMatched);

            if isempty(idxLastMatched)
                warning('PhotCalibTrans:selectCalibrators:NoMatches', ...
                        'No Gaia XP matches found within %.1f arcsec', Args.SearchRadius);
                CalibData = struct('Spec', [], 'SpecErr', [], 'Lambda', [], ...
                                   'LASTData', [], 'GaiaData', [], ...
                                   'MatchDistance', [], 'NumMatches', []);
                return;
            end

            % Extract matched tables
            LastTab = Cat.Table(idxLastMatched, :);
            GaiaTabAll = CatH.Table;
            GaiaTab = GaiaTabAll(gaiaIdx, :);
            Nmatch = height(GaiaTab);

            if Args.Verbose
                fprintf('  Found %d LAST-Gaia matched pairs\n', Nmatch);
            end

            % ====================================================================
            % STEP 4: EXTRACT XP SPECTRA AND PREPARE OUTPUT
            % ====================================================================

            % Extract XP spectra
            GaiaArr = table2array(GaiaTab);
            SpecFlux = GaiaArr(:, FluxIni:FluxEnd);      % [N x 343]
            SpecErr = GaiaArr(:, EFluxIni:EFluxEnd);     % [N x 343]

            % Get wavelength grid (stored in CatH or use standard Gaia XP grid)
            Lambda = catsHTM.xp.gaia_xp_wvl();  % Get standard Gaia XP wavelength grid

            % Extract coordinates
            Gaia_RA = GaiaArr(:, 1) * RAD;   % rad -> deg
            Gaia_Dec = GaiaArr(:, 2) * RAD;  % rad -> deg

            % Extract LAST data
            LAST_X = LastTab.X;
            LAST_Y = LastTab.Y;
            LAST_RA = LastTab.RA;
            LAST_Dec = LastTab.Dec;

            % Extract flux and flux error
            if ismember(Args.FluxColName, LastTab.Properties.VariableNames)
                LAST_Flux = LastTab.(Args.FluxColName);
            else
                error('PhotCalibTrans:selectCalibrators:FluxColNotFound', ...
                      'Flux column %s not found in catalog', Args.FluxColName);
            end

            % Get flux error column name (replace FLUX with FLUXERR)
            FluxErrColName = strrep(Args.FluxColName, 'FLUX', 'FLUXERR');
            if ismember(FluxErrColName, LastTab.Properties.VariableNames)
                LAST_FluxErr = LastTab.(FluxErrColName);
            else
                LAST_FluxErr = sqrt(LAST_Flux);  % Use Poisson approximation
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

            % LAST data structure
            CalibData.LASTData = struct(...
                'Flux', LAST_Flux, ...
                'FluxErr', LAST_FluxErr, ...
                'X', LAST_X, ...
                'Y', LAST_Y, ...
                'RA', LAST_RA, ...
                'Dec', LAST_Dec);

            % Gaia data structure
            CalibData.GaiaData = struct(...
                'RA', Gaia_RA, ...
                'Dec', Gaia_Dec);

            % Match statistics
            CalibData.MatchDistance = Dist_arcsec;  % [N x 1]
            CalibData.NumMatches = nmatch;          % [N x 1]

            if Args.Verbose
                fprintf('Calibrator selection complete: %d matched calibrators.\n\n', Nmatch);
            end
        end
    end

    methods % evaluation
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
            %            'ExpTime' - Exposure time [s]. Default is Obj.ExpTime.
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
                Obj.ExpTime = Args.ExpTime;
            elseif isnan(Obj.ExpTime)
                error('PhotCalibTrans:evaluateZP:NoExpTime', 'ExpTime not available in object or arguments');
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
            TotalFlux_ZP = Obj.ExpTime * Args.ApertureArea_m2 * A / B;  % [N_pos x 1]

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
            %            'ExpTime' - Exposure time [s]. Default is Obj.ExpTime.
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
                    if ~isnan(Obj.RMS) && Obj.NumCalib > 0
                        ZP_Err = Obj.RMS / sqrt(Obj.NumCalib);
                    else
                        ZP_Err = 0;
                    end

                    % Error propagation: MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2)
                    MagABErr = sqrt(MagInstErr.^2 + ZP_Err^2);
                end
            end
        end
    end


    methods

        function Obj = calibrate(Obj, CalibData, TransFuns, OptSeq, Metadata, Args)
            % Perform transmission-based photometric calibration
            % Input  : - Obj - PhotCalibTrans object
            %          - CalibData - Structure with calibrator data:
            %                        .Lambda - Wavelength grid [nm]
            %                        .Spec - Gaia XP spectra [N x WvlPoints]
            %                        .SpecErr - Gaia XP spectra errors [N x WvlPoints]
            %                        .LASTData.Flux - LAST fluxes [N x 1]
            %                        .LASTData.FluxErr - LAST flux errors [N x 1]
            %                        .LASTData.X, Y - Positions [N x 1]
            %                        .LASTData.RA, Dec - Coordinates [rad] [N x 1]
            %          - TransFuns - Transmission function list from predefSeqCompositeFun
            %          - OptSeq - Optimization sequence from predefSeqCompositeFun
            %          - Metadata - Structure with observation metadata:
            %                       .AIRMASS, .EXPTIME, .TEMP
            %          * ...,key,val,...
            %            'WvlRange_nm' - Wavelength range for ZP calculation [nm]. Default is [300, 1100].
            %            'Pressure_mbar' - Atmospheric pressure [mbar]. Default is 965.
            %            'ApertureArea_m2' - Telescope aperture area [m^2]. Default is pi*(0.1397)^2.
            %            'Tran2DType' - Type of 2D transformation. Default is 'cheby1_4_xt'.
            % Output : - Obj - PhotCalibTrans object with calibration results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans();
            %          PC.calibrate(CalibData, FunList, OptSeq, Metadata);

            arguments
                Obj
                CalibData struct
                TransFuns
                OptSeq
                Metadata struct
                Args.WvlRange_nm = [300, 1100]
                Args.Pressure_mbar = 965
                Args.ApertureArea_m2 = pi * (0.1397)^2
                Args.Tran2DType = 'cheby1_4_xt'
            end

            % Extract calibrator data
            Spec = CalibData.Spec;
            Lambda = CalibData.Lambda;  % Wavelength grid from Gaia XP
            Flux = CalibData.LASTData.Flux;
            FluxErr = CalibData.LASTData.FluxErr;
            X = CalibData.LASTData.X;
            Y = CalibData.LASTData.Y;

            % Store metadata (calculate zenith angle from airmass once)
            Obj.AirMass = Metadata.AIRMASS;
            Obj.ZenithAngle = acosd(1.0 / Metadata.AIRMASS);
            Obj.ExpTime = Metadata.EXPTIME;
            Obj.Temperature = Metadata.TEMP;
            Obj.Pressure = Args.Pressure_mbar;
            Obj.ApertureArea_m2 = Args.ApertureArea_m2;

            % Build metadata values for transmission model
            MetaValues = struct(...
                'ZenithAngle_deg', Obj.ZenithAngle, ...
                'Pressure_mbar', Obj.Pressure, ...
                'Temperature_C', Obj.Temperature);

            % Build CompositeFun transmission model
            Model = tools.math.fun.CompositeFun.model(TransFuns, ...
                'MetadataValues', MetaValues, ...
                'UseTran2D', true, ...
                'Tran2DType', Args.Tran2DType);

            % Setup CostArgs for transmission mode
            CostArgs = struct(...
                'WeightMatrix', Spec, ...
                'TransmissionMode', true, ...
                'GaiaWavelength', Lambda, ...
                'ExpTime', Obj.ExpTime, ...
                'Aperture_area_m2', Args.ApertureArea_m2);

            % Fit transmission parameters using multi-stage optimization
            [Model, FitRes] = Model.fitPar(Lambda, Flux, ...
                'X', X, 'Y', Y, ...
                'CostArgs', CostArgs, ...
                'OptimizationSequence', OptSeq);

            % Calculate zero point at field center (uses Obj.ExpTime already set above)
            CalcZP = Obj.evaluateZP(Lambda, 'ApertureArea_m2', Args.ApertureArea_m2);

            % Calculate RMS from last fit stage
            if ~isempty(FitRes) && isfield(FitRes(end), 'RMS')
                CalcRMS = FitRes(end).RMS;
            else
                CalcRMS = NaN;
            end

            % Store calibration results in object
            Obj.ZP = CalcZP;
            Obj.RMS = CalcRMS;
            Obj.NumCalib = numel(Flux);
            Obj.TransModel = Model;
            Obj.FitResults = FitRes;

            % Store configuration used
            Obj.TransFuns = TransFuns;
            Obj.OptSeq = OptSeq;

            % Calculate magnitude uncertainties and Chi2/DOF
            % Convert flux errors to magnitude errors (error propagation: d(mag)/d(flux) = -1.086/flux)
            MagErr = 1.086 * FluxErr(:) ./ Flux(:);
            [Obj.Chi2, Obj.DOF] = Obj.calculateChi2DOF('MagErr', MagErr);

            % Calculate ZP uncertainty (if available from fit)
            if ~isempty(FitRes) && isfield(FitRes(end), 'ZP_Err')
                Obj.ZP_Err = FitRes(end).ZP_Err;
            else
                % Estimate from RMS and number of calibrators
                Obj.ZP_Err = CalcRMS / sqrt(Obj.NumCalib);
            end
        end
    end

    methods % Getters / Query methods
        function Result = isValid(Obj)
            % Check if calibration is valid
            % Input  : - Obj - PhotCalibTrans object
            % Output : - Result - True if calibration has valid zero point
            % Author : D. Kovaleva (Dec 2025)
            % Example: IsValid = PC.isValid();

            Result = ~isnan(Obj.ZP) && isfinite(Obj.ZP);
        end

        function ZP = getZP(Obj, X, Y, Args)
            % Get zero point at specific positions (with position-dependent correction)
            % Input  : - Obj - PhotCalibTrans object
            %          - X - X coordinates [N x 1] or []
            %          - Y - Y coordinates [N x 1] or []
            %          * ...,key,val,...
            %            'ApplyPosCorrection' - Apply position-dependent correction. Default is true.
            % Output : - ZP - Zero point(s) [N x 1 or scalar]
            % Author : D. Kovaleva (Dec 2025)
            % Example: ZP = PC.getZP([], []);  % Base ZP
            %          ZP = PC.getZP(X, Y);    % ZP with position-dependent correction

            arguments
                Obj
                X = []
                Y = []
                Args.ApplyPosCorrection logical = true
            end

            % Base zero point
            ZP = Obj.ZP;

            % Apply position-dependent correction if requested and available
            if Args.ApplyPosCorrection && ~isempty(X) && ~isempty(Y)
                PosParams = Obj.getPosParams();
                if ~isempty(PosParams) && any(PosParams(:) ~= 0)
                    Coords = [X(:), Y(:)];
                    PosCorr = telescope.optics.fieldCorrectionLAST(Coords, PosParams);   %%%% ????
                    ZP = ZP + PosCorr;
                end
            end
        end

        function PosParams = getPosParams(Obj)
            % Get position-dependent correction parameters from TransModel
            % Input  : - Obj - PhotCalibTrans object
            % Output : - PosParams - Position-dependent correction parameters [Nx10] or []
            % Author : D. Kovaleva (Dec 2025)
            % Example: FP = PC.getPosParams();

            PosParams = [];
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.Tran2DObj)
                PosParams = Obj.TransModel.Tran2DObj.ParX;
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
            %          - CatObj - AstroCatalog object with instrumental magnitudes
            %          * ...,key,val,...
            %            'MagColNames' - Instrumental magnitude column names. Default is all MAG_* columns.
            %            'NewColSuffix' - Suffix for new calibrated columns. Default is '_AB'.
            %            'ApplyPosCorrection' - Apply position-dependent corrections. Default is true.
            % Output : - CatObj - AstroCatalog with added calibrated AB magnitude columns
            %                     (e.g., MAG_APER_1 → MAG_APER_1_AB)
            % Author : D. Kovaleva (Dec 2025)
            % Example: Cat = PC.addMagAB(Cat);
            %          Cat = PC.addMagAB(Cat, 'NewColSuffix', '_CAL');
            % Description: Creates new columns with calibrated AB magnitudes = instrumental + ZP.
            %              Preserves original instrumental magnitude columns.
            %              Applies position-dependent corrections if available.

            % TODO: Implement
        end

        function CatObj = addMagABErr(Obj, CatObj, Args)
            % Add calibrated AB magnitude error columns to catalog
            % Input  : - Obj - PhotCalibTrans object
            %          - CatObj - AstroCatalog object with magnitude error columns
            %          * ...,key,val,...
            %            'MagErrColNames' - Magnitude error column names. Default is all MAGERR_* columns.
            %            'NewColSuffix' - Suffix for new calibrated error columns. Default is '_AB'.
            % Output : - CatObj - AstroCatalog with added calibrated AB magnitude error columns
            %                     (e.g., MAGERR_APER_1 → MAGERR_APER_1_AB)
            % Author : D. Kovaleva (Dec 2025)
            % Example: Cat = PC.addMagABErr(Cat);
            % Description: Creates new columns with calibrated AB errors including ZP uncertainty.
            %              Error propagation: MagErr_AB = sqrt(MagErr_inst^2 + ZP_Err^2).
            %              Preserves original instrumental error columns.

            % TODO: Implement
        end
    end

    methods % Setter / Validation methods
        function Obj = setFromFitResults(Obj, FitRes, Metadata, Args)
            % Set calibration from fit results
            % Input  : - Obj - PhotCalibTrans object
            %          - FitRes - Structure from CompositeFun.fitPar output
            %          - Metadata - Structure with observation metadata
            %          * ...,key,val,...
            % Output : - Obj - PhotCalibTrans object with calibration data
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.setFromFitResults(FitRes, Metadata);

            % TODO: Implement
        end

        function Obj = reset(Obj, Args)
            % Reset calibration data while keeping configuration
            % Input  : - Obj - PhotCalibTrans object
            %          * ...,key,val,...
            % Output : - Obj - PhotCalibTrans object with cleared results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.reset();

            % TODO: Implement
        end
    end

    methods % Display / Output methods
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
            if ~isnan(Obj.ZP)
                fprintf('Zero Point: %.4f ± %.4f mag (AB)\n', Obj.ZP, Obj.ZP_Err);
            else
                fprintf('Zero Point: Not calibrated\n');
            end

            if ~isnan(Obj.RMS)
                fprintf('RMS: %.4f mag\n', Obj.RMS);
            end

            fprintf('Calibrators: %d\n', Obj.NumCalib);

            if ~isnan(Obj.Chi2) && ~isnan(Obj.DOF)
                fprintf('Chi2/DOF: %.2f / %d = %.3f\n', Obj.Chi2, Obj.DOF, Obj.Chi2/Obj.DOF);
            end

            if ~isnan(Obj.AirMass)
                fprintf('Airmass: %.3f\n', Obj.AirMass);
            end

            if ~isempty(Obj.TransModel)
                fprintf('Transmission Model: Available\n');
            end

            PosParams = Obj.getPosParams();
            if ~isempty(PosParams) && any(PosParams(:) ~= 0)
                fprintf('Position-dependent Corrections: Available (max: %.4f mag)\n', max(abs(PosParams(:))));
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

        function Obj = fromFitPar(ResFit, Metadata)
            % Create PhotCalibTrans object from CompositeFun.fitPar results
            % Input  : - ResFit - Structure with fit results containing:
            %                     .ZP, .ZP_Err, .RMS, .NumCalibrators, .Model,
            %                     .FitResults, .CalibratorTable, .Chi2, .DOF (optional)
            %          - Metadata - Structure with observation metadata
            % Output : - Obj - PhotCalibTrans object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalibTrans.fromFitPar(ResFit, Metadata);

            arguments
                ResFit struct
                Metadata struct
            end

            Obj = PhotCalibTrans();
            Obj.ZP = ResFit.ZP;
            Obj.ZP_Err = ResFit.ZP_Err;
            Obj.RMS = ResFit.RMS;
            Obj.NumCalib = ResFit.NumCalibrators;
            Obj.TransModel = ResFit.Model;
            Obj.FitResults = ResFit.FitResults;
            Obj.CalibTable = ResFit.CalibratorTable;

            % Get Chi2 and DOF - either from ResFit or calculate from FitResults
            if isfield(ResFit, 'Chi2') && isfield(ResFit, 'DOF')
                Obj.Chi2 = ResFit.Chi2;
                Obj.DOF = ResFit.DOF;
            else
                % Calculate from FitResults and TransModel
                [Obj.Chi2, Obj.DOF] = Obj.calculateChi2DOF();
            end

            % Extract metadata
            if isfield(Metadata, 'AIRMASS')
                Obj.AirMass = Metadata.AIRMASS;
                Obj.ZenithAngle = acosd(1.0 / Obj.AirMass);  
            end
            if isfield(Metadata, 'EXPTIME')
                Obj.ExpTime = Metadata.EXPTIME;
            end
            if isfield(Metadata, 'TEMP')
                Obj.Temperature = Metadata.TEMP;
            end
        end
    end
end
