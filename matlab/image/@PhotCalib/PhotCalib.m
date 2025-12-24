classdef PhotCalib < Component
    % PhotCalib - This class provides container for absolute calibration data and basic functionality for
    %             absolute photometric calibration.
    % Description: Stores and manages photometric calibration data.
    %              Wraps CompositeFun methods and provides header/catalog operations.
    % Author : D. Kovaleva (Dec 2025)
    % Example: 
    %{
     PC = PhotCalib();
     [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();
     TransFuns = [FunCat.Rayleigh, FunCat.Aerosol, FunCat.Mirror];
     Model = tools.math.fun.CompositeFun.model(TransFuns);
     PC.TransModel = Model;
     [Trans, Wvl] = PC.getTransmission();
    
    %}
    %          PC.calibrate(CalibData, FunList, OptSeq, Metadata);
    %
    % Methods:
    %   Constructor:
    %     PhotCalib - Constructor for PhotCalib class
    %   Core Calibration:
    %     calibrate - Perform transmission-based photometric calibration using CompositeFun
    %   Getters:
    %     isValid - Check if calibration is valid (has non-NaN zero point)
    %     getZP - Get zero point at specific positions (with optional position-dependent correction)
    %     getPosParams - Get position-dependent correction parameters from TransModel.Tran2DObj
    %     getTransmission - Compute base transmission (without position correction) on wavelength grid
    %   Setters :
    %     setFromFitResults - Set calibration from fit results [PLACEHOLDER]
    %     reset - Reset calibration data while keeping configuration [PLACEHOLDER]   
    %   Calculation Methods:
    %     calculateChi2DOF - Calculate Chi^2 and degrees of freedom from fit results
    %   Header I/O Methods:
    %     writeToHeader - Write calibration data to AstroHeader [PLACEHOLDER]
    %     readFromHeader - Read calibration data from AstroHeader [PLACEHOLDER]
    %   Catalog Operations:
    %     addMagAB - Add calibrated AB magnitude columns to catalog [PLACEHOLDER]
    %     addMagABErr - Add calibrated AB magnitude error columns to catalog [PLACEHOLDER]
    %   Utility / Comparison Methods:
    %     compare - Compare two PhotCalib objects [PLACEHOLDER]
    %     clone - Create deep copy of PhotCalib object [PLACEHOLDER]
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
    %     fromHeader - Create PhotCalib object from AstroHeader [PLACEHOLDER]
    %     fromFitPar - Create PhotCalib object from CompositeFun.fitPar output

    properties
        % Core calibration results
        ZP                      % Base zero point without positional correction [mag]
        ZP_Err                  % Zero point uncertainty [mag]

        % Quality metrics
        RMS                     % RMS of residuals [mag]
        NumCalib                % Number of calibrators used
        Chi2                    % Chi squared
        DOF                     % Number of degrees of freedom

        % Transmission model
        TransModel              % CompositeFun transmission model object with Tran2D for position-dependent corrections
        FitResults              % Structure array with per-stage fit results

        % Calibration metadata
        AirMass                 % Airmass
        ZenithAngle             % Zenith angle [deg]
        ExpTime                 % Exposure time [s]
        Temperature             % Temperature [C]
        Pressure                % Atmospheric pressure [mbar]

        % Calibrator information
        CalibTable              % Table with the data on calibrators

        % Configuration used
        TransFuns               % Transmission function list (from predefSeqCompositeFun)
        OptSeq                  % Optimization sequence (from predefSeqCompositeFun)
        SearchRadius            % Gaia matching radius [arcsec]
        MagRange                % Calibrator magnitude range [min max]

        % Transmission output
        TransFile               % Filename for saved base transmission
        TransWvl                % Wavelength grid for transmission [nm]
        TransValues             % Base transmission values (without position correction)
    end

    methods % Constructor
        function Obj = PhotCalib(Args)
            % Constructor for PhotCalib class
            % Input  : * ...,key,val,...
            %            'ZP' - Base zero point [mag]. Default is NaN.
            %            'ZP_Err' - Zero point uncertainty [mag]. Default is NaN.
            %            'TransModel' - CompositeFun transmission model. Default is [].
            % Output : - PhotCalib object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalib();
            %          PC = PhotCalib('ZP', 25.5, 'ZP_Err', 0.02);

            arguments
                Args.ZP = NaN
                Args.ZP_Err = NaN
                Args.RMS = NaN
                Args.NumCalib = 0
                Args.Chi2 = NaN
                Args.DOF = NaN
                Args.TransModel = []
                Args.FitResults = []
                Args.AirMass = NaN
                Args.ZenithAngle = NaN
                Args.ExpTime = NaN
                Args.Temperature = NaN
                Args.Pressure = NaN
                Args.CalibTable = []
                Args.TransFuns = []
                Args.OptSeq = []
                Args.SearchRadius = NaN
                Args.MagRange = []
                Args.TransFile = ''
                Args.TransWvl = []
                Args.TransValues = []
            end

            % Call parent constructor
            Obj@Component();

            % Initialize properties
            Obj.ZP = Args.ZP;
            Obj.ZP_Err = Args.ZP_Err;
            Obj.RMS = Args.RMS;
            Obj.NumCalib = Args.NumCalib;
            Obj.Chi2 = Args.Chi2;
            Obj.DOF = Args.DOF;
            Obj.TransModel = Args.TransModel;
            Obj.FitResults = Args.FitResults;
            Obj.AirMass = Args.AirMass;
            Obj.ZenithAngle = Args.ZenithAngle;
            Obj.ExpTime = Args.ExpTime;
            Obj.Temperature = Args.Temperature;
            Obj.Pressure = Args.Pressure;
            Obj.CalibTable = Args.CalibTable;
            Obj.TransFuns = Args.TransFuns;
            Obj.OptSeq = Args.OptSeq;
            Obj.SearchRadius = Args.SearchRadius;
            Obj.MagRange = Args.MagRange;
            Obj.TransFile = Args.TransFile;
            Obj.TransWvl = Args.TransWvl;
            Obj.TransValues = Args.TransValues;
        end
    end

    methods % Core calibration
        function Obj = calibrate(Obj, CalibData, TransFuns, OptSeq, Metadata, Args)
            % Perform transmission-based photometric calibration
            % Input  : - Obj - PhotCalib object
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
            % Output : - Obj - PhotCalib object with calibration results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalib();
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
                'ExpTime', Metadata.EXPTIME, ...
                'Aperture_area_m2', Args.ApertureArea_m2);

            % Fit transmission parameters using multi-stage optimization
            [Model, FitRes] = Model.fitPar(Lambda, Flux, ...
                'X', X, 'Y', Y, ...
                'CostArgs', CostArgs, ...
                'OptimizationSequence', OptSeq);

            % Calculate zero point
            CalcZP = Obj.calculateZeroPoint(Model, Lambda, Metadata.EXPTIME, Args.ApertureArea_m2);

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
            % Input  : - Obj - PhotCalib object
            % Output : - Result - True if calibration has valid zero point
            % Author : D. Kovaleva (Dec 2025)
            % Example: IsValid = PC.isValid();

            Result = ~isnan(Obj.ZP) && isfinite(Obj.ZP);
        end

        function ZP = getZP(Obj, X, Y, Args)
            % Get zero point at specific positions (with position-dependent correction)
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
            % Output : - PosParams - Position-dependent correction parameters [Nx10] or []
            % Author : D. Kovaleva (Dec 2025)
            % Example: FP = PC.getPosParams();

            PosParams = [];
            if ~isempty(Obj.TransModel) && ~isempty(Obj.TransModel.Tran2DObj)
                PosParams = Obj.TransModel.Tran2DObj.ParX;
            end
        end

        function [Trans, Lambda] = getTransmission(Obj, Args)
            % Compute base transmission (without position correction) on wavelength grid
            % Input  : - Obj - PhotCalib object
            %          * ...,key,val,...
            %            'WvlRange_nm' - Wavelength range [min max] [nm]. Default is [300, 1100].
            %            'WvlStep_nm' - Wavelength step [nm]. Default is 1.
            % Output : - Trans - Base transmission values [N x 1]
            %          - Lambda - Wavelength grid [nm] [N x 1]
            % Author : D. Kovaleva (Dec 2025)
            % Example: [Trans, Wvl] = PC.getTransmission();
            %          [Trans, Wvl] = PC.getTransmission('WvlRange_nm', [400, 900], 'WvlStep_nm', 0.5);

            arguments
                Obj
                Args.WvlRange_nm = [300, 1100]
                Args.WvlStep_nm = 1
            end

            if isempty(Obj.TransModel)
                error('PhotCalib:getTransmission:NoModel', 'TransModel is not available');
            end

            % Create wavelength grid
            Lambda = (Args.WvlRange_nm(1):Args.WvlStep_nm:Args.WvlRange_nm(2))';

            % Evaluate base transmission
            if ~isempty(Obj.TransModel.Tran2DObj)
                % With Tran2D: evaluate at field center (reference point)
                % ParNX(1), ParNY(1) define the reference point in pixel coordinates
                % where normalized coordinates are (0,0) and polynomial correction vanishes
                Xc = Obj.TransModel.Tran2DObj.ParNX(1);
                Yc = Obj.TransModel.Tran2DObj.ParNY(1);
                Trans = Obj.TransModel.evaluateWithPosition(Lambda, Xc, Yc);
            else
                % Without Tran2D: evaluate base transmission directly
                Trans = Obj.TransModel.evaluateAllFunParInput(Lambda);
            end
        end
    end

    methods % Calculation methods
        function [Chi2, DOF] = calculateChi2DOF(Obj, Args)
            % Calculate Chi2 and degrees of freedom from fit results
            % Input  : - Obj - PhotCalib object
            %          * ...,key,val,...
            %            'MagErr' - Magnitude uncertainties [N x 1]. If not provided,
            %                       attempts to extract from FitResults. Default is [].
            % Output : - Chi2 - Chi squared value (NaN if uncertainties not available)
            %          - DOF - Degrees of freedom
            % Author : D. Kovaleva (Dec 2025)
            % Example: [Chi2, DOF] = PC.calculateChi2DOF();
            %          [Chi2, DOF] = PC.calculateChi2DOF('MagErr', uncertainties);
            %
            % Note: Proper Chi2 requires magnitude uncertainties.
            %       Chi2 = sum((residuals / uncertainty)^2)
            %       If uncertainties are not available, returns NaN for Chi2.

            arguments
                Obj
                Args.MagErr = []
            end

            Chi2 = NaN;
            DOF = NaN;

            if isempty(Obj.FitResults) || isempty(Obj.TransModel)
                return;
            end

            % Get the last optimization stage
            LastStage = Obj.FitResults(end);

            % Extract residuals from last stage
            if ~isfield(LastStage, 'Residuals')
                return;
            end

            Residuals = LastStage.Residuals(:);
            NData = numel(Residuals);

            % Get magnitude uncertainties
            MagErr = Args.MagErr;
            if isempty(MagErr)
                % Try to extract from FitResults
                if isfield(LastStage, 'MagErr')
                    MagErr = LastStage.MagErr(:);
                elseif isfield(LastStage, 'Uncertainties')
                    MagErr = LastStage.Uncertainties(:);
                end
            end

            % Calculate Chi2 with proper uncertainties
            if ~isempty(MagErr) && all(MagErr > 0) && numel(MagErr) == NData
                Chi2 = sum((Residuals ./ MagErr).^2);
            else
                % Cannot calculate proper Chi2 without valid uncertainties
                Chi2 = NaN;
            end

            % Count total number of free parameters from transmission model
            NParams = Obj.TransModel.numFittedPar();

            % Add Tran2D parameters if present (position-dependent corrections)
            if ~isempty(Obj.TransModel.Tran2DObj)
                % Count non-zero parameters in ParX and ParY
                if ~isempty(Obj.TransModel.Tran2DObj.ParX)
                    NParams = NParams + numel(Obj.TransModel.Tran2DObj.ParX);
                end
                if ~isempty(Obj.TransModel.Tran2DObj.ParY)
                    NParams = NParams + numel(Obj.TransModel.Tran2DObj.ParY);
                end
            end

            % Calculate degrees of freedom
            DOF = NData - NParams;
        end
    end

    methods % Header I/O methods
        function HeaderObj = writeToHeader(Obj, HeaderObj, Args)
            % Write calibration data to AstroHeader
            % Input  : - Obj - PhotCalib object
            %          - HeaderObj - AstroHeader object
            %          * ...,key,val,...
            % Output : - HeaderObj - Updated AstroHeader object
            % Author : D. Kovaleva (Dec 2025)
            % Example: Header = PC.writeToHeader(Header);

            % TODO: Implement
        end

        function Obj = readFromHeader(Obj, HeaderObj, Args)
            % Read calibration data from AstroHeader
            % Input  : - Obj - PhotCalib object
            %          - HeaderObj - AstroHeader object
            %          * ...,key,val,...
            % Output : - Obj - PhotCalib object with data from header
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.readFromHeader(Header);

            % TODO: Implement
        end
    end

    methods % Catalog operations
        function CatObj = addMagAB(Obj, CatObj, Args)
            % Add calibrated AB magnitude columns to catalog
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
            %          - FitRes - Structure from CompositeFun.fitPar output
            %          - Metadata - Structure with observation metadata
            %          * ...,key,val,...
            % Output : - Obj - PhotCalib object with calibration data
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.setFromFitResults(FitRes, Metadata);

            % TODO: Implement
        end

        function Obj = reset(Obj, Args)
            % Reset calibration data while keeping configuration
            % Input  : - Obj - PhotCalib object
            %          * ...,key,val,...
            % Output : - Obj - PhotCalib object with cleared results
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PC.reset();

            % TODO: Implement
        end
    end

    methods % Utility / Comparison methods
        function Diff = compare(Obj1, Obj2, Args)
            % Compare two PhotCalib objects
            % Input  : - Obj1 - First PhotCalib object
            %          - Obj2 - Second PhotCalib object
            %          * ...,key,val,...
            % Output : - Diff - Structure with differences
            % Author : D. Kovaleva (Dec 2025)
            % Example: diff = PC1.compare(PC2);

            % TODO: Implement
            Diff = struct();
        end

        function ObjCopy = clone(Obj)
            % Create deep copy of PhotCalib object
            % Input  : - Obj - PhotCalib object
            % Output : - ObjCopy - Deep copy of object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC2 = PC1.clone();

            % TODO: Implement
            ObjCopy = Obj;
        end
    end

    methods % Display / Output methods
        function summary(Obj, Args)
            % Display photometric calibration summary
            % Input  : - Obj - PhotCalib object
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

            fprintf('\n=== PhotCalib Object ===\n');
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
            % Input  : - Obj - PhotCalib object
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

            % Compute transmission
            [Trans, Lambda] = Obj.getTransmission('WvlRange_nm', Args.WvlRange_nm, 'WvlStep_nm', Args.WvlStep_nm);

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
                    error('PhotCalib:saveTransmission:InvalidFormat', 'Format must be ''ascii'' or ''mat''');
            end
        end
    end

    methods % Plotting methods
        function Fig = plotTransmission(Obj, Args)
            % Plot transmission curve vs wavelength
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
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
            % Input  : - Obj - PhotCalib object
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
        function ZP = calculateZeroPoint(Obj, Model, Lambda, ExpTime, ApertureArea)
            % Calculate photometric zero point from optimized transmission model
            % Input  : - Obj - PhotCalib object
            %          - Model - Optimized CompositeFun transmission model
            %          - Lambda - Wavelength grid [nm] for integration
            %          - ExpTime - Exposure time [s]
            %          - ApertureArea - Telescope aperture area [m^2]
            % Output : - ZP - Base zero point [mag]
            % Author : D. Kovaleva (Dec 2025)
            %
            % Formula: ZP = 2.5*log10(ExpTime * Area * Integral(Trans * Fnu * Lambda * dLambda) / (h*c))
            % where Fnu is the AB system flux density (constant for flat spectrum)

            % Evaluate base transmission
            if ~isempty(Model.Tran2DObj)
                % With Tran2D: evaluate at field center (reference point)
                Xc = Model.Tran2DObj.ParNX(1);
                Yc = Model.Tran2DObj.ParNY(1);
                Trans = Model.evaluateWithPosition(Lambda, Xc, Yc);
            else
                % Without Tran2D: evaluate base transmission directly
                Trans = Model.evaluateAllFunParInput(Lambda);
            end

            % Create flat Fnu spectrum for AB zero-point
            Fnu = constant.Fnu('SI');  % AB system flux density [W/m^2/Hz]
            FlatSpectrum = Fnu * ones(size(Lambda));  % Flat spectrum

            % Apply transmission
            SpecTrans = FlatSpectrum(:) .* Trans(:);

            % Integrate: A = integral(SpecTrans * Lambda * dLambda)
            Integrand = SpecTrans(:) .* Lambda(:);
            A = tools.math.integral.trapzmat(Lambda(:), Integrand(:), 1);

            % Physical constants
            H = constant.h('SI');  % Planck constant [J·s]
            C = constant.c('SI');  % Speed of light [m/s]
            B = H * C * 1e9;       % H*C with nm to m conversion

            % Calculate zero-point flux
            TotalFlux_ZP = ExpTime * ApertureArea * A / B;

            % Convert to magnitude
            ZP = 2.5 * log10(TotalFlux_ZP);
        end
    end

    methods (Static)
        function Obj = fromHeader(HeaderObj, Args)
            % Create PhotCalib object from AstroHeader
      
            % TODO: Implement
            Obj = PhotCalib();
        end

        function Obj = fromFitPar(ResFit, Metadata)
            % Create PhotCalib object from CompositeFun.fitPar results
            % Input  : - ResFit - Structure with fit results containing:
            %                     .ZP, .ZP_Err, .RMS, .NumCalibrators, .Model,
            %                     .FitResults, .CalibratorTable, .Chi2, .DOF (optional)
            %          - Metadata - Structure with observation metadata
            % Output : - Obj - PhotCalib object
            % Author : D. Kovaleva (Dec 2025)
            % Example: PC = PhotCalib.fromFitPar(ResFit, Metadata);

            arguments
                ResFit struct
                Metadata struct
            end

            Obj = PhotCalib();
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
