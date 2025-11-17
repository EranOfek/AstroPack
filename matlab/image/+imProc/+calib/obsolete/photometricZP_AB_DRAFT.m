function [Result, ResFit] = photometricZP_AB_DRAFT(Obj, Args)
    % Transmission-based absolute photometric calibration using Gaia XP spectra
    % This function performs photometric calibration by applying a transmission model
    % to Gaia XP spectra and optimizing transmission parameters to match observed fluxes.
    %
    % Input  : - Obj - AstroImage or AstroCatalog object with sources
    %          * ...,key,val,...
    %            'TransmissionFun' - CompositeFun object with transmission functions.
    %                                If empty (default), loads from YAML configuration.
    %            'YAMLConfigPath' - Path to YAML configuration file.
    %                               Default is '~/matlab/AstroPack/config/CalibPhotAB.yml'.
    %            'SearchRadius' - Gaia matching radius [arcsec]. Default is 1.0.
    %            'MagRange' - Calibrator magnitude range [min max]. Default is [12 16].
    %            'FluxColName' - LAST flux column name. Default is 'FLUX_APER_3'.
    %            'UpdateHeader' - Update AstroImage header with ZP. Default is true.
    %            'UpdateMagCols' - Update magnitude columns with ZP. Default is true.
    %            'MagColName2update' - Magnitude columns to update. Default is 'MAG_'.
    %            'SignZP' - Sign of ZP to add (+1 or -1). Default is 1.
    %            'MagSys' - Magnitude system ('AB' or 'Vega'). Default is 'AB'.
    %            'MagZP' - Instrumental zero point. Default is 25.
    %            'CreateNewObj' - Copy input object. Default is false.
    %            'Verbose' - Enable verbose output. Default is true.
    %            'Plot' - Generate diagnostic plots. Default is false.
    %
    % Output : - Result - Input object (possibly with updated magnitudes)
    %          - ResFit - Structure array with calibration results per object:
    %                     .ZP - Photometric zero point
    %                     .RMS - RMS of residuals [mag]
    %                     .NumCalibrators - Number of calibrators used
    %                     .OptimizedParams - Final transmission parameter values
    %                     .CalibratorTable - Table with calibrator diagnostics
    %                     .FieldParams - Field correction parameters
    %                     .SequenceResults - Results from each optimization stage
    %                     .MagSys - Magnitude system
    %                     .LimMag - Limiting magnitude estimate
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: % Basic usage with default YAML configuration
    %          AI = io.files.load2('LAST_image.mat');
    %          [Result, ResFit] = imProc.calib.photometricZP_AB_DRAFT(AI, 'Verbose', true);
    %
    %          % Advanced usage with custom YAML config
    %          [Result, ResFit] = imProc.calib.photometricZP_AB_DRAFT(AI, ...
    %                                'YAMLConfigPath', '/path/to/custom_config.yml', ...
    %                                'SearchRadius', 1.5, ...
    %                                'Verbose', true);

    arguments
        Obj  % AstroImage or AstroCatalog

        % Transmission and optimization
        Args.TransmissionFun = []  % CompositeFun object (if empty, loads from YAML config)
        Args.YAMLConfigPath = '~/matlab/AstroPack/config/CalibPhotAB.yml'  % YAML configuration file

        % Calibrator selection
        Args.SearchRadius = 1.0  % arcsec
        Args.MagRange = [12 16]
        Args.FluxColName = 'FLUX_APER_3'

        % Catalog update
        Args.UpdateHeader logical = true
        Args.UpdateMagCols logical = true
        Args.MagColName2update = 'MAG_'
        Args.SignZP = 1
        Args.MagSys = 'AB'  % Magnitude system

        % General
        Args.CreateNewObj logical = false
        Args.Verbose logical = true
        Args.Plot logical = false
    end
tic
    % ====================================================================
    % STEP 0: VALIDATE INPUTS AND SETUP
    % ====================================================================

    if Args.Verbose
        fprintf('TRANSMISSION-BASED PHOTOMETRIC CALIBRATION\n');
    end

    % Pre-load absorption interpolants (cached for session)
    if Args.Verbose
        fprintf('Pre-loading absorption data...\n');
    end
    AbsData = astro.transmission.loadAbsorptionInterpolants('Verbose', Args.Verbose);

    % Validate input object type
    if isa(Obj, 'AstroImage')
        IsAstroImage = true;
    elseif isa(Obj, 'AstroCatalog')
        IsAstroImage = false;
    else
        error('Input must be AstroImage or AstroCatalog object');
    end

    % Copy object if requested
    if Args.CreateNewObj
        Result = Obj.copy();
    else
        Result = Obj;
    end

    Nobj = numel(Obj);

    if Args.Verbose
        fprintf('Processing %d object(s)\n', Nobj);
    end

    % Initialize output structures
    ResFit = struct('ZP', cell(1, Nobj), ...
                    'RMS', [], ...
                    'NumCalibrators', [], ...
                    'OptimizedParams', [], ...
                    'SequenceResults', [], ...
                    'LimMag', []);

    % ====================================================================
    % STEP 1: LOOP OVER OBJECTS
    % ====================================================================

    for Iobj = 1:Nobj
        if Args.Verbose
            fprintf('\n--- Processing Object %d/%d ---\n', Iobj, Nobj);
        end

        % ----------------------------------------------------------------
        % STEP 1.1: Extract observation metadata
        % ----------------------------------------------------------------

        if IsAstroImage
            % Extract from AstroImage header
            Metadata = extractMetadataFromImage(Result(Iobj), Args.Verbose);
        else
            % For AstroCatalog, metadata must be provided or use defaults
            % TODO: Add optional metadata input arguments
            error('AstroCatalog input requires metadata. Use AstroImage or provide metadata manually.');
        end

        % Pre-load airmassSMARTS (cached for session)
        % Calculate zenith angle from airmass
        if Metadata.Airmass < 1.0
            error('Invalid Airmass: %.3f (must be >= 1.0)', Metadata.Airmass);
        end
        ZenithAngle_deg = acosd(1.0 / Metadata.Airmass);
        astro.transmission.airmassSMARTS(ZenithAngle_deg);

        % ----------------------------------------------------------------
        % STEP 1.2: Build or use transmission function
        % ----------------------------------------------------------------

        if isempty(Args.TransmissionFun)
            % Build TransFun from YAML configuration
            if Args.Verbose
                fprintf('Building transmission model from YAML config...\n');
            end
            TransFun = imUtil.calib.buildTransFunFromConfig_DRAFT(...
                Args.YAMLConfigPath, Metadata, 'Verbose', Args.Verbose);
        else
            % Use provided TransFun (advanced users)
            if Args.Verbose
                fprintf('Using provided TransmissionFun object.\n');
            end
            TransFun = Args.TransmissionFun;
        end

        % ----------------------------------------------------------------
        % STEP 1.3: Find calibrators with Gaia XP spectra
        % ----------------------------------------------------------------

        if Args.Verbose
            fprintf('Finding Gaia calibrators with XP spectra...\n');
        end

        CalibData = imUtil.calib.findCalibrators(Result(Iobj), Metadata, ...
                                              'SearchRadius', Args.SearchRadius, ...
                                              'MagRange', Args.MagRange, ...
                                              'FluxColName', Args.FluxColName, ...
                                              'Verbose', Args.Verbose);

        if isempty(CalibData.Spec)
            warning('No calibrators found for object %d. Skipping.', Iobj);
            continue;
        end

        if Args.Verbose
            fprintf('Found %d calibrators with Gaia XP spectra\n', length(CalibData.Spec));
        end

        % ----------------------------------------------------------------
        % STEP 1.4: Run optimization sequence from YAML
        % ----------------------------------------------------------------

        if Args.Verbose
            fprintf('\nRunning optimization sequence from YAML config...\n');
        end

        [SequenceResults, FinalCalibData, FinalFieldParams] = imUtil.calib.runOptimizationSequence_YAML_DRAFT(...
            CalibData, TransFun, Args.YAMLConfigPath, 'Verbose', Args.Verbose);

        % ----------------------------------------------------------------
        % STEP 1.5: Calculate final zero point
        % ----------------------------------------------------------------

        % Get final optimized parameters
        FinalParams = TransFun.getAllParStruct();
        FinalStageResults = SequenceResults{end};

        if Args.Verbose
            fprintf('\n--- Calculating Zero Point ---\n');
        end

        % Load YAML config to get wavelength parameters
        YAMLConfig = yaml.ReadYaml(Args.YAMLConfigPath);
        WavelengthRange = YAMLConfig.WavelengthRange_nm;

        % Convert to numeric array if YAML returned cell array
        if iscell(WavelengthRange)
            WavelengthRange = cell2mat(WavelengthRange);
        end

        % Create wavelength array (same as in calculateCostFunction_DRAFT)
        Lambda = linspace(WavelengthRange(1), WavelengthRange(2), ...
                         round((WavelengthRange(2) - WavelengthRange(1)) / 2) + 1)';

        % Calculate zero-point flux for AB system
        % Create flat Fnu spectrum for zero-point calculation
        Fnu = constant.Fnu('SI');  % AB system flux density
        FlatSpectrum = Fnu * ones(size(Lambda));  % Flat spectrum in SI units

        % Calculate transmission function using optimized parameters
        % TransFun already contains all optimized parameters 
        ParamValues = TransFun.valuesAllPar();
        Transmission = TransFun.evaluateAllParInput(Lambda, ParamValues(:)');

        if Args.Verbose
            fprintf('Wavelength range: %.1f - %.1f nm (%d points)\n', ...
                    min(Lambda), max(Lambda), length(Lambda));
            fprintf('Transmission range: %.4f - %.4f (mean: %.4f)\n', ...
                    min(Transmission), max(Transmission), mean(Transmission));
        end

        % Apply transmission to flat spectrum
        SpecTrans_ZP = FlatSpectrum(:) .* Transmission(:);

        % Calculate total flux for zero-point directly
        % Integrand for AB system: SpecTrans / Lambda
        Integrand = SpecTrans_ZP(:) ./ Lambda(:);

        % Integrate over wavelength
        A = tools.math.integral.trapzmat(Lambda(:), Integrand(:), 1);

        % Physical constants
        H = constant.h('SI');  % Planck constant [J·s]
        B = H;  % For zero-point: B = H (not H*C as in flux conversion)

        % Get telescope aperture area
        % Default for LAST telescope
            Ageom = pi * (0.1397)^2;
      
        % Calculate zero-point flux (base, without field corrections)
        % Note: Normalization parameter is already included in Transmission
        TotalFlux_ZP_base = Ageom * A / B;

        % Convert flux to zero-point magnitude: ZP = 2.5 * log10(flux_ZP)
        ZP = 2.5 * log10(TotalFlux_ZP_base);

        % Get median residual for diagnostics (should be close to zero after good fit)
        MedianResidual = median(FinalStageResults.Residuals);

        if Args.Verbose
            fprintf('Fnu (AB): %.4e W/m^2/Hz\n', Fnu);
            fprintf('Planck H: %.4e J·s\n', H);
            fprintf('Ageom: %.4e m^2\n', Ageom);
            fprintf('Integral A: %.4e\n', A);
            fprintf('Zero-point flux (base): %.4e photons\n', TotalFlux_ZP_base);
            fprintf('Zero Point (base, no field corrections): %.4f mag\n', ZP);
            fprintf('Median Residual: %.6f mag (should be ~0 after fit)\n', MedianResidual);
            fprintf('RMS: %.4f mmag\n', FinalStageResults.RMS * 1000);
            fprintf('Calibrators: %d\n', FinalStageResults.NumCalibrators);
        end

        % ----------------------------------------------------------------
        % STEP 1.6: Apply ZP correction to catalog
        % ----------------------------------------------------------------

        if Args.UpdateMagCols
            if Args.Verbose
                fprintf('Updating magnitude columns with ZP and field corrections...\n');
            end

            Result(Iobj) = updateMagnitudeColumns(Result(Iobj), ZP, ...
                                                  Args.MagColName2update, ...
                                                  Args.SignZP, ...
                                                  FinalFieldParams, ...
                                                  Args.Verbose);
        end

        % ----------------------------------------------------------------
        % STEP 1.7: Update header (AstroImage only)
        % ----------------------------------------------------------------

        if IsAstroImage && Args.UpdateHeader
            if Args.Verbose
                fprintf('Updating image header with ZP...\n');
            end

            Result(Iobj) = updateImageHeader(Result(Iobj), ZP, ...
                                            FinalStageResults.RMS, ...
                                            FinalStageResults.NumCalibrators, ...
                                            Args.MagSys, ...
                                            Args.Verbose);
        end

        % ----------------------------------------------------------------
        % STEP 1.8: Create calibrator diagnostics table
        % ----------------------------------------------------------------

        if Args.Verbose
            fprintf('Creating calibrator diagnostics table...\n');
        end

        CalibratorTable = createCalibratorTable(FinalCalibData, TransFun, ...
                                                FinalFieldParams, FinalStageResults.Residuals, ...
                                                Args.FluxColName, Args.Verbose);

        % ----------------------------------------------------------------
        % STEP 1.9: Estimate limiting magnitude
        % ----------------------------------------------------------------

        % TODO: Implement limiting magnitude estimation
        LimMag = NaN;  % Placeholder

        % ----------------------------------------------------------------
        % STEP 1.10: Store results
        % ----------------------------------------------------------------

        ResFit(Iobj).ZP = ZP;
        ResFit(Iobj).RMS = FinalStageResults.RMS;
        ResFit(Iobj).NumCalibrators = FinalStageResults.NumCalibrators;
        ResFit(Iobj).OptimizedParams = FinalParams;
        ResFit(Iobj).FieldParams = FinalFieldParams;
        ResFit(Iobj).CalibratorTable = CalibratorTable;
        ResFit(Iobj).SequenceResults = SequenceResults;
        ResFit(Iobj).MagSys = Args.MagSys;
        ResFit(Iobj).LimMag = LimMag;

        % ----------------------------------------------------------------
        % STEP 1.11: Generate diagnostic plots (if requested)
        % ----------------------------------------------------------------

        if Args.Plot
            generateDiagnosticPlots(CalibData, FinalCalibData, SequenceResults, ZP);
        end
    end

    if Args.Verbose
        fprintf('\n===========================================\n');
        fprintf('CALIBRATION COMPLETE\n');
        fprintf('===========================================\n\n');
    end
toc    
end

%% ========================================================================
%  HELPER FUNCTION: Extract Metadata from AstroImage
%  ========================================================================

function Metadata = extractMetadataFromImage(Image, Verbose)
    % Extract observation metadata from AstroImage header

    Metadata = struct();

    % Extract airmass
    Metadata.Airmass = Image.HeaderData.getVal('AIRMASS');
    if isempty(Metadata.Airmass) || isnan(Metadata.Airmass)
        error('AIRMASS not found in image header. Required for transmission calculation.');
    end

    % Extract temperature (Celsius)
    Metadata.Temperature = Image.HeaderData.getVal('TEMP');
    if isempty(Metadata.Temperature) || isnan(Metadata.Temperature)
        Metadata.Temperature = Image.HeaderData.getVal('MNTTEMP');
    end
    if isempty(Metadata.Temperature) || isnan(Metadata.Temperature)
        error('Temperature not found in image header (TEMP or MNTTEMP). Required for transmission calculation.');
    end

    % Note: Pressure is not extracted from header - it will be read from YAML config

    if Verbose
        fprintf('Observation metadata:\n');
        fprintf('  Airmass: %.3f\n', Metadata.Airmass);
        fprintf('  Temperature: %.1f C\n', Metadata.Temperature);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Find Calibrators 
%  ========================================================================

function CalibData = findCalibratorsForCatalog(Obj, Metadata, Args)
    % Find Gaia calibrators with XP spectra for photometric calibration
    % TODO: Implement full function

    arguments
        Obj
        Metadata struct
        Args.SearchRadius = 3
        Args.MagRange = [12 16]
        Args.FluxColName = 'FLUX_APER_3'
        Args.Verbose logical = true
    end

    error(['findCalibratorsForCatalog not yet implemented. ', ...
           'This function should:\n', ...
           '1. Match catalog sources to Gaia DR3\n', ...
           '2. Filter by magnitude range and XP spectrum availability\n', ...
           '3. Retrieve Gaia XP spectra\n', ...
           '4. Return CalibData structure with Spec, Coords, LASTData, Metadata fields']);
end

%% ========================================================================
%  HELPER FUNCTION: Update Magnitude Columns
%  ========================================================================

function Obj = updateMagnitudeColumns(Obj, ZP, MagColPattern, SignZP, FieldParams, Verbose)
    % Calculate AB magnitudes from flux columns and add calibrated magnitude columns
    %
    % This function creates NEW columns with calibrated AB magnitudes from flux values.
    % Formula: MAG_AB = -2.5*log10(FLUX/Dt) + MAG_ZP
    % where MAG_ZP = ZP_base + FieldCorrection_mag (position-dependent)
    %
    % Example: FLUX_PSF -> MAG_PSF_AB, FLUX_APER_3 -> MAG_APER_3_AB

    if isa(Obj, 'AstroImage')
        Cat = Obj.CatData;
    else
        Cat = Obj;
    end

    % Get number of sources
    [NumSources, ~] = Cat.sizeCatalog();

    % Calculate field corrections if FieldParams are non-zero
    if any(FieldParams ~= 0)
        % Extract X, Y positions from catalog
        X = getCol(Cat, 'X');
        Y = getCol(Cat, 'Y');

        % Build Coords matrix [N x 2]
        Coords = [X(:), Y(:)];

        % Calculate field correction for each source (in magnitude units)
        FieldCorrectionMag = astro.transmission.fieldCorrection(Coords, FieldParams);

        if Verbose
            fprintf('  Field correction range: %.4f to %.4f mag\n', ...
                    min(FieldCorrectionMag), max(FieldCorrectionMag));
        end
    else
        % No field corrections
        FieldCorrectionMag = zeros(NumSources, 1);
    end

    % Calculate position-dependent zero-point for each source
    % MAG_ZP = ZP_base + FieldCorrection
    MAG_ZP = ZP + FieldCorrectionMag;

    % Get exposure time (default 20s for LAST)
    Dt = 20.0;  % seconds

    % Only process specific flux columns: FLUX_APER_3 and FLUX_PSF
    ColNames = Cat.ColNames;
    FluxCols = {};

    % Add FLUX_APER_3 if it exists
    if any(strcmp(ColNames, 'FLUX_APER_3'))
        FluxCols{end+1} = 'FLUX_APER_3';
    end

    % Add FLUX_PSF if it exists
    if any(strcmp(ColNames, 'FLUX_PSF'))
        FluxCols{end+1} = 'FLUX_PSF';
    end

    if Verbose
        fprintf('  Creating %d AB magnitude columns from flux columns\n', length(FluxCols));
    end

    % Create AB magnitude columns from flux
    for i = 1:length(FluxCols)
        FluxColName = FluxCols{i};

        if Cat.isColumn(FluxColName)
            % Get flux data from catalog
            FluxData = getCol(Cat, FluxColName);

            % Create magnitude column name
            % FLUX_PSF -> MAG_PSF_AB, FLUX_APER_3 -> MAG_APER_3_AB
            MagColName = strrep(FluxColName, 'FLUX', 'MAG');
            MagColName = [MagColName '_AB'];

            % Calculate AB magnitude: MAG_AB = -2.5*log10(FLUX/Dt) + MAG_ZP
            % Handle invalid flux values (<=0, NaN, Inf)
            MAG_AB = nan(NumSources, 1);
            ValidFlux = (FluxData > 0) & isfinite(FluxData);
            MAG_AB(ValidFlux) = -2.5 * log10(FluxData(ValidFlux) / Dt) + MAG_ZP(ValidFlux);

            % Add new column to catalog
            Cat = replaceCol(Cat, MAG_AB, MagColName);

            if Verbose
                fprintf('    %s -> %s (from flux)\n', FluxColName, MagColName);
            end
        end
    end

    % Add base ZP value as a column (useful for diagnostics)
    ZPArray = repmat(ZP, NumSources, 1);
    Cat = replaceCol(Cat, ZPArray, 'ZP_AB_BASE');

    % Add position-dependent ZP as a column
    Cat = replaceCol(Cat, MAG_ZP, 'ZP_AB');

    % Add field correction as a column (useful for diagnostics)
    Cat = replaceCol(Cat, FieldCorrectionMag, 'FIELD_CORR_AB');

    if Verbose
        fprintf('  Added ZP_AB_BASE column with base ZP=%.4f mag\n', ZP);
        fprintf('  Added ZP_AB column with position-dependent ZP\n');
        fprintf('  Added FIELD_CORR_AB column with field corrections\n');
    end

    % Update object
    if isa(Obj, 'AstroImage')
        Obj.CatData = Cat;
    else
        Obj = Cat;
    end
end

%% ========================================================================
%  HELPER FUNCTION: Update Image Header
%  ========================================================================

function Image = updateImageHeader(Image, ZP, RMS, NumCalib, MagSys, Verbose)
    % Update AstroImage header with photometric calibration results

    % Prepare keys and values for header update
    Keys = {'PH_ZP', 'PH_RMS', 'PH_NSRC', 'PH_MAGSY'};
    Vals = {ZP, RMS, NumCalib, MagSys};

    % Update header with all keys at once
    Image.HeaderData.replaceVal(Keys, Vals);

    if Verbose
        fprintf('  Header updated: PH_ZP=%.4f, PH_RMS=%.4f, PH_NSRC=%d, PH_MAGSY=%s\n', ...
                ZP, RMS, NumCalib, MagSys);
    end
end

%% ========================================================================
%  HELPER FUNCTION: Generate Diagnostic Plots
%  ========================================================================

function generateDiagnosticPlots(CalibData, FinalCalibData, SequenceResults, ZP)
    % Generate diagnostic plots for photometric calibration
    % TODO: Implement diagnostic plots

    fprintf('Diagnostic plots not yet implemented.\n');

    % Suggested plots:
    % 1. Residuals vs magnitude
    % 2. Residuals vs color
    % 3. Residuals vs position (X, Y)
    % 4. RMS evolution through optimization stages
    % 5. Transmission curve (final optimized)
end

%% ========================================================================
%  HELPER FUNCTION: Create Calibrator Diagnostics Table
%  ========================================================================

function CalibTable = createCalibratorTable(CalibData, TransFun, FieldParams, DiffMagRes, FluxColName, Verbose)
    % Create diagnostics table for calibrators with observed/predicted flux
    %
    % Input  : - CalibData - Final calibrator data after sigma clipping
    %          - TransFun - CompositeFun with optimized transmission parameters
    %          - FieldParams - Field correction parameters [1 x 10]
    %          - DiffMagRes - Magnitude residuals from optimization [N x 1]
    %          - FluxColName - Name of flux column (e.g., 'FLUX_APER_3')
    %          - Verbose - Enable verbose output
    %
    % Output : - CalibTable - Table with calibrator diagnostics

    % Get current transmission parameter values
    CurrentParamValues = TransFun.valuesAllPar();

    % Calculate predicted flux for calibrators using final optimized parameters
    % Note: DiffMagRes is passed in (already calculated), we only need PredictedFlux here
    [~, ~, ~, PredictedFlux] = imUtil.calib.calculateCostFunction_DRAFT(...
        CalibData, CurrentParamValues, TransFun, ...
        'FieldParams', FieldParams, 'Verbose', false);

    % Extract data from CalibData
    NumCalib = length(CalibData.Spec);

    % Get observed flux from LAST data
    ObservedFlux = CalibData.LASTData.(FluxColName);

    % Get positions
    X = CalibData.LASTData.X;
    Y = CalibData.LASTData.Y;

    % Get RA, Dec (from Coords structure array)
    % Use Gaia coordinates (the reference coordinates)
    RA = zeros(NumCalib, 1);
    Dec = zeros(NumCalib, 1);
    for i = 1:NumCalib
        RA(i) = CalibData.Coords(i).Gaia_RA;
        Dec(i) = CalibData.Coords(i).Gaia_Dec;
    end

    % Calculate field correction for each calibrator
    if any(FieldParams ~= 0)
        Coords = [X(:), Y(:)];
        FieldCorrection = astro.transmission.fieldCorrection(Coords, FieldParams);
    else
        FieldCorrection = zeros(NumCalib, 1);
    end

    % Calculate instrumental magnitudes
    MagInst = -2.5 * log10(ObservedFlux);
    MagPred = -2.5 * log10(PredictedFlux);

    % Create table with all diagnostics
    CalibTable = table(RA, Dec, X, Y, ...
                      ObservedFlux, PredictedFlux, ...
                      MagInst, MagPred, ...
                      DiffMagRes, ...
                      FieldCorrection, ...
                      'VariableNames', {'RA', 'Dec', 'X', 'Y', ...
                                       'ObservedFlux_photons', 'PredictedFlux_photons', ...
                                       'MagInst', 'MagPred', ...
                                       'DiffMagRes', ...
                                       'FieldCorrection_mag'});

    if Verbose
        fprintf('  Calibrator table created: %d sources\n', NumCalib);
        fprintf('  Columns: RA, Dec, X, Y, Observed/Predicted Flux, Magnitudes, DiffMagRes, Field Correction\n');
        fprintf('  Mean residual: %.6f mag, RMS: %.6f mag\n', mean(DiffMagRes), std(DiffMagRes));
    end
end
