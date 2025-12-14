function [FunCatalog, StageCatalog] = initCompositeFun()
    % Initialize catalog of transmission functions and optimization stages for CompositeFun
    % Package: imUtil.calib
    % Description: Provides pre-configured building blocks for CompositeFun-based
    %              transmission calibration. Users select which functions and optimization
    %              stages they need from this catalog.
    % Input  : None
    % Output : - FunCatalog: Structure with pre-configured transmission functions.
    %                        Each field is a function specification struct with:
    %                        .name, .handle, .handletype, .params, .fitpar, .paraminfo
    %                        Note: params contains only variable parameters, not metadata
    %                        (ZenithAngle_deg, Pressure, Temperature are passed via MetadataValues)
    %          - StageCatalog: Structure with pre-configured optimization stages.
    %                          Each field is an optimization stage struct with:
    %                          .stagename, .method, .freeparams, .sigmaclip, .sigmathresh, .sigmaiter, .description
    % Author : D. Kovaleva (Dec 2025)
    % Example: 
    %{
              [FunCat, StageCat] = imUtil.calib.initCompositeFun();
              % Build FunList from selected functions
              FunList = [FunCat.Rayleigh, FunCat.Ozone, FunCat.Aerosol, FunCat.Water, FunCat.QE_Legendre];
              % Create model (metadata passed separately)
              MetaValues = struct('ZenithAngle_deg', 45, 'Pressure_mbar', 965, ...
                                  'Temperature_C', 15);
              Model = tools.math.fun.CompositeFun.modelCompositeFun(FunList, ...
                  'MetadataValues', MetaValues, 'UseTran2D', true);
             
              % Example 1: Fit atmospheric parameters using pre-normalized transmission
              % Use when you have direct transmission measurements [N_lambda x 1]
              % (i.e., observed flux already normalized by reference spectra externally)
              % Residuals: (ModelTransmission - ObservedTransmission)
              Lambda = linspace(336, 1020, 343)';  % Wavelength grid [nm]
              ObsFlux = Model.evaluateAllFunParInput(Lambda);  % Simulated transmission for demo
              % In real use: ObsFlux = measured_flux ./ reference_flux (pre-normalized)
              OptSeq = [StageCat.AerosolOpt, StageCat.WaterOpt];
              [Model, FitResult] = Model.fitParCompositeFun(Lambda, ObsFlux, ...
                  'OptimizationSequence', OptSeq, 'Verbose', true);

               % Example 2: Fit transmission + field correction from photon counts
               % Use when you have photon counts [N_stars x 1] and known reference spectra
               % TransmissionMode compares: Model = Transmission × RefSpec × ExpTime × Area
               % Residuals: 2.5*log10(Predicted/Observed) [magnitudes]
               N = 50;  % Number of calibration stars
               X = rand(N,1) * 1726;  % Star X positions [pixels]
               Y = rand(N,1) * 1726;  % Star Y positions [pixels]
               ObsFlux = 8e4 + 4e4 * rand(N,1);  % Observed photon counts per star [N x 1]
               % Create reference spectra for calibrators [N_lambda x N_stars]
               RefSpec = zeros(343, N);
               for i = 1:N
                   alpha = -2 + 3.5 * (i-1)/(N-1);  % Spectral index varying -2 to +1.5
                   RefSpec(:, i) = (3e-17) ./ (Lambda / 500).^alpha;
               end
               % Setup CostArgs with TransmissionMode
               CostArgs = struct('WeightMatrix', RefSpec, 'TransmissionMode', true, ...
                   'ExpTime', 20, 'Aperture_area_m2', pi * (0.1397)^2);
               % Multi-stage optimization with field correction
               OptSeq = [StageCat.AerosolOpt, StageCat.FieldCorrection_Adapted, StageCat.WaterOpt];
               [Model, FitResult] = Model.fitParCompositeFun(Lambda, ObsFlux, ...
                   'CostArgs', CostArgs, 'X', X, 'Y', Y, 'OptimizationSequence', OptSeq, 'Verbose', true);
    %}
    %% ====================================================================
    %% NORMALIZATION FUNCTION
    %% ====================================================================

    % Normalization - constant scaling factor
    % Parameters: [Norm]
    FunCatalog.Normalization = struct();
    FunCatalog.Normalization.name = 'Normalization';
    FunCatalog.Normalization.handle = '@(Lambda, Norm) Norm * ones(size(Lambda))';
    FunCatalog.Normalization.handletype = 'anonymous';
    FunCatalog.Normalization.params = [0.3];  % Typical initial value
    FunCatalog.Normalization.fitpar = [true];  % Fit normalization factor
    FunCatalog.Normalization.paraminfo = struct(...
        'name', {'Norm'}, ...
        'description', {'Normalization factor'}, ...
        'min', {0.01}, ...
        'max', {10.0});

    %% ====================================================================
    %% ATMOSPHERIC TRANSMISSION FUNCTIONS
    %% ====================================================================

    % Rayleigh scattering
    % Parameters: [ZenithAngle_deg, Pressure_mbar]
    % Note: Both are metadata (not fitted)
    FunCatalog.Rayleigh = struct();
    FunCatalog.Rayleigh.name = 'Rayleigh';
    FunCatalog.Rayleigh.handle = '@astro.transmission.rayleighTransmission';
    FunCatalog.Rayleigh.handletype = 'named';
    FunCatalog.Rayleigh.params = [45, 965];  % [ZenithAngle_deg, Pressure_mbar]
    FunCatalog.Rayleigh.fitpar = [false, false];  % Don't fit any parameters
    FunCatalog.Rayleigh.paraminfo = struct(...
        'name', {'ZenithAngle_deg', 'Pressure_mbar'}, ...
        'description', {'Zenith angle [deg]', 'Atmospheric pressure [mbar]'}, ...
        'min', {0, 500}, ...
        'max', {90, 1100});

    % Ozone absorption
    % Parameters: [ZenithAngle_deg, DobsonUnits]
    % Note: ZenithAngle_deg is metadata (not fitted), DobsonUnits is variable
    FunCatalog.Ozone = struct();
    FunCatalog.Ozone.name = 'Ozone';
    FunCatalog.Ozone.handle = '@astro.transmission.ozoneTransmission';
    FunCatalog.Ozone.handletype = 'named';
    FunCatalog.Ozone.params = [45, 300];  % [ZenithAngle_deg, DobsonUnits]
    FunCatalog.Ozone.fitpar = [false, true];  % Don't fit zenith angle, fit ozone column
    FunCatalog.Ozone.paraminfo = struct(...
        'name', {'ZenithAngle_deg', 'DobsonUnits'}, ...
        'description', {'Zenith angle [deg]', 'Total ozone column [DU]'}, ...
        'min', {0, 200}, ...
        'max', {90, 500});

    % Aerosol extinction
    % Parameters: [ZenithAngle_deg, TauAod500, AngstromExponent]
    % Note: ZenithAngle_deg is metadata (not fitted), others are variable
    FunCatalog.Aerosol = struct();
    FunCatalog.Aerosol.name = 'Aerosol';
    FunCatalog.Aerosol.handle = '@astro.transmission.aerosolTransmission';
    FunCatalog.Aerosol.handletype = 'named';
    FunCatalog.Aerosol.params = [45, 0.05, 1.2];  % [ZenithAngle_deg, TauAod500, AngstromExponent]
    FunCatalog.Aerosol.fitpar = [false, true, false];  % Don't fit zenith, fit AOD, fix Angstrom exponent
    FunCatalog.Aerosol.paraminfo = struct(...
        'name', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
        'description', {'Zenith angle [deg]', 'Aerosol optical depth at 500nm', 'Angstrom exponent'}, ...
        'min', {0, 0, 0.5}, ...
        'max', {90, 1.0, 2.5});

    % Water vapor absorption
    % Parameters: [ZenithAngle_deg, PWV_cm, Pressure_mbar]
    % Note: ZenithAngle_deg and Pressure_mbar are metadata (not fitted), PWV_cm is variable
    FunCatalog.Water = struct();
    FunCatalog.Water.name = 'Water';
    FunCatalog.Water.handle = '@astro.transmission.waterTransmission';
    FunCatalog.Water.handletype = 'named';
    FunCatalog.Water.params = [45, 1.5, 965];  % [ZenithAngle_deg, PWV_cm, Pressure_mbar]
    FunCatalog.Water.fitpar = [false, true, false];  % Don't fit zenith/pressure, fit PWV
    FunCatalog.Water.paraminfo = struct(...
        'name', {'ZenithAngle_deg', 'PWV_cm', 'Pressure_mbar'}, ...
        'description', {'Zenith angle [deg]', 'Precipitable water vapor [cm]', 'Atmospheric pressure [mbar]'}, ...
        'min', {0, 0.1, 500}, ...
        'max', {90, 10, 1100});

    % Uniformly Mixed Gases (UMG) transmission
    % Parameters: [ZenithAngle_deg, Temperature_C, Pressure_mbar]
    % Note: All are metadata (not fitted)
    FunCatalog.UMG = struct();
    FunCatalog.UMG.name = 'UMG';
    FunCatalog.UMG.handle = '@astro.transmission.umgTransmission';
    FunCatalog.UMG.handletype = 'named';
    FunCatalog.UMG.params = [45, 15, 965];  % [ZenithAngle_deg, Temperature_C, Pressure_mbar]
    FunCatalog.UMG.fitpar = [false, false, false];  % Don't fit any parameters
    FunCatalog.UMG.paraminfo = struct(...
        'name', {'ZenithAngle_deg', 'Temperature_C', 'Pressure_mbar'}, ...
        'description', {'Zenith angle [deg]', 'Temperature [C]', 'Atmospheric pressure [mbar]'}, ...
        'min', {0, -50, 500}, ...
        'max', {90, 50, 1100});

    %% ====================================================================
    %% TELESCOPE OPTICS TRANSMISSION FUNCTIONS
    %% ====================================================================

    % Mirror reflectivity
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.Mirror = struct();
    FunCatalog.Mirror.name = 'Mirror';
    FunCatalog.Mirror.handle = '@astro.transmission.mirrorTransmission';
    FunCatalog.Mirror.handletype = 'named';
    FunCatalog.Mirror.params = [1];  % Dummy parameter
    FunCatalog.Mirror.fitpar = [false];
    FunCatalog.Mirror.paraminfo = struct(...
        'name', {'DummyParam'}, ...
        'description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'min', {1}, ...
        'max', {1});

    % Corrector transmission
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.Corrector = struct();
    FunCatalog.Corrector.name = 'Corrector';
    FunCatalog.Corrector.handle = '@astro.transmission.correctorTransmission';
    FunCatalog.Corrector.handletype = 'named';
    FunCatalog.Corrector.params = [1];  % Dummy parameter
    FunCatalog.Corrector.fitpar = [false];
    FunCatalog.Corrector.paraminfo = struct(...
        'name', {'DummyParam'}, ...
        'description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'min', {1}, ...
        'max', {1});

    %% ====================================================================
    %% DETECTOR QUANTUM EFFICIENCY FUNCTIONS
    %% ====================================================================

    % QE - Legendre polynomial model (Ofek et al. 2023)
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.QE_Legendre = struct();
    FunCatalog.QE_Legendre.name = 'QE_Legendre';
    FunCatalog.QE_Legendre.handle = '@telescope.detector.qeLegendreLAST';
    FunCatalog.QE_Legendre.handletype = 'named';
    FunCatalog.QE_Legendre.params = [1];  % Dummy parameter
    FunCatalog.QE_Legendre.fitpar = [false];
    FunCatalog.QE_Legendre.paraminfo = struct(...
        'name', {'DummyParam'}, ...
        'description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'min', {1}, ...
        'max', {1});

    % QE - Skewed Gaussian model (Garrappa et al. 2025)
    % Parameters: [Amplitude, Center_nm, Sigma_nm, Gamma]
    FunCatalog.QE_SkewedGaussian = struct();
    FunCatalog.QE_SkewedGaussian.name = 'QE_SkewedGaussian';
    FunCatalog.QE_SkewedGaussian.handle = '@telescope.detector.qeSkewedGaussianLAST';
    FunCatalog.QE_SkewedGaussian.handletype = 'named';
    FunCatalog.QE_SkewedGaussian.params = [328.1936, 570.973, 139.77, -0.1517];  % Default LAST QHY600-PH
    FunCatalog.QE_SkewedGaussian.fitpar = [false, true, false, false];  % Fit center wavelength only
    FunCatalog.QE_SkewedGaussian.paraminfo = struct(...
        'name', {'Amplitude', 'Center_nm', 'Sigma_nm', 'Gamma'}, ...
        'description', {'Amplitude', 'Peak wavelength [nm]', 'Width [nm]', 'Skewness parameter'}, ...
        'min', {200, 400, 50, -1}, ...
        'max', {500, 800, 300, 1});

    %% ====================================================================
    %% OPTIMIZATION STAGE CATALOG
    %% ====================================================================

    % Default optimization sequence (recommended for standard calibration)
    StageCatalog.Default = struct(...
        'stagename', {'NormOnly_Initial', 'NormAndCenter', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'method', {'nonlinear', 'nonlinear', 'linear', 'nonlinear', 'nonlinear'}, ...
        'freeparams', {struct('function', {'Normalization'}, 'parameter', {'Norm'}), ...
                       struct('function', {'Normalization', 'QE_SkewedGaussian'}, 'parameter', {'Norm', 'Center_nm'}), ...
                       [], ...
                       struct('function', {'Normalization'}, 'parameter', {'Norm'}), ...
                       struct('function', {'Water', 'Aerosol'}, 'parameter', {'PWV_cm', 'TauAod500'})}, ...
        'sigmaclip', {true, true, true, false, false}, ...
        'sigmathresh', {3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'sigmaiter', {3, 3, 3, 0, 0}, ...
        'description', {'Initial normalization with outlier removal', 'Optimize normalization and QE center', 'Field corrections using linear least squares', 'Refine normalization after field corrections', 'Optimize water vapor and aerosol'});

    % Individual stages for custom sequences

    % Stage 1: Initial normalization only
    StageCatalog.NormOnly_Initial = struct();
    StageCatalog.NormOnly_Initial.stagename = 'NormOnly_Initial';
    StageCatalog.NormOnly_Initial.method = 'nonlinear';
    StageCatalog.NormOnly_Initial.freeparams = struct('function', {'Normalization'}, 'parameter', {'Norm'});
    StageCatalog.NormOnly_Initial.sigmaclip = true;
    StageCatalog.NormOnly_Initial.sigmathresh = 3.0;
    StageCatalog.NormOnly_Initial.sigmaiter = 3;
    StageCatalog.NormOnly_Initial.description = 'Initial normalization with outlier removal';

    % Stage 2: Normalization + QE center
    StageCatalog.NormAndCenter = struct();
    StageCatalog.NormAndCenter.stagename = 'NormAndCenter';
    StageCatalog.NormAndCenter.method = 'nonlinear';
    StageCatalog.NormAndCenter.freeparams = struct('function', {'Normalization', 'QE_SkewedGaussian'}, 'parameter', {'Norm', 'Center_nm'});
    StageCatalog.NormAndCenter.sigmaclip = true;
    StageCatalog.NormAndCenter.sigmathresh = 3.0;
    StageCatalog.NormAndCenter.sigmaiter = 3;
    StageCatalog.NormAndCenter.description = 'Optimize normalization and QE center';

    % Stage 3: Field correction
    StageCatalog.FieldCorrection_Adapted = struct();
    StageCatalog.FieldCorrection_Adapted.stagename = 'FieldCorrection_Adapted';
    StageCatalog.FieldCorrection_Adapted.method = 'linear';
    StageCatalog.FieldCorrection_Adapted.freeparams = [];  % Empty for field correction
    StageCatalog.FieldCorrection_Adapted.sigmaclip = true;
    StageCatalog.FieldCorrection_Adapted.sigmathresh = 2.0;
    StageCatalog.FieldCorrection_Adapted.sigmaiter = 3;
    StageCatalog.FieldCorrection_Adapted.description = 'Field corrections using linear least squares';

    % Stage 4: Refined normalization
    StageCatalog.Normalization_Refined = struct();
    StageCatalog.Normalization_Refined.stagename = 'Normalization_Refined';
    StageCatalog.Normalization_Refined.method = 'nonlinear';
    StageCatalog.Normalization_Refined.freeparams = struct('function', {'Normalization'}, 'parameter', {'Norm'});
    StageCatalog.Normalization_Refined.sigmaclip = false;
    StageCatalog.Normalization_Refined.sigmathresh = 3.0;
    StageCatalog.Normalization_Refined.sigmaiter = 0;
    StageCatalog.Normalization_Refined.description = 'Refine normalization after field corrections';

    % Stage 5: Atmospheric parameters
    StageCatalog.Atmospheric = struct();
    StageCatalog.Atmospheric.stagename = 'Atmospheric';
    StageCatalog.Atmospheric.method = 'nonlinear';
    StageCatalog.Atmospheric.freeparams = struct('function', {'Water', 'Aerosol'}, 'parameter', {'PWV_cm', 'TauAod500'});
    StageCatalog.Atmospheric.sigmaclip = false;
    StageCatalog.Atmospheric.sigmathresh = 3.0;
    StageCatalog.Atmospheric.sigmaiter = 0;
    StageCatalog.Atmospheric.description = 'Optimize water vapor and aerosol';

    % Additional useful stages

    % Aerosol only
    StageCatalog.AerosolOpt = struct();
    StageCatalog.AerosolOpt.stagename = 'AerosolOpt';
    StageCatalog.AerosolOpt.method = 'nonlinear';
    StageCatalog.AerosolOpt.freeparams = struct('function', {'Aerosol'}, 'parameter', {'TauAod500'});
    StageCatalog.AerosolOpt.sigmaclip = true;
    StageCatalog.AerosolOpt.sigmathresh = 3.0;
    StageCatalog.AerosolOpt.sigmaiter = 3;
    StageCatalog.AerosolOpt.description = 'Optimize aerosol optical depth with sigma clipping';

    % Ozone only
    StageCatalog.OzoneOpt = struct();
    StageCatalog.OzoneOpt.stagename = 'OzoneOpt';
    StageCatalog.OzoneOpt.method = 'nonlinear';
    StageCatalog.OzoneOpt.freeparams = struct('function', {'Ozone'}, 'parameter', {'DobsonUnits'});
    StageCatalog.OzoneOpt.sigmaclip = true;
    StageCatalog.OzoneOpt.sigmathresh = 3.0;
    StageCatalog.OzoneOpt.sigmaiter = 3;
    StageCatalog.OzoneOpt.description = 'Optimize ozone column with sigma clipping';

    % Water vapor only
    StageCatalog.WaterOpt = struct();
    StageCatalog.WaterOpt.stagename = 'WaterOpt';
    StageCatalog.WaterOpt.method = 'nonlinear';
    StageCatalog.WaterOpt.freeparams = struct('function', {'Water'}, 'parameter', {'PWV_cm'});
    StageCatalog.WaterOpt.sigmaclip = true;
    StageCatalog.WaterOpt.sigmathresh = 3.0;
    StageCatalog.WaterOpt.sigmaiter = 3;
    StageCatalog.WaterOpt.description = 'Optimize precipitable water vapor with sigma clipping';

    % Full atmospheric (Aerosol + Ozone + Water)
    StageCatalog.AtmosphereFull = struct();
    StageCatalog.AtmosphereFull.stagename = 'AtmosphereFull';
    StageCatalog.AtmosphereFull.method = 'nonlinear';
    StageCatalog.AtmosphereFull.freeparams = struct('function', {'Aerosol', 'Ozone', 'Water'}, 'parameter', {'TauAod500', 'DobsonUnits', 'PWV_cm'});
    StageCatalog.AtmosphereFull.sigmaclip = true;
    StageCatalog.AtmosphereFull.sigmathresh = 3.0;
    StageCatalog.AtmosphereFull.sigmaiter = 3;
    StageCatalog.AtmosphereFull.description = 'Joint optimization of aerosol, ozone, and water vapor';

end
