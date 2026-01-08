function [FunCatalog, StageCatalog] = predefSeqCompositeFun()
    % Predefined sequences of transmission functions and optimization stages for CompositeFun
    % Package: imUtil.calib
    % Description: Provides pre-configured building blocks for CompositeFun-based
    %              transmission calibration. Users select which functions and optimization
    %              stages they need from this catalog.
    % Input  : None
    % Output : - FunCatalog: Structure with pre-configured transmission functions.
    %                        Available: Normalization, Rayleigh, Ozone, Aerosol, Water, UMG,
    %                                   Mirror, Corrector, QE_Legendre, QE_SkewedGaussian
    %          - StageCatalog: Structure with pre-configured optimization stages.
    %                          Available: NormOnly_Initial, NormAndCenter, FieldCorrection_Adapted,
    %                                     Normalization_Refined, Atmospheric, DefaultLAST
    %                          Note: StageCatalog.DefaultLAST is a 5-stage sequence from Garrappa et al. (2025):
    %                                [NormOnly_Initial, NormAndCenter, FieldCorrection_Adapted,
    %                                 Normalization_Refined, Atmospheric]
    % Author   : D. Kovaleva (Dec 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example:
    %{
              [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();
    
              % Build FunList from selected functions
              FunList = [FunCat.Rayleigh, FunCat.Ozone, FunCat.Aerosol, FunCat.Water, FunCat.QE_Legendre];
             
              % Create model (metadata passed separately)
              MetaValues = struct('ZenithAngle_deg', 45, 'Pressure_mbar', 965, ...
                                  'Temperature_C', 15);
              Model = tools.math.fun.CompositeFun.model(FunList, ...
                  'MetadataValues', MetaValues, 'UseTran2D', true);
             
              % Example 1: Fit atmospheric parameters using pre-normalized transmission
              % Use when you have direct transmission measurements [N_lambda x 1]
              % (i.e., observed flux already normalized by reference spectra externally)
              % Residuals: (ModelTransmission - ObservedTransmission)
              Lambda = linspace(3360, 10200, 343)';  % Wavelength grid [Angstrom]
              ObsFlux = Model.evaluateAllFunParInput(Lambda);  % Simulated transmission for demo
              OptSeq = [StageCat.AerosolOpt, StageCat.WaterOpt];  % Simple atmospheric fit
              % Or use: OptSeq = StageCat.DefaultLAST;  % 5-stage sequence from Garrappa et al. (2025)
              [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
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
               OptSeq = StageCat.DefaultLAST;
               [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
                   'CostArgs', CostArgs, 'X', X, 'Y', Y, 'OptimizationSequence', OptSeq, 'Verbose', true);
    %}
    %% ====================================================================
    %% NORMALIZATION FUNCTION
    %% ====================================================================

    % Normalization - constant scaling factor
    % Parameters: [Norm]
    FunCatalog.Normalization = struct();
    FunCatalog.Normalization.Name = 'Normalization';
    FunCatalog.Normalization.Handle = '@(Lambda, Norm) Norm * ones(size(Lambda))';
    FunCatalog.Normalization.HandleType = 'anonymous';
    FunCatalog.Normalization.Params = [0.3];  % Typical initial value
    FunCatalog.Normalization.FitPar = [true];  % Fit normalization factor
    FunCatalog.Normalization.ParamInfo = struct(...
        'Name', {'Norm'}, ...
        'Description', {'Normalization factor'}, ...
        'Min', {0.01}, ...
        'Max', {10.0});

    %% ====================================================================
    %% ATMOSPHERIC TRANSMISSION FUNCTIONS
    %% ====================================================================

    % Rayleigh scattering
    % Parameters: [ZenithAngle_deg, Pressure_mbar]
    % Note: Both are metadata (not fitted)
    FunCatalog.Rayleigh = struct();
    FunCatalog.Rayleigh.Name = 'Rayleigh';
    FunCatalog.Rayleigh.Handle = '@astro.transmission.rayleighTransmission';
    FunCatalog.Rayleigh.HandleType = 'named';
    FunCatalog.Rayleigh.Params = [45, 965];  % [ZenithAngle_deg, Pressure_mbar]
    FunCatalog.Rayleigh.FitPar = [false, false];  % Don't fit any parameters
    FunCatalog.Rayleigh.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {0, 500}, ...
        'Max', {90, 1100});

    % Ozone absorption
    % Parameters: [ZenithAngle_deg, DobsonUnits]
    % Note: ZenithAngle_deg is metadata (not fitted), DobsonUnits is variable
    FunCatalog.Ozone = struct();
    FunCatalog.Ozone.Name = 'Ozone';
    FunCatalog.Ozone.Handle = '@astro.transmission.ozoneTransmission';
    FunCatalog.Ozone.HandleType = 'named';
    FunCatalog.Ozone.Params = [45, 300];  % [ZenithAngle_deg, DobsonUnits]
    FunCatalog.Ozone.FitPar = [false, true];  % Don't fit zenith angle, fit ozone column
    FunCatalog.Ozone.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'DobsonUnits'}, ...
        'Description', {'Zenith angle [deg]', 'Total ozone column [DU]'}, ...
        'Min', {0, 200}, ...
        'Max', {90, 500});

    % Aerosol extinction
    % Parameters: [ZenithAngle_deg, TauAod500, AngstromExponent]
    % Note: ZenithAngle_deg is metadata (not fitted), others are variable
    FunCatalog.Aerosol = struct();
    FunCatalog.Aerosol.Name = 'Aerosol';
    FunCatalog.Aerosol.Handle = '@astro.transmission.aerosolTransmission';
    FunCatalog.Aerosol.HandleType = 'named';
    FunCatalog.Aerosol.Params = [45, 0.05, 1.2];  % [ZenithAngle_deg, TauAod500, AngstromExponent]
    FunCatalog.Aerosol.FitPar = [false, true, false];  % Don't fit zenith, fit AOD, fix Angstrom exponent
    FunCatalog.Aerosol.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
        'Description', {'Zenith angle [deg]', 'Aerosol optical depth at 500nm', 'Angstrom exponent'}, ...
        'Min', {0, 0, 0.5}, ...
        'Max', {90, 1.0, 2.5});

    % Water vapor absorption
    % Parameters: [ZenithAngle_deg, PWV_cm, Pressure_mbar]
    % Note: ZenithAngle_deg and Pressure_mbar are metadata (not fitted), PWV_cm is variable
    FunCatalog.Water = struct();
    FunCatalog.Water.Name = 'Water';
    FunCatalog.Water.Handle = '@astro.transmission.waterTransmission';
    FunCatalog.Water.HandleType = 'named';
    FunCatalog.Water.Params = [45, 1.5, 965];  % [ZenithAngle_deg, PWV_cm, Pressure_mbar]
    FunCatalog.Water.FitPar = [false, true, false];  % Don't fit zenith/pressure, fit PWV
    FunCatalog.Water.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'PWV_cm', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Precipitable water vapor [cm]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {0, 0.1, 500}, ...
        'Max', {90, 10, 1100});

    % Uniformly Mixed Gases (UMG) transmission
    % Parameters: [ZenithAngle_deg, Temperature_C, Pressure_mbar]
    % Note: All are metadata (not fitted)
    FunCatalog.UMG = struct();
    FunCatalog.UMG.Name = 'UMG';
    FunCatalog.UMG.Handle = '@astro.transmission.umgTransmission';
    FunCatalog.UMG.HandleType = 'named';
    FunCatalog.UMG.Params = [45, 15, 965];  % [ZenithAngle_deg, Temperature_C, Pressure_mbar]
    FunCatalog.UMG.FitPar = [false, false, false];  % Don't fit any parameters
    FunCatalog.UMG.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'Temperature_C', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Temperature [C]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {0, -50, 500}, ...
        'Max', {90, 50, 1100});

    %% ====================================================================
    %% TELESCOPE OPTICS TRANSMISSION FUNCTIONS
    %% ====================================================================

    % Mirror reflectivity
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.Mirror = struct();
    FunCatalog.Mirror.Name = 'Mirror';
    FunCatalog.Mirror.Handle = '@telescope.optics.mirrorReflectanceLAST';
    FunCatalog.Mirror.HandleType = 'named';
    FunCatalog.Mirror.Params = [1];  % Dummy parameter
    FunCatalog.Mirror.FitPar = [false];
    FunCatalog.Mirror.ParamInfo = struct(...
        'Name', {'DummyParam'}, ...
        'Description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'Min', {1}, ...
        'Max', {1});

    % Corrector transmission
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.Corrector = struct();
    FunCatalog.Corrector.Name = 'Corrector';
    FunCatalog.Corrector.Handle = '@telescope.optics.correctorTransmissionLAST';
    FunCatalog.Corrector.HandleType = 'named';
    FunCatalog.Corrector.Params = [1];  % Dummy parameter
    FunCatalog.Corrector.FitPar = [false];
    FunCatalog.Corrector.ParamInfo = struct(...
        'Name', {'DummyParam'}, ...
        'Description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'Min', {1}, ...
        'Max', {1});

    %% ====================================================================
    %% DETECTOR QUANTUM EFFICIENCY FUNCTIONS
    %% ====================================================================

    % QE - Legendre polynomial model (Ofek et al. 2023)
    % Parameters: [DummyParam] - fixed at 1
    FunCatalog.QE_Legendre = struct();
    FunCatalog.QE_Legendre.Name = 'QE_Legendre';
    FunCatalog.QE_Legendre.Handle = '@telescope.detector.qeLegendreLAST';
    FunCatalog.QE_Legendre.HandleType = 'named';
    FunCatalog.QE_Legendre.Params = [1];  % Dummy parameter
    FunCatalog.QE_Legendre.FitPar = [false];
    FunCatalog.QE_Legendre.ParamInfo = struct(...
        'Name', {'DummyParam'}, ...
        'Description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'Min', {1}, ...
        'Max', {1});

    % QE - Skewed Gaussian model (Garrappa et al. 2025)
    % Parameters: [Amplitude, Center_Ang, Sigma_Ang, Gamma]
    FunCatalog.QE_SkewedGaussian = struct();
    FunCatalog.QE_SkewedGaussian.Name = 'QE_SkewedGaussian';
    FunCatalog.QE_SkewedGaussian.Handle = '@telescope.detector.qeSkewedGaussianLAST';
    FunCatalog.QE_SkewedGaussian.HandleType = 'named';
    FunCatalog.QE_SkewedGaussian.Params = [3281.936, 5709.73, 1397.7, -0.1517];  % Default LAST QHY600-PH
    FunCatalog.QE_SkewedGaussian.FitPar = [false, true, false, false];  % Fit center wavelength only
    FunCatalog.QE_SkewedGaussian.ParamInfo = struct(...
        'Name', {'Amplitude', 'Center_Ang', 'Sigma_Ang', 'Gamma'}, ...
        'Description', {'Amplitude', 'Peak wavelength [Angstrom]', 'Width [Angstrom]', 'Skewness parameter'}, ...
        'Min', {2000, 4000, 500, -1}, ...
        'Max', {5000, 8000, 3000, 1});

    %% ====================================================================
    %% OPTIMIZATION STAGE CATALOG
    %% ====================================================================

    % Default optimization sequence for LAST (Garrappa et al. 2025)
    StageCatalog.DefaultLAST = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'nonlinear', 'nonlinear', 'linear', 'nonlinear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {true, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 3, 3, 0, 0}, ...
        'Description', {'Initial normalization with outlier removal', 'Optimize normalization and QE center', 'Field corrections using linear least squares', 'Refine normalization after field corrections', 'Optimize water vapor and aerosol'});

    % Individual stages for custom sequences

    % Stage 1: Initial normalization only
    StageCatalog.NormOnly_Initial = struct();
    StageCatalog.NormOnly_Initial.StageName = 'NormOnly_Initial';
    StageCatalog.NormOnly_Initial.Method = 'nonlinear';
    StageCatalog.NormOnly_Initial.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.NormOnly_Initial.SigmaClip = true;
    StageCatalog.NormOnly_Initial.SigmaThresh = 3.0;
    StageCatalog.NormOnly_Initial.SigmaIter = 3;
    StageCatalog.NormOnly_Initial.Description = 'Initial normalization with outlier removal';

    % Stage 2: Normalization + QE center
    StageCatalog.NormAndCenter = struct();
    StageCatalog.NormAndCenter.StageName = 'NormAndCenter';
    StageCatalog.NormAndCenter.Method = 'nonlinear';
    StageCatalog.NormAndCenter.FreeParams = struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'});
    StageCatalog.NormAndCenter.SigmaClip = true;
    StageCatalog.NormAndCenter.SigmaThresh = 3.0;
    StageCatalog.NormAndCenter.SigmaIter = 3;
    StageCatalog.NormAndCenter.Description = 'Optimize normalization and QE center';

    % Stage 3: Field correction
    StageCatalog.FieldCorrection_Adapted = struct();
    StageCatalog.FieldCorrection_Adapted.StageName = 'FieldCorrection_Adapted';
    StageCatalog.FieldCorrection_Adapted.Method = 'linear';
    StageCatalog.FieldCorrection_Adapted.FreeParams = [];  % Empty for field correction
    StageCatalog.FieldCorrection_Adapted.SigmaClip = true;
    StageCatalog.FieldCorrection_Adapted.SigmaThresh = 2.0;
    StageCatalog.FieldCorrection_Adapted.SigmaIter = 3;
    StageCatalog.FieldCorrection_Adapted.Description = 'Field corrections using linear least squares';

    % Stage 4: Refined normalization
    StageCatalog.Normalization_Refined = struct();
    StageCatalog.Normalization_Refined.StageName = 'Normalization_Refined';
    StageCatalog.Normalization_Refined.Method = 'nonlinear';
    StageCatalog.Normalization_Refined.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.Normalization_Refined.SigmaClip = false;
    StageCatalog.Normalization_Refined.SigmaThresh = 3.0;
    StageCatalog.Normalization_Refined.SigmaIter = 0;
    StageCatalog.Normalization_Refined.Description = 'Refine normalization after field corrections';

    % Stage 5: Atmospheric parameters
    StageCatalog.Atmospheric = struct();
    StageCatalog.Atmospheric.StageName = 'Atmospheric';
    StageCatalog.Atmospheric.Method = 'nonlinear';
    StageCatalog.Atmospheric.FreeParams = struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'});
    StageCatalog.Atmospheric.SigmaClip = false;
    StageCatalog.Atmospheric.SigmaThresh = 3.0;
    StageCatalog.Atmospheric.SigmaIter = 0;
    StageCatalog.Atmospheric.Description = 'Optimize water vapor and aerosol';

 
 end
