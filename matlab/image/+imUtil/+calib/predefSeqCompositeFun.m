function [FunCatalog, StageCatalog] = predefSeqCompositeFun(Args)
    % Predefined sequences of transmission functions and optimization stages for CompositeFun
    % Package: imUtil.calib
    % Description: Provides pre-configured building blocks for CompositeFun-based
    %              transmission calibration. Users select which functions and optimization
    %              stages they need from this catalog.
    %              All physical parameter values and bounds are exposed as optional
    %              name-value arguments. When called with no arguments, LAST defaults
    %              are used. Callers can override shared parameters (e.g., Pressure_mbar)
    %              to set them consistently across all functions that use them.
    % Input  : * ...,key,val,...
    %            'ZenithAngle_deg'   - Zenith angle [deg]. Default is 45.
    %            'Pressure_mbar'     - Atmospheric pressure [mbar]. Default is 965.
    %            'Temperature_C'     - Temperature [C]. Default is 15.
    %            'Norm'              - Normalization factor. Default is 0.5.
    %            'DobsonUnits'       - Ozone column [DU]. Default is 300.
    %            'TauAod500'         - Aerosol optical depth at 500nm. Default is 0.084.
    %            'AngstromExponent'  - Angstrom exponent. Default is 0.6.
    %            'PWV_cm'            - Precipitable water vapor [cm]. Default is 1.4.
    %            '*_Min', '*_Max'    - Bounds for each parameter (see arguments block).
    %            'QE_*'              - QE SkewedGaussian model parameters and bounds.
    %            'LASTTelescopeTransmission' - Struct from telescope.optics.LASTTransmissionFixed().
    %                   Contains fixed components (Mirror, Corrector, QE_Legendre).
    %                   Default is struct() (calls LASTTransmissionFixed internally).
    % Output : - FunCatalog: Structure with pre-configured transmission functions.
    %                        Available: Normalization, Rayleigh, Ozone, Aerosol, Water, UMG,
    %                                   Mirror, Corrector, QE_Legendre, QE_SkewedGaussian
    %          - StageCatalog: Structure with pre-configured optimization stages.
    %                          Available: NormOnly_Initial, NormAndCenter, FieldCorrection_Adapted,
    %                                     Normalization_Refined, Atmospheric, DefaultLAST
    %                          Note: StageCatalog.DefaultLAST is a 6-stage sequence from Garrappa et al. (2025):
    %                                [NormOnly_Initial, NormAndCenter, OrphanClip,
    %                                 FieldCorrection_Adapted, Normalization_Refined, Atmospheric]
    % Author   : D. Kovaleva (Dec 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example:
    %{
              [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun();

              % With observation metadata from header:
              [FunCat, StageCat] = imUtil.calib.predefSeqCompositeFun(...
                  'ZenithAngle_deg', 30, 'Pressure_mbar', 970, 'Temperature_C', 12);

              % Build FunList from selected functions
              FunList = [FunCat.Rayleigh, FunCat.Ozone, FunCat.Aerosol, FunCat.Water, FunCat.QE_Legendre];

              % Create model
              Model = tools.math.fun.CompositeFun.model(FunList, 'UseTran2D', true);

              % Example 1: Fit atmospheric parameters using pre-normalized transmission
              Lambda = linspace(3360, 10200, 343)';
              ObsFlux = Model.evaluateAllFunParInput(Lambda);
              OptSeq = StageCat.DefaultLASTOptSeq;
              [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
                  'OptimizationSequence', OptSeq, 'Verbose', true);

               % Example 2: Fit transmission + field correction from photon counts
               N = 50;
               X = rand(N,1) * 1726;
               Y = rand(N,1) * 1726;
               ObsFlux = 8e4 + 4e4 * rand(N,1);
               RefSpec = zeros(343, N);
               for i = 1:N
                   alpha = -2 + 3.5 * (i-1)/(N-1);
                   RefSpec(:, i) = (3e-17) ./ (Lambda / 500).^alpha;
               end
               CostArgs = struct('WeightMatrix', RefSpec, 'TransmissionMode', true, ...
                   'ExpTime', 20, 'Aperture_area_m2', pi * (0.1397)^2);
               OptSeq = StageCat.DefaultLASTOptSeq;
               [Model, FitResult] = Model.fitPar(Lambda, ObsFlux, ...
                   'CostArgs', CostArgs, 'X', X, 'Y', Y, 'OptimizationSequence', OptSeq, 'Verbose', true);
    %}

    arguments
        % --- Shared observation metadata ---
        Args.ZenithAngle_deg      = 45
        Args.ZenithAngle_deg_Min  = 0
        Args.ZenithAngle_deg_Max  = 90

        Args.Pressure_mbar        = 965
        Args.Pressure_mbar_Min    = 960
        Args.Pressure_mbar_Max    = 970

        Args.Temperature_C        = 15
        Args.Temperature_C_Min    = 0
        Args.Temperature_C_Max    = 50

        % --- Normalization ---
        Args.Norm                 = 0.5
        Args.Norm_Min             = 0.001
        Args.Norm_Max             = 1.0

        % --- Ozone ---
        Args.DobsonUnits          = 300
        Args.DobsonUnits_Min      = 200
        Args.DobsonUnits_Max      = 500

        % --- Aerosol ---
        Args.TauAod500            = 0.084
        Args.TauAod500_Min        = 0.01
        Args.TauAod500_Max        = 1.0
        Args.AngstromExponent     = 0.6
        Args.AngstromExponent_Min = 0.5
        Args.AngstromExponent_Max = 2.5

        % --- Water vapor ---
        Args.PWV_cm               = 1.4
        Args.PWV_cm_Min           = 0.1
        Args.PWV_cm_Max           = 10

        % --- QE Skewed Gaussian ---
        Args.QE_Amplitude         = 3281.936
        Args.QE_Amplitude_Min     = 2000
        Args.QE_Amplitude_Max     = 5000
        Args.QE_Center_Ang        = 5709.73
        Args.QE_Center_Ang_Min    = 5000
        Args.QE_Center_Ang_Max    = 6000
        Args.QE_Sigma_Ang         = 1397.7
        Args.QE_Sigma_Ang_Min     = 500
        Args.QE_Sigma_Ang_Max     = 3000
        Args.QE_Gamma             = -0.1517
        Args.QE_Gamma_Min         = -1
        Args.QE_Gamma_Max         = 1

        % --- Fixed telescope transmission (Mirror, Corrector, QE_Legendre) ---
        Args.LASTTelescopeTransmission struct = struct()  % From telescope.optics.LASTTransmissionFixed()
    end

    %% ====================================================================
    %% NORMALIZATION FUNCTION
    %% ====================================================================

    % Normalization - constant scaling factor
    % Parameters: [Norm]
    FunCatalog.Normalization = struct();
    FunCatalog.Normalization.Name = 'Normalization';
    FunCatalog.Normalization.Handle = '@(Lambda, Norm) Norm * ones(size(Lambda))';
    FunCatalog.Normalization.HandleType = 'anonymous';
    FunCatalog.Normalization.Params = [Args.Norm];
    FunCatalog.Normalization.FitPar = [true];  % Fit normalization factor
    FunCatalog.Normalization.ParamInfo = struct(...
        'Name', {'Norm'}, ...
        'Description', {'Normalization factor'}, ...
        'Min', {Args.Norm_Min}, ...
        'Max', {Args.Norm_Max});

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
    FunCatalog.Rayleigh.Params = [Args.ZenithAngle_deg, Args.Pressure_mbar];
    FunCatalog.Rayleigh.FitPar = [false, false];  % Don't fit any parameters
    FunCatalog.Rayleigh.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.Pressure_mbar_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.Pressure_mbar_Max});

    % Ozone absorption
    % Parameters: [ZenithAngle_deg, DobsonUnits]
    % Note: ZenithAngle_deg is metadata (not fitted), DobsonUnits is variable
    FunCatalog.Ozone = struct();
    FunCatalog.Ozone.Name = 'Ozone';
    FunCatalog.Ozone.Handle = '@astro.transmission.ozoneTransmission';
    FunCatalog.Ozone.HandleType = 'named';
    FunCatalog.Ozone.Params = [Args.ZenithAngle_deg, Args.DobsonUnits];
    FunCatalog.Ozone.FitPar = [false, false];  % Don't fit zenith angle, fit ozone column
    FunCatalog.Ozone.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'DobsonUnits'}, ...
        'Description', {'Zenith angle [deg]', 'Total ozone column [DU]'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.DobsonUnits_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.DobsonUnits_Max});

    % Aerosol extinction
    % Parameters: [ZenithAngle_deg, TauAod500, AngstromExponent]
    % Note: ZenithAngle_deg is metadata (not fitted), others are variable
    FunCatalog.Aerosol = struct();
    FunCatalog.Aerosol.Name = 'Aerosol';
    FunCatalog.Aerosol.Handle = '@astro.transmission.aerosolTransmission';
    FunCatalog.Aerosol.HandleType = 'named';
    FunCatalog.Aerosol.Params = [Args.ZenithAngle_deg, Args.TauAod500, Args.AngstromExponent];
    FunCatalog.Aerosol.FitPar = [false, true, false];  % Don't fit zenith, fit AOD, fix Angstrom exponent
    FunCatalog.Aerosol.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'TauAod500', 'AngstromExponent'}, ...
        'Description', {'Zenith angle [deg]', 'Aerosol optical depth at 500nm', 'Angstrom exponent'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.TauAod500_Min, Args.AngstromExponent_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.TauAod500_Max, Args.AngstromExponent_Max});

    % Water vapor absorption
    % Parameters: [ZenithAngle_deg, PWV_cm, Pressure_mbar]
    % Note: ZenithAngle_deg and Pressure_mbar are metadata (not fitted), PWV_cm is variable
    FunCatalog.Water = struct();
    FunCatalog.Water.Name = 'Water';
    FunCatalog.Water.Handle = '@astro.transmission.waterTransmission';
    FunCatalog.Water.HandleType = 'named';
    FunCatalog.Water.Params = [Args.ZenithAngle_deg, Args.PWV_cm, Args.Pressure_mbar];
    FunCatalog.Water.FitPar = [false, true, false];
    FunCatalog.Water.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'PWV_cm', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Precipitable water vapor [cm]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.PWV_cm_Min, Args.Pressure_mbar_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.PWV_cm_Max, Args.Pressure_mbar_Max});

    % Uniformly Mixed Gases (UMG) transmission
    % Parameters: [ZenithAngle_deg, Temperature_C, Pressure_mbar]
    % Note: All are metadata (not fitted)
    FunCatalog.UMG = struct();
    FunCatalog.UMG.Name = 'UMG';
    FunCatalog.UMG.Handle = '@astro.transmission.umgTransmission';
    FunCatalog.UMG.HandleType = 'named';
    FunCatalog.UMG.Params = [Args.ZenithAngle_deg, Args.Temperature_C, Args.Pressure_mbar];
    FunCatalog.UMG.FitPar = [false, false, false];  % Don't fit any parameters
    FunCatalog.UMG.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'Temperature_C', 'Pressure_mbar'}, ...
        'Description', {'Zenith angle [deg]', 'Temperature [C]', 'Atmospheric pressure [mbar]'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.Temperature_C_Min, Args.Pressure_mbar_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.Temperature_C_Max, Args.Pressure_mbar_Max});

    %% ====================================================================
    %% FIXED TELESCOPE TRANSMISSION (Mirror, Corrector, QE_Legendre)
    %% ====================================================================

    % Load from LASTTransmissionFixed (or use provided struct)
    if isempty(fieldnames(Args.LASTTelescopeTransmission))
        TelFuns = telescope.optics.LASTTransmissionFixed();
    else
        TelFuns = Args.LASTTelescopeTransmission;
    end
    FunCatalog.Mirror = TelFuns.Mirror;
    FunCatalog.Corrector = TelFuns.Corrector;
    FunCatalog.QE_Legendre = TelFuns.QE_Legendre;

    %% ====================================================================
    %% DETECTOR QE - FITTED COMPONENT
    %% ====================================================================

    % QE - Skewed Gaussian model (Garrappa et al. 2025)
    % Parameters: [Amplitude, Center_Ang, Sigma_Ang, Gamma]
    FunCatalog.QE_SkewedGaussian = struct();
    FunCatalog.QE_SkewedGaussian.Name = 'QE_SkewedGaussian';
    FunCatalog.QE_SkewedGaussian.Handle = '@telescope.detector.qeSkewedGaussianLAST';
    FunCatalog.QE_SkewedGaussian.HandleType = 'named';
    FunCatalog.QE_SkewedGaussian.Params = [Args.QE_Amplitude, Args.QE_Center_Ang, Args.QE_Sigma_Ang, Args.QE_Gamma];
    FunCatalog.QE_SkewedGaussian.FitPar = [false, true, false, false];  % Fit center wavelength only
    FunCatalog.QE_SkewedGaussian.ParamInfo = struct(...
        'Name', {'Amplitude', 'Center_Ang', 'Sigma_Ang', 'Gamma'}, ...
        'Description', {'Amplitude', 'Peak wavelength [Angstrom]', 'Width [Angstrom]', 'Skewness parameter'}, ...
        'Min', {Args.QE_Amplitude_Min, Args.QE_Center_Ang_Min, Args.QE_Sigma_Ang_Min, Args.QE_Gamma_Min}, ...
        'Max', {Args.QE_Amplitude_Max, Args.QE_Center_Ang_Max, Args.QE_Sigma_Ang_Max, Args.QE_Gamma_Max});

    %% ====================================================================
    %% FUNCTION LIST CATALOG
    %% ====================================================================

    % Default LAST function list (Garrappa et al. 2025)
    FunCatalog.DefaultLASTFunList = [FunCatalog.Normalization, ...
                                     FunCatalog.Rayleigh, ...
                                     FunCatalog.Aerosol, ...
                                     FunCatalog.Ozone, ...
                                     FunCatalog.Water, ...
                                     FunCatalog.UMG, ...
                                     FunCatalog.Mirror, ...
                                     FunCatalog.Corrector, ...
                                     FunCatalog.QE_SkewedGaussian, ...
                                     FunCatalog.QE_Legendre];

    %% ====================================================================
    %% OPTIMIZATION STAGE CATALOG
    %% ====================================================================

    % Default optimization sequence for LAST (Garrappa et al. 2025)
    % 6 stages: NormOnly → NormAndCenter → OrphanClip(3σ) → FieldCorrection(2σ,min30) → NormRefined → Atmospheric
    StageCatalog.DefaultLASTOptSeq = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'nonlinear', 'nonlinear', 'nonlinear', 'linear', 'nonlinear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {true, false, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 0, 1, 3, 0, 0}, ...
        'MinCalibrators', {0, 0, 0, 30, 0, 0}, ...
        'Description', {'Initial normalization with outlier removal', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections using linear least squares', 'Refine normalization after field corrections', 'Optimize water vapor and aerosol'});

    % LAST optimization sequence with linear Norm stages (faster)
    % Stages 1, 3, 5 use analytical solution for Norm instead of nonlinear optimization
    StageCatalog.LAST_NormLin = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {true, false, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 0, 1, 3, 0, 0}, ...
        'MinCalibrators', {0, 0, 0, 30, 0, 0}, ...
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections using linear least squares', 'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % Individual stages for custom sequences

    % Stage 1: Initial normalization only (nonlinear version)
    StageCatalog.NormOnly_Initial = struct();
    StageCatalog.NormOnly_Initial.StageName = 'NormOnly_Initial';
    StageCatalog.NormOnly_Initial.Method = 'nonlinear';
    StageCatalog.NormOnly_Initial.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.NormOnly_Initial.SigmaClip = true;
    StageCatalog.NormOnly_Initial.SigmaThresh = 3.0;
    StageCatalog.NormOnly_Initial.SigmaIter = 3;
    StageCatalog.NormOnly_Initial.Description = 'Initial normalization with outlier removal';

    % Stage 1 linear: Initial normalization using analytical solution
    StageCatalog.NormOnly_Initial_Lin = struct();
    StageCatalog.NormOnly_Initial_Lin.StageName = 'NormOnly_Initial_Lin';
    StageCatalog.NormOnly_Initial_Lin.Method = 'linear';
    StageCatalog.NormOnly_Initial_Lin.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.NormOnly_Initial_Lin.SigmaClip = true;
    StageCatalog.NormOnly_Initial_Lin.SigmaThresh = 3.0;
    StageCatalog.NormOnly_Initial_Lin.SigmaIter = 3;
    StageCatalog.NormOnly_Initial_Lin.Description = 'Initial normalization (analytical) with outlier removal';

    % Stage 2: Normalization + QE center
    StageCatalog.NormAndCenter = struct();
    StageCatalog.NormAndCenter.StageName = 'NormAndCenter';
    StageCatalog.NormAndCenter.Method = 'nonlinear';
    StageCatalog.NormAndCenter.FreeParams = struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'});
    StageCatalog.NormAndCenter.SigmaClip = true;
    StageCatalog.NormAndCenter.SigmaThresh = 3.0;
    StageCatalog.NormAndCenter.SigmaIter = 3;
    StageCatalog.NormAndCenter.Description = 'Optimize normalization and QE center';

    % Orphan clip: Single-pass 3 sigma outlier removal after QE fitting
    % Matches Python's orphan sigma_clip between out1 and out4_bis
    StageCatalog.OrphanClip = struct();
    StageCatalog.OrphanClip.StageName = 'OrphanClip';
    StageCatalog.OrphanClip.Method = 'nonlinear';
    StageCatalog.OrphanClip.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.OrphanClip.SigmaClip = true;
    StageCatalog.OrphanClip.SigmaThresh = 3.0;
    StageCatalog.OrphanClip.SigmaIter = 1;
    StageCatalog.OrphanClip.Description = 'Outlier removal after QE fitting (3-sigma, single pass)';

    % Orphan clip linear: Same using analytical Norm solution
    StageCatalog.OrphanClip_Lin = struct();
    StageCatalog.OrphanClip_Lin.StageName = 'OrphanClip';
    StageCatalog.OrphanClip_Lin.Method = 'linear';
    StageCatalog.OrphanClip_Lin.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.OrphanClip_Lin.SigmaClip = true;
    StageCatalog.OrphanClip_Lin.SigmaThresh = 3.0;
    StageCatalog.OrphanClip_Lin.SigmaIter = 1;
    StageCatalog.OrphanClip_Lin.Description = 'Outlier removal after QE fitting (analytical, 3-sigma, single pass)';

    % Field correction with MinCalibrators safeguard
    StageCatalog.FieldCorrection_Adapted = struct();
    StageCatalog.FieldCorrection_Adapted.StageName = 'FieldCorrection_Adapted';
    StageCatalog.FieldCorrection_Adapted.Method = 'linear';
    StageCatalog.FieldCorrection_Adapted.FreeParams = [];  % Empty for field correction
    StageCatalog.FieldCorrection_Adapted.SigmaClip = true;
    StageCatalog.FieldCorrection_Adapted.SigmaThresh = 2.0;
    StageCatalog.FieldCorrection_Adapted.SigmaIter = 3;
    StageCatalog.FieldCorrection_Adapted.MinCalibrators = 30;
    StageCatalog.FieldCorrection_Adapted.Description = 'Field corrections using linear least squares';

    % Stage 4: Refined normalization (nonlinear version)
    StageCatalog.Normalization_Refined = struct();
    StageCatalog.Normalization_Refined.StageName = 'Normalization_Refined';
    StageCatalog.Normalization_Refined.Method = 'nonlinear';
    StageCatalog.Normalization_Refined.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.Normalization_Refined.SigmaClip = false;
    StageCatalog.Normalization_Refined.SigmaThresh = 3.0;
    StageCatalog.Normalization_Refined.SigmaIter = 0;
    StageCatalog.Normalization_Refined.Description = 'Refine normalization after field corrections';

    % Stage 4 linear: Refined normalization using analytical solution
    StageCatalog.Normalization_Refined_Lin = struct();
    StageCatalog.Normalization_Refined_Lin.StageName = 'Normalization_Refined_Lin';
    StageCatalog.Normalization_Refined_Lin.Method = 'linear';
    StageCatalog.Normalization_Refined_Lin.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.Normalization_Refined_Lin.SigmaClip = false;
    StageCatalog.Normalization_Refined_Lin.SigmaThresh = 3.0;
    StageCatalog.Normalization_Refined_Lin.SigmaIter = 0;
    StageCatalog.Normalization_Refined_Lin.Description = 'Refine normalization (analytical) after field corrections';

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
