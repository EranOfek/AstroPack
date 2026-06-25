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
    %            'QE_Legendre_Params' - QE Legendre coefficients [1x9]. Default from Ofek et al. (2023).
    %            'QE_Legendre_Min'    - Min bound for Legendre coefficients. Default is -10.
    %            'QE_Legendre_Max'    - Max bound for Legendre coefficients. Default is 10.
    % Output : - FunCatalog: Structure with pre-configured transmission functions.
    %                        Available: Normalization, Rayleigh, Ozone, Aerosol, Water, UMG,
    %                                   LASTTransmissionFixed, QE_SkewedGaussian
    %          - StageCatalog: Structure with pre-configured optimization stages.
    %                          Available: NormOnly_Initial, NormAndCenter, FieldCorrection_Adapted,
    %                                     Normalization_Refined, Atmospheric, NormAndTran2D_Linear,
    %                                     DefaultLAST,
    %                                     LAST_NormLin, LAST_NormLin_FixPWV, LAST_NormLin_FixTau,
    %                                     LAST_NormLin_OuterClip,
    %                                     LAST_NormLin_OrphanOuter, LAST_NormLin_OuterClip_FixPWV,
    %                                     LAST_NormLin_OuterClip_FixTau,
    %                                     LAST_Joint_OuterClip,
    %                                     LAST_Joint_OuterClip_S1Clip,
    %                                     LAST_Joint_2Iter
    %                          Note: StageCatalog.DefaultLAST is a 6-stage sequence from Garrappa et al. (2025):
    %                                [NormOnly_Initial, NormAndCenter, OrphanClip,
    %                                 FieldCorrection_Adapted, Normalization_Refined, Atmospheric]
    %                                LAST_NormLin_FixPWV / LAST_NormLin_FixTau are LAST_NormLin variants with
    %                                one atmospheric parameter held fixed (Water PWV_cm or Aerosol TauAod500
    %                                respectively); Atmospheric stage fits only the free parameter. Internal
    %                                sigma clipping (OrphanClip + FieldCorrection 2-sigma) is preserved.
    %                                LAST_NormLin_OuterClip is a 5-stage sequence with all internal sigma
    %                                clipping disabled, intended for use with the outer clip-and-refit loop
    %                                in CompositeFun.fitPar (Args.OuterSigmaClip=true).
    %                                LAST_NormLin_OrphanOuter is a 6-stage variant that keeps the OrphanClip
    %                                stage active (single-pass 3-sigma) but disables all other internal clipping;
    %                                also intended to run with Args.OuterSigmaClip=true.
    %                                LAST_NormLin_OuterClip_FixPWV is the same as LAST_NormLin_OuterClip but
    %                                with Water PWV_cm fixed at its initial value (Args.PWV_cm, default 1.4 cm);
    %                                Atmospheric stage fits only Aerosol TauAod500.
    %                                LAST_NormLin_OuterClip_FixTau is the mirror variant with Aerosol
    %                                TauAod500 fixed at its initial value (Args.TauAod500, default 0.084);
    %                                Atmospheric stage fits only Water PWV_cm.
    %                                LAST_Joint_OuterClip is a 4-stage sequence that fits Norm and the
    %                                10 cheby1_4_xt Tran2D coefficients jointly in stage 1 (single linear
    %                                LS, split by Tran2D(0,0)=0), followed by NormAndCenter, Atmospheric,
    %                                and a final analytical Norm. Tran2D is not refit after stage 1.
    %                                Designed to run with Args.OuterSigmaClip=true, Args.OuterMaxIter=2.
    %                                LAST_Joint_OuterClip_S1Clip is identical to LAST_Joint_OuterClip
    %                                except stage 1 also performs one 3-sigma inner clip + refit, gated
    %                                to outer iter 1 only by fitMultiStage's JOINT_FC branch (outer iter
    %                                2 skips the inner clip; the outer sigma clip handles iter-2 cleanup).
    %                                LAST_Joint_2Iter is a 2-stage sequence that fits Norm+Tran2D
    %                                jointly in stage 1 (JOINT_FC linear LS) and Center+TauAod500+
    %                                PWV_cm in stage 2 (single nonlinear lsqnonlin). All internal sigma
    %                                clipping disabled. Pair with OuterSigmaClip=true, OuterMaxIter=2,
    %                                OuterSigmaThresh=3, OuterStdFunc='std', and (optionally)
    %                                WeightedOuterIters=[false true] in CalibArgs to run iter 1
    %                                unweighted (uniform weights) and iter 2 with the per-source
    %                                MagErr weights (WeightingMode='combined' gives the recommended
    %                                inst+spectral quadrature).
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

        % --- CO2 (UMG abundance scalar; metadata, never fitted) ---
        % Default 395 ppm matches the SMARTS/dast reference (Simone parity).
        % Surfaced here so callers can override per-image without poking
        % umgTransmission's positional ParamMatrix from outside CompositeFun.
        Args.Co2_ppm              = 395
        Args.Co2_ppm_Min          = 350
        Args.Co2_ppm_Max          = 500

        % --- Normalization ---
        Args.Norm                 = 0.5
        Args.Norm_Min             = 0.0
        Args.Norm_Max             = 10.0

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
        Args.QE_Center_Ang_Min    = 3000
        Args.QE_Center_Ang_Max    = 10000
        Args.QE_Sigma_Ang         = 1397.7
        Args.QE_Sigma_Ang_Min     = 500
        Args.QE_Sigma_Ang_Max     = 3000
        Args.QE_Gamma             = -0.1517
        Args.QE_Gamma_Min         = -1
        Args.QE_Gamma_Max         = 1

        % --- QE Legendre (fixed coefficients for LASTTransmissionFixed) ---
        Args.QE_Legendre_Params   = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60]
        Args.QE_Legendre_Min      = -10
        Args.QE_Legendre_Max      = 10
    end

    %% ====================================================================
    %% NORMALIZATION FUNCTION
    %% ====================================================================

    % Normalization - constant scaling factor
    % Parameters: [Norm]
    FunCatalog.Normalization = struct();

    FunCatalog.Normalization.Name = 'Normalization';
    FunCatalog.Normalization.Handle = '@astro.transmission.normalization';
    FunCatalog.Normalization.HandleType = 'named';
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
    % Parameters: [ZenithAngle_deg, Temperature_C, Pressure_mbar, Co2_ppm]
    % Note: All are metadata (not fitted). Co2_ppm surfaced June 2026 so the
    % CO2 abundance can be tuned per image without editing umgTransmission.
    FunCatalog.UMG = struct();
    FunCatalog.UMG.Name = 'UMG';
    FunCatalog.UMG.Handle = '@astro.transmission.umgTransmission';
    FunCatalog.UMG.HandleType = 'named';
    FunCatalog.UMG.Params = [Args.ZenithAngle_deg, Args.Temperature_C, Args.Pressure_mbar, Args.Co2_ppm];
    FunCatalog.UMG.FitPar = [false, false, false, false];  % Don't fit any parameters

    FunCatalog.UMG.ParamInfo = struct(...
        'Name', {'ZenithAngle_deg', 'Temperature_C', 'Pressure_mbar', 'Co2_ppm'}, ...
        'Description', {'Zenith angle [deg]', 'Temperature [C]', 'Atmospheric pressure [mbar]', 'CO2 abundance [ppm]'}, ...
        'Min', {Args.ZenithAngle_deg_Min, Args.Temperature_C_Min, Args.Pressure_mbar_Min, Args.Co2_ppm_Min}, ...
        'Max', {Args.ZenithAngle_deg_Max, Args.Temperature_C_Max, Args.Pressure_mbar_Max, Args.Co2_ppm_Max});

    %% ====================================================================
    %% FIXED TELESCOPE TRANSMISSION (combined: Mirror * Corrector * QE_Legendre)
    %% ====================================================================

    % LASTTransmissionFixed — single CompositeFun-compatible function
    % Returns product of Mirror, Corrector, QE_Legendre; all parameters fixed
    NLeg = numel(Args.QE_Legendre_Params);
    FunCatalog.LASTTransmissionFixed = struct();

    FunCatalog.LASTTransmissionFixed.Name = 'LASTTransmissionFixed';
    FunCatalog.LASTTransmissionFixed.Handle = '@telescope.optics.LASTTransmissionFixed';
    FunCatalog.LASTTransmissionFixed.HandleType = 'named';
    FunCatalog.LASTTransmissionFixed.Params = Args.QE_Legendre_Params;
    FunCatalog.LASTTransmissionFixed.FitPar = false(1, NLeg);  % All fixed
    LegNames = arrayfun(@(k) sprintf('L%d', k), 0:(NLeg-1), 'UniformOutput', false);
    LegDescs = arrayfun(@(k) sprintf('QE Legendre coeff %d', k), 0:(NLeg-1), 'UniformOutput', false);
    LegMins  = num2cell(Args.QE_Legendre_Min * ones(1, NLeg));
    LegMaxs  = num2cell(Args.QE_Legendre_Max * ones(1, NLeg));
    FunCatalog.LASTTransmissionFixed.ParamInfo = struct(...
        'Name', LegNames, ...
        'Description', LegDescs, ...
        'Min', LegMins, ...
        'Max', LegMaxs);

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
    %% INDIVIDUAL TELESCOPE COMPONENTS (for granular LASTFunList)
    %% ====================================================================

    % Mirror reflectivity — data-driven, no fittable params
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

    % Corrector transmission — data-driven, no fittable params
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

    % QE Legendre polynomial model — all coefficients fixed
    FunCatalog.QE_Legendre = struct();

    FunCatalog.QE_Legendre.Name = 'QE_Legendre';
    FunCatalog.QE_Legendre.Handle = '@telescope.detector.qeLegendreLAST';
    FunCatalog.QE_Legendre.HandleType = 'named';
    FunCatalog.QE_Legendre.Params = Args.QE_Legendre_Params;
    FunCatalog.QE_Legendre.FitPar = false(1, NLeg);
    FunCatalog.QE_Legendre.ParamInfo = struct(...
        'Name', LegNames, ...
        'Description', LegDescs, ...
        'Min', LegMins, ...
        'Max', LegMaxs);

    %% ====================================================================
    %% FUNCTION LIST CATALOG
    %% ====================================================================

    % Default LAST function list — uses combined LASTTransmissionFixed (8 entries)
    FunCatalog.DefaultLASTFunList = [FunCatalog.Normalization, ...
                                     FunCatalog.Rayleigh, ...
                                     FunCatalog.Aerosol, ...
                                     FunCatalog.Ozone, ...
                                     FunCatalog.Water, ...
                                     FunCatalog.UMG, ...
                                     FunCatalog.LASTTransmissionFixed, ...
                                     FunCatalog.QE_SkewedGaussian];

    % Granular LAST function list — separate Mirror, Corrector, QE_Legendre (10 entries)
    FunCatalog.LASTFunList = [FunCatalog.Normalization, ...
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
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...   % 30-floor on every clipping stage (1, 3, 4)
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
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...   % 30-floor on every clipping stage (1, 3, 4)
        'SigmaStdFunc', {'mad_std', 'mad_std', 'mad_std', 'std', 'mad_std', 'mad_std'}, ...   % FC stage uses sample std (matches astropy / Simone); others stay on robust mad_std
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections using linear least squares', 'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % LAST_NonlinFC: like LAST_NormLin_Astropy in stages 1-3, 5, 6, but stage 4
    % is a joint nonlinear LM fit of the 10 FC coefficients (kx0, kx, kx2, kx3,
    % kx4, ky, ky2, ky3, ky4, kxy) via lsqnonlin, mirroring Simone's leastsq
    % fitter4_bis. The 10th coefficient is parameterised as kxy_raw and enters
    % the model as -kxy_raw^2 (MATLAB sign convention, so that effective cross
    % term = -(Simone.kxy)^2 ≤ 0). The fit's free vector is seeded with N(0,
    % Args.Tran2DPerturbStd) at stage 4 entry only, after rng(Args.Tran2DRngSeed)
    % - so previous Norm/Center stages run with Tran2DObj.ParX = 0.
    %
    % Pair with SigmaClipMethod='median_signed' and Tran2DType='cheby1_4_xt'.
    % FreeParams field uses the char sentinel 'NONLIN_FC' to trigger the new
    % handler in CompositeFun.fitMultiStage.
    StageCatalog.LAST_NonlinFC = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Nonlin', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'nonlinear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       'NONLIN_FC', ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {true, false, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 0, 1, 3, 0, 0}, ...
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...
        'SigmaStdFunc', {'std', 'std', 'std', 'std', 'std', 'std'}, ...
        'Description', {'Initial normalization (astropy clip)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (astropy clip)', ...
                        'Field corrections (joint nonlinear lsqnonlin, Simone kxy^2 parameterisation)', ...
                        'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % LAST_NormLin variant with Simone/LAST-Python-parity sigma clipping on
    % every stage. Each clipping stage carries an explicit per-stage
    % SigmaClipMethod='median' and SigmaStdFunc='std', so the OptSeq is
    % self-consistent: the user does NOT need to pass SigmaClipMethod at the
    % calibrate level — fitMultiStage reads it off the Stage struct directly.
    % The 'median' choice mirrors Simone's `ResidFunc(..., magres=True)`
    % which feeds np.abs(data-model) into astropy.stats.sigma_clip
    % (cenfunc='median', stdfunc='std', maxiters=5) — i.e. astropy iteration
    % on |r| rather than signed r. See tools.math.stat.sigmaClip docstring
    % for the math (note: this is MORE AGGRESSIVE than the nominal SigmaThresh
    % suggests — effective single-sided cut on the signed scale is ~2.48 sigma
    % at SigmaThresh=3 because median(|r|) ≈ 0.6745 sigma and std(|r|) ≈
    % 0.6028 sigma for a Gaussian).
    % All other fields verbatim from LAST_NormLin (3-stage outer iteration on
    % the clipping stages 1/3/4, same SigmaThresh, same MinCalibrators).
    StageCatalog.LAST_NormLin_Astropy = struct(...
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
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...
        'SigmaClipMethod', {'median', 'median', 'median', 'median', 'median', 'median'}, ...   % Simone/LAST-Python parity: astropy clip on |r|, not signed r
        'SigmaStdFunc', {'std', 'std', 'std', 'std', 'std', 'std'}, ...   % astropy-style: sample std on every clipping stage
        'Description', {'Initial normalization (astropy clip on |r|)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (astropy clip on |r|)', ...
                        'Field corrections (astropy clip on |r|)', 'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % LAST_NormLin variant with Water PWV_cm fixed at its initial value
    % (Args.PWV_cm, default 1.4 cm). Atmospheric stage fits only Aerosol TauAod500.
    % Internal sigma clipping (OrphanClip + FieldCorrection 2-sigma) matches LAST_NormLin.
    StageCatalog.LAST_NormLin_FixPWV = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Aerosol'}, 'Parameter', {'TauAod500'})}, ...
        'SigmaClip', {true, false, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 0, 1, 3, 0, 0}, ...
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...   % 30-floor on every clipping stage (1, 3, 4)
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections using linear least squares', 'Refine normalization (analytical)', 'Optimize aerosol only (PWV fixed)'});

    % LAST_NormLin variant with Aerosol TauAod500 fixed at its initial value
    % (Args.TauAod500, default 0.084). Atmospheric stage fits only Water PWV_cm.
    % Internal sigma clipping (OrphanClip + FieldCorrection 2-sigma) matches LAST_NormLin.
    StageCatalog.LAST_NormLin_FixTau = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water'}, 'Parameter', {'PWV_cm'})}, ...
        'SigmaClip', {true, false, true, true, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {3, 0, 1, 3, 0, 0}, ...
        'MinCalibrators', {30, 0, 30, 30, 0, 0}, ...   % 30-floor on every clipping stage (1, 3, 4)
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections using linear least squares', 'Refine normalization (analytical)', 'Optimize PWV only (TauAod500 fixed)'});

    % LAST sequence designed for outer-loop sigma clipping (no internal clip).
    % Use with fitPar args 'OuterSigmaClip', true, 'OuterStdFunc', 'mad_std'.
    % Same five core stages as LAST_NormLin minus OrphanClip; all SigmaClip=false.
    StageCatalog.LAST_NormLin_OuterClip = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {false, false, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {0, 0, 0, 0, 0}, ...
        'MinCalibrators', {0, 0, 30, 0, 0}, ...
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Field corrections (no internal clip)', 'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % LAST sequence designed for outer-loop sigma clipping while keeping the
    % single-pass OrphanClip after the QE fitting stage (NormAndCenter).
    % Use with fitPar args 'OuterSigmaClip', true, 'OuterStdFunc', 'mad_std'.
    % All other internal clipping is disabled.
    StageCatalog.LAST_NormLin_OrphanOuter = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'OrphanClip', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'})}, ...
        'SigmaClip', {false, false, true, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {0, 0, 1, 0, 0, 0}, ...
        'MinCalibrators', {0, 0, 30, 30, 0, 0}, ...   % 30-floor on every clipping stage (stage 3 only here; stage 4 kept as defensive 30)
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Outlier removal after QE fitting (3-sigma, single pass)', ...
                        'Field corrections (no internal clip)', 'Refine normalization (analytical)', 'Optimize water vapor and aerosol'});

    % LAST sequence variant of LAST_NormLin_OuterClip with PWV_cm fixed at its
    % initial value (Args.PWV_cm, default 1.4 cm). Atmospheric stage fits only
    % Aerosol TauAod500 — useful for breaking the PWV/Aerosol degeneracy when
    % external water-vapor data are available.
    % Use with fitPar args 'OuterSigmaClip', true, 'OuterStdFunc', 'mad_std'.
    StageCatalog.LAST_NormLin_OuterClip_FixPWV = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Aerosol'}, 'Parameter', {'TauAod500'})}, ...
        'SigmaClip', {false, false, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {0, 0, 0, 0, 0}, ...
        'MinCalibrators', {0, 0, 30, 0, 0}, ...
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Field corrections (no internal clip)', 'Refine normalization (analytical)', 'Optimize aerosol only (PWV fixed)'});

    % Same as LAST_Joint_OuterClip, but stage 1 also performs one inner
    % sigma-clip pass (SigmaClip=true, SigmaIter=1, SigmaThresh=3) on the
    % FIRST outer iteration only. fitMultiStage's JOINT_FC branch gates the
    % inner clip on OuterIter==1: on outer iter 2 the clip is skipped (the
    % outer sigma clip between iters already cleans the set). Used to harden
    % the joint Norm+Tran2D fit against outliers that would otherwise bias
    % the initial Tran2D map fed to stages 2-4.
    % Run with 'OuterSigmaClip', true, 'OuterMaxIter', 2.
    StageCatalog.LAST_Joint_OuterClip_S1Clip = struct(...
        'StageName', {'NormAndTran2D_Linear', 'NormAndCenter', 'Atmospheric', 'Normalization_Refined'}, ...
        'Method', {'linear', 'nonlinear', 'nonlinear', 'linear'}, ...
        'FreeParams', {'JOINT_FC', ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'})}, ...
        'SigmaClip', {true, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 3.0}, ...
        'SigmaIter', {1, 0, 0, 0}, ...
        'MinCalibrators', {30, 30, 30, 30}, ...
        'Description', {'Joint Norm + Tran2D linear fit, split by Tran2D(0,0)=0, inner 3-sigma clip on outer iter 1', ...
                        'Optimize normalization and QE center', ...
                        'Optimize water vapor and aerosol', ...
                        'Refine normalization (analytical)'});

    % Joint Norm + Tran2D linear sequence, 4 stages, outer-clipped.
    % Stage 1 (NormAndTran2D_Linear) fits all 10 cheby1_4_xt coefficients in a
    % single linear LS solve. Norm is degenerate with kx0 in this fit; after
    % the fit the split is fixed by Tran2D(0,0) = 0 (Norm <- field-centre
    % value, Tran2D ParX shifted so the field correction is zero at (Xc, Yc)).
    % Tran2D is NOT refit after stage 1. Designed to run with fitPar args
    % 'OuterSigmaClip', true, 'OuterMaxIter', 2 -- one stages-1-4 pass, one
    % sigma clip, one stages-1-4 pass on survivors, residuals stored from
    % the final pass.
    StageCatalog.LAST_Joint_OuterClip = struct(...
        'StageName', {'NormAndTran2D_Linear', 'NormAndCenter', 'Atmospheric', 'Normalization_Refined'}, ...
        'Method', {'linear', 'nonlinear', 'nonlinear', 'linear'}, ...
        'FreeParams', {'JOINT_FC', ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       struct('Function', {'Water', 'Aerosol'}, 'Parameter', {'PWV_cm', 'TauAod500'}), ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'})}, ...
        'SigmaClip', {false, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 3.0, 3.0}, ...
        'SigmaIter', {0, 0, 0, 0}, ...
        'MinCalibrators', {30, 0, 0, 0}, ...
        'Description', {'Joint Norm + Tran2D linear fit, split by Tran2D(0,0)=0', ...
                        'Optimize normalization and QE center', ...
                        'Optimize water vapor and aerosol', ...
                        'Refine normalization (analytical)'});

    % Compact 2-stage outer-clipped sequence designed for the two-pass
    % (unweighted -> sigma-clip -> weighted) workflow:
    %   Stage 1 (linear)    : NormAndTran2D_Linear (JOINT_FC) -- joint linear LS over
    %                         Normalization Norm + 10 cheby1_4_xt Tran2D coefficients,
    %                         split by Tran2D(0,0) = 0 (Norm <- field-centre value,
    %                         ParX shifted so the field correction is zero at (Xc, Yc)).
    %   Stage 2 (nonlinear) : single lsqnonlin over QE_SkewedGaussian.Center_Ang,
    %                         Aerosol.TauAod500, and Water.PWV_cm.
    % All internal sigma clipping disabled (the single 3-sigma cleanup happens in
    % the outer loop between iterations 1 and 2).
    % Run with:
    %     'OuterSigmaClip', true, 'OuterMaxIter', 2,
    %     'OuterSigmaThresh', 3.0, 'OuterStdFunc', 'std',
    %     'WeightingMode', 'combined', 'WeightedOuterIters', [false true].
    % Iter 1 runs on the full calibrator pool with uniform weights, the outer
    % clip removes 3-sigma outliers on the signed residuals, and iter 2 refits
    % the same two stages on survivors with per-source MagErr weights
    % (sqrt((1.086 * FluxErr)^2 + MagErr_spectral^2) in 'combined' mode).
    StageCatalog.LAST_Joint_2Iter = struct(...
        'StageName',    {'NormAndTran2D_Linear', 'CenterAerosolWater'}, ...
        'Method',       {'linear', 'nonlinear'}, ...
        'FreeParams',   {'JOINT_FC', ...
                         struct('Function',  {'QE_SkewedGaussian', 'Aerosol',   'Water'}, ...
                                'Parameter', {'Center_Ang',        'TauAod500', 'PWV_cm'})}, ...
        'SigmaClip',    {false, false}, ...
        'SigmaThresh',  {3.0, 3.0}, ...
        'SigmaIter',    {0, 0}, ...
        'MinCalibrators', {30, 30}, ...
        'Description',  {'Joint Norm + Tran2D linear fit, split by Tran2D(0,0)=0', ...
                         'Optimize QE center, aerosol AOD500, and water PWV (single lsqnonlin)'});

    % Mirror variant of LAST_NormLin_OuterClip_FixPWV with the opposite parameter
    % held fixed: Aerosol TauAod500 stays at its initial value (Args.TauAod500,
    % default 0.084), and Atmospheric stage fits only Water PWV_cm. Useful for
    % isolating the PWV component of the PWV/Aerosol degeneracy.
    % Use with fitPar args 'OuterSigmaClip', true, 'OuterStdFunc', 'mad_std'.
    StageCatalog.LAST_NormLin_OuterClip_FixTau = struct(...
        'StageName', {'NormOnly_Initial', 'NormAndCenter', 'FieldCorrection_Adapted', 'Normalization_Refined', 'Atmospheric'}, ...
        'Method', {'linear', 'nonlinear', 'linear', 'linear', 'nonlinear'}, ...
        'FreeParams', {struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'}), ...
                       [], ...
                       struct('Function', {'Normalization'}, 'Parameter', {'Norm'}), ...
                       struct('Function', {'Water'}, 'Parameter', {'PWV_cm'})}, ...
        'SigmaClip', {false, false, false, false, false}, ...
        'SigmaThresh', {3.0, 3.0, 2.0, 3.0, 3.0}, ...
        'SigmaIter', {0, 0, 0, 0, 0}, ...
        'MinCalibrators', {0, 0, 30, 0, 0}, ...
        'Description', {'Initial normalization (analytical)', 'Optimize normalization and QE center', ...
                        'Field corrections (no internal clip)', 'Refine normalization (analytical)', 'Optimize PWV only (TauAod500 fixed)'});

    % Individual stages for custom sequences

    % Stage 1: Initial normalization only (nonlinear version)
    StageCatalog.NormOnly_Initial = struct();
    StageCatalog.NormOnly_Initial.StageName = 'NormOnly_Initial';
    StageCatalog.NormOnly_Initial.Method = 'nonlinear';
    StageCatalog.NormOnly_Initial.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.NormOnly_Initial.SigmaClip = true;
    StageCatalog.NormOnly_Initial.SigmaThresh = 3.0;
    StageCatalog.NormOnly_Initial.SigmaIter = 3;
    StageCatalog.NormOnly_Initial.MinCalibrators = 30;
    StageCatalog.NormOnly_Initial.Description = 'Initial normalization with outlier removal';

    % Stage 1 linear: Initial normalization using analytical solution
    StageCatalog.NormOnly_Initial_Lin = struct();
    StageCatalog.NormOnly_Initial_Lin.StageName = 'NormOnly_Initial_Lin';
    StageCatalog.NormOnly_Initial_Lin.Method = 'linear';
    StageCatalog.NormOnly_Initial_Lin.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.NormOnly_Initial_Lin.SigmaClip = true;
    StageCatalog.NormOnly_Initial_Lin.SigmaThresh = 3.0;
    StageCatalog.NormOnly_Initial_Lin.SigmaIter = 3;
    StageCatalog.NormOnly_Initial_Lin.MinCalibrators = 30;
    StageCatalog.NormOnly_Initial_Lin.Description = 'Initial normalization (analytical) with outlier removal';

    % Stage 2: Normalization + QE center
    StageCatalog.NormAndCenter = struct();
    StageCatalog.NormAndCenter.StageName = 'NormAndCenter';
    StageCatalog.NormAndCenter.Method = 'nonlinear';
    StageCatalog.NormAndCenter.FreeParams = struct('Function', {'Normalization', 'QE_SkewedGaussian'}, 'Parameter', {'Norm', 'Center_Ang'});
    StageCatalog.NormAndCenter.SigmaClip = true;
    StageCatalog.NormAndCenter.SigmaThresh = 3.0;
    StageCatalog.NormAndCenter.SigmaIter = 3;
    StageCatalog.NormAndCenter.MinCalibrators = 30;
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
    StageCatalog.OrphanClip.MinCalibrators = 30;
    StageCatalog.OrphanClip.Description = 'Outlier removal after QE fitting (3-sigma, single pass)';

    % Orphan clip linear: Same using analytical Norm solution
    StageCatalog.OrphanClip_Lin = struct();
    StageCatalog.OrphanClip_Lin.StageName = 'OrphanClip';
    StageCatalog.OrphanClip_Lin.Method = 'linear';
    StageCatalog.OrphanClip_Lin.FreeParams = struct('Function', {'Normalization'}, 'Parameter', {'Norm'});
    StageCatalog.OrphanClip_Lin.SigmaClip = true;
    StageCatalog.OrphanClip_Lin.SigmaThresh = 3.0;
    StageCatalog.OrphanClip_Lin.SigmaIter = 1;
    StageCatalog.OrphanClip_Lin.MinCalibrators = 30;
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

    % Joint linear stage: fits Norm and all 10 cheby1_4_xt Tran2D coefficients
    % in a single linear LS solve. Norm is absorbed into kx0 by the LS (the two
    % are perfectly degenerate); after the fit the split is fixed by
    % Tran2D(0,0) = 0 -- Norm carries the field-centre value, Tran2D ParX is
    % shifted so the field correction is exactly zero at (Xc, Yc).
    % FreeParams = 'JOINT_FC' is a sentinel recognised by fitMultiStage.
    StageCatalog.NormAndTran2D_Linear = struct();
    StageCatalog.NormAndTran2D_Linear.StageName = 'NormAndTran2D_Linear';
    StageCatalog.NormAndTran2D_Linear.Method = 'linear';
    StageCatalog.NormAndTran2D_Linear.FreeParams = 'JOINT_FC';
    StageCatalog.NormAndTran2D_Linear.SigmaClip = false;
    StageCatalog.NormAndTran2D_Linear.SigmaThresh = 3.0;
    StageCatalog.NormAndTran2D_Linear.SigmaIter = 0;
    StageCatalog.NormAndTran2D_Linear.MinCalibrators = 30;
    StageCatalog.NormAndTran2D_Linear.Description = 'Joint Norm + Tran2D linear fit, split by Tran2D(0,0)=0';


 end
