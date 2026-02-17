function FixedFuns = LASTTransmissionFixed(Args)
    % Fixed (non-fitted) telescope transmission components for LAST
    % Package: telescope.optics
    % Description: Returns FunCatalog entries for LAST telescope components
    %              whose transmission is fixed (not fitted during calibration):
    %              Mirror reflectance, Corrector transmission, QE Legendre.
    %              QE_SkewedGaussian is NOT included here because its
    %              Center_Ang parameter is fitted during optimization.
    % Input  : * ...,key,val,...
    %            'QE_Legendre_Params' - Legendre coefficients [1x9].
    %                   Default is from Ofek et al. 2023.
    %            'QE_Legendre_Min' - Min bound for all Legendre coefficients.
    %                   Default is -10.
    %            'QE_Legendre_Max' - Max bound for all Legendre coefficients.
    %                   Default is 10.
    % Output : - FixedFuns - Struct with fields:
    %              .Mirror       - FunCatalog entry for mirror reflectance
    %              .Corrector    - FunCatalog entry for corrector transmission
    %              .QE_Legendre  - FunCatalog entry for QE Legendre model
    % Author : D. Kovaleva (Feb 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50; Ofek et al. 2023, PASP 135, 124502.
    % Example: FixedFuns = telescope.optics.LASTTransmissionFixed();
    %          % Custom Legendre coefficients:
    %          FixedFuns = telescope.optics.LASTTransmissionFixed('QE_Legendre_Params', zeros(1,9));

    arguments
        Args.QE_Legendre_Params   = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60]
        Args.QE_Legendre_Min      = -10
        Args.QE_Legendre_Max      = 10
    end

    %% Mirror reflectivity — data-driven, no fittable params
    FixedFuns.Mirror = struct();
    FixedFuns.Mirror.Name = 'Mirror';
    FixedFuns.Mirror.Handle = '@telescope.optics.mirrorReflectanceLAST';
    FixedFuns.Mirror.HandleType = 'named';
    FixedFuns.Mirror.Params = [1];  % Dummy parameter
    FixedFuns.Mirror.FitPar = [false];
    FixedFuns.Mirror.ParamInfo = struct(...
        'Name', {'DummyParam'}, ...
        'Description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'Min', {1}, ...
        'Max', {1});

    %% Corrector transmission — data-driven, no fittable params
    FixedFuns.Corrector = struct();
    FixedFuns.Corrector.Name = 'Corrector';
    FixedFuns.Corrector.Handle = '@telescope.optics.correctorTransmissionLAST';
    FixedFuns.Corrector.HandleType = 'named';
    FixedFuns.Corrector.Params = [1];  % Dummy parameter
    FixedFuns.Corrector.FitPar = [false];
    FixedFuns.Corrector.ParamInfo = struct(...
        'Name', {'DummyParam'}, ...
        'Description', {'Dummy parameter for CompositeFun compatibility'}, ...
        'Min', {1}, ...
        'Max', {1});

    %% QE Legendre polynomial model — all coefficients fixed (Ofek et al. 2023)
    NLeg = numel(Args.QE_Legendre_Params);
    FixedFuns.QE_Legendre = struct();
    FixedFuns.QE_Legendre.Name = 'QE_Legendre';
    FixedFuns.QE_Legendre.Handle = '@telescope.detector.qeLegendreLAST';
    FixedFuns.QE_Legendre.HandleType = 'named';
    FixedFuns.QE_Legendre.Params = Args.QE_Legendre_Params;
    FixedFuns.QE_Legendre.FitPar = false(1, NLeg);  % All fixed
    LegNames = arrayfun(@(k) sprintf('L%d', k), 0:(NLeg-1), 'UniformOutput', false);
    LegDescs = arrayfun(@(k) sprintf('Legendre coeff %d', k), 0:(NLeg-1), 'UniformOutput', false);
    LegMins  = num2cell(Args.QE_Legendre_Min * ones(1, NLeg));
    LegMaxs  = num2cell(Args.QE_Legendre_Max * ones(1, NLeg));
    FixedFuns.QE_Legendre.ParamInfo = struct(...
        'Name', LegNames, ...
        'Description', LegDescs, ...
        'Min', LegMins, ...
        'Max', LegMaxs);
end
