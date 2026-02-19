function Result = LASTTransmissionFixed(Lambda, Par, Args)
    % Fixed telescope transmission component for LAST
    % Package: telescope.optics
    % Description: CompositeFun-compatible function that returns the product
    %              of LAST fixed telescope components:
    %              Mirror reflectance * Corrector transmission * QE Legendre.
    %              All parameters are fixed (not fitted during calibration).
    %              Uses persistent caching for efficiency across repeated calls.
    % Input  : - Lambda, Wavelength array [Angstrom].
    %          - Par, QE Legendre coefficients [1x9].
    %                  Default from Ofek et al. (2023).
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'UsePersistentCache' - Enable persistent caching. Default is true.
    %            'Tolerance' - Tolerance for cache comparison. Default is 1e-12.
    %            'GetArgNames' - Return ArgNames structure instead of calculating.
    %                   Default is false.
    % Output : - Fixed telescope transmission vector [numel(Lambda) x 1].
    %                   OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Feb 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50; Ofek et al. 2023, PASP 135, 124502.
    % Example: Lambda = (3000:20:11000)';
    %          Trans = telescope.optics.LASTTransmissionFixed(Lambda);
    %          % With custom QE Legendre coefficients:
    %          Trans = telescope.optics.LASTTransmissionFixed(Lambda, zeros(1,9));

    arguments
        Lambda = linspace(3000, 11000, 401)'
        Par = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60]
        Args.Return = []
        Args.UsePersistentCache logical = true
        Args.Tolerance = 1e-12
        Args.GetArgNames logical = false
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {}, 'Description', {}, 'Min', {}, 'Max', {});
        for i = 0:8
            Result(i+1).Name = i+1;
            Result(i+1).Description = sprintf('QE Legendre L%d', i);
            Result(i+1).Min = -10;
            Result(i+1).Max = 10;
        end
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Persistent cache
    persistent CachedResult CachedLambda CachedPar

    if Args.UsePersistentCache && ~isempty(CachedResult)
        if compareParams(Lambda, CachedLambda, Args.Tolerance) && ...
           compareParams(Par, CachedPar, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Compute product of fixed telescope components
    Mirror    = telescope.optics.mirrorReflectanceLAST(Lambda);
    Corrector = telescope.optics.correctorTransmissionLAST(Lambda);
    QELeg     = telescope.detector.qeLegendreLAST(Lambda, Par);
    Result    = Mirror .* Corrector .* QELeg;

    % Store in persistent cache if enabled
    if Args.UsePersistentCache
        CachedResult = Result;
        CachedLambda = Lambda;
        CachedPar = Par;
    end
end

function isSame = compareParams(ParamsCurrent, ParamsCached, Tolerance)
    % Compare numerical parameters with tolerance
    if nargin < 3
        Tolerance = 1e-12;
    end
    isSame = isequal(size(ParamsCurrent), size(ParamsCached)) && ...
             all(abs(ParamsCurrent(:) - ParamsCached(:)) < Tolerance);
end
