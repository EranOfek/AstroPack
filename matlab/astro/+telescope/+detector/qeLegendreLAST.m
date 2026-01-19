function Result = qeLegendreLAST(Lambda, Par, Args)
    % Legendre polynomial model for perturbations to instrumental
    % transmission optimized for LAST, Legendre coefficients from Ofek et al. (2023)
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %          - Par (double array): Legendre coefficients [L0, L1, ..., L8].
    %            Default values from Ofek et al. (2023).
    %            If GetArgNames flag is true, returns ArgNames structure.
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating. Default is false.
    % Output : - Result (double array): Exponential of Legendre polynomial expansion.
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % References: 1. Ofek et al. 2023, PASP 135, Issue 1054, id.124502.
    %             2. Garrappa et al. 2025, A&A 699, A50.
    % Example: Lambda = linspace(3000, 11000, 401)';
    %          QEpert = telescope.detector.qeLegendreLAST(Lambda);
    %          % Use with CompositeFun:
    %          Model = tools.math.fun.CompositeFun();
    %          Li = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60];
    %          Model.addFun('QE Legendre', @telescope.detector.qeLegendreLAST, [], 'Par', Li, 'FitPar', false(1,9));

    arguments
        Lambda = linspace(3000, 11000, 401)'
        % Default Legendre coefficients from Ofek et al. (2023)
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
            Result(i+1).Description = sprintf('L%d', i);
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

    % Persistent cache (includes both Lambda and Par)
    persistent CachedResult CachedLambda CachedPar

    if Args.UsePersistentCache && ~isempty(CachedResult)
        if compareParams(Lambda, CachedLambda, Args.Tolerance) && ...
           compareParams(Par, CachedPar, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Ensure Par is a row vector
    Li = Par(:)';

    % Rescale wavelength to [-1, 1]
    Lam_rescaled = 2 * (Lambda - min(Lambda)) / (max(Lambda) - min(Lambda)) - 1;

    % Calculate Legendre polynomials
    N = length(Li);
    M = numel(Lam_rescaled);
    Leg = zeros(N, M);
    for n = 0:N-1
        Legn = legendre(n, Lam_rescaled);
        Leg(n+1, :) = Legn(1, :);
    end

    % Calculate Legendre expansion and return exponential (transposed to column vector)
    Leg_expansion = Li * Leg;
    Result = exp(Leg_expansion)';

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
