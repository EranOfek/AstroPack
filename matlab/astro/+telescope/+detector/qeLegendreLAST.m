function Result = qeLegendreLAST(Lambda, unusedParam, Args)
    % Legendre polynomial model for perturbations to instrumental
    % transmission optimized for LAST, Legendre coefficients from Ofek et al. (2023)
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %          - unusedParam - Dummy parameter for CompositeFun compatibility.
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
    %          Model.addFun('QE Legendre', @telescope.detector.qeLegendreLAST, [], 'Par', [1], 'FitPar', [false]);

    arguments
        Lambda = linspace(3000, 11000, 401)'
        unusedParam = 1
        Args.Return = []
        Args.UsePersistentCache logical = true
        Args.Tolerance = 1e-12
        Args.GetArgNames logical = false
    end

    % Return ArgNames structure if requested (dummy parameter for CompositeFun compatibility)
    if Args.GetArgNames
        Result = struct('Name', {1}, 'Description', {'unusedParam'}, 'Min', {1}, 'Max', {1});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Persistent cache
    persistent CachedResult CachedLambda

    if Args.UsePersistentCache && ~isempty(CachedResult)
        if compareParams(Lambda, CachedLambda, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Legendre coefficients (constants from Ofek et al. 2023)
    Li = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60];

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
