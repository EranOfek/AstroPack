function Result = qeSkewedGaussianLAST(Lambda, ParamMatrix, Args)
    % Skewed Gaussian quantum efficiency model for LAST QHY600-PH CMOS camera
    % Input  : - Lambda (double array): Wavelength array in nm.
    %            If GetArgNames flag is true, returns ArgNames structure.
    %          - ParamMatrix (double matrix): Parameter matrix where each row is
    %            [Amplitude, Center_nm, Sigma_nm, Gamma].
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating. Default is false.
    % Output : - Result (double matrix): QE matrix (wavelengths x parameter_sets).
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % References: 1. Ofek et al. 2023, PASP 135, Issue 1054, id.124502.
    %             2. Garrappa et al. 2025, A&A 699, A50.
    % Example:
    %   % Basic usage:
    %   Lambda = linspace(300, 1100, 401)';
    %   ParamMatrix = [328.1936, 570.973, 139.77, -0.1517];  % Default LAST parameters
    %   Result = telescope.detector.qeSkewedGaussianLAST(Lambda, ParamMatrix);
    %
    %   % Get parameter information:
    %   ArgNames = telescope.detector.qeSkewedGaussianLAST('GetArgNames', true);
    %
    %   % Usage with CompositeFun:
    %   Model = tools.math.fun.CompositeFun();
    %   Model.addFun('QE LAST', @telescope.detector.qeSkewedGaussianLAST, [], ...
    %                'Par', [328.1936, 570.973, 139.77, -0.1517], 'FitPar', [false, true, false, false]);

    arguments
        Lambda = linspace(300, 1100, 401)'
        ParamMatrix = [328.1936, 570.973, 139.77, -0.1517]  % [Amplitude, Center_nm, Sigma_nm, Gamma]
        Args.Return = []
        Args.UsePersistentCache logical = true
        Args.Tolerance = 1e-12
        Args.GetArgNames logical = false
    end

    % Return ArgNames structure if requested
    if Args.GetArgNames
        Result = struct('Name', {1, 2, 3, 4}, ...
                       'Description', {'Amplitude', 'Center_nm', 'Sigma_nm', 'Gamma'}, ...
                       'Min', {100, 400, 50, -1}, ...
                       'Max', {500, 800, 300, 1});
        return;
    end

    % Check for pre-computed results (external cache)
    if ~isempty(Args.Return)
        Result = Args.Return;
        return;
    end

    % Persistent cache
    persistent CachedResult CachedParams CachedLambda

    if Args.UsePersistentCache && ~isempty(CachedResult)
        if compareParams(Lambda, CachedLambda, Args.Tolerance) && ...
           compareParams(ParamMatrix, CachedParams, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    % Validate input dimensions
    if size(ParamMatrix, 2) ~= 4
        error('ParamMatrix must have 4 columns: [Amplitude, Center_nm, Sigma_nm, Gamma]');
    end

    % Extract parameters
    Amplitude = ParamMatrix(:, 1);  % Column vector
    Center = ParamMatrix(:, 2);     % nm
    Sigma = ParamMatrix(:, 3);      % nm
    Gamma = ParamMatrix(:, 4);      % skewness parameter
    NumParamSets = size(ParamMatrix, 1);
    NumWavelengths = length(Lambda);

    % Initialize result matrix
    Result = zeros(NumWavelengths, NumParamSets);

    % Calculate QE for each parameter set
    for i = 1:NumParamSets
        X = (Lambda - Center(i)) / Sigma(i);
        Gaussian = exp(-0.5 * X.^2);
        Skew = 1 + erf(Gamma(i) * X / sqrt(2));
        normalization = 1 / (Sigma(i) * sqrt(2 * pi));
        Result(:, i) = Amplitude(i) * normalization * Gaussian .* Skew;
    end

    % Store in persistent cache if enabled
    if Args.UsePersistentCache
        CachedResult = Result;
        CachedParams = ParamMatrix;
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
