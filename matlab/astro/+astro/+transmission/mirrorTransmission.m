function Result = mirrorTransmission(Lambda, unusedParam, Args)
    % Mirror transmission using pre-computed polynomial function handle
    % Input  : - Lambda (double array): Wavelength array in Angstrom.
    %          - unusedParam - Dummy parameter for CompositeFun compatibility.
    %            If GetArgNames flag is true, returns ArgNames structure.
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching. Default is [].
    %            'GetArgNames' - Return ArgNames structure instead of calculating. Default is false.
    % Output : - Result (double array): Mirror transmission values (0-1).
    %            OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: Lambda = linspace(3000, 11000, 401)';
    %          Trans = astro.transmission.mirrorTransmission(Lambda);
    %          % Use with CompositeFun:
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('Mirror transmission', @astro.transmission.mirrorTransmission, [], 'Par', [1], 'FitPar', [false]);

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

    % Load and apply polynomial function handle
    DataPath = fullfile(getenv('HOME'), 'matlab/data/spec/Telescope/LAST/Mirror_reflectance.mat');
    LoadedData = load(DataPath, 'Mirror_reflectance');
    Result = LoadedData.Mirror_reflectance(Lambda);

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
