function Result = mirrorReflectanceLAST(Lambda, unusedParam, Args)
    % LAST mirror reflectance using pre-computed polynomial function handle
    % Input  : - Lambda - Wavelength array [Angstrom].
    %                   If GetArgNames flag is true, returns ArgNames structure.
    %          - unusedParam - Dummy parameter for CompositeFun compatibility.
    %                   Default is 1.
    %          * ...,key,val,...
    %            'Return' - Pre-computed results for caching.
    %                   Default is [].
    %            'UsePersistentCache' - Enable persistent caching.
    %                   Default is true.
    %            'Tolerance' - Tolerance for cache comparison.
    %                   Default is 1e-12.
    %            'GetArgNames' - Return ArgNames structure instead of calculating.
    %                   Default is false.
    %            'DataPath' - Path to mirror reflectance data file.
    %                   Default is fullfile(getenv('HOME'), 'matlab/data/spec/Telescope/LAST/Mirror_reflectance.mat').
    % Output : - Result - Mirror reflectance values (0-1).
    %                   OR ArgNames structure if GetArgNames is true.
    % Author : D. Kovaleva (Oct 2025)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: Lambda = linspace(3000, 11000, 401)';
    %          Refl = telescope.optics.mirrorReflectanceLAST(Lambda);
    %          Model = tools.math.fun.CompositeFun();
    %          Model.addFun('Mirror', @telescope.optics.mirrorReflectanceLAST, [], ...
    %                       'Par', [1], 'FitPar', [false]);

    arguments
        Lambda = linspace(3000, 11000, 401)'
        unusedParam = 1
        Args.Return = []
        Args.UsePersistentCache logical = true
        Args.Tolerance = 1e-12
        Args.GetArgNames logical = false
        Args.DataPath = fullfile(getenv('HOME'), 'matlab/data/spec/Telescope/LAST/Mirror_reflectance.mat')
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
    % Note: Internal function handle expects nm, so convert from Angstrom
    LoadedData = load(Args.DataPath, 'Mirror_reflectance');
    Lambda_nm = Lambda / 10;  % Convert Angstrom to nm (1 nm = 10 Angstrom)
    Result = LoadedData.Mirror_reflectance(Lambda_nm);

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
