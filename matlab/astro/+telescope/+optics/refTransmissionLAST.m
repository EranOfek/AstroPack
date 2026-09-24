function Result = refTransmissionLAST(Lambda, Args)
    % Reference transmission template for LAST (used for error propagation)
    % Description: Returns a fixed reference transmission curve for error
    %              estimation in photometric calibration. This avoids circular
    %              dependency (can't use fitted transmission to estimate errors
    %              that feed into the fit).
    %              Uses pre-computed template from Garrappa et al. 2025.
    % Input  : - Lambda - Wavelength array [Angstrom].
    %          * ...,key,val,...
    %            'UsePersistentCache' - Enable persistent caching.
    %                   Default is true.
    %            'Tolerance' - Tolerance for cache comparison.
    %                   Default is 1e-12.
    %            'DataPath' - Path to transmission template .mat file.
    %                   Default is '~/matlab/data/spec/Telescope/LAST/Transmission_template.mat'.
    % Output : - Result - Reference transmission values (0-1) [N_wvl x 1].
    % Author : D. Kovaleva (Jan 2026)
    % Reference: Garrappa et al. 2025, A&A 699, A50.
    % Example: Lambda = linspace(3000, 11000, 401)';
    %          RefTrans = telescope.optics.refTransmissionLAST(Lambda);

    arguments
        Lambda = linspace(3000, 11000, 401)'
        Args.UsePersistentCache logical = true
        Args.Tolerance = 1e-12
        Args.DataPath = fullfile(getenv('HOME'), 'matlab/data/spec/Telescope/LAST/Transmission_template.mat')
    end

    % Persistent cache for interpolant and results
    persistent CachedInterp CachedResult CachedLambda

    % Check result cache first
    if Args.UsePersistentCache && ~isempty(CachedResult)
        if compareParams(Lambda, CachedLambda, Args.Tolerance)
            Result = CachedResult;
            return;
        end
    end

    Lambda = Lambda(:);  % Ensure column vector

    % Load interpolant if not cached
    if isempty(CachedInterp)
        LoadedData = load(Args.DataPath, 'Transmission_template');
        CachedInterp = LoadedData.Transmission_template;
    end

    % Convert wavelength from Angstrom to nm (template is in nm)
    Lambda_nm = Lambda / 10;

    % Interpolate transmission
    Result = CachedInterp(Lambda_nm);

    % Ensure non-negative and handle out-of-range
    Result(Result < 0) = 0;
    Result(isnan(Result)) = 0;

    % Store in result cache if enabled
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
