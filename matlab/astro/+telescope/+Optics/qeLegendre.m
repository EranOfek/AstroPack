function Leg_model = legendreModel(Lam_rescaled, Li)
    % Calculates Legendre polynomial model for perturbations to
    % instrumental transmission.  Returns cashed result if called repeatedly with 
    % empty input. Part of the Transmission package for absolute photometric calibration. 
    % The input is supposed to be held fixed.
    % Input  : - Lam_rescaled (double array): input data rescaled to [-1,1]
    %                                         (initially wavelength array in nm)
    %          - Li: vector of Legendre coefficients
    % Output : - Leg_model (double array): Exponential of Legendre polynomial expansion
    % Author : D. Kovaleva (Jul 2025)
    % References: 1. Ofek et al. 2023, PASP 135, Issue 1054, id.124502 - for
    %                default Li values;
    %             2. Garrappa et al. 2025, A&A 699, A50.
    % Example : % First call with parameters
    %           Leg_model = astro.atmosphere.legendreModel(); % Use defaults and cache
    %           % Later calls without arguments
    %           Leg_model = astro.atmosphere.legendreModel(); % Return cached result

    
    arguments
        Lam_rescaled = linspace(-1, 1, 401)
        Li = [-0.30, 0.34, -1.89, -0.82, -3.73, -0.669, -2.06, -0.24, -0.60] % default Legendre expansion coefficients
    end

    persistent cachedModel cachedLam cachedLi

    % Check if requesting cached data (no Lam_rescaled provided or empty)
    if isempty(Lam_rescaled)
        if isempty(cachedModel)
            error('No cached Legendre model data. Call with Lam_rescaled first.');
        end
        Leg_model = cachedModel;
        return;
    end

    % Check if we can use cached data (same inputs)
    if ~isempty(cachedModel) && isequal(Lam_rescaled, cachedLam) && isequal(Li, cachedLi)
        Leg_model = cachedModel;
        return;
    end

    % Calculate Legendre polynomials
    % Preallocate cell array for speed
    Leg0 = cell(length(Li), 1);
    for n = 0:length(Li)-1
        Legn = legendre(n, Lam_rescaled);
        Leg0{n+1} = Legn(1, :);
    end
    Leg = vertcat(Leg0{:});

    % Calculate Legendre model
    Leg_expansion = Li*Leg;

    % Return exponential of the expansion
    Leg_model = exp(Leg_expansion);

    % Cache the results
    cachedModel = Leg_model;
    cachedLam = Lam_rescaled;
    cachedLi = Li;
end