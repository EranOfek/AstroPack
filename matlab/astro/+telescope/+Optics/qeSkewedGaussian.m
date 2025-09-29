function Sg_model = skewedGaussianModel(Lam, Args)
    % Calculate skewed Gaussian model for instrumental transmission 
    % (quantum efficiency model measured for the LAST QHY600-PH CMOS camera).   
    % Returns cashed result if the inputs did not change since last call. 
    % Part of the Transmission package for absolute photometric calibration. 
    % Input  : - Lam : Wavelength array in nm (optional if cached)
    %          * ...,key,val,...
    %            Amplitude
    %            Center
    %            Sigma
    %            Gamma
    % Output : - Sg_model (double array): Skewed Gaussian values
    % Author : D. Kovaleva (Jan 2025)
    % References: 1. Ofek et al. 2023, PASP 135, Issue 1054, id.124502 -
    %                best-fit CCD quantum efficiency parameters for
    %                default values;
    %             2. Garrappa et al. 2025, A&A 699, A50.
    % Example: % First call with wavelength array
    %          Lam = linspace(300,1100,401);
    %          Sg = astro.atmosphere.skewedGaussianModel(Lam);
    %          % Later calls without arguments
    %          Sg = astro.atmosphere.skewedGaussianModel();
    
    arguments
        Lam = linspace(300, 1100, 401) % wavelength array, nm
        Args.Amplitude = 328.1936 %
        Args.Center = 570.973     % nm
        Args.Sigma = 139.77       % nm
        Args.Gamma = -0.1517      % skewness parameter
    end

    persistent cachedModel cachedLam cachedArgs

    % Check if requesting cached data (no Lam provided or empty)
    if isempty(Lam)
        if isempty(cachedModel)
            error('No cached skewed Gaussian data. Call with Lam array first.');
        end
        Sg_model = cachedModel;
        return;
    end

    % Check if we can use cached data (same Lam and Args)
    if ~isempty(cachedModel) && isequal(Lam, cachedLam) && isequal(Args, cachedArgs)
        Sg_model = cachedModel;
        return;
    end

    % Calculate the skewed Gaussian model
    % Normalized X values
    X = (Lam - Args.Center) / Args.Sigma;

    % Standard Gaussian component
    Gaussian = exp(-0.5 * X.^2);

    % Skewness component using error function
    Skew = 1 + erf(Args.Gamma * X / sqrt(2));

    % Combined skewed Gaussian with proper normalization (Garrappa et al. 2025, (8))
    normalization = 1 / (Args.Sigma * sqrt(2 * pi));
    Sg_model = Args.Amplitude * normalization * Gaussian .* Skew;

    % Cache the results
    cachedModel = Sg_model;
    cachedLam = Lam;
    cachedArgs = Args;
end