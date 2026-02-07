function [Oblateness, P_Precession] = rotationFlattening_freePrecession(Mass, Radius, Period, Args)
    % Rotation induced flattening of a rigid body and free-precession period
    % Input  : - Mass [g]
    %          - Radius [cm]
    %          - Rotation period [s]
    %          * ...,key,val,...
    %            'k' - Moment of inretia coef. k = I/(M R^2)
    %                   I.e., 0.4 for a constant density solid body.
    %                   0.3307 for a solid body with Earth like density
    %                   profile.
    %                   Default is 0.2.
    %            'n' - Polytropic index. If given then will used instead of
    %                   k. For WD the range of 1.5 to 3 is reasonable.
    %                   Default is [].
    % Output : - Oblatness = 1/flattening.
    %          - Free precession period [s].
    %
    % Notes:
    %   - Uses Darwin–Radau to map (k,q)->epsilon.
    %   - P_Precession ≈ Period*(1-epsilon)/epsilon (assumes small wobble angle, cos(theta)≈1).
    %   - Broadcasting is done via implicit expansion (MATLAB R2016b+).
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: [e,p]=astro.stars.rotationFlattening_freePrecession(5.984e27,6371e5,86400,'k',0.3307)
    
    arguments
        Mass   double {mustBePositive}
        Radius double {mustBePositive}
        Period double {mustBePositive}
        Args.k (1,1) double {mustBePositive} = 0.2
        Args.n (1,1) double = NaN
    end
    
    % --- Constants (CGS) ---
    G = 6.67430e-8; % cm^3 g^-1 s^-2
    
    % --- Choose k: either direct or from polytropic index n ---
    K = Args.k;
    
    if isfinite(Args.n)
        % Approximate k(n) by interpolation between common polytrope limits:
        % n=1.5 -> k~0.205, n=3 -> k~0.0754 (clamp outside range)
        N1 = 1.5;  K1 = 0.205;
        N2 = 3.0;  K2 = 0.0754;
    
        N = Args.n;
        if N <= N1
            K = K1;
        elseif N >= N2
            K = K2;
        else
            K = K1 + (K2 - K1) * (N - N1) / (N2 - N1);
        end
    end
    
    % --- Rotation parameter q (vectorized via implicit expansion) ---
    Omega = 2*pi ./ Period;                     % s^-1
    Q = (Omega.^2) .* (Radius.^3) ./ (G.*Mass); % dimensionless
    
    % --- Darwin–Radau inversion: epsilon from k and q ---
    S = (5/2) * (1 - (3/2)*K);
    Eta = S^2 - 1;
    
    Oblateness = (5 .* Q) ./ (2 .* (Eta + 2));
    
    % --- Basic physical sanity checks (vectorized) ---
    if any(~isfinite(Oblateness(:))) || any(Oblateness(:) <= 0)
        error('Computed Oblateness contains non-physical values. Check inputs or near-breakup regime.');
    end
    
    % --- Precession period (small-wobble symmetric-top approximation) ---
    P_Precession = Period .* (1 - Oblateness) ./ Oblateness;

end
