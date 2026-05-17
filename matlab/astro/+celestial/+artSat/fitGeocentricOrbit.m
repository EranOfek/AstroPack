function Result = fitGeocentricOrbit(JD, RA, Dec, Args)
% Simple fit of a Keplerian geocentric orbit to topocentric observations.
%
% Given a sequence of (RA, Dec) astrometric observations from a ground station,
% determines the 6 Keplerian orbital elements by:
%
%  (1) Computing observer ECI positions via WGS84 ellipsoid + Local Sidereal Time
%  (2) Fitting the best-fit great circle to the LOS arc; computing signed angular
%      deviations from it.  Physically: a body at infinite distance on a linear
%      track lies exactly on a great circle; nonzero deviations β(t) encode the
%      orbit curvature κ⊥ ∝ μ/Δ² and thereby constrain range and orbit.
%  (3) Forming an initial orbit via the Laplace method: differentiates the LOS
%      vectors at the reference epoch (3-point Lagrange), then solves the
%      scalar Laplace equation (degree-8 polynomial in topocentric range Δ)
%      by numerical root search + bisection.
%  (4) Recovering initial velocity from the range-rate equation.
%  (5) Refining the orbit with weighted Levenberg-Marquardt least squares on
%      angular residuals; recovering formal parameter errors from the Jacobian.
%
% Input  : - JD      [N×1]   Julian Dates of observations                    (N ≥ 3)
%          - RA      [N×1]   Topocentric Right Ascension                     [deg]
%          - Dec     [N×1]   Topocentric Declination                         [deg]
%          * ...,key,val,...
%            'GeoPos' -  [1×3]   Observer geodetic position:
%                   [EastLon(deg), Lat(deg), Height above WGS84(m)]
%                   Default: [35, 30, 415]  (central Israel)
%
%            'ErrRA' -   [N×1 or scalar]  1-σ positional uncertainty in RA (×cos Dec) [arcsec]
%                   Default: 1 arcsec uniformly.
%            'ErrDec' -  [N×1 or scalar]  1-σ positional uncertainty in Dec           [arcsec]
%                   Default: 1 arcsec uniformly.
%
% Output : - A structure with the following fields:
%            .Elements        [1×6]   Keplerian elements at RefEpoch:
%                            [a(km), e, i(deg), Ω(deg), ω(deg), M₀(deg)]
%            .ErrElements     [1×6]   1-σ formal errors on each element
%                            [km, –, deg, deg, deg, deg]
%            .StateVec        [1×6]   State vector at RefEpoch [x,y,z km | vx,vy,vz km/s]
%            .ErrStateVec     [1×6]   1-σ formal errors [km (×3), km/s (×3)]
%            .CovStateVec     [6×6]   Full covariance matrix of the state vector
%            .RefEpoch        [1×1]   JD of the reference epoch (middle observation)
%            .Range_km        [N×1]   Topocentric range at each epoch             [km]
%            .Residuals_arcsec[N×2]   Post-fit [ΔRA·cosDec, ΔDec] residuals   [arcsec]
%            .RMS_arcsec      [1×1]   Unweighted RMS angular residual          [arcsec]
%            .Chi2            [1×1]   Weighted χ² = Σ[(ΔRA·cosDec/σ_RA)² + (ΔDec/σ_Dec)²]
%            .DoF             [1×1]   Degrees of freedom = 2N − 6
%            .Chi2red         [1×1]   Reduced χ² = χ²/DoF  (≈1 for well-fitted model)
%            .GreatCircle     struct  Great-circle fit result:
%            .Pole          [1×3]   Unit normal to best-fit great circle (ECI)
%            .PoleRA        [1×1]   Pole Right Ascension                      [deg]
%            .PoleDec       [1×1]   Pole Declination                          [deg]
%            .Dev_arcsec    [N×1]   Signed deviations β = arcsin(N̂·ê)      [arcsec]
%            .ObsECI          [N×3]   Observer geocentric ECI positions          [km]
%            .ConvergedLS     logical  Least-squares convergence flag
%            .InitDelta_km    [1×1]   Initial range estimate from Laplace method  [km]
%
%   IMPORTANT — interpretation of Chi2 and formal errors:
%     When ErrRA / ErrDec are the true measurement uncertainties, χ²/DoF ≈ 1
%     indicates a good fit and the formal parameter errors are well-calibrated.
%     With the default 1 arcsec errors, χ²/DoF = RMS²_arcsec; multiply the
%     formal errors by sqrt(χ²/DoF) to rescale to actual scatter.
%
% ─── External dependency ──────────────────────────────────────────────────────
%   celestial.time.lst(JD, EastLon_rad) → [FractionOfDay, LST_rad]
%   (MAAT – Matlab Astronomy & Astrophysics Toolbox, E. Ofek)
%
% ─── References ───────────────────────────────────────────────────────────────
%   Vallado D.A. (2013) "Fundamentals of Astrodynamics and Applications" 4th ed.
%   Bate, Mueller & White (1971) "Fundamentals of Astrodynamics"
% Author : Clause + Eran Ofek (May 2026)
% Example: R = celestial.artSat.fitGeocentricOrbit(JD,RA,Dec, 'ErrRA',1, 'ErrDec',1)




    arguments
        JD           (:,1) double
        RA           (:,1) double
        Dec          (:,1) double
        Args.GeoPos  (1,3) double = [35, 30, 415]
        Args.ErrRA   (:,1) double = []   % 1-σ RA uncertainty per obs  [arcsec]
        Args.ErrDec  (:,1) double = []   % 1-σ Dec uncertainty per obs [arcsec]
    end

    %% ── Constants ────────────────────────────────────────────────────────────
    MU_E     = 398600.4418;       % Earth gravitational parameter [km³/s²]
    RE_WGS84 = 6378.137;          % WGS84 semi-major axis         [km]
    E2_WGS84 = 0.00669437999014;  % WGS84 first eccentricity²
    ARCSEC   = 206264.806247;     % arcseconds per radian
    DEG2RAD  = pi / 180;

    N = numel(JD);
    assert(N >= 3, ...
        'fitGeocentricOrbit: at least 3 observations required (%d provided)', N);
    assert(2*N > 6, ...
        'fitGeocentricOrbit: need 2N > 6 for nonzero DoF (have N=%d)', N);

    JD  = JD(:);   RA = RA(:);   Dec = Dec(:);
    T   = (JD - JD(1)) * 86400;    % seconds from first epoch

    %% ── Per-observation weights [N×1] ───────────────────────────────────────
    % Default = 1 arcsec each coordinate.
    % Weight  w = 1/σ_rad = ARCSEC/σ_arcsec  [rad⁻¹].
    % Weighted residual (dimensionless): res_w = Δ_rad · w = Δ_arcsec / σ_arcsec.
    ErrRA  = parseErrVec(Args.ErrRA,  N, 1.0);   % [N×1]  arcsec
    ErrDec = parseErrVec(Args.ErrDec, N, 1.0);   % [N×1]  arcsec
    WRA    = ARCSEC ./ ErrRA;                     % [N×1]  rad⁻¹
    WDec   = ARCSEC ./ ErrDec;                    % [N×1]  rad⁻¹

    %% ── Step 1: Observer ECI positions  [N×3] km ────────────────────────────
    ObsECI = computeObserverECI(JD, Args.GeoPos, RE_WGS84, E2_WGS84);

    %% ── Step 2: Topocentric unit LOS vectors  [N×3] ─────────────────────────
    RAr  = RA  * DEG2RAD;
    Decr = Dec * DEG2RAD;
    eLOS = [cos(Decr).*cos(RAr), cos(Decr).*sin(RAr), sin(Decr)];

    %% ── Step 3: Great-circle fit and arc deviations ──────────────────────────
    GCFit = fitGreatCircle(eLOS);

    %% ── Step 4: Laplace initial orbit ────────────────────────────────────────
    i1 = 1;
    i2 = round((N + 1) / 2);
    i3 = N;

    fprintf('fitGeocentricOrbit: running Laplace initialisation...\n');
    [Delta_init, SV0] = laplaceInit(T, ObsECI, eLOS, i1, i2, i3, MU_E, RE_WGS84);
    fprintf('  Laplace range estimate: %.1f km\n', Delta_init);

    %% ── Step 5: Weighted Levenberg-Marquardt least squares ───────────────────
    %
    % Residual function returns dimensionless vector (units of σ):
    %   res(1:N)    = ΔRA_k · cos(Dec_k) / σ_RA_k
    %   res(N+1:2N) = ΔDec_k             / σ_Dec_k
    %
    % Therefore lsqnonlin's resnorm = Σ res² = χ²  directly.
    % The Jacobian J at the solution satisfies  Cov_SV = (JᵀJ)⁻¹.
    T_ref  = T(i2);
    dT     = T - T_ref;

    resFun = @(sv) angResiduals(sv, dT, ObsECI, eLOS, MU_E, WRA, WDec);

    lsOpts = optimoptions('lsqnonlin', ...
        'Algorithm',              'levenberg-marquardt', ...
        'MaxIterations',           500,     ...
        'MaxFunctionEvaluations',  50000,   ...
        'FunctionTolerance',       1e-13,   ...
        'StepTolerance',           1e-13,   ...
        'FiniteDifferenceType',    'central', ... % symmetric differences → better Jacobian
        'Display',                 'off');

    % 7th output: Jacobian of resFun at the solution (sparse [2N×6])
    [SVopt, chi2, ~, exitflag, ~, ~, jac] = lsqnonlin(resFun, SV0, [], [], lsOpts);
    converged = (exitflag > 0);

    %% ── Step 6: χ², DoF, and covariance of state vector ─────────────────────
    dof     = 2*N - 6;
    chi2red = chi2 / dof;

    % Cov_SV = (JᵀJ)⁻¹  computed via SVD for rank safety.
    % Singular values below machine-precision threshold are zeroed
    % (protects against near-singular geometry / very short arcs).
    Jfull  = full(jac);                               % [2N × 6]
    [~, Sj, Vj] = svd(Jfull, 0);
    sv     = diag(Sj);                                % [6×1]
    tol_sv = max(size(Jfull)) * eps(max(sv));
    sinv   = zeros(size(sv));
    sinv(sv > tol_sv) = 1 ./ sv(sv > tol_sv);
    Cov_SV = Vj * diag(sinv.^2) * Vj';               % [6×6]  km², km²/s²
    ErrSV  = sqrt(abs(diag(Cov_SV)));                 % [6×1]  1-sigma

    %% ── Step 7: Post-fit residuals, ranges, orbital elements ─────────────────
    [resRad, ~, ~, Ranges] = predictAndResidue(SVopt, dT, ObsECI, eLOS, MU_E);

    ResArcSec  = reshape(resRad, N, 2) * ARCSEC;     % [N×2]  unweighted [arcsec]
    RMS_arcsec = sqrt(mean(resRad.^2)) * ARCSEC;

    Elems = rv2elements(SVopt(1:3)', SVopt(4:6)', MU_E);

    %% ── Step 8: Error propagation to Keplerian elements ─────────────────────
    %
    % Chain rule:  Cov_Elem = J_e · Cov_SV · J_eᵀ
    % where J_e = ∂Elems/∂SV  [6×6] computed by central-difference perturbation.
    J_elem   = elemJacobian(SVopt, MU_E);             % [6×6]
    Cov_Elem = J_elem * Cov_SV * J_elem';             % [6×6]
    ErrElem  = sqrt(abs(diag(Cov_Elem)));             % [6×1]  1-sigma

    %% ── Assemble output ──────────────────────────────────────────────────────
    Result.Elements              = Elems;
    Result.ErrElements           = ErrElem(:)';       % [1×6]
    Result.StateVec              = SVopt;
    Result.ErrStateVec           = ErrSV(:)';         % [1×6]
    Result.CovStateVec           = Cov_SV;            % [6×6]
    Result.RefEpoch              = JD(i2);
    Result.Range_km              = Ranges;
    Result.Residuals_arcsec      = ResArcSec;         % [N×2]
    Result.RMS_arcsec            = RMS_arcsec;
    Result.Chi2                  = chi2;
    Result.DoF                   = dof;
    Result.Chi2red               = chi2red;
    Result.GreatCircle.Pole      = GCFit.Pole;
    Result.GreatCircle.PoleRA    = GCFit.PoleRA;
    Result.GreatCircle.PoleDec   = GCFit.PoleDec;
    Result.GreatCircle.Dev_arcsec = GCFit.Dev * ARCSEC;
    Result.ObsECI                = ObsECI;
    Result.ConvergedLS           = converged;
    Result.InitDelta_km          = Delta_init;

    fprintf('fitGeocentricOrbit: chi2=%.2f  dof=%d  chi2_red=%.3f  converged=%d\n', ...
            chi2, dof, chi2red, converged);
    fprintf('  RMS      =  %.4f arcsec\n', RMS_arcsec);
    fprintf('  %-8s   %12s  ±  %s\n', 'Element', 'Value', 'Formal 1-sigma');
    fprintf('  %-8s   %12.3f  ±  %.3f  km\n',  'a',     Elems(1), ErrElem(1));
    fprintf('  %-8s   %12.7f  ±  %.7f\n',       'e',     Elems(2), ErrElem(2));
    fprintf('  %-8s   %12.5f  ±  %.5f  deg\n', 'i',     Elems(3), ErrElem(3));
    fprintf('  %-8s   %12.5f  ±  %.5f  deg\n', 'Omega', Elems(4), ErrElem(4));
    fprintf('  %-8s   %12.5f  ±  %.5f  deg\n', 'omega', Elems(5), ErrElem(5));
    fprintf('  %-8s   %12.5f  ±  %.5f  deg\n', 'M0',    Elems(6), ErrElem(6));
end


%% ============================================================================
%%  LOCAL FUNCTIONS
%% ============================================================================

function v = parseErrVec(v_in, N, default_val)
%parseErrVec  Build a per-observation uncertainty vector [N×1].
%
% Accepts:
%   []         → uniform default_val (arcsec)
%   scalar s   → uniform s
%   [N×1]      → per-observation values (length validated)

    if isempty(v_in)
        v = repmat(default_val, N, 1);
    elseif isscalar(v_in)
        assert(v_in > 0, 'fitGeocentricOrbit: error values must be positive');
        v = repmat(double(v_in), N, 1);
    else
        v = double(v_in(:));
        assert(numel(v) == N, ...
            'fitGeocentricOrbit: ErrRA/ErrDec has %d elements but N=%d observations', ...
            numel(v), N);
        assert(all(v > 0), 'fitGeocentricOrbit: all error values must be positive');
    end
end


function R = computeObserverECI(JD, GeoPos, RE, E2)
%computeObserverECI  Geodetic observer position → geocentric ECI [km].
%
% Uses the identity that because LST = GMST + λ_E, the ECEF→ECI rotation
% by GMST combined with the station's east longitude λ_E gives exactly LST
% as the azimuthal angle in the equatorial plane:
%
%   x_ECI = ρ_⊥ · cos(LST)
%   y_ECI = ρ_⊥ · sin(LST)      ρ_⊥ = (N_φ + h) cos φ
%   z_ECI = z_ECEF               z   = (N_φ(1-e²) + h) sin φ

    lon_rad = GeoPos(1) * (pi/180);
    lat_rad = GeoPos(2) * (pi/180);
    h_km    = GeoPos(3) / 1000;

    N_phi  = RE / sqrt(1 - E2 * sin(lat_rad)^2);
    rho_xy = (N_phi + h_km) * cos(lat_rad);
    z_ecef = (N_phi*(1-E2) + h_km) * sin(lat_rad);

    [LST] = celestial.time.lst(JD(:), lon_rad);   % LST [rad]
    LST = LST(:).*2.*pi;

    R = [rho_xy * cos(LST), ...
         rho_xy * sin(LST), ...
         repmat(z_ecef, numel(JD), 1)];               % [N×3]  km
end


function GC = fitGreatCircle(eLOS)
%fitGreatCircle  Best-fit great circle to unit direction vectors via SVD.
%
% Pole N̂ = right singular vector of eLOS [N×3] for the *smallest* singular
% value — the direction "least seen" in the data.
%
% Deviation:  β_i = arcsin(N̂ · ê_i)   [rad]

    [~, ~, V] = svd(eLOS, 0);
    Pole = V(:,3)';
    if Pole(3) < 0, Pole = -Pole; end

    Dev  = asin(clamp(eLOS * Pole'));

    GC.Pole    = Pole;
    GC.PoleRA  = atan2d(Pole(2), Pole(1));
    GC.PoleDec = asind(Pole(3));
    GC.Dev     = Dev;
end


function [Delta_ref, SV0] = laplaceInit(T, ObsECI, eLOS, i1, i2, i3, MU, RE)
%laplaceInit  Initial orbit determination via the Laplace method.
%
% Solves the scalar Laplace equation:
%
%   f(Δ) = A·Δ + B + μC / r(Δ)³ = 0
%
% where:
%   A = (ê × ê̈)·ê̇    B = (ê × R̈)·ê̇    C = (ê × R)·ê̇
%   r(Δ)² = |R|² + 2(R·ê)Δ + Δ²
%
% Derivation: cross EOM with ê to eliminate Δ̈; dot with ê̇ to eliminate Δ̇
% (using (ê × ê̇)·ê̇ ≡ 0).  Range-rate Δ̇ from EOM dotted with ê̇.

    OMEGA_E = 7.2921150e-5;   % Earth sidereal rotation rate [rad/s]

    tau1 = T(i1) - T(i2);    % < 0
    tau3 = T(i3) - T(i2);    % > 0

    e1 = eLOS(i1,:)';  e2 = eLOS(i2,:)';  e3 = eLOS(i3,:)';
    R2 = ObsECI(i2,:)';

    % 3-point Lagrange derivative coefficients at t₂
    c1 = -tau3           / (tau1 * (tau1 - tau3));
    c2 = -(tau1 + tau3)  / (tau1 * tau3);
    c3 =  tau1           / ((tau1 - tau3) * tau3);

    d1 =  2 / (tau1 * (tau1 - tau3));
    d2 =  2 / (tau1 * tau3);
    d3 =  2 / ((tau3 - tau1) * tau3);

    edot2  = c1*e1 + c2*e2 + c3*e3;
    eddot2 = d1*e1 + d2*e2 + d3*e3;

    Rddot2 = -OMEGA_E^2 * [R2(1); R2(2); 0];   % observer accel [km/s²]
    Rdot2  =  OMEGA_E   * [-R2(2); R2(1); 0];  % observer vel   [km/s]

    % Laplace scalar coefficients
    A_coeff = dot(cross(e2, eddot2), edot2);
    B_coeff = dot(cross(e2, Rddot2), edot2);
    C_coeff = dot(cross(e2, R2),     edot2);

    R2sq = dot(R2, R2);
    q    = dot(R2, e2);

    f_lap = @(D) A_coeff.*D + B_coeff + ...
                 MU * C_coeff ./ max(R2sq + 2*q*D + D.^2, 1e-6).^1.5;

    if abs(A_coeff) < 1e-25
        warning('fitGeocentricOrbit:laplaceDegenerate', ...
                'Laplace A≈0 (arc too short / radial geometry). Defaulting Δ=6700 km.');
        Delta_ref = 6700;
    else
        Dgrid = [logspace(log10(220),  log10(1000),  300), ...
                 logspace(log10(1000), log10(1e5),   700)];
        fgrid = arrayfun(f_lap, Dgrid);
        sc    = find(diff(sign(fgrid)) ~= 0);

        if isempty(sc)
            [~, idx]  = min(abs(fgrid));
            Delta_ref = Dgrid(idx);
            warning('fitGeocentricOrbit:laplaceNoRoot', ...
                    'No Laplace root in [220,1e5] km; using closest: %.1f km', Delta_ref);
        else
            Delta_ref = NaN;
            for ks = 1:numel(sc)
                Dr = fzero(f_lap, [Dgrid(sc(ks)), Dgrid(sc(ks)+1)]);
                if Dr > 0 && norm(R2 + Dr*e2) > RE + 100
                    Delta_ref = Dr;  break;
                end
            end
            if isnan(Delta_ref)
                Delta_ref = fzero(f_lap, [Dgrid(sc(1)), Dgrid(sc(1)+1)]);
                warning('fitGeocentricOrbit:laplaceSub', ...
                        'All Laplace roots subterranean; using %.1f km', Delta_ref);
            end
        end
    end

    r2vec = R2 + Delta_ref * e2;
    r2mag = norm(r2vec);

    % Range-rate from EOM · ê̇
    edot2_sq = dot(edot2, edot2);
    if edot2_sq < 1e-30
        Deltadot2 = 0;
        warning('fitGeocentricOrbit:zeroEdot', 'ê̇≈0; setting Δ̇=0');
    else
        Deltadot2 = (-(MU/r2mag^3)*dot(r2vec, edot2) ...
                     - dot(Rddot2, edot2)              ...
                     - Delta_ref*dot(eddot2, edot2))   ...
                    / (2 * edot2_sq);
    end

    v2vec = Rdot2 + Deltadot2*e2 + Delta_ref*edot2;
    SV0   = [r2vec(:)', v2vec(:)'];
end


function res = angResiduals(SV, dT, ObsECI, eLOS_obs, MU, WRA, WDec)
%angResiduals  Weighted angular residuals [2N×1] in units of σ (dimensionless).
%
% res(1:N)    = ΔRA_k · cos(Dec_k) · WRA_k    =  ΔRA·cosDec / σ_RA
% res(N+1:2N) = ΔDec_k             · WDec_k   =  ΔDec / σ_Dec
%
% With this normalisation:  resnorm = Σ res² = χ²
% and lsqnonlin's Jacobian satisfies  Cov_SV = (JᵀJ)⁻¹.

    N    = size(ObsECI, 1);
    RAo  = atan2(eLOS_obs(:,2), eLOS_obs(:,1));
    Deco = asin(clamp(eLOS_obs(:,3)));

    RAp  = zeros(N,1);
    Decp = zeros(N,1);
    for k = 1:N
        rv      = keplerPropagate(SV, dT(k), MU);
        rTopo   = rv(1:3) - ObsECI(k,:);
        eTopo   = rTopo / norm(rTopo);
        RAp(k)  = atan2(eTopo(2), eTopo(1));
        Decp(k) = asin(clamp(eTopo(3)));
    end

    dRA  = wrapToPi(RAp - RAo);
    dDec = Decp - Deco;

    res = [dRA .* cos(Deco) .* WRA; ...   % [N×1]  ΔRA·cosDec / σ_RA
           dDec             .* WDec];     % [N×1]  ΔDec / σ_Dec
end


function [res, RA_pred, Dec_pred, Range] = predictAndResidue(SV, dT, ObsECI, eLOS_obs, MU)
%predictAndResidue  Unweighted residuals [2N×1] in radians + predicted sky coords.
% Used for post-fit display; weights NOT applied.

    N    = size(ObsECI, 1);
    RAo  = atan2(eLOS_obs(:,2), eLOS_obs(:,1));
    Deco = asin(clamp(eLOS_obs(:,3)));

    RA_pred  = zeros(N,1);
    Dec_pred = zeros(N,1);
    Range    = zeros(N,1);

    for k = 1:N
        rv          = keplerPropagate(SV, dT(k), MU);
        rTopo       = rv(1:3) - ObsECI(k,:);
        rang        = norm(rTopo);
        eTopo       = rTopo / rang;
        RA_pred(k)  = atan2(eTopo(2), eTopo(1));
        Dec_pred(k) = asin(clamp(eTopo(3)));
        Range(k)    = rang;
    end

    dRA  = wrapToPi(RA_pred - RAo);
    dDec = Dec_pred - Deco;
    res  = [dRA .* cos(Deco); dDec];     % [2N×1]  radians, unweighted
end


function J = elemJacobian(SV, MU)
%elemJacobian  Numerical Jacobian ∂Elems/∂SV [6×6] via central differences.
%
% Elems = [a(km), e, i(deg), Ω(deg), ω(deg), M₀(deg)]
% SV    = [x,y,z km | vx,vy,vz km/s]
%
% Step sizes:  δr = 1 km (positions),  δv = 1e-3 km/s  = 1 m/s (velocities).
%
% Angular elements (i, Ω, ω, M₀, columns 3-6) are wrapped to [−180°,+180°]
% before dividing by 2δ to handle the 0°/360° branch cut correctly.

    delta = [1, 1, 1, 1e-3, 1e-3, 1e-3];   % [km ×3, km/s ×3]
    J     = zeros(6, 6);

    for j = 1:6
        SVp = SV;  SVp(j) = SVp(j) + delta(j);
        SVm = SV;  SVm(j) = SVm(j) - delta(j);

        Ep  = rv2elements(SVp(1:3)', SVp(4:6)', MU);   % [1×6]
        Em  = rv2elements(SVm(1:3)', SVm(4:6)', MU);   % [1×6]

        dE  = (Ep - Em) / (2 * delta(j));              % finite difference

        % Wrap angular differences (elements 3-6 are degrees)
        for k = 3:6
            dE(k) = (mod(Ep(k) - Em(k) + 180, 360) - 180) / (2 * delta(j));
        end

        J(:, j) = dE(:);
    end
end


function rv1 = keplerPropagate(rv0, dt, MU)
%keplerPropagate  Propagate Keplerian two-body orbit: universal variable method.
%
% Solves the universal Kepler equation in χ (Bate, Mueller & White §2.8):
%
%   √μ·Δt = (r₀·vᵣ/√μ)·χ²c₂(ψ) + (1−r₀α)·χ³c₃(ψ) + r₀·χ,   ψ = αχ²
%
% Single formulation valid for elliptic, hyperbolic, and parabolic orbits.
% dF/dχ = r₁ (current radius) — clean exact Newton derivative.
% Lagrange f,g coefficients:  r₁ = f·r₀ + g·v₀,  v₁ = ḟ·r₀ + ġ·v₀.

    r0v  = rv0(1:3)';    v0v  = rv0(4:6)';
    r0   = norm(r0v);    v0   = norm(v0v);
    vr0  = dot(r0v, v0v) / r0;
    sqmu = sqrt(MU);

    alpha = 2/r0 - v0^2/MU;   % 1/a  (>0 ellipse, <0 hyperbola, ≈0 parabola)

    % Initial guess for χ
    if alpha > 1e-8
        chi = sqmu * dt * alpha;
    elseif alpha < -1e-8
        a     = 1 / alpha;
        rdv   = dot(r0v, v0v);
        denom = rdv + sign(dt)*sqrt(-MU*a)*(1 - r0*alpha);
        chi   = sign(dt) * sqrt(-a) * log(max(1e-30, -2*MU*alpha*dt/denom));
    else
        hv  = cross(r0v, v0v);
        p   = dot(hv, hv) / MU;
        s   = 0.5 * atan(1 / (3*sqrt(MU/p^3)*dt + eps));
        w   = atan(tan(s)^(1/3));
        chi = sqrt(2*p) * cot(2*w);
    end

    % Newton–Raphson on F(χ) = 0
    for it = 1:60   %#ok<FXUP>
        psi      = chi^2 * alpha;
        [c2, c3] = stumpffCS(psi);
        F   = (r0*vr0/sqmu)*chi^2*c2 + (1-r0*alpha)*chi^3*c3 + r0*chi - sqmu*dt;
        dF  = (r0*vr0/sqmu)*chi*(1-psi*c3) + (1-r0*alpha)*chi^2*c2 + r0;
        dchi = F / dF;
        chi  = chi - dchi;
        if abs(dchi) < 1e-10*(1+abs(chi)), break; end
    end

    psi      = chi^2 * alpha;
    [c2, c3] = stumpffCS(psi);
    r1  = chi^2*c2 + (r0*vr0/sqmu)*chi*(1-psi*c3) + r0*(1-psi*c2);

    fL  =  1   - (chi^2/r0)  * c2;
    gL  =  dt  - (chi^3/sqmu)* c3;
    fdL =  (sqmu/(r1*r0)) * chi * (psi*c3-1);
    gdL =  1   - (chi^2/r1)  * c2;

    rv1 = [fL*r0v' + gL*v0v',  fdL*r0v' + gdL*v0v'];
end


function [c2, c3] = stumpffCS(psi)
%stumpffCS  Stumpff functions c₂(ψ) and c₃(ψ).

    if psi > 1e-6
        sqp = sqrt(psi);
        c2  = (1 - cos(sqp))   / psi;
        c3  = (sqp - sin(sqp)) / (psi * sqp);
    elseif psi < -1e-6
        sqn = sqrt(-psi);
        c2  = (1 - cosh(sqn))   / psi;
        c3  = (sinh(sqn) - sqn) / (-psi * sqn);
    else
        c2  = 0.5   - psi/24    + psi^2/720;
        c3  = 1/6   - psi/120   + psi^2/5040;
    end
end


function Elems = rv2elements(r, v, MU)
%rv2elements  State vector [r km, v km/s] → Keplerian elements [1×6].
%
% Output: [a(km), e, i(deg), Ω(deg), ω(deg), M₀(deg)]
%   Angles in [0°,180°] for i, [0°,360°) for Ω, ω, M₀.
%   Undefined angles (equatorial/circular) set to 0.

    r = r(:); v = v(:);
    rmag = norm(r);

    hvec = cross(r, v);
    hmag = norm(hvec);
    evec = cross(v, hvec) / MU - r / rmag;
    e    = norm(evec);

    energy = norm(v)^2/2 - MU/rmag;
    a = -MU / (2*energy);   % Inf for parabola (energy≈0)

    i_deg = acosd(clamp(hvec(3) / hmag));

    Nvec  = cross([0;0;1], hvec);
    Nmag  = norm(Nvec);

    if Nmag > 1e-10
        Omega = acosd(clamp(Nvec(1)/Nmag));
        if Nvec(2) < 0, Omega = 360 - Omega; end
    else
        Omega = 0;
    end

    if Nmag > 1e-10 && e > 1e-8
        omega = acosd(clamp(dot(Nvec,evec)/(Nmag*e)));
        if evec(3) < 0, omega = 360 - omega; end
    else
        omega = 0;
    end

    if e > 1e-8
        nu = acos(clamp(dot(evec,r)/(e*rmag)));
        if dot(r,v) < 0, nu = 2*pi - nu; end
    else
        nu = acos(clamp(dot(Nvec,r)/(Nmag*rmag)));
        if r(3) < 0, nu = 2*pi - nu; end
    end

    if e < 1 - 1e-7        % elliptic
        cosE = (e + cos(nu))               / (1 + e*cos(nu));
        sinE = sqrt(max(0,1-e^2))*sin(nu)  / (1 + e*cos(nu));
        E0   = atan2(sinE, cosE);
        M0   = mod((E0 - e*sin(E0))*180/pi, 360);
    elseif e > 1 + 1e-7    % hyperbolic
        sinhH = sqrt(max(0,e^2-1))*sin(nu) / (1+e*cos(nu));
        H0    = asinh(sinhH);
        M0    = mod((e*sinh(H0) - H0)*180/pi, 360);
    else
        M0 = 0;             % parabolic
    end

    Elems = [a, e, i_deg, Omega, omega, M0];
end


function x = clamp(x)
%clamp  Clamp to [−1, 1] for safe asin/acos.
    x = max(-1.0, min(1.0, x));
end
