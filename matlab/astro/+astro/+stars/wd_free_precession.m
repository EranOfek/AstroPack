function results = wd_free_precession(opts)
%WD_FREE_PRECESSION  Predict a white dwarf's oblateness and free (Euler)
%precession period from its EOS, rotation, internal magnetic field, and
%crystallization state.
%
%   results = wd_free_precession('Prot', P, Name, Value, ...)
%
% Requires MATLAB R2019b or later (uses a function argument-validation
% block with name-value pairs). Not compatible with GNU Octave.
%
% -------------------------------------------------------------------------
% PHYSICS / PIPELINE
% -------------------------------------------------------------------------
%   1. Non-rotating structure: integrate hydrostatic equilibrium with the
%      supplied EOS (rho as a function of P) to get rho(r), M, R, the
%      moment-of-inertia factor C = I/(M R^2), and the gravitational
%      structure constant alphaG = W/(G M^2/R).
%   2. Rotational oblateness: Clairaut's equation (solved exactly with the
%      density profile from step 1), or the closed-form Darwin-Radau
%      approximation as a fast/fallback method.
%   3. Magnetic distortion: energy-ratio estimate
%         eps_B = kB * E_mag / W ,   E_mag ~ (2*pi/3) Bbar^2 R^3 / mu0
%      kB > 0 for a toroidal-field-dominated (oblate-making) interior,
%      kB < 0 for a poloidal/dipole-dominated (prolate-making) interior.
%   4. Elastic "mountain" from a crystallized core/fraction:
%         eps_elastic_max = ThetaBreak * muOverP * f(Xcr)
%      with f(Xcr) = Xcr^geomExponent a crude geometric weight for how
%      much of the quadrupole moment (~r^4-weighted) the crystallized
%      region can support; f(Xcr)->1 as the star becomes fully
%      crystallized. Because a WD crystallizes from the CENTER outward
%      (unlike a neutron star's outer solid crust), small Xcr means a
%      small solid core buried deep inside -- heavily suppressed by the
%      r^4 weighting -- hence geomExponent > 1 by default.
%   5. Combine (signed sum) -> eps_total -> P_free = P_rot / |eps_total|.
%      Because the sign of the frozen-in elastic term is not derived from
%      equilibrium physics, the function reports both a single
%      "best-guess" value (elasticSign, elasticEfficiency) and, if
%      Mode = "range", the full constructive/destructive envelope.
%
% All internal physics is SI (kg, m, s, Tesla). Convenience inputs are in
% the units astronomers usually quote (solar masses, km, Gauss, seconds).
%
% -------------------------------------------------------------------------
% NAME-VALUE ARGUMENTS (all optional except Prot; defaults in parens)
% -------------------------------------------------------------------------
%  -- Structure / EOS (choose ONE mode) --
%   ModeStructure       "polytrope" (default) | "given"
%   -- polytrope mode --
%   Gamma                polytropic index, P = K*rho^Gamma        (5/3)
%   K                    polytropic constant, SI (Pa (kg/m^3)^-Gamma)
%                         ([] -> calibrated default, see get_default_K)
%   RhoC                 central density, kg/m^3                  (1e10)
%   -- given mode (skip structure solve) --
%   M                    mass, Msun                                (--)
%   R                    radius, km                                (--)
%   C                    moment-of-inertia factor I/(M R^2)         (0.21)
%   AlphaG               gravitational structure constant           (0.6)
%
%  -- Rotation --
%   Prot                 rotation period, seconds                  (REQUIRED)
%   RotMethod             "clairaut" (default, needs rho(r)) | "radau"
%
%  -- Magnetic field --
%   B                    characteristic INTERNAL field, Gauss        (0)
%   KB                   geometry coefficient (+toroidal/-poloidal) (0.5)
%
%  -- Crystallization / elastic mountain --
%   Xcr                  crystallized mass fraction, 0-1             (0)
%   ThetaBreak            breaking strain of the Coulomb lattice      (0.07)
%   MuOverP              shear-modulus-to-pressure ratio in the       (0.01)
%                         crystallized region (WD core estimate)
%   GeomExponent          exponent in f(Xcr)=Xcr^GeomExponent          (4/3)
%   ElasticEfficiency    fraction of the theoretical max mountain     (1.0)
%                         actually realized, 0-1
%   ElasticSign           +1 (reinforces rotation, default/"generic"
%                         expectation) or -1 (opposes/cancels rotation)
%   Mode                 "point" (default, use ElasticSign) | "range"
%                         ("range" also returns the full envelope from
%                          ElasticSign=-1 to +1)
%
% -------------------------------------------------------------------------
% OUTPUT
% -------------------------------------------------------------------------
%   results.M, results.R, results.C, results.alphaG   structure outputs
%   results.eps_rot, results.eps_B, results.eps_elastic_max
%   results.eps_total, results.P_free                 point prediction
%   results.range.eps_total_min/max, results.range.P_free_shortest/longest
%                                                       (if Mode="range")
%   results.rho, results.r, results.m                  structure profile
%
% Example:
%   r = wd_free_precession('Prot', 86400, 'RhoC', 1e10, 'B', 1e6, ...
%                           'Xcr', 0.6, 'Mode', 'range');
%   fprintf('P_free (best guess) = %.3g yr\n', r.P_free/3.15576e7);
%
% -------------------------------------------------------------------------

arguments
    opts.ModeStructure (1,1) string {mustBeMember(opts.ModeStructure,["polytrope","given"])} = "polytrope"

    % polytrope-mode EOS
    opts.Gamma (1,1) double {mustBePositive} = 5/3
    opts.K double = []                      % [] -> calibrated default
    opts.RhoC (1,1) double {mustBePositive} = 1e10

    % given-mode structure (skips the EOS integration)
    opts.M double = []
    opts.R double = []
    opts.C (1,1) double {mustBePositive} = 0.21
    opts.AlphaG (1,1) double {mustBePositive} = 0.6

    % rotation
    opts.Prot (1,1) double {mustBePositive}
    opts.RotMethod (1,1) string {mustBeMember(opts.RotMethod,["clairaut","radau"])} = "clairaut"

    % magnetic field
    opts.B (1,1) double {mustBeNonnegative} = 0
    opts.KB (1,1) double = 0.5

    % crystallization / elastic mountain
    opts.Xcr (1,1) double {mustBeGreaterThanOrEqual(opts.Xcr,0), mustBeLessThanOrEqual(opts.Xcr,1)} = 0
    opts.ThetaBreak (1,1) double {mustBePositive} = 0.07
    opts.MuOverP (1,1) double {mustBePositive} = 0.01
    opts.GeomExponent (1,1) double {mustBePositive} = 4/3
    opts.ElasticEfficiency (1,1) double {mustBeGreaterThanOrEqual(opts.ElasticEfficiency,0), mustBeLessThanOrEqual(opts.ElasticEfficiency,1)} = 1.0
    opts.ElasticSign (1,1) double {mustBeMember(opts.ElasticSign,[-1,1])} = 1

    opts.Mode (1,1) string {mustBeMember(opts.Mode,["point","range"])} = "point"
end

G  = 6.67430e-11;      % m^3 kg^-1 s^-2
mu0 = 4*pi*1e-7;       % vacuum permeability, SI
MSUN = 1.98892e30;     % kg

% ---------------- 1. Structure ----------------
switch opts.ModeStructure
    case "polytrope"
        K = opts.K;
        if isempty(K)
            K = get_default_K(opts.Gamma);
        end
        st = solve_polytrope_structure(opts.Gamma, K, opts.RhoC, G);
    case "given"
        if isempty(opts.M) || isempty(opts.R)
            error('wd_free_precession:missingGiven', ...
                'ModeStructure="given" requires both M (Msun) and R (km).');
        end
        st = struct('M', opts.M*MSUN, 'R', opts.R*1e3, 'C', opts.C, ...
                     'alphaG', opts.AlphaG, 'r', [], 'rho', [], 'm', [], 'Pc', NaN);
end

M = st.M; R = st.R; C = st.C; alphaG = st.alphaG;

% ---------------- 2. Rotational oblateness ----------------
Omega = 2*pi/opts.Prot;
zeta = Omega^2 * R^3 / (3*G*M);

use_clairaut = (opts.RotMethod == "clairaut") && ~isempty(st.r);
if use_clairaut
    eps_rot = clairaut_ellipticity(st.r, st.rho, R, zeta);
else
    if opts.RotMethod == "clairaut"
        warning('wd_free_precession:noProfile', ...
            ['RotMethod="clairaut" requested but no density profile ' ...
             'is available (ModeStructure="given"); falling back to ' ...
             'the Darwin-Radau approximation.']);
    end
    eps_rot = darwin_radau_ellipticity(C, zeta);
end

% ---------------- 3. Magnetic distortion ----------------
B_T = opts.B * 1e-4;   % Gauss -> Tesla
E_mag = (2*pi/3) * B_T^2 * R^3 / mu0;
W     = alphaG * G * M^2 / R;
eps_B = opts.KB * E_mag / W;

% ---------------- 4. Elastic mountain from crystallization ----------------
if opts.ModeStructure == "polytrope"
    Pchar = st.Pc;   % central pressure, characteristic scale
else
    Pchar = alphaG * G * M^2 / R^4;   % rough pressure scale if no profile
end
f_Xcr = opts.Xcr ^ opts.GeomExponent;
eps_elastic_max = opts.ThetaBreak * opts.MuOverP * f_Xcr;

% ---------------- 5. Combine ----------------
eps_elastic = opts.ElasticSign * opts.ElasticEfficiency * eps_elastic_max;
eps_total   = eps_rot + eps_B + eps_elastic;
P_free      = opts.Prot / abs(eps_total);

results = struct();
results.M = M; results.R = R; results.C = C; results.alphaG = alphaG;
results.eps_rot = eps_rot;
results.eps_B = eps_B;
results.eps_elastic_max = eps_elastic_max;
results.eps_elastic = eps_elastic;
results.eps_total = eps_total;
results.P_free = P_free;
results.Prot = opts.Prot;
results.r = st.r; results.rho = st.rho; results.m = st.m; results.Pc = Pchar;

if opts.Mode == "range"
    eB_env = abs(eps_elastic_max) * opts.ElasticEfficiency;
    eps_max_env = eps_rot + eps_B + eB_env;   % fully constructive
    eps_min_env = eps_rot + eps_B - eB_env;   % fully destructive
    results.range.eps_total_max = eps_max_env;
    results.range.eps_total_min = eps_min_env;
    results.range.P_free_shortest = opts.Prot / max(abs(eps_max_env), abs(eps_min_env));
    if abs(eps_min_env) < 1e-300
        results.range.P_free_longest = Inf;
    else
        results.range.P_free_longest = opts.Prot / abs(eps_min_env);
    end
end

end % ================= main function =================


% =========================================================================
function K = get_default_K(Gamma)
% Rough default polytropic constant so a Gamma=5/3 model gives WD-like
% radii (only used if the caller does not supply K explicitly).
% Non-relativistic degenerate electron gas:
%   K = ((3*pi^2)^(2/3)/5) * hbar^2/(m_e*(mu_e*m_u)^(5/3))
hbar = 1.054571817e-34; me = 9.1093837015e-31; mu_ = 1.66053906660e-27;
mu_e = 2; % mean molecular weight per electron for fully ionized C/O
if abs(Gamma-5/3) > 1e-6
    % generic fallback for other Gamma: an O(1)-in-SI placeholder; supply
    % K explicitly for non-standard Gamma.
    K = 3.2e6;
else
    K = ((3*pi^2)^(2/3)/5) * hbar^2 / (me * (mu_e*mu_)^(5/3));
end
end


% =========================================================================
function st = solve_polytrope_structure(Gamma, K, rho_c, G)
%SOLVE_POLYTROPE_STRUCTURE Integrate dP/dr, dm/dr for P=K*rho^Gamma
% starting from a small-r series expansion near the center, stopping when
% P first reaches zero (the surface). Returns r, rho(r), m(r), R, M,
% C=I/(M R^2), alphaG=W/(G M^2/R), and the central pressure Pc.

rho_of_P = @(P) (max(P,0)/K).^(1/Gamma);
Pc = K * rho_c^Gamma;

r0 = 1;                       % starting radius, 1 m (deep inside a WD)
m0 = (4/3)*pi*rho_c*r0^3;
P0 = Pc - (2/3)*pi*G*rho_c^2*r0^2;

opts_ode = odeset('Events', @(r,y) surface_event(r,y), 'RelTol',1e-9,'AbsTol',[Pc*1e-12, m0*1e-12]);
rspan = [r0, 1e10];   % generous outer bound, event stops it at the surface

[rvec,yvec] = ode45(@(r,y) structure_rhs(r,y,rho_of_P,G), rspan, [P0; m0], opts_ode);

R = rvec(end);
M = yvec(end,2);
Pvec = yvec(:,1); mvec = yvec(:,2);
rhovec = rho_of_P(Pvec);

% moment of inertia I = (8*pi/3) * int rho r^4 dr
I  = (8*pi/3) * trapz(rvec, rhovec .* rvec.^4);
C  = I / (M*R^2);

% gravitational structure constant: W = 4*pi*G*int rho(r)*m(r)*r dr
W  = 4*pi*G * trapz(rvec, rhovec .* mvec .* rvec);
alphaG = W / (G*M^2/R);

st = struct('M',M,'R',R,'C',C,'alphaG',alphaG,'r',rvec,'rho',rhovec,'m',mvec,'Pc',Pc);
end

function dydr = structure_rhs(r,y,rho_of_P,G)
P = y(1); m = y(2);
rho = rho_of_P(P);
dPdr = -G*m*rho/r^2;
dmdr = 4*pi*r^2*rho;
dydr = [dPdr; dmdr];
end

function [value,isterminal,direction] = surface_event(~,y)
value = y(1);        % P = 0 at the surface
isterminal = 1;
direction = -1;
end


% =========================================================================
function eps0 = darwin_radau_ellipticity(C, zeta)
%DARWIN_RADAU_ELLIPTICITY  Closed-form rotational dynamical ellipticity
% (I3-I1)/I1 from the moment-of-inertia factor C=I/(M R^2) and the
% rotation parameter zeta = Omega^2 R^3/(3 G M).
eta0 = 5/2 - (15/4)*C;
eps_surface = (15/2)*zeta / (1 + eta0^2);     % geometric flattening eps(R)
J2_over_eps0 = -3/10 + (5/2)*C - (15/8)*C^2;
J2 = J2_over_eps0 * eps_surface;
eps0 = (J2/C) / (1 - J2/C);                   % (I3-I1)/I1, exact in J2/C
end


% =========================================================================
function eps_dyn = clairaut_ellipticity(r, rho, R, zeta)
%CLAIRAUT_ELLIPTICITY  Solve Clairaut's equation exactly for the given
% density profile rho(r) and return the rotational dynamical ellipticity
% (I3-I1)/I1, via the linear shooting method (the ODE is homogeneous; the
% rotation rate only enters through the surface boundary condition, so the
% true solution is a scalar multiple of any regular trial solution).

% Precompute mean-enclosed-density on the structure grid ONCE (cumulative
% integral), then interpolate -- recomputing a fresh trapz integral at
% every ODE step would be O(n) per evaluation and far too slow.
Menc = 4*pi*cumtrapz(r, rho.*r.^2);
rho_bar_grid = Menc ./ ((4/3)*pi*r.^3);
rho_bar_grid(1) = rho(1);   % r->0 limit: mean density -> central density

rho_of_r = @(rr) interp1(r, rho, rr, 'linear', 'extrap');
mean_rho_of_r = @(rr) interp1(r, rho_bar_grid, rr, 'linear', 'extrap');

a0 = max(r(1), R*1e-6);
IC = [1; 0];   % regular near-center behavior: eps(a0)=1 (arbitrary), eps'(a0)=0

ode_opts = odeset('RelTol',1e-8,'AbsTol',1e-10);
[~,evec] = ode45(@(a,y) clairaut_rhs(a,y,rho_of_r,mean_rho_of_r), [a0 R], IC, ode_opts);

eps_raw   = evec(end,1);
depsda_raw = evec(end,2);
BC_raw = R*depsda_raw + 2*eps_raw;

lambda = (15/2)*zeta / BC_raw;
eps0 = lambda * eps_raw;                      % physical eps(R)

% Convert geometric flattening eps(R) to the dynamical ellipticity via the
% same J2 <-> C relation used in the Radau route (accurate to the same
% order as the rest of this code for the small ellipticities relevant here).
I  = (8*pi/3) * trapz(r, rho .* r.^4);
M  = 4*pi*trapz(r, rho.*r.^2);
C  = I/(M*R^2);
J2_over_eps0 = -3/10 + (5/2)*C - (15/8)*C^2;
J2 = J2_over_eps0*eps0;
eps_dyn = (J2/C)/(1-J2/C);
end

function dyda = clairaut_rhs(a,y,rho_of_r,mean_rho_of_r)
eps = y(1); deps = y(2);
ratio = rho_of_r(a)/mean_rho_of_r(a);
d2eps = (6*eps - 6*ratio*(a*deps+eps)) / a^2;
dyda = [deps; d2eps];
end
