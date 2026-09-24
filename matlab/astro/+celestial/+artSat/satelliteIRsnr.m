function [SNR, Res] = satelliteIRsnr(Args)
% satelliteIRsnr - S/N for thermal-IR observation of an Earth-orbiting satellite.
% Description: Compute the signal-to-noise ratio for a ground- or space-based
%              telescope observing an artificial satellite in the thermal
%              infrared (nominally 3-25 micron), where the satellite is
%              detected via its own thermal emission (plus, optionally,
%              reflected sunlight).
%
%              The source is modelled as a gray-body of projected area
%              SatArea and emissivity SatEmis at temperature SatTemp,
%              observed against a background composed of sky emission and
%              self-emission of the (warm) optics. The PSF is an elliptical
%              Gaussian whose width is USER DEFINED (SigmaPSF) -- i.e. the
%              system is NOT assumed to be diffraction limited. The angular
%              size of the satellite and the trailing (smear) due to its
%              apparent motion are added in quadrature to the PSF width.
%
%              The S/N is obtained by summing (S/N)^2 over the entire PSF
%              with optimal (inverse-variance, PSF-weighted) extraction:
%
%                 (S/N)^2 = SUM_i [ (Ns*f_i*t)^2 / (Ns*f_i*t + V_bg) ]
%
%              where f_i is the fraction of the source flux falling in pixel
%              i, Ns is the total source count rate, and V_bg is the
%              per-pixel variance from background, dark current and read
%              noise (multiplied by the background-subtraction penalty
%              Kappa). Note that this sum gives S/N proportional to
%              sqrt(ExpTime) exactly.
%
%              In the continuum limit the sum has the closed form
%                 S/N = sqrt( Ns*t*G(rho) ),  G(rho) = 1 - ln(1+rho)/rho
%              with rho = (peak-pixel source rate)/(per-pixel noise rate).
%              This is available via Method='analytic'. G->1 in the
%              source-shot-noise limit and G->rho/2 in the background limit,
%              where it reduces to the classical matched-filter result with
%              effective solid angle Omega_eff = 4*pi*SigmaPSF^2.
%
%              All inputs have defaults, so satelliteIRsnr() returns the S/N
%              for the nominal configuration described below. All numeric
%              inputs may be arrays (implicitly expanded), which allows e.g.
%              scanning over distance, diameter or temperature in one call.
%
% Input  : * Pairs of ...,key,val,... The following keys are available:
%
%   -- Bandpass --
%            'Lambda'      - Central wavelength [micron]. Default is 10.
%            'DeltaLambda' - Bandpass width [micron]. Default is 1.
%
%   -- Telescope / system --
%            'D'           - Telescope (effective) aperture diameter [cm].
%                            Default is 100.
%            'Obscuration' - Linear central obscuration (D_inner/D), used to
%                            reduce the collecting area. Default is 0.
%            'Eta'         - Total system efficiency: atmospheric
%                            transmission x optics throughput x detector QE.
%                            Dimensionless, 0-1. Default is 0.1.
%            'SigmaPSF'    - Gaussian PSF sigma [arcsec] (NOT FWHM).
%                            FWHM = 2.3548*SigmaPSF. This is the user-defined
%                            angular resolution; set it from seeing, AO
%                            residual, diffraction, optical quality or
%                            pointing jitter as appropriate. Default is 0.5.
%            'PixPerFWHM'  - Detector sampling, in pixels per PSF FWHM.
%                            2 = Nyquist. Default is 2.
%
%   -- Target --
%            'Dist'        - Telescope-satellite distance (slant range) [km].
%                            Default is 1000.
%            'SatArea'     - Projected (cross-sectional) emitting area of the
%                            satellite [m^2]. Default is 1.
%            'SatEmis'     - Satellite emissivity at Lambda (0-1).
%                            Default is 0.9.
%            'SatTemp'     - Satellite surface temperature [K].
%                            Default is 300.
%            'AngRate'     - Apparent angular rate of the satellite across
%                            the sky [arcsec/s]. Used only to compute the
%                            in-frame trailing. Set 0 for perfect rate
%                            tracking. Default is 0.
%                            (LEO at 1000 km is ~1500 arcsec/s untracked.)
%
%   -- Backgrounds --
%            'SkyTemp'     - Effective sky temperature [K]. Default is 250.
%            'SkyEmis'     - Sky emissivity at Lambda (~1-transmission).
%                            Default is 0.1.
%            'OpticsTemp'  - Temperature of the warm optics [K].
%                            Default is 280.
%            'OpticsEmis'  - Effective emissivity of the optics train seen by
%                            the detector. Default is 0.15.
%            'ExtraBkgRate'- Any additional background [e-/s/pixel] (e.g.
%                            zodiacal, thermal leaks). Default is 0.
%            'Kappa'       - Background-subtraction noise penalty,
%                            Kappa = 1 + 1/Noff. Kappa=2 for equal on/off
%                            chop-nod; Kappa=1 for a noiseless background
%                            model. Default is 2.
%
%   -- Reflected sunlight (optional; matters at <6 micron / cold targets) --
%            'Albedo'      - Satellite geometric albedo at Lambda. Set to 0
%                            to disable. Default is 0.
%            'PhaseFunc'   - Phase function value Phi(alpha), 0-1 (1 = full
%                            phase / opposition). Default is 1.
%            'SolarDist'   - Heliocentric distance [AU]. Default is 1.
%
%   -- Detector / integration --
%            'ExpTime'     - Total on-source integration time [s].
%                            Default is 1.
%            'FrameTime'   - Single-frame (non-destructive read) time [s].
%                            If empty, it is set automatically to the
%                            well-depth limit WellDepth/BackgroundRate,
%                            capped by ExpTime. Default is [].
%            'LimitFrameBySmear' - If true, additionally cap FrameTime so
%                            that the trailing is < SigmaPSF, i.e.
%                            FrameTime <= sqrt(12)*SigmaPSF/AngRate.
%                            Default is false.
%            'WellDepth'   - Full-well / saturation level [e-/pixel].
%                            Default is 1e7.
%            'DarkCurrent' - Dark current [e-/s/pixel]. Default is 200.
%            'ReadNoise'   - Read noise per frame [e- rms/pixel].
%                            Default is 500.
%
%   -- Numerics --
%            'Method'      - 'discrete' : exact sum over pixels (default).
%                            'analytic' : continuum closed form with
%                            G(rho); ~5% optimistic at Nyquist sampling.
%            'NsigmaAper'  - Half-size of the summation box, in units of the
%                            effective PSF sigma. Default is 6.
%            'SubPixOffset'- [x,y] position of the source centroid within a
%                            pixel, in pixel units. [0 0] = pixel centre,
%                            [0.5 0.5] = pixel corner. Default is [0 0].
%            'Verbose'     - Print a summary table. Default is false.
%
% Output : - SNR : signal-to-noise ratio of the (PSF-weighted) source
%                  detection, over the full ExpTime. Same size as the
%                  broadcast size of the inputs.
%          - Res : structure with intermediate quantities, including:
%              .SrcRate       - Total source count rate [e-/s] (thermal+refl)
%              .SrcRateThermal- Thermal component [e-/s]
%              .SrcRateRefl   - Reflected-sunlight component [e-/s]
%              .BkgRatePix    - Sky+optics+extra background [e-/s/pixel]
%              .DarkRatePix   - Dark current [e-/s/pixel]
%              .RNRatePix     - Read-noise variance rate [e-^2/s/pixel]
%              .NoiseRatePix  - Kappa*(Bkg+Dark+RN) [e-^2/s/pixel]
%              .PixScale      - Pixel size [arcsec]
%              .OmegaPix      - Pixel solid angle [sr]
%              .SigmaSat      - Satellite angular size contribution [arcsec]
%              .SigmaSmear    - In-frame trailing contribution [arcsec]
%              .SigmaEffPar   - Effective PSF sigma, along-track [arcsec]
%              .SigmaEffPerp  - Effective PSF sigma, cross-track [arcsec]
%              .OmegaEff      - 4*pi*SigPar*SigPerp [sr] (matched-filter area)
%              .Neff          - Effective number of noise pixels, 1/sum(f^2)
%              .Rho           - Peak-pixel source rate / noise rate
%              .G             - G(rho) = 1-ln(1+rho)/rho
%              .FrameTime     - Adopted frame time [s]
%              .Nframes       - Number of frames
%              .Saturated     - true if peak pixel exceeds WellDepth
%              .FluxJy        - In-band source flux density [Jy]
%              .SNR_bgLimited - S/N in the pure background-limited approx.
%              .SNR_shotLimit - sqrt(SrcRate*ExpTime), the absolute ceiling
%              .TimeForSNR10  - ExpTime needed to reach S/N=10 [s]
%              .Regime        - dominant noise term (string)
% Tested : MATLAB R2021a and later (requires the arguments block).
% Author : Claude + Eran Ofek (Aug 2026)
% Example:
%          % Nominal 1-m telescope, 300 K satellite at 1000 km, N band:
%          [SNR,Res] = satelliteIRsnr('Verbose',true);
%
%          % Scan over distance:
%          d = logspace(2,4.6,100);
%          snr = satelliteIRsnr('Dist',d,'ExpTime',1);
%          loglog(d,snr); xlabel('Distance [km]'); ylabel('S/N');
%
%          % Compare M and N band for a cold (200 K) target:
%          satelliteIRsnr('Lambda',[5 10],'SatTemp',200,'DeltaLambda',[0.5 4])
%
%          % Untracked LEO pass, smear-limited frames:
%          satelliteIRsnr('AngRate',1500,'LimitFrameBySmear',true,'Verbose',true)
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    % Bandpass
    Args.Lambda                = 10;      % [micron]
    Args.DeltaLambda           = 1;       % [micron]
    % Telescope / system
    Args.D                     = 100;     % [cm]
    Args.Obscuration           = 0;       % [D_in/D]
    Args.Eta                   = 0.1;     % total efficiency
    Args.SigmaPSF              = 0.5;     % [arcsec] Gaussian sigma
    Args.PixPerFWHM            = 2;       % 2 = Nyquist
    % Target
    Args.Dist                  = 1000;    % [km]
    Args.SatArea               = 1;       % [m^2]
    Args.SatEmis               = 0.9;
    Args.SatTemp               = 300;     % [K]
    Args.AngRate               = 0;       % [arcsec/s]
    % Backgrounds
    Args.SkyTemp               = 250;     % [K]
    Args.SkyEmis               = 0.1;
    Args.OpticsTemp            = 280;     % [K]
    Args.OpticsEmis            = 0.15;
    Args.ExtraBkgRate          = 0;       % [e-/s/pix]
    Args.Kappa                 = 2;       % 1 + 1/Noff
    % Reflected sunlight
    Args.Albedo                = 0;
    Args.PhaseFunc             = 1;
    Args.SolarDist             = 1;       % [AU]
    % Detector / integration
    Args.ExpTime               = 1;       % [s]
    Args.FrameTime             = [];      % [s], [] = auto (well-depth)
    Args.LimitFrameBySmear     = false;
    Args.WellDepth             = 1e7;     % [e-/pix]
    Args.DarkCurrent           = 200;     % [e-/s/pix]
    Args.ReadNoise             = 500;     % [e- rms]
    % Numerics
    Args.Method                = 'discrete';
    Args.NsigmaAper            = 6;
    Args.SubPixOffset          = [0 0];
    Args.Verbose (1,1) logical = false;
end

%--------------------------- physical constants ---------------------------
C_light  = 299792458;          % [m/s]
H_planck = 6.62607015e-34;     % [J s]
C2       = 1.438776877e4;      % hc/k [micron K]
ARCSEC   = 4.848136811e-6;     % [rad/arcsec]
SR_ARCS2 = ARCSEC.^2;          % [sr/arcsec^2]
OMEGA_SUN= 6.7940e-5;          % solar solid angle at 1 AU [sr]
T_SUN    = 5772;               % [K]
FWHM_SIG = 2*sqrt(2*log(2));   % 2.35482

% photon radiance [photons/s/m^2/sr/micron]; expm1 for numerical stability
photRad = @(lam,T) 2.*C_light.*1e18./lam.^4 ./ expm1(C2./(lam.*T));

%------------------------------- geometry ---------------------------------
Atel     = pi./4 .* (Args.D./100).^2 .* (1 - Args.Obscuration.^2);   % [m^2]
Dist_m   = Args.Dist .* 1e3;                                          % [m]

% pixel scale is set by the *design* PSF (SigmaPSF), not by the smeared one
PixScale = FWHM_SIG .* Args.SigmaPSF ./ Args.PixPerFWHM;              % [arcsec]
OmegaPix = PixScale.^2 .* SR_ARCS2;                                   % [sr]

%--------------------------------- rates ----------------------------------
% Source: thermal gray-body emission
SrcThermal = Args.Eta .* Args.SatEmis .* photRad(Args.Lambda, Args.SatTemp) ...
             .* Args.DeltaLambda .* Args.SatArea .* Atel ./ Dist_m.^2;

% Source: reflected sunlight (Lambertian, I = a*Phi*F*A/pi per sr)
SolarPhot  = photRad(Args.Lambda, T_SUN) .* OMEGA_SUN ./ Args.SolarDist.^2;  % [ph/s/m^2/micron]
SrcRefl    = Args.Eta .* Args.Albedo .* Args.PhaseFunc .* SolarPhot ...
             .* Args.DeltaLambda .* Args.SatArea .* Atel ./ (pi .* Dist_m.^2);

SrcRate    = SrcThermal + SrcRefl;                                    % [e-/s]

% Background per pixel
EpsB       = Args.SkyEmis    .* photRad(Args.Lambda, Args.SkyTemp) + ...
             Args.OpticsEmis .* photRad(Args.Lambda, Args.OpticsTemp);
BkgRatePix = Args.Eta .* Args.DeltaLambda .* Atel .* OmegaPix .* EpsB + Args.ExtraBkgRate;

%------------------------------ frame time --------------------------------
if isempty(Args.FrameTime)
    Tframe = Args.WellDepth ./ max(BkgRatePix + Args.DarkCurrent, eps);
else
    Tframe = Args.FrameTime;
end
if Args.LimitFrameBySmear
    Tsmear = sqrt(12).*Args.SigmaPSF ./ max(Args.AngRate, eps);
    Tframe = min(Tframe, Tsmear);
end
Tframe  = min(Tframe, Args.ExpTime);          % cannot exceed total exposure
Nframes = Args.ExpTime ./ Tframe;

%--------------------------- effective PSF width --------------------------
% Uniform disk of radius Rs has sigma = Rs/2 ; trailing over Tframe has
% sigma = rate*Tframe/sqrt(12) (along-track only).
Rsat      = sqrt(Args.SatArea./pi);                                   % [m]
SigmaSat  = (Rsat ./ Dist_m) ./ 2 ./ ARCSEC;                          % [arcsec]
SigmaSmr  = Args.AngRate .* Tframe ./ sqrt(12);                       % [arcsec]

SigPar    = sqrt(Args.SigmaPSF.^2 + SigmaSat.^2 + SigmaSmr.^2);       % along-track
SigPerp   = sqrt(Args.SigmaPSF.^2 + SigmaSat.^2);                     % cross-track

%------------------------- per-pixel noise variance -----------------------
RNRatePix    = Args.ReadNoise.^2 ./ Tframe;            % [e-^2/s/pix]
NoiseRatePix = Args.Kappa .* (BkgRatePix + Args.DarkCurrent + RNRatePix);
VarPix       = NoiseRatePix .* Args.ExpTime;           % [e-^2/pix] over ExpTime

%-------------------- broadcast everything to a common size ---------------
One    = zeros(size(SrcRate .* VarPix .* OmegaPix .* SigPar .* SigPerp .* Args.ExpTime));
SrcRate= SrcRate + One;   VarPix   = VarPix   + One;   OmegaPix = OmegaPix + One;
SigPar = SigPar  + One;   SigPerp  = SigPerp  + One;   PixScale = PixScale + One;
Texp   = Args.ExpTime + One;

% peak-pixel source counts and rho (rho is independent of ExpTime)
PeakFrac = OmegaPix ./ (2.*pi.*SigPar.*SigPerp .* SR_ARCS2);
Rho      = PeakFrac .* SrcRate .* Texp ./ VarPix;

%---------------------------- integrate over PSF --------------------------
SNR  = zeros(size(One));
Neff = zeros(size(One));

switch lower(Args.Method)
    case 'analytic'
        % G(rho) = 1 - ln(1+rho)/rho , with series expansion for small rho
        G = 1 - log1p(Rho)./Rho;
        Small = Rho < 1e-6;
        G(Small) = Rho(Small)./2 - Rho(Small).^2./3;
        SNR  = sqrt(SrcRate .* Texp .* G);
        Neff = 4.*pi.*SigPar.*SigPerp.*SR_ARCS2 ./ OmegaPix;

    case 'discrete'
        G = 1 - log1p(Rho)./Rho;                 % reported for reference only
        Small = Rho < 1e-6;
        G(Small) = Rho(Small)./2 - Rho(Small).^2./3;
        for I = 1:numel(SNR)
            Px  = PixScale(I);
            Sp  = SigPar(I);
            Sq  = SigPerp(I);
            Nx  = ceil(Args.NsigmaAper.*Sp./Px);
            Ny  = ceil(Args.NsigmaAper.*Sq./Px);
            Ix  = (-Nx:Nx).';
            Iy  = (-Ny:Ny);
            X0  = Args.SubPixOffset(1).*Px;
            Y0  = Args.SubPixOffset(2).*Px;
            % exact integral of the Gaussian over each pixel
            Fx  = 0.5.*( erf(((Ix+0.5).*Px - X0)./(sqrt(2).*Sp)) - ...
                         erf(((Ix-0.5).*Px - X0)./(sqrt(2).*Sp)) );
            Fy  = 0.5.*( erf(((Iy+0.5).*Px - Y0)./(sqrt(2).*Sq)) - ...
                         erf(((Iy-0.5).*Px - Y0)./(sqrt(2).*Sq)) );
            Frac = Fx*Fy;                        % outer product, sums to ~1
            Sig  = SrcRate(I).*Texp(I).*Frac;    % source counts per pixel
            SNR(I)  = sqrt(sum(Sig.^2 ./ (Sig + VarPix(I)), 'all'));
            Neff(I) = 1./sum(Frac.^2, 'all');
        end

    otherwise
        error('Unknown Method - use ''discrete'' or ''analytic''.');
end

%------------------------------- diagnostics ------------------------------
% flux density [Jy] : F_nu = eps*A*B_nu(T)/d^2
Bnu    = 2.*H_planck.*C_light./ (Args.Lambda.*1e-6).^3 ./ expm1(C2./(Args.Lambda.*Args.SatTemp));
FluxJy = Args.SatEmis .* Args.SatArea .* Bnu ./ Dist_m.^2 .* 1e26;

PeakCounts = PeakFrac .* SrcRate .* Tframe + (BkgRatePix + Args.DarkCurrent).*Tframe;

% dominant noise term
NB = Args.Kappa.*BkgRatePix + One;
ND = Args.Kappa.*Args.DarkCurrent + One;
NR = Args.Kappa.*RNRatePix + One;
NS = SrcRate.*PeakFrac + One;
[~, IdxMax] = max(cat(3, NB, ND, NR, NS), [], 3);
RegimeStr = ["background-limited","dark-current-limited","read-noise-limited","source-shot-limited"];
Regime = RegimeStr(IdxMax);

% NOTE: fields are assigned individually (not via struct(...)) because
% struct() with array-valued fields would return a struct ARRAY.
Res.SrcRate        = SrcRate;
Res.SrcRateThermal = SrcThermal + One;
Res.SrcRateRefl    = SrcRefl + One;
Res.BkgRatePix     = BkgRatePix + One;
Res.DarkRatePix    = Args.DarkCurrent + One;
Res.RNRatePix      = RNRatePix + One;
Res.NoiseRatePix   = NoiseRatePix + One;
Res.PixScale       = PixScale;
Res.OmegaPix       = OmegaPix;
Res.SigmaSat       = SigmaSat + One;
Res.SigmaSmear     = SigmaSmr + One;
Res.SigmaEffPar    = SigPar;
Res.SigmaEffPerp   = SigPerp;
Res.OmegaEff       = 4.*pi.*SigPar.*SigPerp.*SR_ARCS2;
Res.Neff           = Neff;
Res.Rho            = Rho;
Res.G              = G;
Res.FrameTime      = Tframe + One;
Res.Nframes        = Nframes + One;
Res.PeakPixCounts  = PeakCounts;
Res.Saturated      = PeakCounts > Args.WellDepth;
Res.FluxJy         = FluxJy + One;
Res.SNR_bgLimited  = SrcRate.*Texp ./ sqrt(Neff.*VarPix);
Res.SNR_shotLimit  = sqrt(SrcRate.*Texp);
Res.TimeForSNR10   = Texp .* (10./SNR).^2;
Res.Regime         = Regime;

%--------------------------------- verbose --------------------------------
if Args.Verbose
    fprintf('\n--- satelliteIRsnr ---------------------------------------\n');
    fprintf('  lambda / dlambda   : %g / %g micron\n', Args.Lambda(1), Args.DeltaLambda(1));
    fprintf('  D / Dist           : %g cm / %g km\n', Args.D(1), Args.Dist(1));
    fprintf('  sigma_PSF (FWHM)   : %.3f (%.3f) arcsec\n', Args.SigmaPSF(1), FWHM_SIG*Args.SigmaPSF(1));
    fprintf('  pixel scale        : %.4f arcsec   (Omega_pix = %.3e sr)\n', Res.PixScale(1), Res.OmegaPix(1));
    fprintf('  sigma_sat / smear  : %.4f / %.4f arcsec\n', Res.SigmaSat(1), Res.SigmaSmear(1));
    fprintf('  sigma_eff par/perp : %.4f / %.4f arcsec\n', Res.SigmaEffPar(1), Res.SigmaEffPerp(1));
    fprintf('  source rate        : %.4e e-/s   (%.4g Jy)\n', Res.SrcRate(1), Res.FluxJy(1));
    fprintf('  background rate    : %.4e e-/s/pix\n', Res.BkgRatePix(1));
    fprintf('  read-noise var rate: %.4e e-^2/s/pix\n', Res.RNRatePix(1));
    fprintf('  frame time         : %.4e s  (%.4g frames)\n', Res.FrameTime(1), Res.Nframes(1));
    fprintf('  peak pixel counts  : %.3e e-/frame  (saturated: %d)\n', Res.PeakPixCounts(1), Res.Saturated(1));
    fprintf('  rho / G(rho)       : %.4e / %.4e\n', Res.Rho(1), Res.G(1));
    fprintf('  N_eff (noise pix)  : %.3f\n', Res.Neff(1));
    fprintf('  regime             : %s\n', Res.Regime(1));
    fprintf('  ExpTime            : %g s\n', Args.ExpTime(1));
    fprintf('  S/N                : %.4g   (bg-limited approx %.4g, shot ceiling %.4g)\n', ...
            SNR(1), Res.SNR_bgLimited(1), Res.SNR_shotLimit(1));
    fprintf('  time for S/N=10    : %.4e s\n', Res.TimeForSNR10(1));
    fprintf('----------------------------------------------------------\n\n');
end

end