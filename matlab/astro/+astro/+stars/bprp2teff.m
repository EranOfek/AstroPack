function [Teff, Logg, Res] = bprp2teff(BP_RP, Args)
    % Effective temperature and log(g) from Gaia BP-RP (+ G, parallax), uniformly for all stars
    %   Teff from the dereddened color via the empirical dwarf sequence of
    %   Pecaut & Mamajek (2013; EEM table version 2022.04.16), used for all
    %   luminosity classes (for giants the dwarf relation is off by ~100-200 K,
    %   less than the spacing of the Pickles classes).
    %   Dwarf/giant from the absolute magnitude M_G: a star with a reliable
    %   parallax that is brighter than the main sequence at its color by more
    %   than GiantDeltaMag is a giant; all others are dwarfs.
    %   log(g) of a dwarf is from the table mass and radius, of a giant it is
    %   the constant GiantLogg (chosen to fall into the giant branch of
    %   astro.stars.tlogg2picklesClass).
    %   Reddening: E(B-V) along the full line of sight (e.g. from
    %   astro.extinction.sky_ebv, a 2D map) is scaled to the star's distance
    %   with a plane-parallel exponential dust layer,
    %   E(d) = Ebv * (1 - exp(-d*sin|b|/h)). [PLACEHOLDER until a 3D map is used]
    %   Stars without a reliable parallax get the full Ebv.
    %   Hotter than B9V (BP-RP0 < -0.12) the table has no Gaia colors; there it
    %   is extended with synthetic Pickles dwarf colors shifted by -0.085 mag
    %   to match the table at B9V/A0V. The color is nearly insensitive to Teff
    %   in this range (O5V-O9V span 0.03 mag), see Res.HotExtension.
    % Input  : - Observed Gaia BP-RP [mag] (vector).
    %          * ...,key,val,...
    %            'G'      - Gaia G [mag], needed for the dwarf/giant test. Default is [].
    %            'Plx'    - Parallax [mas]. Default is [].
    %            'PlxErr' - Parallax error [mas]. Default is [].
    %            'Ebv'    - Total line-of-sight E(B-V) [mag]. Default is 0.
    %            'GalLat' - Galactic latitude [deg], for the dust-layer scaling;
    %                   [] applies the full Ebv to every star. Default is [].
    %            'DustScaleHeight' - Dust layer scale height [pc]. Default is 125.
    %            'MinGalLat' - |b| floor [deg] in the dust-layer model (the
    %                   model gives no reddening at b=0). Default is 5.
    %            'R_BPRP' - E(BP-RP)/E(B-V). Default is 1.339 (Casagrande &
    %                   VandenBerg 2018: R_BP=3.374, R_RP=2.035).
    %            'R_G'    - A_G/E(B-V). Default is 2.740 (same reference).
    %            'MinPlxSN' - Minimal Plx/PlxErr for the parallax to be used.
    %                   Default is 5.
    %            'GiantDeltaMag' - Giant if M_G is brighter than the main
    %                   sequence by more than this [mag]. Default is 1.5
    %                   (above the 0.75 mag equal-mass binary sequence).
    %            'DwarfLogg' - Dwarf log(g) floor, keeps dwarfs (O stars:
    %                   log(g)~3.95) in the dwarf branch of
    %                   astro.stars.tlogg2picklesClass. Default is 4.0.
    %            'GiantLogg' - log(g) given to giants. Default is 3.5.
    % Output : - Teff [K] (NaN where BP-RP is NaN).
    %          - log(g) [cgs].
    %          - Structure with per-star columns:
    %            .BP_RP0  - dereddened color.
    %            .Ebv     - E(B-V) applied.
    %            .M_G0    - extinction-corrected absolute G (NaN without a reliable parallax).
    %            .PlxOK   - the parallax was used.
    %            .IsGiant - classified as a giant.
    %            .OutOfRange   - BP-RP0 outside the table; Teff clipped to the table end.
    %            .HotExtension - BP-RP0 in the synthetic O/B extension (Teff poorly constrained).
    % Author : D. Kovaleva (Oct 2026)
    % Example: [Teff,Logg] = astro.stars.bprp2teff(0.823)   % the Sun: 5770 K, 4.44
    %          Gal  = celestial.coo.coco([RA Dec],'j2000.0','g','d','d');
    %          Ebv  = astro.extinction.sky_ebv(deg2rad(RA), deg2rad(Dec));
    %          [Teff,Logg,Res] = astro.stars.bprp2teff(BP_RP, 'G',G, 'Plx',Plx, 'PlxErr',PlxErr, 'Ebv',Ebv, 'GalLat',Gal(:,2));
    %          R = astro.stars.tlogg2picklesClass(Teff(1), Logg(1));

    arguments
        BP_RP
        Args.G                    = [];
        Args.Plx                  = [];
        Args.PlxErr               = [];
        Args.Ebv                  = 0;
        Args.GalLat               = [];
        Args.DustScaleHeight      = 125;
        Args.MinGalLat            = 5;
        Args.R_BPRP               = 1.339;
        Args.R_G                  = 2.740;
        Args.MinPlxSN             = 5;
        Args.GiantDeltaMag        = 1.5;
        Args.DwarfLogg            = 4.0;
        Args.GiantLogg            = 3.5;
    end

    % Dwarf sequence: Teff [K], (BP-RP)0, M_G (M_V for O/B), R [Rsun], M [Msun].
    % O5V..B8V: BP-RP = synthetic Pickles dwarfs - 0.085; Teff, M_V, R, M from the EEM table.
    % B9V..M8.5V: EEM table version 2022.04.16 (cooler rows are not monotonic in BP-RP).
    Tab = [41400 -0.718 -5.35 11.45  43.0
           33300 -0.690 -4.20  7.72  20.2
           31400 -0.628 -3.90  7.16  17.7
           26000 -0.498 -3.00  5.71  11.8
           17000 -0.362 -1.20  3.61   5.4
           14500 -0.232 -0.55  3.27   4.3
           12300 -0.159  0.00  2.86   3.38
           10700 -0.120  0.515 2.49   2.75
           10400 -0.087  0.615 2.45   2.68
            9700 -0.037  1.00  2.193  2.18
            9300  0.005  1.16  2.136  2.05
            8800  0.068  1.345 2.117  1.98
            8600  0.110  1.69  1.861  1.86
            8250  0.166  1.92  1.794  1.93
            8100  0.194  1.98  1.785  1.88
            7910  0.222  2.09  1.775  1.83
            7760  0.263  2.19  1.750  1.77
            7590  0.320  2.27  1.747  1.81
            7400  0.327  2.37  1.747  1.75
            7220  0.377  2.51  1.728  1.61
            7020  0.434  2.69  1.679  1.50
            6820  0.490  2.89  1.622  1.46
            6750  0.518  2.99  1.578  1.44
            6670  0.546  3.10  1.533  1.38
            6550  0.587  3.26  1.473  1.33
            6350  0.640  3.56  1.359  1.25
            6280  0.670  3.66  1.324  1.21
            6180  0.694  3.90  1.221  1.18
            6050  0.719  4.105 1.167  1.13
            5990  0.767  4.195 1.142  1.08
            5930  0.784  4.325 1.100  1.06
            5860  0.803  4.462 1.060  1.03
            5770  0.823  4.635 1.012  1.00
            5720  0.832  4.703 1.002  0.99
            5680  0.841  4.757 0.991  0.985
            5660  0.850  4.801 0.977  0.98
            5600  0.869  4.914 0.949  0.97
            5550  0.880  5.006 0.927  0.95
            5480  0.900  5.098 0.914  0.94
            5380  0.950  5.34  0.853  0.90
            5270  0.983  5.553 0.813  0.88
            5170  1.01   5.65  0.797  0.86
            5100  1.10   5.83  0.783  0.82
            4830  1.21   6.20  0.755  0.78
            4600  1.34   6.53  0.713  0.73
            4440  1.43   6.83  0.701  0.70
            4300  1.53   7.02  0.669  0.69
            4100  1.70   7.57  0.630  0.64
            3990  1.73   7.74  0.615  0.62
            3930  1.79   8.03  0.608  0.59
            3850  1.84   8.16  0.588  0.57
            3770  1.97   8.44  0.544  0.54
            3660  2.09   8.82  0.501  0.50
            3620  2.13   8.98  0.482  0.47
            3560  2.23   9.29  0.446  0.44
            3470  2.39   9.67  0.421  0.40
            3430  2.50  10.05  0.361  0.37
            3270  2.78  10.87  0.300  0.27
            3210  2.94  11.21  0.274  0.23
            3110  3.16  12.04  0.217  0.184
            3060  3.35  12.45  0.196  0.162
            2930  3.71  13.35  0.156  0.123
            2810  4.16  14.26  0.137  0.102
            2740  4.50  14.40  0.126  0.093
            2680  4.65  14.72  0.120  0.090
            2630  4.72  15.20  0.116  0.088
            2570  4.86  15.20  0.114  0.085
            2420  5.10  15.90  0.104  0.080];
    TabBPRP = Tab(:,2);
    TabLogT = log10(Tab(:,1));
    TabMG   = Tab(:,3);
    TabLogg = 4.438 + log10(Tab(:,5)) - 2.*log10(Tab(:,4));    % log(g_sun)=4.438
    HotEdge = -0.120;                                          % B9V: bluest EEM Gaia color

    BP_RP = BP_RP(:);
    N     = numel(BP_RP);
    Ebv   = Args.Ebv(:) .* ones(N,1);

    % parallax usable?
    if isempty(Args.Plx) || isempty(Args.PlxErr)
        PlxOK = false(N,1);
        Plx   = NaN(N,1);
    else
        Plx   = Args.Plx(:) .* ones(N,1);
        PlxOK = Plx > 0 & Plx./(Args.PlxErr(:).*ones(N,1)) >= Args.MinPlxSN;
    end

    % reddening scaled to distance (dust layer); full Ebv without a usable parallax
    if ~isempty(Args.GalLat)
        SinB   = max(abs(sind(Args.GalLat(:).*ones(N,1))), sind(Args.MinGalLat));
        Dist   = 1000 ./ Plx;                                  % [pc]
        Frac   = 1 - exp(-Dist .* SinB ./ Args.DustScaleHeight);
        Ebv(PlxOK) = Ebv(PlxOK) .* Frac(PlxOK);
    end

    BP_RP0 = BP_RP - Args.R_BPRP .* Ebv;
    X      = min(max(BP_RP0, TabBPRP(1)), TabBPRP(end));       % clip to the table
    Teff   = 10.^interp1(TabBPRP, TabLogT, X, 'linear');
    LoggMS = max(interp1(TabBPRP, TabLogg, X, 'linear'), Args.DwarfLogg);
    MG_MS  = interp1(TabBPRP, TabMG, X, 'linear');

    % dwarf/giant from the absolute magnitude
    if isempty(Args.G)
        PlxOK(:) = false;
        M_G0     = NaN(N,1);
    else
        M_G0  = Args.G(:) - Args.R_G .* Ebv + 5.*log10(Plx./100);
        PlxOK = PlxOK & ~isnan(M_G0);
        M_G0(~PlxOK) = NaN;
    end
    IsGiant = PlxOK & M_G0 < MG_MS - Args.GiantDeltaMag;

    Logg          = LoggMS;
    Logg(IsGiant) = Args.GiantLogg;
    Teff(isnan(BP_RP0)) = NaN;
    Logg(isnan(BP_RP0)) = NaN;

    if nargout > 2
        Res.BP_RP0       = BP_RP0;
        Res.Ebv          = Ebv;
        Res.M_G0         = M_G0;
        Res.PlxOK        = PlxOK;
        Res.IsGiant      = IsGiant;
        Res.OutOfRange   = BP_RP0 < TabBPRP(1) | BP_RP0 > TabBPRP(end);
        Res.HotExtension = BP_RP0 < HotEdge;
    end
end
