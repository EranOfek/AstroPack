function [XYZ, UVW, UVW_LSR, VGC] = galRectangularCoo(l,b,plx,RV,mu_l,mu_b,Args)
    % Galactic position & velocity in rectangular coordinates.
    %   Computes Galactic positions and velocities from observables.
    %   Output is always N×3 matrices:
    %       XYZ      = [X  Y  Z]                (Galactocentric position)     [kpc]
    %       UVW      = [U  V  W]                (Heliocentric velocities)     [km/s]
    %       UVW_LSR  = [U_LSR  V_LSR  W_LSR]    (Velocities vs. LSR)          [km/s]
    %       VGC      = [VXgc  VYgc  VZgc]       (Galactocentric velocities)   [km/s]
    %
    % Input  : - (l) Galactic long. [rad].
    %          - (b) Galactic lat. [rad].
    %          - (plx) Parallax [arcsec].
    %          - (RV) Radial velocity [km/s].
    %          - (mu_l) Proper motion in l-direction [arcsec/yr].
    %            I.e., multiplied by cos(b).
    %            See astro.galaxy.eqPM2galPM
    %          - (mu_b) Proper motion in b-direction [arcsec/yr]
    %          * ...,key,val,...
    %            'Rsun' - Default is 8.2 kpc
    %            'U_sun' - Default is 11.1 km/s toward GC
    %            'V_sun' - Default is 12.24 km/s in direction of rotation
    %            'W_sun' - Default is 7.25 km/s toward NGP
    %            'Vc' - Default is 238.0 km/s circular velocity at Rsun
    % Output : - [X, Y, Z] Galactocentric position     [kpc]
    %          - [U, V, W] Heliocentric velocities     [km/s]
    %          - [U_LSR, V_LSR, W_LSR] Velocities relative to LSR   [km/s]
    %          - [VXgc, VYgc, VZgc] Galactocentric velocities   [km/s]
    %            Helocentric frame: U toward GC, V toward rotation, W toward NGP.
    %            Galactocentric X toward Sun, Y rotation, Z NGP.
    % Author : ChatGPT + Eran Ofek (Dec 2025)
    % Example: [A,B,C,D]=astro.galaxy.galRectangularCoo([0;0],[0; pi./2],0.01,100,0.01,0.0)

    arguments
        l
        b
        plx
        RV
        mu_l
        mu_b
        Args.Rsun (1,1) double = 8.2
        Args.U_sun (1,1) double = 11.1
        Args.V_sun (1,1) double = 12.24
        Args.W_sun (1,1) double = 7.25
        Args.Vc    (1,1) double = 238.0
    end

    % Extract structured defaults
    Rsun = Args.Rsun;
    U_sun = Args.U_sun;
    V_sun = Args.V_sun;
    W_sun = Args.W_sun;
    Vc    = Args.Vc;

    %----------------------------------------------
    % Ensure column vectors
    %----------------------------------------------
    l    = l(:);
    b    = b(:);
    plx  = plx(:);
    RV   = RV(:);
    mu_l = mu_l(:);
    mu_b = mu_b(:);

    %----------------------------------------------
    % Distance
    %----------------------------------------------
    d_pc  = 1 ./ plx;       % pc
    d_kpc = d_pc / 1000.0;  % kpc

    %----------------------------------------------
    % Tangential velocities
    %----------------------------------------------
    k = 4.74047;  % km/s per ("/yr * pc)
    Vl = k .* mu_l .* d_pc;
    Vb = k .* mu_b .* d_pc;

    %----------------------------------------------
    % Angular factors
    %----------------------------------------------
    cb = cos(b); sb = sin(b);
    cl = cos(l); sl = sin(l);

    %----------------------------------------------
    % Heliocentric position
    %----------------------------------------------
    x = d_kpc .* cb .* cl;
    y = d_kpc .* cb .* sl;
    z = d_kpc .* sb;

    %----------------------------------------------
    % Galactocentric position
    %----------------------------------------------
    X = Rsun - x;
    Y = y;
    Z = z;

    XYZ = [X Y Z];

    %----------------------------------------------
    % Heliocentric velocities U,V,W
    %----------------------------------------------
    Vr = RV;

    U = Vr .* cb .* cl ...
      - Vl .* sl ...
      - Vb .* sb .* cl;

    V = Vr .* cb .* sl ...
      + Vl .* cl ...
      - Vb .* sb .* sl;

    W = Vr .* sb ...
      + Vb .* cb;

    UVW = [U V W];

    %----------------------------------------------
    % Velocities relative to the LSR
    %----------------------------------------------
    U_LSR = U + U_sun;
    V_LSR = V + V_sun;
    W_LSR = W + W_sun;

    UVW_LSR = [U_LSR  V_LSR  W_LSR];

    %----------------------------------------------
    % Galactocentric velocities
    %----------------------------------------------
    VXgc = U + U_sun;
    VYgc = V + (Vc + V_sun);
    VZgc = W + W_sun;

    VGC = [VXgc  VYgc  VZgc];
end
