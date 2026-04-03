function [OutXYZ] = properMotionParallax(InXYZ, InEpoch, OutEpoch, PM_RA, PM_Dec, Plx, RV, Args)
    % Barycentric ICRS positions to observer-frame ICRS using proper motion, parallax, and radial velocity.
    %
    % This function assumes uniform rectilinear barycentric motion in 3D.
    % The input direction is interpreted as a barycentric ICRS direction
    % at InEpoch (e.g., like a Gaia catalog direction), together with the
    % astrometric quantities PM_RA, PM_Dec, parallax, and radial velocity.
    %
    % The function propagates the barycentric position to OutEpoch and then
    % returns the ICRS direction as seen from the observer barycentric position
    % at OutEpoch.
    %
    % Input  : - (InXYZ) 3xN barycentric ICRS unit-vector cosine directions
    %            of target stars at InEpoch.
    %          - (InEpoch) Scalar JD of input epoch.
    %          - (OutEpoch) Scalar JD of output epoch.
    %          - (PM_RA) Proper motion in RA*cos(Dec), [mas/yr], length N.
    %          - (PM_Dec) Proper motion in Dec, [mas/yr], length N.
    %          - (Plx) Parallax, [mas], length N.
    %          - (RV) Radial velocity, [km/s], length N.
    %          * ...,key,val,...
    %            'ObserverPos' - 3x1 observer barycentric position at OutEpoch, [AU].
    %                 Default is [0;0;0].
    %            'ApplyPlx' - Logical. If true, return the observer-frame direction
    %                 at OutEpoch. If false, return the barycentric propagated
    %                 direction at OutEpoch. Default is true.
    %
    % Output : - (OutXYZ) 3xN propagated unit-vector cosine directions 
    %            of the target stars at OutEpoch.
    %            If Args.ApplyPlx==true, these are ICRS directions in the
    %            observer frame at OutEpoch.
    %            If Args.ApplyPlx==false, these are barycentric ICRS
    %            directions at OutEpoch.
    %
    % Notes:
    %   - The propagation is exact geometrically within the assumption of
    %     uniform rectilinear barycentric motion.
    %   - No small-angle approximation is used.
    %   - The input astrometric quantities are assumed to be consistent with
    %     the input barycentric ICRS direction.
    %   - Parallax is required because the distance is needed in order to
    %     convert proper motion into tangential velocity.
    %
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % See also: celestial.coo.proper_motion_parallax
    % Example: [OutXYZ] = celestial.convert.properMotionParallax([1 1 1].',2451545,2460000,10,10,0,0)

    arguments
        InXYZ (3,:) double
        InEpoch (1,1) double
        OutEpoch (1,1) double
        PM_RA 
        PM_Dec 
        Plx 
        RV 
        Args.ObserverPos (3,1) double = zeros(3,1)
        Args.ApplyPlx (1,1) logical = true
    end

    warning('THIS FUNCTION WAS NOT TESTED YET');

    %--------------------------%
    % Constants and reshaping  %
    %--------------------------%
    JulianYearDays = 365.25;
    Mas2Rad        = pi ./ (180 .* 3600 .* 1000);
    KmPerAU        = 149597870.7;
    SecPerDay      = 86400;
    AUPerPc        = 648000 ./ pi;
    MinPlx         = 0.0001;  % [mas]

    N = size(InXYZ, 2);

    PM_RA  = reshape(PM_RA,  1, []);
    PM_Dec = reshape(PM_Dec, 1, []);
    Plx    = reshape(Plx,    1, []);
    RV     = reshape(RV,     1, []);

    if numel(PM_RA)~=N || numel(PM_Dec)~=N || numel(Plx)~=N || numel(RV)~=N
        error('PM_RA, PM_Dec, Plx, and RV must each have length N=size(InXYZ,2).');
    end

    FlagPlx = Plx < MinPlx;
    Plx(FlagPlx) = MinPlx;

    DtDays = OutEpoch - InEpoch;

    %--------------------------%
    % Unit conversions         %
    %--------------------------%
    PM_RA_RadDay  = PM_RA  .* Mas2Rad ./ JulianYearDays;
    PM_Dec_RadDay = PM_Dec .* Mas2Rad ./ JulianYearDays;
    RV_AUDay      = RV .* SecPerDay ./ KmPerAU;
    DistAU        = AUPerPc .* 1000 ./ Plx;   % Plx in mas

    %--------------------------%
    % Normalize input vectors  %
    %--------------------------%
    InNorm = vecnorm(InXYZ, 2, 1);
    InU    = InXYZ ./ InNorm;

    %--------------------------%
    % Validity mask            %
    %--------------------------%
    Valid = all(isfinite(InU), 1) & isfinite(PM_RA_RadDay) & isfinite(PM_Dec_RadDay) ...
          & isfinite(RV_AUDay) & isfinite(DistAU) & (Plx > 0);

    OutXYZ = nan(3, N);

    if ~any(Valid)
        return;
    end

    % Keep only valid columns for the heavy calculations
    InU_V         = InU(:, Valid);
    PM_RA_V       = PM_RA_RadDay(Valid);
    PM_Dec_V      = PM_Dec_RadDay(Valid);
    RV_V          = RV_AUDay(Valid);
    DistAU_V      = DistAU(Valid);

    %-----------------------------------------------%
    % Exact barycentric position vector at InEpoch  %
    %-----------------------------------------------%
    R0 = InU_V .* DistAU_V;

    % Exact barycentric unit direction at input epoch
    U0 = R0 ./ vecnorm(R0, 2, 1);

    %-----------------------------------------------%
    % Tangent basis vectors at input epoch          %
    %-----------------------------------------------%
    RA0  = atan2(U0(2,:), U0(1,:));
    Dec0 = atan2(U0(3,:), hypot(U0(1,:), U0(2,:)));

    P0 = [-sin(RA0); ...
           cos(RA0); ...
           zeros(1, numel(RA0))];

    Q0 = [-sin(Dec0).*cos(RA0); ...
          -sin(Dec0).*sin(RA0); ...
           cos(Dec0)];

    %-----------------------------------------------%
    % Exact barycentric space velocity vector V0    %
    %-----------------------------------------------%
    V0 = U0 .* RV_V + P0 .* (DistAU_V .* PM_RA_V) + Q0 .* (DistAU_V .* PM_Dec_V);

    %-----------------------------------------------%
    % Exact propagated barycentric position R1      %
    %-----------------------------------------------%
    R1 = R0 + V0 .* DtDays;

    %-----------------------------------------------%
    % Output direction at OutEpoch                  %
    %-----------------------------------------------%
    if Args.ApplyPlx
        ObsVec = R1 - Args.ObserverPos;
        OutU_V = ObsVec ./ vecnorm(ObsVec, 2, 1);
    else
        OutU_V = R1 ./ vecnorm(R1, 2, 1);
    end

    OutXYZ(:, Valid) = OutU_V;

end