function [Rise, Set, RiseAz, SetAz] = riseSet(JD, RA, Dec, Alt, Args)
    % Next rise and set times for fixed equatorial coordinates.
    % Package: celestial.time
    % Description: Compute the next rise and set Julian Dates (after JD)
    %              for object coordinates RA/Dec at a given observer
    %              position and altitude threshold.
    %              This routine solves for target local sidereal angles and
    %              refines crossings iteratively.
    %              Coordinates are treated as fixed over the interval
    %              (no proper motion/parallax/topocentric correction here).
    % Input  : - Reference Julian Date (scalar).
    %          - Right ascension (scalar/array).
    %          - Declination (scalar/array).
    %          - Altitude threshold (scalar). Default is 0.
    %            Use negative values for apparent horizon corrections
    %            (e.g., refraction + solar/lunar radius).
    %          * ...,key,val,...
    %            'ObsPos' - Observer geodetic position
    %                   [Lon(deg, east-positive), Lat(deg), Height(m)].
    %                   Default is [35 30 415].
    %            'InUnits' - Input units for RA/Dec/Alt: 'deg' or 'rad'.
    %                   Default is 'deg'.
    %            'STType' - Sidereal time type for celestial.time.lst:
    %                   'm' (mean) or 'a' (apparent). Default is 'a'.
    % Output : - Next rise time(s) in JD, same size as expanded RA/Dec.
    %            NaN for coordinates that do not rise/set at this altitude
    %            (e.g., circumpolar or never-rises cases).
    %          - Next set time(s) in JD, same size conventions as Rise.
    %          - (Optional) Azimuth at rise time(s), same size as Rise.
    %            Returned in the same angular units convention as input
    %            coordinates (deg if InUnits='deg', otherwise rad).
    %          - (Optional) Azimuth at set time(s), same units as RiseAz.
    % Author : Eran Ofek + Cursor (Apr 2026)
    % Example: [R,S] = celestial.time.riseSet(2460400.5, 180, 30);
    %          [R,S,RAz,SAz] = celestial.time.riseSet(2460400.5, 180, 30);
    %          [R,S] = celestial.time.riseSet(2460400.5, RA, Dec, -0.5667, ...
    %                     'ObsPos',[35 30 415], 'InUnits','deg', 'STType','a');
    
    arguments
        JD (1,1)
        RA 
        Dec 
        Alt (1,1)      = 0;
        Args.ObsPos    = [35 30 415]   % [Lon deg, Lat deg, Height m]
        Args.InUnits   = 'deg'
        Args.STType    = 'a'
    end
    
    if strcmpi(Args.InUnits,'deg')
        RA  = deg2rad(RA);
        Dec = deg2rad(Dec);
        Alt = deg2rad(Alt);
    end
    
    Lon = deg2rad(Args.ObsPos(1));   % east-positive
    Lat = deg2rad(Args.ObsPos(2));
    
    [RA, Dec] = compatibleArrays(RA, Dec);
    
    CosH0 = (sin(Alt) - sin(Lat).*sin(Dec)) ./ (cos(Lat).*cos(Dec));
    
    Rise = nan(size(RA));
    Set  = nan(size(RA));
    
    % Numerical guard near horizon/circumpolar boundary.
    Tol   = 10.*eps;
    Valid = abs(CosH0) <= (1 + Tol);
    CosH0 = min(1, max(-1, CosH0));
    H0 = nan(size(RA));
    H0(Valid) = acos(CosH0(Valid));
    
    TargetRise = mod(RA - H0, 2*pi);
    TargetSet  = mod(RA + H0, 2*pi);
    
    Omega = 2*pi .* 1.0027379093;  % rad / solar day
    
    Rise(Valid) = nextCrossing(JD, TargetRise(Valid), Lon, Omega, Args.STType);
    Set(Valid)  = nextCrossing(JD, TargetSet(Valid),  Lon, Omega, Args.STType);

    if nargout>2
        RiseAz = nan(size(RA));
        SetAz  = nan(size(RA));
        OutUnits = Args.InUnits;
        if ~strcmpi(OutUnits, 'deg')
            OutUnits = 'rad';
        end

        [RiseAz(Valid), ~] = celestial.coo.radec2azalt(Rise(Valid), ...
                                                        RA(Valid), ...
                                                        Dec(Valid), ...
                                                        'GeoCoo', Args.ObsPos, ...
                                                        'InUnits', 'rad', ...
                                                        'OutUnits', OutUnits, ...
                                                        'LSTType', Args.STType);
        [SetAz(Valid), ~]  = celestial.coo.radec2azalt(Set(Valid), ...
                                                       RA(Valid), ...
                                                       Dec(Valid), ...
                                                       'GeoCoo', Args.ObsPos, ...
                                                       'InUnits', 'rad', ...
                                                       'OutUnits', OutUnits, ...
                                                       'LSTType', Args.STType);
    end
end

%--------------------------------------------------------------------------

function JDcross = nextCrossing(JD0, TargetLST, Lon, Omega, STType)

LST0 = 2*pi .* celestial.time.lst(JD0, Lon, STType);

Delta = mod(TargetLST - LST0, 2*pi);
JDcross = JD0 + Delta ./ Omega;

for I = 1:3
    LST = 2*pi .* celestial.time.lst(JDcross, Lon, STType);
    F = wrapPi(LST - TargetLST);
    JDcross = JDcross - F ./ Omega;
end

SiderealDay = 1 ./ 1.0027379093;
TooEarly = JDcross <= JD0;
JDcross(TooEarly) = JDcross(TooEarly) + SiderealDay;

end

%--------------------------------------------------------------------------

function X = wrapPi(X)
X = mod(X + pi, 2*pi) - pi;
end

%--------------------------------------------------------------------------

function [A, B] = compatibleArrays(A, B)

if isscalar(A) && ~isscalar(B)
    A = A + zeros(size(B));
elseif isscalar(B) && ~isscalar(A)
    B = B + zeros(size(A));
elseif ~isequal(size(A), size(B))
    error('RA and Dec must have the same size, unless one of them is scalar.');
end

end
