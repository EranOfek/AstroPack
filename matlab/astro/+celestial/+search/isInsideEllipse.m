function [Result, DistNorm] = isInsideEllipse(CatRA, CatDec, CatPA, CatA, CatE, RA, Dec, Args)
    % Check if list of coordinates [RA, Dec] are within any of a set of sky ellipses
    % defined by [CatRA, CatDec, CatPA, CatA, CatE].
    % Input  : - Vector of RA of ellipse centers. Default units are radians.
    %          - Vector of Dec of ellipse centers. Default units are radians.
    %          - Vector of PA (position angle) of the ellipses. Default units are radians.
    %            PA is measured on the sky from North toward East (i.e., +Y to +X).
    %          - Vector of ellipse semi-major axes. Default units are arcseconds.
    %          - Vector of ellipse eccentricities (0 <= E < 1). For each ellipse,
    %            the semi-minor axis is B = A*sqrt(1 - E.^2).
    %          - Vector of RA coordinates to test. Default units are radians.
    %          - Vector of Dec coordinates to test. Default units are radians.
    %          * ..., key,val, ... pairs of optional arguments:
    %            'Units'   : Units of CatRA, CatDec, CatPA, RA, Dec. Default is 'rad'.
    %            'UnitsA'  : Units of CatA (semi-major axis). Default is 'arcsec'.
    % Output : - A matrix of logicals [Nellipse x Npoints].
    %            True if the corresponding [RA, Dec] point lies inside each ellipse.
    %          - Matrix (Ncat×Npoints) of the normalized distance of each
    %            point from each ellipse center in the ellipse-aligned frame:
    %            DistNorm = r / R_ellipse(phi). Points on the ellipse have
    %            
    % %DistNorm = 1; inside < 1; outside > 1.
    % Notes  : - Uses spherical geometry: great-circle distance and position angle from
    %            each ellipse center to each point (PA measured North→East).
    %          - The inside test is performed in the ellipse frame using the standard
    %            quadratic form (x'/A)^2 + (y'/B)^2 <= 1, where A is the semi-major axis
    %            and B = A*sqrt(1 - E.^2) is the semi-minor axis.
    % Author : Eran Ofek (2025 Oct)
    % Example: Result = celestial.search.isInsideEllipse(CatRA,CatDec,CatPA,CatA,CatE,RA,Dec);

    arguments
        CatRA
        CatDec
        CatPA
        CatA
        CatE
        RA
        Dec
        Args.Units  = 'rad';
        Args.UnitsA = 'arcsec';
    end

    %--- Units
    ConvAng = convert.angular(Args.Units,  'rad');   % angles → rad
    ConvA   = convert.angular(Args.UnitsA, 'rad');   % semi-major a → rad

    %--- Shapes & conversions
    CatRA  = CatRA(:)  .* ConvAng;          % Nc×1
    CatDec = CatDec(:) .* ConvAng;          % Nc×1
    CatPA  = CatPA(:)  .* ConvAng;          % Nc×1  (PA: North→East)
    CatA   = CatA(:)   .* ConvA;            % Nc×1
    CatE   = CatE(:);                       % Nc×1

    RA  = RA(:).'  .* ConvAng;              % 1×Np
    Dec = Dec(:).' .* ConvAng;              % 1×Np

    Nc = numel(CatRA);
    Np = numel(RA);

    % %--- Tangent-plane small-angle offsets (East/North) about each center
    % CosDec0 = cos(CatDec);                  % Nc×1
    % DRA     = RA  - CatRA;                  % Nc×Np (implicit expansion)
    % DDec    = Dec - CatDec;                 % Nc×Np
    % XEast   = DRA  .* CosDec0;              % Nc×Np (radians, +East)
    % YNorth  = DDec;                         % Nc×Np (radians, +North)
    % 
    % 
    % %--- Rotate into ellipse frame (major axis angle = PA from North→East)
    % S = sin(CatPA);                         % Nc×1
    % C = cos(CatPA);                         % Nc×1
    % XPrime =  XEast.*S + YNorth.*C;         % along major axis
    % YPrime =  XEast.*C - YNorth.*S;         % along minor axis


    % --- Spherical: distance & PA from CENTER -> POINT (Nc×Np)
    % NOTE: use (CatRA,CatDec) as the *first* pair so PA is from center to point.
    [Dist, ~, PAcp] = celestial.coo.sphere_dist_fast(CatRA, CatDec, RA, Dec);
    
    % --- Angle relative to the ellipse major axis (PA measured North→East)
    PArel  = PAcp - CatPA;            % Nc×Np via implicit expansion
    
    % --- Components in the ellipse-aligned frame on the tangent plane
    % XPrime: along major axis; YPrime: along minor axis
    XPrime = Dist .* cos(PArel);      % Nc×Np
    YPrime = Dist .* sin(PArel);      % Nc×Np

    %--- Axes (a,b) and normalized distance
    A = CatA;                               % Nc×1 (radians)
    B = A .* sqrt(1 - CatE.^2);             % Nc×1
    ZeroB = (B == 0);                       % avoid divide-by-zero for e→1
    if any(ZeroB)
        B(ZeroB) = eps(realmin);
    end

    % DistNorm: distance in units of ellipse radius at that direction
    %   DistNorm = sqrt((x'/a)^2 + (y'/b)^2) = r / R_ellipse(phi)
    DistNorm = sqrt( (XPrime./A).^2 + (YPrime./B).^2 );  % Nc×Np

    % Inside test
    Result = DistNorm <= 1;                     % Nc×Np
    
end
