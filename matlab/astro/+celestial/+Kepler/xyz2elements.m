function [Result,OrbEl] = xyz2elements(R, V, Epoch, Args)
    % Convert rectangular heliocentric ecliptic coo. and vel. to orbital elements.
    % Input  : - (R) A matrix of positions.
    %            If Dim=1 (default), then this is a 3 x N matrix in wgich
    %            the lines corresponds to the X, Y, Z coordinates and the
    %            columns to different objects or isnstances.
    %            The coordinates are in the heliocentric reference frame,
    %            either in J2000.0 equatorial or ecliptic frame (see
    %            'CooSys'). Units should be consistent.
    %            Default units are [au, day]
    %          - The same as (R), but for the velocity.
    %            Default units are [au, day]
    %          - Epoch in which the coordinates are given.
    %            Default units are days (e.g., Julian days).
    %          * ...,key,val,... 
    %            'CooSys' - Coordinate system of the input R and V.
    %                   'ec' - J2000.0 ecliptic system.
    %                   'eq' - J2000.0 equatorial system (convert to
    %                       ecliptic).
    %                   Default is 'ec'.
    %            'Mu' - G*M for the Sun or central object.
    %                   Default is 0.00029591220828538141
    %                   Units are [solar mass, au, day]
    %            'K' - Gaussian gravitational constant with consistent units
    %                   Default is 0.017202098950000
    %            'Dim' - Dimension along the XYZ coordinates are provided
    %                   in the first two input arguments.
    %                   Default is 1.
    %            'AngUnits' - Angular units of outputs: 'deg' | 'rad'.
    %                   Default is 'deg'.
    % Output : - A structure containing the derived orbital elements.
    %          - A celestial.OrbitalEl object with the orbital elements.
    % Author : Eran Ofek (2023 Nov) 
    % Example: 
    %          OrbEl=celestial.OrbitalEl.loadSolarSystem('num',[9804:9805])
    %          [X,V]=OrbEl.elements2pos('CooSys','ec', 'RefFrame','helio');
    %          [Res,EE]=celestial.Kepler.xyz2elements(X,V, OrbEl.Epoch);
    %          [Res.A.' - OrbEl.A, Res.Eccen.' - OrbEl.Eccen, Res.PeriDist.' - OrbEl.PeriDist]
    %          [Res.Incl.' - OrbEl.Incl, Res.W.' - OrbEl.W, Res.Node.' - OrbEl.Node]
    %          [Res.MeanMotion.' - OrbEl.meanMotion]
    %

    arguments
        R
        V
        Epoch
        Args.CooSys            = 'ec';
        Args.Mu                = 0.00029591220828538141;  % G*M of the Sun
        Args.K                 = 0.017202098950000;
        Args.Dim               = 1;  % dimension along the [X, Y, Z] are given
        Args.AngUnits          = 'deg';
        
        Args.Number            = [];
        Args.Designation       = [];
        Args.MagType           = '';
        Args.MagPar            = [];
    end
    RAD = 180./pi;
    
    if Args.Dim==2
        R = R.';
        V = V.';
    end

    switch lower(Args.CooSys)
        case 'ec'
            % do nothing
        case 'eq'
            % convert to ecliptic
            RotM = celestial.coo.rotm_coo('e');
            R    = RotM * R;
            V    = RotM * V;
        otherwise
            error('Unknown CooSys option');
    end
            
    
    Nbody = size(R, 2);

    % Following Methods of orbits determination (Boulet); pp 151-157 
    % Eq. 4.68
    AbsR  = sqrt(sum(R.^2, 1));
    AbsV2 = sum(V.^2, 1);
    AbsRV = dot(R, V, 1);

    % The eccentricity vector (Eq. 4.69)
    % The eccentricity vector of a Kepler orbit is the dimensionless vector with direction pointing from apoapsis to periapsis and with magnitude equal to the orbit's scalar eccentricity. 
    EccenVec = (AbsV2./Args.Mu - 1./AbsR).*R - (AbsRV./Args.Mu).*V;
    Eccen    = sqrt(sum(EccenVec.^2, 1));

    % The angular momentum vector (Eq. 4.72)
    H     = cross(R, V, 1);
    AbsH2 = sum(H.^2, 1);
    AbsH  = sqrt(AbsH2);
    % unit vector in z direction
    K = [0; 0; 1];
    % The ascending node vector (Eq. 4.75)
    N = cross(repmat(K, 1, Nbody), H, 1);
    AbsN = sqrt(sum(N.^2, 1));

    % Semi major axis (Eq. 4.78)
    A = 1./(2./AbsR - AbsV2./Args.Mu);
    % Semi minor axis
    B = A.*sqrt(1 - Eccen.^2);

    % periastron distance (Eq. 4.81-4.82)
    Aux = AbsH2./Args.Mu;
    PeriDist = Aux./(1 + Eccen);

    % Inclination (Eq. 4.84)
    Incl = acos(H(3,:)./AbsH);

    % Longitude of ascending node (Eq. 4.86)
    Node = acos(N(1,:)./AbsN);
    FlagNy = N(2,:)<0;
    Node(FlagNy) = 2.*pi - Node(FlagNy);

    % Argument of periastron (Eq. 4.88)
    W = acos(dot(N, EccenVec, 1)./(AbsN.*Eccen));
    FlagEz = EccenVec(3,:)<0;
    W(FlagEz) = 2.*pi - W(FlagEz);

    % Mean anomaly (Eq. 4.89-4.106)
    Xbar = (Aux - AbsR)./Eccen;
    Ybar = sqrt(Aux./Args.Mu) .* AbsRV./Eccen;
    % True anomaly
    Nu = atan2(Ybar, Xbar);
    % Test: OrbEl.keplerSolve(OrbEl.Epoch).*RAD - Nu'.*RAD

    % Eccentric anomaly
    E = 2.*atan(sqrt( (1 - Eccen)./(1 + Eccen) ).*tan(0.5.*Nu));
    FlagE = E<0;
    E(FlagE) = 2.*pi + E(FlagE);
    % Test: [~,~,Et]=OrbEl.keplerSolve(OrbEl.Epoch); Et.*RAD - E.'


    FlagEll = Eccen<1;
    FlagPar = Eccen==1;
    FlagHyp = Eccen>1;

    % Elliptical orbit

    % Mean anomaly
    M = E - Eccen.*sin(E);
    % mean motion (n)
    %MeanMotion = Args.K.*sqrt(Args.Mu./A.^3);
    MeanMotion = sqrt(Args.Mu./A.^3);
    % Test: OrbEl.meanMotion - MeanMotion'.*RAD

    % hyperbolic orbit
    B(FlagHyp) = -A(FlagHyp).*sqrt(Eccen(FlagHyp).^2 - 1);
    SinH = Ybar(FlagHyp)./B(FlagHyp);
    H = asin(H);
    if any(FlagHyp)
        M(FlagHyp) = Eccen(FlagHyp).*SinH - H;
        MeanMotion(FlagHyp) = Args.K.*sqrt(Args.Mu./(-1.*(A(FlagHyp).^3)));
    end

    % parabolic orbit
    if any(FlagPar)
        D = AbsRV(FlagPar)./sqrt(Args.Mu);
        M(FlagPar) = PeriDist(FlagPar).*D + (D.^3)./6;
        MeanMotion(FlagPar) = Args.K.*sqrt(Args.Mu);
    end

    % periastron time
    T = Epoch(:).' - M./MeanMotion;


    ConvAng = convert.angular('rad', Args.AngUnits);
    Incl    = ConvAng.*Incl;
    Node    = ConvAng.*Node;
    W       = ConvAng.*W;
    Nu      = ConvAng.*Nu;
    M       = ConvAng.*M;
    MeanMotion = ConvAng.*MeanMotion; % [angle/day]

    Result.A     = A;
    Result.PeriDist = PeriDist;
    Result.Eccen = Eccen;
    Result.Incl  = Incl;
    Result.Node  = Node;
    Result.W     = W;
    Result.Nu    = Nu;
    Result.Tp    = T;
    Result.M     = M;
    Result.MeanMotion = MeanMotion;
    Result.Epoch = Epoch;

    if nargout>1
        % Store in celestial.OrbitalEl object
        OrbEl = celestial.OrbitalEl;
        OrbEl.MagType     = Args.MagType;
        OrbEl.MagPar      = Args.MagPar;
        OrbEl.Number      = Args.Number;
        OrbEl.Designation = Args.Designation;
        OrbEl.Node        = Result.Node(:);
        OrbEl.W           = Result.W(:);
        OrbEl.Incl        = Result.Incl(:);
        OrbEl.A           = Result.A(:);
        OrbEl.PeriDist    = Result.PeriDist(:);
        OrbEl.Eccen       = Result.Eccen(:);
        OrbEl.Tp          = Result.Tp(:);
        OrbEl.Epoch       = Result.Epoch(:);
        OrbEl.Mepoch      = M(:);
        OrbEl.AngUnits    = Args.AngUnits;
    end
end
