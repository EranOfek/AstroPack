function U2 = aberrationSolarSystem(U, E_dotH, Delta)
    % Apply aberration of light to solar system observer and body
    % Input  : - (U) A 3 row matrix of object barycentric position.
    %            All coordinate systems should be consistent.
    %          - A 3 row matrix of observer barycentric velocity.
    %          - A vector of observer-object distance.
    %            Note that for a star This will be 1, and U is the unit
    %            vector of the star position.
    %            If empty, then calculated from U.
    %            Default is [].
    % Output : - Abberated U.
    %
    % Here:
    %   u_B - The Barycentric position of the object.
    %   E_B - The Barycentric position of the earth.
    %   S_B - The Barycentric position of the Sun.
    %   E_H - The Heliocenric position of the Earth (E_B - S_B)
    % U = u_B(t-tau) - E_B(t)
    % Q = u_B(t-tau) - S_B(t-tau)  
    % For definitions and formulae, see Explanatory Supplement to the Astronomical
    % Alamanac (Seidelmann 2006), chapter 3.315, p. 148.
    % Vel should be in the Barycentric system, but here we
    % approximate it in the Heliocentric system
    %
    % Author : Eran Ofek (Jan 2016)
    % Example: U2 = celestial.SolarSys.aberrationSolarSystem(U, E_dotH, Delta)
            
    arguments
        U
        E_dotH
        Delta   = [];
    end
    
    Caud = constant.c.*86400./constant.au;  % speed of light [au/day]

    if isempty(Delta)
        Delta = sqrt(sum(U.^2, 1));
    end
    
    P       = U./Delta;
    V       = E_dotH./Caud;
    AbsV    = sqrt(sum(V.^2, 1));
    InvBeta = sqrt(1- AbsV.^2);
    F1      = dot(P, V, 1);
    F2      = 1 + F1./(1 + InvBeta);

    % The abberated position of the body in the geocentric inertial
    % frame that is moving with velocioty V of the Earth relative
    % to the natural frame:

    U2 = (InvBeta.*U + F2.*Delta.*V)./(1 + F1);

    % non reltivsitic approximation
    % U2 = U + Delta.*V;
end