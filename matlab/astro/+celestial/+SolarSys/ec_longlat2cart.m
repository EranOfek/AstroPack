function [X,Y,Z] = ec_longlat2cart(L,B,R,JD,OutType)
    % Convert Heliocentric ecliptic long/lat/rad referred to mean equinox of date to cartesian coordinates.
    % Input  : - Heliocentric ecliptic long refered to mean equinox of date.
    %          - Heliocentric ecliptic lat refered to mean equinox of date.
    %          - Heliocentric ecliptic rad refered to mean equinox of date.
    %          - Vector of JD (one per Long/lat/rad).
    %          - Output type (always cartesian): 
    %               'Ecdate' - Ecliptic of date.
    %               'Eqdate' - Equatorial mean equinox of date.
    %               'EqJ2000' - Equatorial mean equinox of J2000.
    % Output : - Vector of X.
    %          - Vector of Y.
    %          - Vector of Z.
    % Author : Eran Ofek (Oct 2021)
    % Example: [X,Y,Z] = celestial.SolarSys.ec_longlat2cart([1 2],[0 1],[1 2],2451545+[100, 200])
    
    arguments
        L
        B
        R
        JD           = [];
        OutType      = 'EqJ2000';   % 'Ecdate' | 'Eqdate' | 'EqJ2000'
    end
    
    % convert from Eclitpic long/lat/rad to Ecliptic cartesizn coordinates
    X = R.*cos(L);
    Y = R.*sin(L);
    Z = R.*tan(B);
    
    switch lower(OutType)
        case 'ecdate'
            % do nothing - already in Ecliptic date coo
        case 'eqdate'
            % 
            
            Nt      = numel(JD);
            PosVec  = zeros(3, Nt);
            for It=1:1:Nt
                RotM         = celestial.coo.rotm_coo('E', JD(It));
                PosVec(:,It) = RotM * [X(It); Y(It); Z(It)];
            end
            X = PosVec(1,:).';
            Y = PosVec(2,:).';
            Z = PosVec(3,:).';
            
        case 'eqj2000'
            %
            Nt      = numel(JD);
            PosVec  = zeros(3, Nt);
            for It=1:1:Nt
                RotM1         = celestial.coo.rotm_coo('E', JD(It));
                RotM2         = celestial.coo.rotm_coo('p', JD(It));
                PosVec(:,It)  = RotM2 * (RotM1 * [X(It); Y(It); Z(It)]);
            end
            X = PosVec(1,:).';
            Y = PosVec(2,:).';
            Z = PosVec(3,:).';
            
        otherwise
            error('Unknown OutType option');
    end
end