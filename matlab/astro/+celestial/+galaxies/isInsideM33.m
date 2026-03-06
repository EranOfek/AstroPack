function [Flag,Z] = isInsideM33(RA, Dec, Units)
    % Check if coordinates are within the M33 footprints.
    % Input  : - J2000 RA
    %          - J2000 Dec
    %          - Units: 'rad'|'deg'. Default is 'deg'. 
    % Output : - A vector of logical flags indicating if corrdinates are
    %            within the LMC footprints.
    %          - M33 effective redshift (assuming distance of 830 kpc).
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Flag] = celestial.galaxies.isInsideM33(1.4, -1.2, 'rad')

    arguments
        RA
        Dec
        Units   = 'deg';
    end
    
    Z = 0.00019367;
    
    Conv = convert.angular(Units, 'rad');
    Long = RA(:).*Conv;
    Lat  = Dec(:).*Conv;

    % M33 coordinates/ radians
    % RA=celestial.coo.convertdms('01:33:50','SH','r');
    % Dec=celestial.coo.convertdms('+30:39:37','SD','r');

    GalRA  = 0.409425153697003;
    GalDec = 0.535122796798272;
    GalRadius = 0.0106175; % rad   % semi major axis 36.5 arcmin
    CosDec    = cos(GalDec);

    Flag = Dec>(GalDec-GalRadius) & Dec<(GalDec+GalRadius) & RA>(GalRA-GalRadius./CosDec) & RA<(GalRA+GalRadius./CosDec);
    if any(Flag)
        Ind = find(Flag);
        Dist = celestial.coo.sphere_dist_fast(GalRA, GalDec, RA(Ind), Dec(Ind));
        Flag(Ind) = Dist<GalRadius;
    end
    

end
