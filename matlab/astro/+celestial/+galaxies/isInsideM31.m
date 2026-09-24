function [Flag,Z] = isInsideM31(RA, Dec, Units)
    % Check if coordinates are within the M31 footprints.
    % Input  : - J2000 RA
    %          - J2000 Dec
    %          - Units: 'rad'|'deg'. Default is 'deg'. 
    % Output : - A vector of logical flags indicating if corrdinates are
    %            within the LMC footprints.
    %          - M31 effective redshift (assuming distance of 765 kpc).
    % Author : Eran Ofek (2026 Mar) 
    % Example: [Flag] = celestial.galaxies.isInsideM31(1.4, -1.2, 'rad')

    arguments
        RA
        Dec
        Units   = 'deg';
    end
    
    Z = 0.0001785;
    
    Conv = convert.angular(Units, 'rad');
    Long = RA(:).*Conv;
    Lat  = Dec(:).*Conv;

    % M31 coordinates/ radians
    % RA=celestial.coo.convertdms('00:42:44','SH','r');
    % Dec=celestial.coo.convertdms('+41:16:09','SD','r');
    % PA=36./RAD;
    % A=190./60./2./RAD;
    % B=70./60./2./RAD;
    GalRA = 0.186459341754728;
    GalDec = 0.720282837887626;
    A = 0.0276343798232436;
    B = 0.010181087303300;
    PA = 0.628318530717959;
    CosDec = cos(GalDec);
    
    
    Flag = Dec>(GalDec-A) & Dec<(GalDec+A) & RA>(GalRA-A./CosDec) & RA<(GalRA+A./CosDec);
    if any(Flag)
        Ind = find(Flag);
        Flag(Ind) = celestial.galaxies.insideEllipse(RA(Ind), Dec(Ind), GalRA, GalDec, A, B, PA);
    end
    
end
