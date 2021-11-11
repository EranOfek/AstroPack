function Mag=asteroid_magnitude(R,Delta,Beta,H,G)
% Calculate the magnitude of minor planets in the HG system
% Package: celestial.SolarSys
% Description: Calculate the magnitude of minor planets in the HG system.
%              Valid for phase angles (Beta) in range 0 to 120 deg.
% Input  : - MP-Sun distance in au.
%          - MP-observer distance in au.
%          - Phase angle in radians (Sun-Target-Observer angle).
%          - The mean absolute visual magnitude (H).
%          - The slope parameter (G), default is 0.15.
%            If G is NaN, then G is ignored and only H is used.
% Output : - The minor planet visual magnitude.
% Tested : Matlab 5.3
%     By : Eran O. Ofek                    Oct 2001
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Mag=celestial.SolarSys.asteroid_magnitude(3,2,0,15,0.15)
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    R
    Delta
    Beta
    H
    G     = 0.15;
end

INNG = ~isnan(G);

Phi1 = exp(-3.33.*tan(0.5.*Beta).^0.63);
Phi2 = exp(-1.87.*tan(0.5.*Beta).^1.22);

Mag = H + 5.*log10(R.*Delta);
Mag(INNG) = Mag(INNG)- 2.5.*log10((1-G(INNG)).*Phi1(INNG) + G(INNG).*Phi2(INNG));




