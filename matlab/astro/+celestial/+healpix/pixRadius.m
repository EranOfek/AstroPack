function [PixelRadius, MaxPixRadius] = pixRadius(Nside)
    % Return the HEALPix pixel radius (equal-area and maximal), given its Nside.
    % Input  : - Healpix Nside (scalar or array).
    % Output : - Radius [radians] of a spherical cap whose area equals the
    %            pixel area, 4*pi/(12*Nside^2). Same size as Nside.
    %          - Max. pixel radius [radians]: the largest angular distance
    %            between a pixel centre and any point of its boundary
    %            (same as healpy max_pixrad). Same size as Nside.
    % Author : Eran Ofek (2024 Sep); A.M. Krassilchtchikov (2026 Oct)
    % Example: [R, MaxR] = celestial.healpix.pixRadius(16)

    % equal-area cap: 1-cos(R) = 1/(6*Nside^2), written via asin to keep
    % precision at large Nside
    PixelRadius = 2.*asin(1./(sqrt(12).*Nside));

    % angle between the two points that bound the largest pixel
    % (Gorski et al. 2005; healpy max_pixrad)
    Za = 2./3;
    Pa = pi./(4.*Nside);
    Zb = 1 - (1 - 1./Nside).^2./3;
    Sa = sqrt(1 - Za.^2);
    Sb = sqrt(1 - Zb.^2);
    % Vb lies at azimuth 0: Vb = [Sb, 0, Zb]
    Xa = Sa.*cos(Pa);
    Ya = Sa.*sin(Pa);
    CrossX = Ya.*Zb;
    CrossY = Za.*Sb - Xa.*Zb;
    CrossZ = -Ya.*Sb;
    MaxPixRadius = atan2(sqrt(CrossX.^2 + CrossY.^2 + CrossZ.^2), Xa.*Sb + Za.*Zb);

end
