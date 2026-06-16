function [PixelRadius, MaxPixRadius] = pixRadius(Nside)
    % Return the healpix diameter, given its NSide. 
    % Input  : - Healpix Nside.
    % Output : - Healpix radius [radians] which circle area is equivalent
    %            to the healpix area.
    %          - Max. healpix radius
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.pixRadius(16)

    % Total number of pixels
    %Npix = 12 .* Nside.^2;
    PixelRadius = pi ./ (sqrt(3) .* Nside);
    MaxPixRadius = 1./Nside;
    
end
