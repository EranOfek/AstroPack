function [PixelRadius] = healpix_radius(Nside)
    % Return the healpix radius, given its NSide. 
    % Input  : - Healpix Nside.
    % Output : - Healpix max. radius [radians].
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.healpix_radius(16)

    % Total number of pixels
    %Npix = 12 .* Nside.^2;
    PixelRadius = pi / (sqrt(3) .* Nside);
    
    
end
