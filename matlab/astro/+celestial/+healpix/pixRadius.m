function [PixelRadius] = pixRadius(Nside)
    % Return the healpix radius, given its NSide. 
    % Input  : - Healpix Nside.
    % Output : - Healpix max. radius [radians].
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.pixRadius(16)

    % Total number of pixels
    %Npix = 12 .* Nside.^2;
    PixelRadius = pi ./ (sqrt(3) .* Nside);
    
end
