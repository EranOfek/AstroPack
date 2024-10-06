function [Npix] = nPix(Nside)
    % Return the number of pixels in healpix given its Nside.
    % Input  : - Nside.
    % Output : - Number of pixels. Note that the indices are running from 0
    %            to Npix-1.
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.nPix(16)

    Npix = 12 .* Nside.^2;
end
