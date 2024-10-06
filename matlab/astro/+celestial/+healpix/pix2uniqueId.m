function [UniqueId] = pix2uniqueId(Nside, Pix)
    % Return healpix unique Id from Nside and pixel index.
    %       See also: celestial.healpix.uniqueId2pi
    % Input  : - Nside.
    %          - Pixel index.
    % Output : - Unique Id (Pix + 4.*Nside.^2)
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.pix2uniqueId(16,1)

    UniqueId = Pix + 4.*Nside.^2;

end
