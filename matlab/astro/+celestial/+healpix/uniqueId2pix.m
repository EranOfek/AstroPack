function [Nside, Pix] = uniqueId2pix(UniqueId)
    % Convert healpix unique Id to Nside and pixel index.
    %     See also: celestial.healpix.pix2uniqueId
    % Input  : - UniqueId 
    % Output : - Nside.
    %          - Pixel index.
    % Author : Eran Ofek (2024 Sep) 
    % Example: [Nside, Pix] = celestial.healpix.uniqueId2pix(1025)
    %          celestial.healpix.pix2uniqueId(Nside,Pix)

    Nside = 2.^(floor(log2(UniqueId./4)./2));
    Pix   = UniqueId - 4.*Nside.^2;

end
