function [Nside, Pix] = uniqueId2pix(Nside, UniqueId)
    % Convert healpix unique Id to Nside and pixel index.
    %     See also: celestial.healpix.pix2uniqueId
    % Input  : - Nside. If empty, will calculate.
    %          - UniqueId 
    % Output : - Nside.
    %          - Pixel index.
    % Author : Eran Ofek (2024 Sep) 
    % Example: [Nside, Pix] = celestial.healpix.uniqueId2pix([],1025)
    %          celestial.healpix.pix2uniqueId(Nside,Pix)


    if isempty(Nside)
        Nside = 2.^(floor(log2(UniqueId./4)./2));
    end
    Pix   = UniqueId - 4.*Nside.^2;

end
