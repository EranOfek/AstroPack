function [Result] = nRing(NSide)
    % Number of rings in healpix
    %     Healpix use an equal co-altitude partition. This function return
    %     the number of rings given NSide.
    % Input  : - NSide.
    % Output : - Number of rings (4*NSide+1) including the soutern pole.
    % Author : Eran Ofek (2024 Dec) 
    % Example: celestial.healpix.nRing(2^16)

    Result = 4.*NSide+1;

end
