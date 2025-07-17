function [Result] = increasePixelResolution(Ipix0, Nside0, Nside1)
    % Given Ipix @ Nside0 calculate the corresponding indices at Nside1 > Nside0  
    %     NESTED scheme only!
    % Input  : - pixel indices at resolution Nside0
    %          - initial resolution Nside0
    %          - increased resolution Nside1    
    % Output : - pixel indices at resolution Nside1
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: Ipix0 = 36136; Nside0 = 2^7; Nside1 = 2^8;
    %          Ipix1 = celestial.healpix.increasePixelResolution(Pix0, Nside0, Nside1)
    arguments
        Ipix0
        Nside0
        Nside1
    end    
    %
    assert(mod(Nside1, Nside0) == 0, 'Nside1 must be a multiple of Nside0');        
    R2 = (Nside1 / Nside0)^2;
    Result = [];   
    for i=1:numel(Ipix0)
        First  = Ipix0(i) * R2;
        Last   = (Ipix0(i) + 1) * R2 - 1; 
        Result = [Result; (First : Last)']; 
    end        
end
