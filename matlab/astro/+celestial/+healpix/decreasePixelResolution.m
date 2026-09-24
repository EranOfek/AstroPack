function [Result] = decreasePixelResolution(Ipix0, Nside0, Nside1)
    % Given Ipix @ Nside0 calculate the corresponding indices at Nside1 < Nside0  
    %     NESTED scheme only!
    % Input  : - pixel indices at resolution Nside0
    %          - initial resolution Nside0
    %          - decreased resolution Nside1    
    % Output : - pixel indices at resolution Nside1
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: Ipix0 = [144545 144546 144544 144547]; Nside0 = 2^8; Nside1 = 2^7;
    %          Ipix1 = celestial.healpix.decreasePixelResolution(Ipix1, Nside0, Nside1)    
    arguments
        Ipix0
        Nside0
        Nside1        
    end    
    % Check that Nside0 is divisible by Nside1
    assert(mod(Nside0, Nside1) == 0, 'Nside0 must be a multiple of Nside1');
    
    R = Nside0 / Nside1;
    Result = [];   
    for i=1:numel(Ipix0)
        Result = [Result; floor(Ipix0(i) / R^2)];
    end
end
