function [X, Y, F] = ipix2xyf(Ipix, Nside)
    % Convert a nested HEALPix pixel index to (X, Y, F [face number]) 
    %     Optional detailed description
    % Input  : - the healpix index (Ipix)
    %          - Nside healpix resolution
    % Output : - x coordinate
    %          - y coordinate
    %          - f (face)
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: Nside = 2^8; Ipix = 652100;
    %         [X, Y, F] = celestial.healpix.ipix2xyf(Ipix,Nside)
    %         celestial.healpix.xyf2ipix(X,Y,F,Nside)
    arguments
        Ipix
        Nside        
    end
    %     
    NPface = Nside * Nside;
    F = floor(Ipix / NPface);
    P = mod(Ipix, NPface);    
    % Decode p into (ix, iy) using bit interleaving (Morton order)
    X = 0;
    Y = 0;
    for I = 0:log2(Nside)-1
        X = bitor(X, bitshift(bitget(P, 2*I+1), I));
        Y = bitor(Y, bitshift(bitget(P, 2*I+2), I));
    end
end
