function Ipix = xyf2ipix(X, Y, F, Nside)
    % Convert (x, y, face number) to HEALPix pixel index in the NESTED scheme
    %     Optional detailed description
    % Input  : - x coordinate
    %          - y coordinate
    %          - f (face)
    %          - Nside healpix resolution
    % Output : - the healpix index (Ipix)
    % Author : A.M. Krassilchtchikov (2025 Jul) 
    % Example: X = 100; Y = 200; F = 7; Nside = 2^8;
    %          Ipix = celestial.healpix.xyf2ipix(X,Y,F,Nside)
    %          [X, Y, F] = celestial.healpix.ipix2xyf(Ipix,Nside)
    arguments
        X
        Y
        F
        Nside
    end
    % Interleave bits of x and y to form the position within the face
    P = 0;
    for I = 0:log2(Nside)-1
        P = bitor(P, bitshift(bitget(X, I+1), 2*I));
        P = bitor(P, bitshift(bitget(Y, I+1), 2*I + 1));
    end
    %
    Ipix = F * Nside^2 + P;
end
