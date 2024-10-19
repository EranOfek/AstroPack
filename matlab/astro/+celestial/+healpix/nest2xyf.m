function [Face, X, Y] = nest2xyf(NSide, Pix)
    % Converted nested pixel index into Face index and X/Y local coordinates in face.
    %       In HEALPix, face refers to one of 12 primary devisions of the
    %       sphere. Face index runs from 0 to 11.
    % Input  : - NSide.
    %          - Pixel index.
    % Output : - Face index (0 to 11)
    %          - IX pixel index in face.
    %          - IY pixel index in face.
    % Author : Eran Ofek (2024 Oct) 
    % Example: [Face, X, Y] = celestial.healpix.nest2xyf(2.^16, [1])

    error('incorrect');

    % Convert nested index to face and (ix, iy) coordinates
   
    %
    Npface = uint64(NSide.^2);
    Pix    = uint64(Pix);
    Face   = uint64(floor(double(Pix) ./ double(Npface)));
    
    % 2. Get the pixel index within the face
    Ipix = Pix - Face .* Npface;
    
    % 3. Decompose ipix into x and y coordinates using bit interleaving
    X = zeros(size(Pix), 'uint64');
    Y = zeros(size(Pix), 'uint64');
    
    % Number of bits to process
    Nb = floor(log2(NSide)); 
    
    for I = 0:Nb-1
        % Extract even bits for ix
        I = uint64(I);
        X = bitor(X, bitshift(bitand(Ipix, 2.^(2.*I)), -I));
        Y = bitor(Y, bitshift(bitand(Ipix, 2.^(2.*I+1)), -(I+1)));
        
    end
    
    
end
