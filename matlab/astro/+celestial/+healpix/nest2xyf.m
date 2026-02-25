function [X, Y, Face] = nest2xyf(NSide, Pix, Args)
    % Convert HEALPix nested pixel index to (X,Y,Face).
    %
    % Input  : - (NSide) scalar positive integer (power of 2 in standard HEALPix)
    %          - (Pix) scalar or array of integer pixel indices in [0 .. 12*NSide^2 - 1]
    %          * ..., key,val,...
    %            'CheckRange' - Default is false.
    %
    % Output : - X coordinates (same size as Pix).
    %          - Y coordinates.
    %          - Face.
    %
    % Notes  : This is the standard NESTED (bit-interleaved) decoding.
    %          Works for scalar or array Pix.
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: [X,Y,F] = healpixNestedPix2xyf(NSide, Pix);
    
    arguments
        NSide (1,1)
        Pix 
        Args.CheckRange logical = false;
    end
    
    % Ensure integer class with enough bits
    Pix = uint64(Pix);
    NSideU = uint64(NSide);
    
    Npix = uint64(12) * NSideU * NSideU;
    
    if Args.CheckRange
        if any(Pix(:) >= Npix)
            error('Pix out of range: must satisfy 0 <= Pix <= 12*NSide^2 - 1.');
        end
    end
    
    % Face is the top bits (Pix / NSide^2)
    Face = uint64(floor(double(Pix) ./ double(NSideU*NSideU))); %#ok<FNDSB>
    % Avoid double if you prefer: integer division
    Face = idivide(Pix, NSideU*NSideU, 'floor');
    
    % Pixel-in-face index
    Ip = Pix - Face .* (NSideU*NSideU);
    
    % Decode interleaved bits: Ip -> (X,Y)
    [X, Y] = local_deinterleave_xy(Ip, NSideU);
    
    % Cast outputs to MATLAB default integer type (uint32 is plenty for typical NSide)
    X = uint32(X);
    Y = uint32(Y);
    Face = uint32(Face);
    
    end
    
    %-----------------------------------------------------------------------
    function [X, Y] = local_deinterleave_xy(Ip, NSideU)
    %local_deinterleave_xy Deinterleave NESTED bits into X and Y.
    %
    % Ip: uint64 array in [0 .. NSide^2-1]
    % NSideU: uint64 scalar
    
    % Number of bits needed: NSide = 2^K  => K = log2(NSide)
    % In standard HEALPix, NSide is power of two.
    K = uint32(round(log2(double(NSideU))));
    if bitshift(uint64(1), K) ~= NSideU
        error('NSide must be a power of two for standard HEALPix NESTED indexing.');
    end
    
    X = zeros(size(Ip), 'uint64');
    Y = zeros(size(Ip), 'uint64');
    
    % Bit positions: Ip has bits [0 .. 2*K-1]
    % Even bits -> X, odd bits -> Y
    % X bit i comes from Ip bit (2*i)
    % Y bit i comes from Ip bit (2*i+1)
    for I = 0:(K-1)
        X = bitor(X, bitshift(bitand(Ip, bitshift(uint64(1), 2*I)), -double(I)));
        Y = bitor(Y, bitshift(bitand(Ip, bitshift(uint64(1), 2*I + 1)), -double(I+1)));
    end
    
    % Now X,Y are in [0..NSide-1]
end