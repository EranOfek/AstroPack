function PixInd = xyf2nest(NSide, X, Y, Face, Args)
    % Convert (NSide, X, Y, Face) to HEALPix nested pixel index.
    %   See also celestial.healpix.xyf2ipix 
    % Input  : - (NSide) scalar positive integer (power of 2)
    %          - X coordinates.
    %          - Y coordinates.
    %          - Face.   
    % Output : - Healpix pixel index (uint64) same size as X.
    %
    % Notes  : 
    %          - Implements exact Morton bit interleaving.
    %          - Supports scalar or array inputs.
    %          - All inputs must be same size (or scalar-expandable).
    % Author : ChatGPT + Eran Ofek (Feb 2026)
    % Example: [p] = celestial.healpix.xyf2nest(NSide,X,Y,F);

    arguments
        NSide (1,1)
        X
        Y
        Face % {mustBeInteger, mustBeNonnegative}
        Args.CheckRange = true
    end
    
    % Convert to uint64 for safe bit ops
    NSideU = uint64(NSide);
    X = uint64(X);
    Y = uint64(Y);
    Face = uint64(Face);
    
    % Validate NSide power-of-two
    K = uint32(round(log2(double(NSideU))));
    if bitshift(uint64(1), K) ~= NSideU
        error('NSide must be a power of two for standard HEALPix NESTED.');
    end
    
    if Args.CheckRange
        if any(X(:) >= NSideU) || any(Y(:) >= NSideU)
            error('X and Y must satisfy 0 <= X,Y < NSide.');
        end
        if any(Face(:) > 11)
            error('Face must be in [0 .. 11].');
        end
    end
    
    % Interleave bits: Morton encoding
    Ip = zeros(size(X), 'uint64');
    
    for I = 0:(K-1)
        % Extract bit I from X and Y
        Xbit = bitand(bitshift(X, -double(I)), uint64(1));
        Ybit = bitand(bitshift(Y, -double(I)), uint64(1));
    
        % Place into Ip at positions 2*I and 2*I+1
        Ip = bitor(Ip, bitshift(Xbit, 2*I));
        Ip = bitor(Ip, bitshift(Ybit, 2*I + 1));
    end
    
    % Combine with face
    PixInd = Face .* (NSideU*NSideU) + Ip;

end