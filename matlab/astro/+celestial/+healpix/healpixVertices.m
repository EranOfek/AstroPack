function [CornerLon, CornerLat] = healpixVertices(NSide, Pix, Type)
    % Return the 4 exact vertices (corners) of HEALPix pixels.
    %   Corners are computed from the face grid with the standard HEALPix
    %   mapping (Gorski et al. 2005; same as healpy.boundaries with step=1).
    % Input  : - (NSide) HEALPix NSide (scalar).
    %          - A vector of pixel indices (0-based). Pass an integer
    %            class (e.g. int64) for NSide > 2^24, where doubles cannot
    %            hold all indices exactly.
    %          - Type: 'nested' | 'ring'. Default is 'nested'.
    %            'ring' is supported up to NSide=2^16.
    % Output : - (CornerLon) - [Npix x 4] matrix of vertex longitudes
    %            [rad], in the range [0, 2*pi).
    %          - (CornerLat) - [Npix x 4] matrix of vertex latitudes [rad].
    %          Vertices are ordered [north, west, south, east].
    % Author : Eran Ofek (Apr 2026); A.M. Krassilchtchikov (2026 Oct)
    % Example: [Lon, Lat] = celestial.healpix.healpixVertices(16, [197;31], 'nested');

    arguments
        NSide (1,1)
        Pix
        Type   = 'nested';
    end

    Pix  = Pix(:);
    Type = lower(string(Type));

    if ~(Type=="nested" || Type=="ring")
        error('Type must be ''nested'' or ''ring''.');
    end

    % keep integer indices exact (doubles hold them only up to NSide~2^24)
    if isinteger(Pix)
        if any(Pix < 0)
            error('Pixel index out of range. Valid range is 0 .. 12*NSide^2-1');
        end
        Pix = uint64(Pix);
    elseif any(Pix < 0 | Pix ~= round(Pix))
        error('Pixel index out of range. Valid range is 0 .. 12*NSide^2-1');
    end
    if any(uint64(Pix) >= uint64(12) .* uint64(NSide).^2)
        error('Pixel index out of range. Valid range is 0 .. 12*NSide^2-1');
    end

    % ring -> nested via the pixel centre
    if Type=="ring"
        if NSide > 2^16
            error('Type ''ring'' is supported up to NSide=2^16 (limit of mex.ang2pix_nested)');
        end
        [Lon0, Lat0] = celestial.healpix.mex.pix2ang_ring(NSide, double(Pix));
        Pix = celestial.healpix.mex.ang2pix_nested(NSide, Lon0, Lat0);
        Pix = Pix(:);
    end

    [Ix, Iy, Face] = celestial.healpix.nest2xyf(NSide, Pix);
    Ix   = double(Ix(:));
    Iy   = double(Iy(:));
    Face = double(Face(:));

    % ring and longitude offsets of the 12 base faces
    JRLL = [2 2 2 2 3 3 3 3 4 4 4 4];
    JPLL = [1 3 5 7 0 2 4 6 1 3 5 7];
    Jr0  = JRLL(Face+1).';
    Jp0  = JPLL(Face+1).';

    % corner offsets on the face grid: north, west, south, east
    Dx = [1 0 0 1];
    Dy = [1 1 0 0];
    X  = (Ix + Dx) ./ NSide;          % [Npix x 4], in [0,1]
    Y  = (Iy + Dy) ./ NSide;

    Jr = Jr0 - X - Y;                 % ring coordinate in units of NSide, [0,4]
    IsNorth = Jr < 1;
    IsSouth = Jr > 3;
    Nr = ones(size(Jr));
    Nr(IsNorth) = Jr(IsNorth);
    Nr(IsSouth) = 4 - Jr(IsSouth);

    Z = (2 - Jr) .* 2 ./ 3;           % equatorial belt
    Tmp = Nr.^2 ./ 3;
    Z(IsNorth) = 1 - Tmp(IsNorth);
    Z(IsSouth) = Tmp(IsSouth) - 1;

    % longitude
    T = mod(Jp0 .* Nr + X - Y, 8);
    Phi = (pi/4) .* T ./ Nr;
    Phi(Nr < 1e-15) = 0;               % pole
    CornerLon = mod(Phi, 2.*pi);

    % latitude; near the poles use 1-|z| directly to keep precision
    CornerLat = asin(Z);
    IsCap = IsNorth | IsSouth;
    CosLat = sqrt(Tmp .* (2 - Tmp));  % = sqrt(1 - z^2) in the polar caps
    CornerLat(IsCap) = sign(Z(IsCap)) .* acos(min(1, CosLat(IsCap)));
end
