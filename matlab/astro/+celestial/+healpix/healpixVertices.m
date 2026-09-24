function [CornerLon, CornerLat] = healpixVertices(NSide, Pix, Type)
    % Return the 4 exact vertices of HEALPix pixels.
    %
    % Input  : - (NSide) HEALPix NSide.
    %          - A vector of pixel indices (0-based).
    %          - Type: 'nested' | 'ring'. Default is 'nested'.
    %
    % Output : - (CornerLon) - [Npix x 4] matrix of vertex longitudes [rad].
    %          - (CornerLat) - [Npix x 4] matrix of vertex latitudes  [rad].
    %
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Notes:
    %   - Vertices are returned in cyclic order:
    %       [north, west, south, east]
    %     in the local HEALPix diamond.
    %   - These are the true HEALPix vertices.
    %   - Pixel indices are assumed 0-based.
    %
    % Example:
    %   [Lon, Lat] = healpixVertices(16, [197;31], 'nested');

    arguments
        NSide 
        Pix 
        Type   = 'nested';
    end

    Pix = Pix(:);
    Type = lower(string(Type));

    if ~(Type=="nested" || Type=="ring")
        error('Type must be ''nested'' or ''ring''.');
    end

    NPixTot = 12 .* NSide.^2;
    if any(Pix < 0 | Pix >= NPixTot)
        error('Pixel index out of range. Valid range is 0 .. 12*NSide^2-1');
    end

    % Convert to nested if needed
    if Type=="ring"
        % If you already have a trusted ring2nest routine in your package,
        % use it here. For example:
        %
        % PixNest = celestial.healpix.ring2nest(NSide, Pix);
        %
        % Otherwise use the local converter below:
        PixNest = ring2nest_local(NSide, Pix);
    else
        PixNest = Pix;
    end

    Np = numel(PixNest);
    CornerLon = zeros(Np,4);
    CornerLat = zeros(Np,4);

    for Ipix = 1:Np
        [Face, Ix, Iy] = nest2xyf_local(NSide, PixNest(Ipix));

        % Pixel corners in integer face-grid coordinates.
        % If pixel cell is [Ix,Ix+1] x [Iy,Iy+1], then the four vertices are:
        %
        %   north : (Ix+0.5, Iy+1)
        %   west  : (Ix,     Iy+0.5)
        %   south : (Ix+0.5, Iy)
        %   east  : (Ix+1,   Iy+0.5)
        %
        % We convert these to the local face coordinates used by HEALPix:
        %   x = (jx - jy)/NSide
        %   y = (jx + jy)/NSide - 1
        %
        % where jx,jy are on the face grid.

        Jx = [Ix+0.5, Ix,     Ix+0.5, Ix+1  ];
        Jy = [Iy+1,   Iy+0.5, Iy,     Iy+0.5];

        X = (Jx - Jy) ./ NSide;
        Y = (Jx + Jy) ./ NSide - 1.0;

        for K = 1:4
            [Z, Phi] = xyf2zphi_local(Face, X(K), Y(K));
            CornerLon(Ipix,K) = wrapToPi_local(Phi);
            CornerLat(Ipix,K) = asin(max(-1,min(1,Z)));
        end
    end
end


%==========================================================================
function [Face, Ix, Iy] = nest2xyf_local(NSide, PixNest)
% Convert NESTED pixel number -> face, ix, iy
%
% Face is 0..11
% Ix, Iy are 0..NSide-1

    NPFace = NSide.^2;
    Face   = floor(PixNest ./ NPFace);
    IpFace = mod(PixNest, NPFace);

    [Ix, Iy] = deinterleave_bits_local(IpFace);
end


%==========================================================================
function [Ix, Iy] = deinterleave_bits_local(Code)
% Morton decode: interleaved bits -> ix,iy

    Ix = 0;
    Iy = 0;

    BitPos = 0;
    while Code > 0
        Ix = bitor(Ix, bitshift(bitand(Code,1), BitPos));
        Code = bitshift(Code, -1);

        Iy = bitor(Iy, bitshift(bitand(Code,1), BitPos));
        Code = bitshift(Code, -1);

        BitPos = BitPos + 1;
    end
end


%==========================================================================
function Code = interleave_bits_local(Ix, Iy)
% Morton encode: ix,iy -> interleaved bits

    Code = 0;
    BitPos = 0;

    while (Ix > 0) || (Iy > 0)
        Code = bitor(Code, bitshift(bitand(Ix,1), 2*BitPos));
        Code = bitor(Code, bitshift(bitand(Iy,1), 2*BitPos + 1));

        Ix = bitshift(Ix, -1);
        Iy = bitshift(Iy, -1);
        BitPos = BitPos + 1;
    end
end


%==========================================================================
function [Z, Phi] = xyf2zphi_local(Face, X, Y)
% Standard HEALPix face coordinates -> (z,phi)
%
% X,Y are local face coordinates.
% Face is 0..11.
%
% This is the exact HEALPix mapping for the face diamond.

    if Face < 4
        % north polar faces
        Phi0 = Face * (pi/2);
        [Z, Phi] = xy2zphi_north_polar_local(X, Y, Phi0);

    elseif Face < 8
        % equatorial faces
        Phi0 = (Face - 4) * (pi/2);
        [Z, Phi] = xy2zphi_equatorial_local(X, Y, Phi0);

    else
        % south polar faces
        Phi0 = (Face - 8) * (pi/2);
        [Z, Phi] = xy2zphi_south_polar_local(X, Y, Phi0);
    end

    Phi = mod(Phi, 2*pi);
end


%==========================================================================
function [Z, Phi] = xy2zphi_equatorial_local(X, Y, Phi0)
% Exact HEALPix mapping in equatorial faces.

    Z = (2/3) .* Y;

    % In equatorial faces the boundaries are linear in X at fixed Y.
    Phi = Phi0 + (pi/4) .* X;
end


%==========================================================================
function [Z, Phi] = xy2zphi_north_polar_local(X, Y, Phi0)
% Exact HEALPix mapping in north polar faces.
%
% Here Y runs from 0 to 1 on the face diamond.
% sigma = 1 - Y determines z.

    Sigma = 1 - Y;   % 0 at poleward tip, 1 at equatorial join
    Sigma = max(0, Sigma);

    Z = 1 - (Sigma.^2)./3;

    % Exact azimuth formula in polar cap
    Den = max(Sigma, eps);
    Phi = Phi0 + (pi/4) .* (X ./ Den);
end


%==========================================================================
function [Z, Phi] = xy2zphi_south_polar_local(X, Y, Phi0)
% Exact HEALPix mapping in south polar faces.

    Sigma = 1 + Y;   % 0 at poleward tip, 1 at equatorial join
    Sigma = max(0, Sigma);

    Z = -1 + (Sigma.^2)./3;

    Den = max(Sigma, eps);
    Phi = Phi0 + (pi/4) .* (X ./ Den);
end


%==========================================================================
function PixNest = ring2nest_local(NSide, PixRing)
% Convert RING -> NESTED using standard HEALPix formulas.

    PixRing = PixRing(:);
    N = numel(PixRing);
    PixNest = zeros(N,1);

    for I = 1:N
        [Z, Phi] = pix2zphi_ring_local(NSide, PixRing(I));
        PixNest(I) = zphi2nest_local(NSide, Z, Phi);
    end
end


%==========================================================================
function [Z, Phi] = pix2zphi_ring_local(NSide, Ipix)
% Standard HEALPix RING pixel center -> (z,phi)
% Ipix is 0-based.

    NCap  = 2*NSide*(NSide-1);
    NPix  = 12*NSide^2;
    Nl2   = 2*NSide;
    Nl4   = 4*NSide;

    Ip = Ipix + 1;  % internal 1-based

    if Ip <= NCap
        % North polar cap
        Iring = floor(0.5*(1 + sqrt(2*Ip - 1)));
        Iphi  = Ip - 2*Iring*(Iring-1);

        Z   = 1 - (Iring^2)/(3*NSide^2);
        Phi = (Iphi - 0.5) * pi / (2*Iring);

    elseif Ip <= NPix - NCap
        % Equatorial region
        IpEq  = Ip - NCap - 1;
        Iring = floor(IpEq / Nl4) + NSide;
        Iphi  = mod(IpEq, Nl4) + 1;

        Fodd = 0.5 * (1 + mod(Iring + NSide, 2));
        Z    = (Nl2 - Iring) * (2/(3*NSide));
        Phi  = (Iphi - Fodd) * pi / (2*NSide);

    else
        % South polar cap
        IpSouth = NPix - Ip + 1;
        Iring   = floor(0.5*(1 + sqrt(2*IpSouth - 1)));
        Iphi    = 4*Iring + 1 - (IpSouth - 2*Iring*(Iring-1));

        Z   = -1 + (Iring^2)/(3*NSide^2);
        Phi = (Iphi - 0.5) * pi / (2*Iring);
    end

    Phi = mod(Phi, 2*pi);
end


%==========================================================================
function PixNest = zphi2nest_local(NSide, Z, Phi)
% Standard HEALPix (z,phi) -> NESTED pixel number, 0-based.

    Za = abs(Z);
    TT = mod(Phi, 2*pi) * (2/pi);   % in [0,4)

    if Za <= 2/3
        % Equatorial region
        Jp = floor(NSide * (0.5 + TT - 0.75*Z));
        Jm = floor(NSide * (0.5 + TT + 0.75*Z));

        Ifp = floor(Jp / NSide);
        Ifm = floor(Jm / NSide);

        if Ifp == Ifm
            Face = mod(Ifp,4) + 4;
        elseif Ifp < Ifm
            Face = mod(Ifp,4);
        else
            Face = mod(Ifm,4) + 8;
        end

        Ix = mod(Jm, NSide);
        Iy = NSide - mod(Jp, NSide) - 1;

    else
        % Polar caps
        Ntt = floor(TT);
        Tp  = TT - Ntt;

        Tmp = NSide * sqrt(3*(1-Za));
        Jp  = floor(Tp * Tmp);
        Jm  = floor((1-Tp) * Tmp);

        if Z >= 0
            Face = mod(Ntt,4);
            Ix   = NSide - Jm - 1;
            Iy   = NSide - Jp - 1;
        else
            Face = mod(Ntt,4) + 8;
            Ix   = Jp;
            Iy   = Jm;
        end
    end

    PixNest = Face * NSide^2 + interleave_bits_local(Ix, Iy);
end


%==========================================================================
function Phi = wrapToPi_local(Phi)
% Wrap angle to [-pi, pi)

    Phi = mod(Phi + pi, 2*pi) - pi;
end
