function [CornerLons, CornerLats] = pixelCorners(NSide, Pix, Type)
    % Return healpix pixel corners.
    % Input  : - Healpix Nside.
    %          - Pixel index.
    %          - Type: 'nested'|'ring'. Default is 'nested'.
    % Output : - A two column matrix, in which each line corresponds to the
    %            longitude corners of the corresponding pixel (radians).
    %          - A two column matrix, in which each line corresponds to the
    %            latitude corners of the corresponding pixel (radians).
    % Author : Eran Ofek (Sep 2024)
    % Example: [Lon,Lat]=celestial.healpix.pixelCorners(16,[197;31],'nested')

    arguments
        NSide
        Pix
        Type   = 'nested';
    end

    Pix = Pix(:);

    N = numel(Pix);

    % Find the pixel's center coordinates
    switch lower(Type)
        case 'nested'
            [CenterLon, CenterLat] = celestial.healpix.mex.pix2ang_nested(NSide, Pix);
        case 'ring'
            [CenterLon, CenterLat] = celestial.healpix.mex.pix2ang_ring(NSide, Pix);
        otherwise
            error('Unknown Type option');
    end

    % Calculate half the side length of a pixel in radians
    PixelRadius = pi ./ (sqrt(3) .* NSide);
    
    % Initialize arrays to store corner coordinates
   
    % Unit vectors for 4 corner shifts: right, up, left, down
    CornerOffsets = [1 -1 -1 +1; +1 +1 -1 -1];
    % 
    %     +1, +1; % Top-right
    %     -1, +1; % Top-left
    %     -1, -1; % Bottom-left
    %     +1, -1; % Bottom-right
    % ];
    
    dLon = CornerOffsets(1,:) .* (PixelRadius ./ cos(CenterLat)); % Adjust longitude by cosine factor
    dLat = CornerOffsets(2,:) .* PixelRadius;
    CornerLons = CenterLon + dLon;
    CornerLats = CenterLat + dLat;

    Flag = CornerLons>pi;
    CornerLons(Flag) = CornerLons(Flag) - 2 * pi;
    Flag = CornerLons<(-pi);
    CornerLons(Flag) = CornerLons(Flag) + 2 * pi;

    Flag = CornerLats>(pi./2);
    CornerLats(Flag) = pi./2;
    Flag = CornerLats<(-pi./2);
    CornerLats(Flag) = -pi./2;

end
