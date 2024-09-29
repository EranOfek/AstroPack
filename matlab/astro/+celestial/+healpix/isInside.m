function IsInside = isInside(NSide, Index, Lon, Lat, Args)
    % Check if a given point (Lon, Lat) is inside a specified HEALPix pixel
    % Input  : - NSide (HEALPix resolution parameter)
    %          - Healpix pixel index.
    %          - Array of Longitude (default radians).
    %          - Array of Latitudes (default radians).
    %          * ...,key,val,...
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'CooUnits' - Input coordinates units. Default is 'rad'.
    % Output: - An array of logical flags indicating if the coordinates are
    %           inside the specific healpix pixel.
    % Author : Eran Ofek (Sep 2024)
    % Example: celestial.healpix.isInside(16,197,1,1,'nested')
    
    arguments
        NSide
        Index
        Lon
        Lat
        Args.Type     = 'nested';
        Args.CooUnits = 'rad';
    end

    Factor = convert.angular(Args.CooUnits,'rad');
    Lon    = Lon.*Factor;
    Lat    = Lat.*Factor;

    SizeLon = size(Lon);
    Lon = Lon(:);
    Lat = Lat(:);
    N = numel(Lon);

    % Obtain the corner coordinates of the HEALPix pixel
    [CornerLons, CornerLats] = celestial.healpix.pixelCorners(NSide, Index, Args.Type);
    
    % Convert (Lon, Lat) and corner coordinates to Cartesian vectors
    [X, Y, Z] = sph2cart(Lon, Lat, 1); % Using radius r = 1
    PointVec = [X, Y, Z].';
    
    CornerVecs = zeros(3, 4);
    for i = 1:4
        [Xc, Yc, Zc] = sph2cart(CornerLons(i), CornerLats(i), 1);
        CornerVecs(:, i) = [Xc; Yc; Zc];
    end
    
    % Check if point is inside pixel using a vector cross product method

    Flag = false(4,N);
    for I = 1:4
        % Compute the normal vector for the plane formed by two adjacent corners and the center
        NextIdx = mod(I, 4) + 1;
        NormalVec = cross(CornerVecs(:, I), CornerVecs(:, NextIdx));
        
        Flag(I,:) = sum(NormalVec.*PointVec,1)>0;
    end
    IsInside = all(Flag,1);

    IsInside = reshape(IsInside,SizeLon);

end
