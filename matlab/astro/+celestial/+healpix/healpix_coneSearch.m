function PixelIndices = healpix_cone_search_adjusted(NSide, Lon0, Lat0, Radius)
    % healpix_cone_search_adjusted performs a cone search efficiently with radius
    % adjustments for each resolution level using an angular comparison.
    %
    % Input:
    %   NSide   - The HEALPix resolution parameter
    %   Lon0    - Longitude of the search center (radians)
    %   Lat0    - Latitude of the search center (radians)
    %   Radius  - Search radius (radians)
    %
    % Output:
    %   PixelIndices - Array of HEALPix pixel indices within the search radius
    
    % Convert center longitude/latitude to Cartesian vector
    CenterVec = sph2cart_vec(Lon0, Lat0);

    % Preallocate pixel indices array
    MaxPixelIndices = 1000;
    PixelIndices = zeros(MaxPixelIndices, 1);
    Idx = 0;

    % Start the recursive search from the top-level pixels
    TopLevelPixels = 0:11; % HEALPix starts with 12 base pixels (faces)
    
    for Pix = TopLevelPixels
        [Idx, PixelIndices] = recursive_cone_search_adjusted(Pix, 1, NSide, Radius, CenterVec, Idx, PixelIndices);
    end
    
    % Trim the array to actual size
    PixelIndices = PixelIndices(1:Idx);
end

function [Idx, PixelIndices] = recursive_cone_search_adjusted(Pix, CurrentNSide, TargetNSide, Radius, CenterVec, Idx, PixelIndices)
    % Recursive cone search with adjusted radius per level
    
    % Calculate the center of the current pixel in Cartesian coordinates
    [Long, Lat] = celestial.healpix.mex.pix2ang_ring(CurrentNSide, Pix); % Get longitude and latitude
    PixelVec = sph2cart_vec(Long, Lat); % Convert to Cartesian
    
    % Adjust the effective radius based on current Nside
    EffectiveRadius = (2 * sqrt(3)) / CurrentNSide; % More accurate effective radius
    
    % Calculate the angular distance between the pixel center and the search center
    AngularDistance = acos(dot(PixelVec, CenterVec));
    
    % Check if the pixel center is outside the search radius
    if AngularDistance > (Radius + EffectiveRadius)
        return; % Skip this pixel if outside the adjusted search radius
    end
    
    if CurrentNSide == TargetNSide
        % Add this pixel to the list since we've reached the desired resolution
        Idx = Idx + 1;
        
        % Ensure there's enough space in PixelIndices
        if Idx > length(PixelIndices)
            PixelIndices = [PixelIndices; zeros(length(PixelIndices), 1)];
        end
        
        PixelIndices(Idx) = Pix;
        return;
    end

    % If not at the target resolution, subdivide this pixel
    SubNSide = 2 * CurrentNSide;
    SubPixels = 4 * Pix; % Each parent pixel divides into 4 subpixels
    for SubPix = SubPixels:(SubPixels + 3)
        [Idx, PixelIndices] = recursive_cone_search_adjusted(SubPix, SubNSide, TargetNSide, Radius, CenterVec, Idx, PixelIndices);
    end
end

function Vec = sph2cart_vec(Lon, Lat)
    % Converts spherical coordinates to Cartesian coordinates
    % Lon = longitude (radians), Lat = latitude (radians)
    X = cos(Lat) * cos(Lon);
    Y = cos(Lat) * sin(Lon);
    Z = sin(Lat);
    Vec = [X, Y, Z];
end
