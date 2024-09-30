function [VertexLons, VertexLats] = healpix_pixel_vertices(NSide, Index)
    % Calculate the vertices of a given HEALPix pixel using the NESTED indexing scheme.
    %
    % Input:
    %   NSide  - The resolution parameter of HEALPix
    %   Index  - The HEALPix pixel index (in NESTED scheme)
    %
    % Output:
    %   VertexLons - Longitudes (radians) of the pixel's vertices
    %   VertexLats - Latitudes (radians) of the pixel's vertices
    
    % Number of vertices for each pixel (4 for a HEALPix pixel)
    numVertices = 4;
    
    % Get the center of the pixel in longitude and latitude
    [CenterLon, CenterLat] = celestial.healpix.mex.pix2ang_nested(NSide, Index);
    
    % Convert the center to Cartesian coordinates
    [CenterX, CenterY, CenterZ] = sph2cart(CenterLon, CenterLat, 1);
    CenterVec = [CenterX, CenterY, CenterZ];
    
    % Compute an approximate angular radius for the pixel
    PixelRadius = sqrt(3) / (2 * NSide); % Angular radius in radians
    
    % Define local offsets for the four corners in the local face coordinate system
    cornerOffsets = [1, 1; -1, 1; -1, -1; 1, -1] * PixelRadius;
    
    % Initialize arrays for storing vertex coordinates
    VertexLons = zeros(1, numVertices);
    VertexLats = zeros(1, numVertices);
    
    % Compute the actual vertices by rotating around the pixel center
    for k = 1:numVertices
        % Apply the offset to compute the vertex position
        dLon = cornerOffsets(k, 1) / cos(CenterLat);
        dLat = cornerOffsets(k, 2);
        
        % Convert back to Cartesian coordinates
        [VX, VY, VZ] = sph2cart(CenterLon + dLon, CenterLat + dLat, 1);
        
        % Convert to spherical coordinates
        [V_Lon, V_Lat, ~] = cart2sph(VX, VY, VZ);
        
        % Store the vertex coordinates
        VertexLons(k) = V_Lon;
        VertexLats(k) = V_Lat;
    end
end
