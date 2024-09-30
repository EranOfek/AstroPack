function [Result] = coneSearch(Nside, Lon, Lat, Radius, Args)
    % cone search for healpix pixels.
    %   Return all the pixel indices that may be in the cone search.
    %   The list may contains nearby irrelevant pixels.
    % Input  : - Nside.
    %          - Longitude (scalar).
    %          - Latitude (scalar).
    %          - Search radius (scalar).
    %          * ...,key,val,... 
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'CooUnits' - Input coordinate units. Default is 'rad'.
    %            'RadiusUnits' - Input search radius units. Default is 'rad'.
    % Output : - Column vector of pixel indices.
    % Author : Eran Ofek (2024 Sep) 
    % Example: celestial.healpix.coneSearch(16,1,1,0.01)

    arguments
        Nside
        Lon
        Lat
        Radius
        Args.Type        = 'nested';
        Args.CooUnits    = 'rad';
        Args.RadiusUnits = 'rad';
    end

    Factor = convert.angular(Args.CooUnits,'rad');
    Lon    = Factor.*Lon;
    Lat    = Factor.*Lat;
    Factor = convert.angular(Args.RadiusUnits,'rad');
    Radius = Factor.*Radius;

    % Convert center longitude/latitude to Cartesian vector
    %CenterVec = sph2cart_vec(Lon0, Lat0);

    % Preallocate pixel indices array
    %MaxPixelIndices = 1000;
    %PixelIndices = zeros(MaxPixelIndices, 1);
    %Idx = 0;

    % Start the recursive search from the top-level pixels
    TopLevelPixels = 0:11; % HEALPix starts with 12 base pixels (faces)
    
    Result = [];
    for Pix = TopLevelPixels
        [Result] = recursiveConeSearch(Nside, 1, Pix, Lon, Lat, Radius, Result, Args.Type);
        %(Pix, 1, NSide, Radius, CenterVec, Idx, PixelIndices);
    end

end

function PixelIndices=recursiveConeSearch(TargetNside, CurrentNside, Pix, Lon, Lat, Radius, PixelIndices, Type)
    % Recursive cone search

  
    % Calculate the center of the current pixel in Cartesian coordinates
    [PixLon, PixLat] = celestial.healpix.pix2ang(CurrentNside, Pix, 'Type',Type,'CooUnits','rad');

    PixelRadius = pi ./ (sqrt(3) .* CurrentNside);

    % Calculate the angular distance between the pixel center and the search center
    AngularDistance = celestial.coo.sphere_dist_fast(Lon, Lat, PixLon, PixLat);
    %acos(dot(PixelVec, CenterVec));
    
    % Check if the pixel center is outside the search radius
    if AngularDistance > (Radius + PixelRadius)
        % Skip this pixel if outside the adjusted search radius
    else
        
        if CurrentNside == TargetNside
            % Add this pixel to the list since we've reached the desired resolution
            %Idx = Idx + 1;
            
            % Ensure there's enough space in PixelIndices
            %if Idx > length(PixelIndices)
            %    PixelIndices = [PixelIndices; zeros(length(PixelIndices), 1)];
            %end
            
            PixelIndices = [PixelIndices;Pix];
            
        else

            % If not at the target resolution, subdivide this pixel
            SubNside = 2 .* CurrentNside;
            SubPixels = 4 .* Pix; % Each parent pixel divides into 4 subpixels
            for SubPix = SubPixels:(SubPixels + 3)
                [PixelIndices] = recursiveConeSearch(TargetNside, SubNside, SubPix, Lon, Lat, Radius, PixelIndices, Type);
            end
        end
    end
end


