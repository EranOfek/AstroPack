function [PixLon, PixLat] = pix2ang(Nside, Pix, Args)
    % Return the lon/lat of the centers of the healpix pixels given their index.
    % Input  : - Nside (scalar).
    %          - An array of pixel indices.
    %          * ...,key,val,...
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'CooUnits' - Output coordinates units. Default is 'rad'.
    % Output : - Array of longitudes.
    %          - Array of latitudes.
    % Compilation: mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" pix2ang_nested.cpp
    %              mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" pix2ang_ring.cpp
    % Author : Eran Ofek (2024 Sep) 
    % Example: [Lon, Lat] = celestial.healpix.pix2ang(16, 197)

    arguments
        Nside
        Pix
        Args.Type     = 'nested';
        Args.CooUnits = 'rad';              
    end

    switch Args.Type
        case 'nested'
            [PixLon, PixLat] = celestial.healpix.mex.pix2ang_nested(Nside, Pix); % Get longitude and latitude
        case 'ring'
            [PixLon, PixLat] = celestial.healpix.mex.pix2ang_ring(Nside, Pix); % Get longitude and latitude
        otherwise
            error('Unknown Type option');
    end

    Factor = convert.angular('rad',Args.CooUnits);
    PixLon    = Factor.*PixLon;
    PixLat    = Factor.*PixLat;

end
