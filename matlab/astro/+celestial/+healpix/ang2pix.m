function [Pix] = ang2pix(Nside, Lon, Lat, Args)
    % Convert lon/lat to the index of the nearest healpix pixel.
    % Input  : - Nside
    %          - Array of longitude.
    %          - Array of latitude.
    %          * ...,key,val,... 
    %            'Type' - 'nested'|'ring'. Default is 'nested'.
    %            'CooUnits - Units of input coordinates. Default is 'rad'.
    % Output : - Array of healpix pixel indices (int64)
    % Compliation: mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" ang2pix_nested.cpp
    %              mex CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" ang2pix_ring.cpp
    % Author : Eran Ofek (2024 Sep) 
    % Example: Pix=celestial.healpix.ang2pix(16,1,1)

    arguments
        Nside
        Lon
        Lat
        Args.Type      = 'nested';
        Args.CooUnits  = 'rad';
    end

    Factor = convert.angular(Args.CooUnits,'rad');
    Lon    = Factor.*Lon;
    Lat    = Factor.*Lat;

    switch Args.Type
        case 'nested'
            [Pix] = celestial.healpix.mex.ang2pix_nested(Nside, Lon, Lat);
        case 'ring'
            [Pix] = celestial.healpix.mex.ang2pix_ring(Nside, Lon, Lat);
        otherwise
            error('Unknown Type option');
    end

end
