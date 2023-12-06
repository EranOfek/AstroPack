function [Lon1, Lat1, Az] = points2gc(Lon1, Lat1, Lon2, Lat2, Units)
    % Convert two points on a sphere to great circle representation (Lon, Lat, Az)
    % Input  : - Array of longitudes of first point.
    %          - Array of latitudes of first point.
    %          - Array of longitudes of second point.
    %          - Array of latitudes of second point.
    %          - Units for input and output: ['rad'] | 'deg'.
    % Output : - Array of longitudes of first point.
    %          - Array of latitudes of first point.
    %          - Array of position angles (azimuth) between the two points.
    %            Azimuth is the angle a line makes with a meridian, measured clockwise from north.
    %            Thus the azimuth of due north is 0°,
    %            due east is 90°, due south is 180°, and due west is 270°.
    % Author : Eran Ofek (Apr 2022)
    % Example: [Lon1, Lat1, Az] = celestial.coo.points2gc(1, 1, 1.1, 1.1, 'deg')
    
    arguments
        Lon1
        Lat1
        Lon2
        Lat2
        Units  = 'rad';
    end
    
    switch lower(Units)
        case 'rad'
            % do nothing
            IsRad = true;
        otherwise
            IsRad = false;
            ConvFactor = convert.angular(Units,'rad');
            Lon1       = Lon1.*ConvFactor;
            Lat1       = Lat1.*ConvFactor;
            Lon2       = Lon2.*ConvFactor;
            Lat2       = Lat2.*ConvFactor;
    end
    
    [~,~,Az] = celestial.coo.sphere_dist_fast(Lon2, Lat2, Lon1, Lat1);
    
    if ~IsRad
        % return to input units
        ConvFactor = convert.angular('rad', Units);
        Lon1       = Lon1.*ConvFactor;
        Lat1       = Lat1.*ConvFactor;
        Az         = Az.*ConvFactor;
    end
    
end
