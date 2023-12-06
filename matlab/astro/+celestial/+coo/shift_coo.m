function [Lon, Lat] = shift_coo(Lon, Lat, DLon, DLat, Units)
    % Shift spherical coordinates by lon/lat.
    % Input  : - Longitude.
    %          - Latitude.
    %          - Shift in longitude on small circle, but angular units.
    %          - Shift in latitude on great circle.
    %          - Units. Default is 'rad'.
    % Output : - Shifted Longitude.
    %          - Shifted Latitude.
    % Author : Eran Ofek (Mar 2022)
    % Example: [Lon, Lat] = celestial.coo.shift_coo([0 0 359], [0 89 0], [1 0 3], [1 2 0], 'deg')
    
    arguments
        Lon
        Lat
        DLon
        DLat
        Units      = 'rad';
    end

    % convert to radians
    ConvFactor = convert.angular(Units, 'rad');
    Lon        = Lon.*ConvFactor;
    Lat        = Lat.*ConvFactor;
    DLon       = DLon.*ConvFactor;
    DLat       = DLat.*ConvFactor;

    DLon = DLon./cos(Lat);

    Lon = Lon + DLon;
    Lat = Lat + DLat;

    % make sure in range
    Lon = mod(Lon, 2.*pi);
    FlagL = Lat>(pi./2);
    Lat(FlagL) = pi - Lat(FlagL);
    FlagL = Lat<(-pi./2);
    Lat(FlagL) = -pi + abs(Lat(FlagL));
                
    % convert to units
    ConvFactor = convert.angular('rad', Units);
    Lon        = Lon.*ConvFactor;
    Lat        = Lat.*ConvFactor;
    
end