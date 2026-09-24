function [PA, Theta] = position_angle(Lon1, Lat1, Lon2, Lat2, Units, InTPI)
    % Calculate the position angle between two positions on the sky.
    % Input  : - Array of Longitude 1.
    %          - Array of Latitude 1.
    %          - Array of Longitude 2.
    %          - Array of Latitude 2.
    %          - Units: 'deg'|'rad'. If [], use 'rad'. Default is [].
    %          - Logical indicating if to return the output in range 0..2pi
    %            Default is true.
    % Output : - Position Angle of position 2 in respect to 1, measured
    %            from the north eastward.
    %          - Angle of 2 in respect to 1, measured from the east
    %            northward.
    % Author : Eran Ofek (2026 Mar) 
    % Example: [PA,Theta]=celestial.coo.position_angle(1,1,1.1,1.01)

    arguments
        Lon1
        Lat1
        Lon2
        Lat2
        Units   = [];
        InTPI   = true;
    end

    if ~isempty(Units)
        % units are provided
        switch lower(Units)
            case 'rad'
                Conv = 1;
            case 'deg'
                Conv = pi./RAD;
            otherwise
                error('Unknown Units option');
        end

        Lon1 = Lon1.*Conv;
        Lat1 = Lat1.*Conv;
        Lon2 = Lon2.*Conv;
        Lat2 = Lat2.*Conv;

    end

    DeltaLon = Lon2 - Lon1;

    PA = atan2(sin(DeltaLon), cos(Lat1).*tan(Lat2) - sin(Lat1).*cos(DeltaLon));

    Theta = pi./2 - PA;

    if InTPI
        PA = mod(PA, 2.*pi);
        Theta = mod(Theta, 2.*pi);
    end


end
