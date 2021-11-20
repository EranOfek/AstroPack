function Dist = sphere_dist_fastSmall(RA1, Dec1, RA2, Dec2)
    % Spherical distance approximation for small angular distances
    %       For 50'' distances this approximation is good to about 1 mas.
    %       This is about ~x2 faster than celestial.coo.sphere_dist_fast
    % Input  : - Longitude 1 [radians]
    %          - Latitude 1 [radians]
    %          - Longitude 2 [radians]
    %          - Latitude 2 [radians]
    % Output : - Angular disatance small angle approximation [radians]
    % Author : Eran Ofek (Nov 2021)
    % Example: D2=celestial.coo.sphere_dist_fastSmall(R1,R1,R2,R2)
    
    Dist = sqrt(((RA1 - RA2).*cos(Dec1)).^2 + (Dec1-Dec2).^2);
    
end

