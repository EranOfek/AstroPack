function Result = isPointInsidePolygon(lon0, lat0, polygon)
    % determine if a point on a sphere is inside a given polygon
    % NB: will not work if the region contains both poles!
    % Input: - longitude in deg
    %        - latitute in deg
    %        - a polygon as a 2-column matrix of [lon, lat] in deg
    % Output: - logical: true or false
    % Author: A.M. Krassilshchikov (Dec 2023)
    % Example: Pol = [0, 0; 45, -30; 90, 0; 90, 40; 45, 20; 0, 40];
    %          Pt  = [80, -1];
    %          Result = isPointInsidePolygon(Pt(1), Pt(2), Pol);
    lon0 = deg2rad(lon0); 
    lat0 = deg2rad(lat0);
    pol  = deg2rad(polygon);
    len  = length(polygon);
    % Count intersections of 2 rays from the test point to the 2 poles 
    % with all the edges of the polygon
    nN = 0; nS = 0;
    for i = 1:len
        lon1 = pol(i, 1);
        lat1 = pol(i, 2);
        lon2 = pol(mod(i, len) + 1, 1);
        lat2 = pol(mod(i, len) + 1, 2);
        % Check if the ray to the North pole intersects the edge
        if xor(lat1 > lat0, lat2 > lat0) && (lon0 < (lon2 - lon1) * (lat0 - lat1) / (lat2 - lat1) + lon1)
            nN = nN + 1;
        end
        % Check if the ray to the South pole intersects the edge
        if xor(lat1 < lat0, lat2 < lat0) && (lon0 < (lon2 - lon1) * (lat0 - lat1) / (lat2 - lat1) + lon1)
            nS = nS + 1;
        end
    end
    % even number of intersections to both poles means the point is outside the polygon
    Result = mod(nN, 2) == 1 || mod(nS, 2) == 1;
end
