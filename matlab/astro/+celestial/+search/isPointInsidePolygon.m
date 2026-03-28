% function Result = isPointInsidePolygon(lon0, lat0, polygon)
%     % determine if a point on a sphere is inside a given polygon
%     % NB: will not work if the region contains both poles!
%     % Input: - array of longitudes in deg
%     %        - array of latitudes in deg
%     %        - a polygon as a 2-column matrix of [lon, lat] in deg
%     % Output: - a vector of logical
%     % Author: A.M. Krassilshchikov (Dec 2023)
%     % Example: Pol = [0, 0; 45, -30; 90, 0; 90, 40; 45, 20; 0, 40];
%     %          Pt  = [80, -1];
%     %          Result = celestial.search.isPointInsidePolygon(Pt(1), Pt(2), Pol);    
%     lon0 = deg2rad(lon0); 
%     lat0 = deg2rad(lat0);
%     pol  = deg2rad(polygon);
%     len  = length(polygon);
%     NPoint = numel(lon0);
%     Result = false(1,NPoint);
%     % convert the longitudes to [0, 360]:
%     lon0 = lon0 + ((lon0 < 0)-(lon0 > 2*pi)) * 2*pi;
%     pol(:,1) = pol(:,1) + ((pol(:,1) < 0) - (pol(:,1) > 2*pi))* 2*pi; 
%     % Count intersections of 2 rays from the test point to the 2 poles 
%     % with all the edges of the polygon
%     for iPoint = 1:NPoint
%         nN = 0; nS = 0;
%         for i = 1:len
%             lon1 = pol(i, 1);
%             lat1 = pol(i, 2);
%             lon2 = pol(mod(i, len) + 1, 1);
%             lat2 = pol(mod(i, len) + 1, 2);
%             % Check if the ray to the North pole intersects the edge
%             if xor(lat1 > lat0(iPoint), lat2 > lat0(iPoint)) && (lon0(iPoint) < (lon2 - lon1) * (lat0(iPoint) - lat1) / (lat2 - lat1) + lon1)
%                 nN = nN + 1;
%             end
%             % Check if the ray to the South pole intersects the edge
%             if xor(lat1 < lat0(iPoint), lat2 < lat0(iPoint)) && (lon0(iPoint) < (lon2 - lon1) * (lat0(iPoint) - lat1) / (lat2 - lat1) + lon1)
%                 nS = nS + 1;
%             end
%         end
%         % even number of intersections to both poles means the point is outside the polygon
%         Result(iPoint) = mod(nN, 2) == 1 || mod(nS, 2) == 1;
%     end    
% end

function Result = isPointInsidePolygon(lon0, lat0, polygon)
      % Vectorized point-in-spherical-polygon test.
      % NB: will not work if the region contains both poles!                                                                                    
      % Input: - array of longitudes in deg
      %        - array of latitudes in deg                                                                                                      
      %        - a polygon as a 2-column matrix of [lon, lat] in deg
      % Output: - a vector of logical                               
      % Author: A.M. Krassilshchikov (Dec 2023) + Claude (Mar 2026)
      % Example: Pol = [0, 0; 45, -30; 90, 0; 90, 40; 45, 20; 0, 40];
      %          Pt  = [80, -1];
      %          Result = celestial.search.isPointInsidePolygon(Pt(1), Pt(2), Pol);
      %
      lon0 = deg2rad(lon0(:));   % Npt x 1                                                                                                      
      lat0 = deg2rad(lat0(:));   % Npt x 1
      pol  = deg2rad(polygon);                                                                                                                  
      NEdge = size(pol, 1);                                                                                                                      
 
      % Wrap longitudes to [0, 2*pi]                                                                                                            
      lon0 = mod(lon0, 2*pi);
      pol(:,1) = mod(pol(:,1), 2*pi);                                                                                                            
                 
      % Edge endpoint arrays: 1 x NEdge (row vectors for implicit expansion)                                                                    
      idx2 = [2:NEdge, 1];
      lon1 = pol(:,1).';          % 1 x NEdge                                                                                                    
      lat1 = pol(:,2).';          % 1 x NEdge                                                                                                    
      lon2 = pol(idx2,1).';       % 1 x NEdge                                                                                                    
      lat2 = pol(idx2,2).';       % 1 x NEdge                                                                                                    
                                                                                                                                                 
      % Intersection longitude at each point's latitude with each edge                                                                          
      % Npt x NEdge via implicit expansion                                                                                                      
      dLat = lat2 - lat1;                           % 1 x NEdge                                                                                  
      lonCross = (lon2 - lon1) .* (lat0 - lat1) ./ dLat + lon1;  % Npt x NEdge                                                                  
                                                                                                                                                 
      rayRight = lon0 < lonCross;                   % Npt x NEdge                                                                                
                                                                                                                                                 
      % North pole ray: edge straddles point latitude from below to above                                                                        
      crossN = xor(lat1 > lat0, lat2 > lat0);       % Npt x NEdge
      nN = sum(crossN & rayRight, 2);               % Npt x 1                                                                                    
                                                                                                                                                 
      % South pole ray: edge straddles point latitude from above to below                                                                        
      crossS = xor(lat1 < lat0, lat2 < lat0);       % Npt x NEdge                                                                                
      nS = sum(crossS & rayRight, 2);               % Npt x 1
                                                                                                                                                 
      Result = (mod(nN, 2) == 1 | mod(nS, 2) == 1).';                                                                                            
  end                                                                     