function [RA0, Dec0, R0] = spherical_polygon_circum_circle(P, Args)
    % Find the approximate center and radius of the circum circle for a spherical polygon
    %     NB: only for relatively small polygons of <~30 deg size not including the poles
    % Input  : - a polygon: Nx2 array of [RA, Dec] in degrees
    %          * ...,key,val,... 
    % Output : - the coordinates of the center of the circum circle and its radius [deg]
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: P=[350, 70; 10, 80; 20, 80; 30, 75; 5, 60];
    %          [RA0, Dec0, R0] = celestial.polygon.spherical_polygon_circum_circle(P)
    arguments
        P    
        Args.Plot = false;
    end
    %
    RA  = deg2rad(P(:,1));
    Dec = deg2rad(P(:,2));
    
    % convert to Cartesian:
    x = cos(Dec) .* cos(RA);
    y = cos(Dec) .* sin(RA);
    z = sin(Dec);
    
    % find the mean and normalize:
    v = [x, y, z];
    v_mean = mean(v, 1);
    v_mean = v_mean / norm(v_mean);
    
    % Compute angular distances
    dots = v * v_mean';            % dot products with center vector
    dots = min(max(dots, -1), 1);  % clip to avoid acos domain error
    angles = acos(dots);           % in radians

    % Radius of the approximated spherical cap
    radius_rad = max(angles);
    
    % convert back to RA, Dec
    Dec0 = rad2deg(asin(v_mean(3)));
    RA0  = rad2deg(atan2(v_mean(2), v_mean(1)));
    if RA0 < 0
        RA0 = 360+RA0;
    end
    R0   = rad2deg(radius_rad);    
    
    % plot the polygon and the circle 
    if Args.Plot        
        figure(1)
        axesm('aitoff', 'Frame', 'on', 'Grid', 'on');
        plotm(P(:,2),P(:,1),'+'); hold on;
        [Lat,Lon]=reckon(Dec0, RA0, R0, (0:1:360));
        plotm(Lat,Lon,'k-')
    end
end

% Error estimation:
% 
%     For polygons up to 10° across:
%         εr≲0.01∘εr​≲0.01∘
%         εc≲0.005∘εc​≲0.005∘
% 
%     For polygons 30° across:
%         εr∼0.05∘εr​∼0.05∘
%         εc∼0.02∘εc​∼0.02∘
% 
%     For polygons 60° across:
%         εr∼0.1–0.2∘εr​∼0.1–0.2∘
%         εc∼0.1∘εc​∼0.1∘ or more
% 
%     For polygons >90° across, the approximation can be off by degrees.
