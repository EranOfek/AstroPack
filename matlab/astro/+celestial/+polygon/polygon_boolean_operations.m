function Result = polygon_boolean_operations(P0, P1, Args)
    % Check intersection and containment of spherical polygons 
    %     NB: based on rasterization to healpix 
    %     Comparison of too large polygons at high resolution would lead 
    %     to a crash in the cone search function 
    % Input  : - P0 - a polygon: Nx2 array of [RA, Dec] in degrees 
    %          - P1 - a polygon or a cell array of polygons
    %          * ...,key,val,... 
    %            'R0'         - a raster of the first polygon
    %            'Resolution' - desired accuracy = raster resolution [arcsec]
    %            'TestPlot'   - plot the results 
    % Output : - a struct containing boolean data on intersection 
    %            and mutual containment of the input polygons:
    %               Intersect: [1 1 1 0 1]
    %             P0containP1: [1 0 0 0 0]
    %             P1containP0: [1 0 0 0 1]
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: P0 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    %          P1 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    %          P2 = [11, 69.5; 11, 70.3; 9.6, 70.3; 9.6, 69.5];
    %          P3 = [9.9, 70.1; 9.9, 70.3; 9.6, 70.3; 9.6, 70.1];
    %          P4 = [8 70.55; 8 70.7; 11 70.7; 11 70.53];    
    %          P5 = [9, 69; 11, 69; 11, 71; 9, 71];
    %          Res = celestial.polygon.polygon_boolean_operations(P0, {P1,P2,P3,P4,P5})
    arguments
        P0
        P1
        Args.R0         = []; % optional healpix raster of the first polygon 
        Args.Resolution = 10; % [arcsec] 
        % NB: at <5 arcsec the scheme becomes unstable due to 
        % loss of accuracy of celestial.healpix.coneSearch for large Nside
        Args.TestPlot   = false;
    end
    % raster the first polygon:
    if isempty(Args.R0)
        R0 = celestial.healpix.rasterize_polygon(P0,'Resolution',Args.Resolution);
    else
        R0 = Args.R0;
    end
    %
    if iscell(P1)
        Np = numel(P1); % number of polygons in the cell array
    else
        Np = 1;
    end
    Result.Intersect   = zeros(1,Np);
    Result.P0containP1 = zeros(1,Np);
    Result.P1containP0 = zeros(1,Np);    
    % raster the second polygon(s) and compare the pixel lists:
    for Ip = 1:Np
        if iscell(P1)
            P = P1{Ip};
        else
            P = P1;
        end    
        R = celestial.healpix.rasterize_polygon(P,'Resolution',Args.Resolution);
        %
        if ~isempty(intersect(R0, R))
            Result.Intersect(Ip)   = 1;
            Result.P0containP1(Ip) = all(ismember(R, R0));
            Result.P1containP0(Ip) = all(ismember(R0, R));        
        end
    end
    % plot the results (test) 
    if Args.TestPlot
        figure(1); clf; hold on; axis equal; grid on
        plot_polygon(P0); 
        plot_polygon(P1{1});
        plot_polygon(P1{2});
        plot_polygon(P1{3});
        plot_polygon(P1{4});
        plot_polygon(P1{5});
    end
end

function plot_polygon(P)
        RA = P(:,1); Dec = P(:,2);
        RA(end+1) = RA(1); Dec(end+1) = Dec(1);
        plot(RA, Dec, '-o');
end
