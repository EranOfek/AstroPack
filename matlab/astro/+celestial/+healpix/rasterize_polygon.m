function [Result, Nside] = rasterize_polygon(P, Args)
    % Rasterize a spherical polygon into HEALpix at a given Nside or resolution
    %     Optional detailed description
    % Input  : - polygon: Nx2 array of [RA, Dec] in degrees 
    %          * ...,key,val,... 
    %          'Nside'      - desired raster resolution
    %          'Resolution' - desired raster resolution [arcsec]
    %          'CheckPlot'  - boolean (plot an illustration)
    %          'UseMex'     - boolean (def. true)
    % Output : - indices of the HEALpix pixels filling the polygon
    %          - Nside of the HEALpix in the raster
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: P = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    %          [R, Nside] = celestial.healpix.rasterize_polygon(P)
    arguments        
        P
        Args.Nside      = 2^16; 
        Args.Resolution = 5;       % [arcsec]  
        Args.CheckPlot  = false;
        Args.UseMex     = true;
    end
    RAD = 180/pi;
    NsideRad = [2, 27.585653017957394; ...     % radius of healpix in deg
                4, 14.5722306700779; ...
                8,  7.47282699728271; ...
               16,  3.7823672156460226; ...
               32,  1.902601860011511; ...
               64,  0.9541480607387777; ...
              128,  0.47778497003680387; ...
              256,  0.23907012000928965; ...
              512,  0.11957945660469947; ...
             1024,  0.059800825955419704; ...
             2048,  0.02990318720521464; ...
             4096,  0.014952287136343813; ...
             8192,  0.007476316948721642; ...
            16384,  0.00373820181913693; ...
            32768,  0.0018691117457205135; ...
            65536,  0.0009345585818920722; ...
           131072,  0.00046727996820400576; ...
           262144,  0.00023364015341454294; ...
           524288,  0.00011682011904060968; ...
          1048576,  5.841007009601502e-05];
    % determine the center and the size of the polygon: 
    [RA0, Dec0, R0] = celestial.polygon.spherical_polygon_circum_circle(P);   
    % determine the Nside corresponding to the desired resolution:
    if ~isempty(Args.Nside)
        Nside = Args.Nside;
    elseif ~isempty(Args.Resolution)
        Nside  = NsideRad(find(NsideRad(:,2) < Args.Resolution/3600, 1,'first'),1);
    else
        error('Either Nside or Resolution must be defined');
    end
    % search all the HEALpix at this resolution within the given radius from the center:
    if Args.UseMex
        [Ind,PixLon,PixLat] = celestial.healpix.mex.coneSearch(Nside,RA0,Dec0,R0);
        % determine which of them are actually inside the polygon: 
        Inside = celestial.search.isPointInsidePolygon(PixLon, PixLat, P); 
    else
        [Ind,PixLon,PixLat] = celestial.healpix.coneSearch(Nside,RA0,Dec0,R0,'CooUnits','deg','RadiusUnits','deg');
         % for Nside >  65536 = 2^16 need to employ a more accurate function:
         % [Ind] = celestial.healpix.coneSearchRecur(Nside,RA0,Dec0,R0,'CooUnits','deg','RadiusUnits','deg');
        % determine which of them are actually inside the polygon:
        Inside = celestial.search.isPointInsidePolygon(PixLon*RAD, PixLat*RAD, P);
    end
    
    Result = Ind(Inside>0);
    % graphical check:
    if Args.CheckPlot
        figure(1)
        axesm('aitoff', 'Frame', 'on', 'Grid', 'on');
        plotm(PixLat(Inside>0)*RAD,PixLon(Inside>0)*RAD,'*')
        plotm(P(:,2),P(:,1),'+','Color','red'); hold on;        
    end
end
