function [Cost, Ngrid, Uncovered] = costTOO(Region, Grid, Args)
    % this is a cost function of spherical region coverage with circles:
    % the worse is the coverage in regards uncovered area and the number 
    % of circles, the higher is the cost
    arguments
        Region = [  0,  0;  ... % RA, Dec
                45,-30;         % an example region
                90,  0;  ...
                90, 40; ...
                70, 20; ...
                50, 40; ...
                45, 20; ...
                 0, 40; ...
                20, 10];            
        Grid = [ 10, 10; ...   % an example grid of pointings
                 15, 15; ...
                 20, 20; ...
                 30, 30];        
        Args.R  = 7;            % radius of the FOV circle in [deg]
        Args.alpha =  1;
        Args.beta  =  20;
        Args.plot  = false;
    end
    
    RAD = 180/pi;
    
    % the number of regions    
    Ngrid = size(Grid,1);
    
    % the cost increases with the number of grid points 
    % and with the uncovered surface of the region
    % the particular formula may vary
    Uncovered = uncoveredArea(Region, Grid, Args.R);
    Cost = Ngrid^Args.alpha * (1 + Uncovered)^Args.beta;
    
    % plot the region and the grid
    if Args.plot
        figure(2); clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        % plotm(pol(:,2)./RAD,pol(:,1)./RAD,'-','Color','red')
        fillm(Region(:,2)./RAD,Region(:,1)./RAD,'w')
        for Ip = 1:size(Grid,1)
            [Lon,Lat]=celestial.coo.celestial_circ(Grid(Ip,1)/RAD,Grid(Ip,2)/RAD,Args.R/RAD,300);
            Lon(Lon<0) = Lon(Lon<0)+2*pi;
            plotm(Lat,Lon,'.','Color','blue')
        end
    end
    
end

% auxiliary internal functions

function Result = uncoveredArea(Pol, Grid, R, Args)
    % determine the percentage of the uncovered area
    arguments
        Pol      
        Grid 
        R    
        Args.ControlGrid = '~/matlab/data/ULTRASAT/healpix0.013deg.txt' % 'healpix_grid_nside_512_npix_3145728_pixarea_0.013_deg.txt'; % healpix0.003deg.txt
    end
    
    RAD = 180/pi;
    
    % determine a fine grid covering the region:
    % take an equal area healpix grid and cut from it a subgrid that falls 
    % inside the Region
    
    Grid0  = readmatrix(Args.ControlGrid); 
    Ngr0   = size(Grid0,1);    
    Ngr    = size(Grid,1);
    
    Grid   = Grid./RAD;                    
    R      = R/RAD;                         
    
    InPol = 0;
    InCirc= 0;
       
    for Ip = 1:Ngr0
        if celestial.search.isPointInsidePolygon(Grid0(Ip,1), Grid0(Ip,2), Pol)
            InPol = InPol + 1;
            % check if the point falls within one of the circles
            Inside = 0; Ic = 1;
            while Inside == 0 && Ic <= Ngr
                if celestial.coo.sphere_dist_fast(Grid0(Ip,1)/RAD,Grid0(Ip,2)/RAD,Grid(Ic,1),Grid(Ic,2)) < R
                    Inside = 1;
                end
                Ic = Ic + 1;
            end            
            if Inside == 1
                InCirc = InCirc + 1;
            end
        end
    end
    
    % the uncovered area percent:
    Result = (InPol - InCirc) / InPol;
    
end