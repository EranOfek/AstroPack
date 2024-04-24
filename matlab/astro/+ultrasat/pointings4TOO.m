function [Grid, GridOpt] = pointings4TOO(Pol, Args)
    % construct ULTRASAT pointings to fill a given sky polygon
    % NB: this is a first version taking the pointings from the best
    % all-sky grid and selecting those whose center fall into the polygon
    % -- We need to add to the list of pointings also the order of
    % observations (start from the center and go in a spiral?)
    % -- Do we wish a pointing centered on the geometrical center of the
    % region or any coverage is OK? If the region contains zones of
    % different probability, how do we add this as "weight" for the
    % coverage? Just split into probablity bins and cover separately? 
    % Input: - a sky polygon as a a vector of [RA, Dec]
    %          * ...,key,val,...
    %          'InitialGridFile' - an  all-sky grid to build the initial set of pointings
    %          'FOVradius' - the radius of the telescope FOV
    %          'RepairMC'  - whether to try to improve the intial solution 
    % Output: - a list of pointing centers as [RA, Dec]
    %        -  an improved list of pointing centers (if a repair algorithm is invoked)
    % Author: A.M. Krassilchtchikov (Jan 2024)
    % Example: Grid = pointings4TOO(Polygon, 'RepairMC', true);
    arguments            
        Pol = [  0,  0;  ... % RA, Dec
            45,-30;
            90,  0;  ...
            90, 40; ...
            70, 20; ...
            50, 40; ...
            45, 20; ...
            0, 40; ...
            20, 10];       
        Args.AlertMap        = 'alert1.csv'
        Args.InitialGridFile = '~/matlab/data/ULTRASAT/all_sky_grid_charged_particles_350_rep1.txt'
        Args.FOVradius       = 7; % deg NB: the radius should, in principle, match the grid 
        Args.RepairMC        = false;
        
        Args.ProbLevel       = 0.9; % 0.6 % probability level
    end
    
    NsideRad = [2, 27.585653017957394; ... % radius of healpix in deg
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
    
    RAD = 180/pi;

%     Pol = [0,-49;  ... % RA, Dec
%         105,-49;
%         113,-63;  ...
%         134,-78; ...
%         172,-85; ...
%         0,-85];
%     
%     Pol = [359,-49;  ... % RA, Dec
%         276,-49;
%         289,-60;  ...
%         293,-79; ...
%         357,-85];
%     
%     Pol = [88,51;  ... % RA, Dec
%         276,51;
%         303,64;  ...
%         358,79; ...
%         359,90; ...
%         0,90; ...
%         0,82; ...
%         75,79; ...
%         98,64; ...
%         88,59];

%     % Test point and figure:
%     p = [10, 10]; % RA, Dec
% 
%     figure(2); clf
%     axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
%     plotm(Pol(:,2)./RAD,Pol(:,1)./RAD,'-','Color','blue')
%     plotm(p(2)./RAD, p(1)./RAD,'o','Color','red')
% 
%     % Check if the point is inside the polygon
%     isPointInsidePolygon(p(1), p(2), Pol)

    % the input is a set of healpix of various resolution (nside) and
    % probability
    % 0. select a subset above a given threshold
    % 1. from the subset make a homogeneous grid of healpix of the maximal nside
    % 2. from the AlSS grid select all the pointings so that the distance to
    % any of the healpix is < R = 7 deg
    % 3. make a polygon from the healpix subset (?) and estimate the uncovered
    % fraction
    % 4. draw the polygon and the pointings as circles of R 
    
    % NB: if the healpix size is small (e.g., Nside > 512, Rpix <= 0.1 deg),
    % there is not need to consider it, we just need to calculate the
    % distance of the pointings' centers from the healpix above certain
    % radius 
    
    Grid0 = readmatrix(Args.InitialGridFile);
    Np   = length(Grid0);
    Grid = zeros(Np,2);

    figure(1); clf
    axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
    
    % read from a file: nside, ipix, RA, Dec     
    R = readtable(Args.AlertMap);     
    
    Ind0 = find(R.PROBDENSITY>100*Args.ProbLevel);  % R.PROBDENSITY is in percent!  
    Uniq = R.UNIQ(Ind0,:);
    RA   = R.RA(Ind0,:);
    Dec  = R.DEC(Ind0,:);
%     
%     % this is a coordinate check
%     % now it seems that the coordinates in the input csv files are OK
%             
%     Nside = 2.^floor(log(Uniq/4)/(2*log(2)));
%     Ipix  = Uniq - 4 * Nside.^2;
%     IpixFile = 'Ipix.txt';
%     writematrix(Ipix,IpixFile);
%         
% %     python3 ~/matlab/AstroPack/matlab/astro/+celestial/+grid/healpix2coo.py 1024 Ipix.txt 1 1
% %     Script ='~/matlab/AstroPack/matlab/astro/+celestial/+grid/healpix2coo.py';
% %     system(['python3' ' ' Script ' ' Nside(1) ' ' IpixFile ' ' 1 ' ' 1]);
% 
%     Coo = readmatrix('healpix_nside_1024_coo.txt');
%     RA = Coo(:,1); Dec = Coo(:,2);
% %
% %     writematrix([RA Dec],'coords.txt','Delimiter'," ")
% %     celestial.grid.get_healpix_numbers(Nside(1),'coords.txt',1,'Nested',1);
% %     Ind  = log(Nside)/log(2)-1; 
% %     
    Ind  = floor(log(Uniq/4)/(2*log(2)))-1;    
    PixRad = NsideRad(Ind(:,1),2)/RAD;
    
%     plot.skyCircles(RA,Dec,'Rad',PixRad,'PlotOnMap',true,'Color','black','NumPoints',10);
    plotm(Dec./RAD,RA./RAD,'*') % plot the healpix pixels 
        
    % find all the 7-deg grid pixels intersecting with the alert pixels 
    for Ip = 1:Np
        Rd = celestial.coo.sphere_dist_fast(Grid0(Ip,1)/RAD,Grid0(Ip,2)/RAD,RA./RAD,Dec./RAD);
        if any(Rd < PixRad + Args.FOVradius/RAD) 
            Grid(Ip,:) = Grid0(Ip,:);
            plot.skyCircles(Grid0(Ip,1),Grid0(Ip,2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','blue');
            fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
        end
    end

    if false % turned off 
        % another option for the sky region (Dec, RA, SemiMaj, Exc):
        [Lat,Lon] = ellipse1(20., 40., [25 0.95], 60.,[],[],'degrees',100); 
        Pol = [Lon Lat];

        % plotm(pol(:,2)./RAD,pol(:,1)./RAD,'-','Color','red')
        fillm(Pol(:,2)./RAD,Pol(:,1)./RAD,'w')

        for Ip = 1:Np
            if celestial.search.isPointInsidePolygon(Grid0(Ip,1), Grid0(Ip,2), Pol)
                Grid(Ip,:) = Grid0(Ip,:);
                plot.skyCircles(Grid0(Ip,1),Grid0(Ip,2),'Rad',Args.FOVradius,'PlotOnMap',true,'Color','blue');
    %             plot.skyEllipses(Grid0(Ip,1),Grid0(Ip,2),Args.FOVradius,0.,'PlotOnMap',true,'Color','blue');
                fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
            end
        end
        
        [Cost0,~,Uncov] = ultrasat.costTOO(Pol,Grid);
        GridOpt = Grid;
        fprintf('Uncovered fraction: %.2f\n',Uncov);        
    end
    
    Grid = Grid( abs(Grid(:,1))>0 | abs(Grid(:,2))>0, :);
    Ng   = size(Grid,1);
           
    if Args.RepairMC        
        
        fprintf('Initial Cost: %.1f, Uncovered fraction: %.3f\n', Cost0, Uncov);
        MinScale = 1; MaxScale = 3; % deg
        MaxIter  = 100; % maximal number of random iterations %
        % for a complex region with 300 iterations we get only ~ 8%
        % improvement of the uncovered fraction   
        CostMin = Cost0;
        for IRand = 1:MaxIter
            RandScale = MinScale + rand(Ng,2) .* (MaxScale-MinScale);
            GridRand = Grid + RandScale .* (0.5-rand(Ng,2));
            [CostRand,~,Uncov] = ultrasat.costTOO(Pol,GridRand);
            fprintf('Cost: %.1f, Uncovered fraction: %.3f\n', CostRand, Uncov);
            if CostRand < CostMin
                GridOpt = GridRand;
                CostMin = CostRand;
                %             fprintf('Cost: %d', CostMin);
            end
        end        
        [CostOpt,~,Uncov] = costTOO(Pol,GridOpt,'plot',1);
        fprintf('Best Cost: %.1f, Uncovered fraction: %.3f\n', CostOpt, Uncov);        
    end
    
    % some more optimization facilities that did not appear efficient:
    
%     options = optimoptions(@fminunc, 'Algorithm', 'quasi-newton', 'Display', 'iter');
%     optimizedCircleCenters = fminunc(@costTOO, Grid, options);
    
%     options = optimset('Display', 'iter','MaxIter',20,'TolFun',0.2,'TolX',0.1);
%     Grid1 = fminsearch(@costTOO, Grid, options);
%     
%     Cost1 = costTOO(Pol,Grid1,'plot',true);

%     options = struct('GradObj','on','Display','iter','LargeScale','off','HessUpdate','bfgs',...
%         'InitialHessType','identity','GoalsExactAchieve',0,'TolX',1e-2);
%     options = struct('GradObj','off','Display','iter','LargeScale','off','HessUpdate','bfgs',...
%         'InitialHessType','identity','GoalsExactAchieve',0,'TolX',1e-2);
%     [Grid2,~] = fminlbfgs(@costTOO,Grid,options);
%     
%     Cost2 = costTOO(Pol,Grid2,'plot',true);
    
end