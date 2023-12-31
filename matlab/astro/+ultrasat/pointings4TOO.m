function Grid = pointings4TOO(Pol, Args)

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
        Args.InitialGridFile = '/home/sasha/ULTRASAT/SkyGrid/charged_particles_350_rep1.txt'
        Args.FOVradius       = 7; % deg NB: the radius should, in principle, match the grid 
    end

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

%     % Test point and test figure:
%     p = [10, 10]; % RA, Dec
% 
%     figure(2); clf
%     axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
%     plotm(Pol(:,2)./RAD,Pol(:,1)./RAD,'-','Color','blue')
%     plotm(p(2)./RAD, p(1)./RAD,'o','Color','red')
% 
%     % Check if the point is inside the polygon
%     isPointInsidePolygon(p(1), p(2), Pol)

    Grid0 = readmatrix(Args.InitialGridFile);
    Np   = length(Grid0);
    Grid = zeros(Np,2);

    figure(1); clf
    axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
    % plotm(pol(:,2)./RAD,pol(:,1)./RAD,'-','Color','red')
    fillm(Pol(:,2)./RAD,Pol(:,1)./RAD,'w')

    for Ip = 1:Np
        if celestial.search.isPointInsidePolygon(Grid0(Ip,1), Grid0(Ip,2), Pol)
            Grid(Ip,:) = Grid0(Ip,:);
            [Lon,Lat]=celestial.coo.celestial_circ(Grid0(Ip,1)/RAD,Grid0(Ip,2)/RAD,Args.FOVradius/RAD,300);
            Lon(Lon<0) = Lon(Lon<0)+2*pi;
            plotm(Lat,Lon,'.','Color','blue')
            fprintf('%d %.2f %.2f\n',Ip, Grid0(Ip,1), Grid0(Ip,2))
        end
    end
    
    Grid = Grid( abs(Grid(:,1))>0 | abs(Grid(:,2))>0, :);

end