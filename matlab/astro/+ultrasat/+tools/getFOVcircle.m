function Circle = getFOVcircle(RA, Dec, Args)
    % get approximate coordinates of the ULTRASAT FOV circle given its center
    %
    % Input : - RA [deg]
    %         - Dec [deg]
    % Output : - a matrix of circle coordinates [deg]
    arguments
        RA
        Dec      
        Args.Radius     = 7; % [deg] 
        Args.NumPoints  = 300; 
        Args.Plot       = false
        Args.Color      = 'blue';
    end
    %
    RAD = 180/pi;    
    [OutRA,OutDec] = celestial.coo.celestial_circ(RA/RAD,Dec/RAD,Args.Radius/RAD,Args.NumPoints);    
    Circle(:,1) = OutRA .* RAD; Circle(:,2) = OutDec .*RAD;
    % 
    if Args.Plot
        figure; clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(RA/RAD,Dec/RAD,'.','Color','red');
        plotm(Circle(:,2)/RAD,Circle(:,1)/RAD,'.','Color',Args.Color);
    end
end