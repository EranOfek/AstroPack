function Circle = getFOVcircle(RA, Dec, Args)
    % get approximate coordinates of the ULTRASAT FOV circle given its center
    %
    % Input : - RA [deg]
    %         - Dec [deg]
    % Output : - a matrix of circle coordinates [deg]
    % Author: A.M. Krassilchtchikov (2024 Nov)
    % Example: RA = 215; Dec = 60; Circle = ultrasat.tools.getFOVcircle(RA, Dec);
    arguments
        RA
        Dec      
        Args.Radius     = 7.19; % [deg] 7 + 0.19 due to the gap
        Args.NumPoints  = 300; 
        Args.Plot       = false
        Args.Color      = 'blue';
    end
    %
    RAD = 180/pi;    
    [OutRA,OutDec] = celestial.coo.celestial_circ(RA/RAD,Dec/RAD,Args.Radius/RAD,Args.NumPoints);    
    OutRA(OutRA<0) = OutRA(OutRA<0) + 2*pi;  % need to have only positive RA
    Circle(:,1) = OutRA .* RAD; Circle(:,2) = OutDec .*RAD;    
    % 
    if Args.Plot
        figure; clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(RA./RAD,Dec./RAD,'.','Color','red');
        plotm(Circle(:,2)./RAD,Circle(:,1)./RAD,'.','Color',Args.Color);
    end
end