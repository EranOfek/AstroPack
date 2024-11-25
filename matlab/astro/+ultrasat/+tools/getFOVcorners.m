function Corners = getFOVcorners(RA, Dec, Args)
    % get approximate coordinates of the ULTRASAT FOV corners given its center
    %
    % Input : - RA [deg]
    %         - Dec [deg]
    % Output : - a matrix of corner coordinates [deg]
    arguments
        RA
        Dec      
        Args.HalfFOVRad = 7.3557; % half-size of the square FOV w/account of the gap [deg] 
        Args.Plot       = false
        Args.Color      = 'blue';
    end
    %
    RAD = 180/pi;    
    [OutRA,OutDec] = celestial.coo.center2corners(RA/RAD,Dec/RAD,Args.HalfFOVRad/RAD,Args.HalfFOVRad/RAD);
    Corners(:,1) = OutRA .* RAD; Corners(:,2) = OutDec .*RAD;
    % 
    if Args.Plot
        figure; clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(RA/RAD,Dec/RAD,'.','Color','red');
        plotm(Corners(:,2)/RAD,Corners(:,1)/RAD,'.','Color',Args.Color);
    end
end