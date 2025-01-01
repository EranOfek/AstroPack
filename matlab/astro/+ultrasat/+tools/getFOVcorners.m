function Corners = getFOVcorners(RA, Dec, Args)
    % get coordinates of the ULTRASAT FOV corners given its center and roll angle
    %
    % Input : - RA [deg]
    %         - Dec [deg]
    %       * ...,key,val,... 
    %       'Roll' - roll angle [deg]
    %       'HalfFOVRad' - half FOV radius (takes the gap into account) [deg]
    % Output : - a matrix of corner coordinates [deg]
    % Author: A.M. Krassilchtchikov (2024 Nov)
    % Example: RA = 215; Dec = 60; Corners = ultrasat.tools.getFOVcorners(RA, Dec, 'Roll', 30)
    arguments
        RA
        Dec      
        Args.Roll       = 0;      % roll angle [deg]      
        Args.HalfFOVRad = 7.3557; % half-size of the square FOV w/account of the gap [deg] 
        Args.Plot       = false
        Args.Color      = 'blue';
    end
    %
    RAD = 180/pi;    
%     [OutRA,OutDec] = celestial.coo.center2corners(RA/RAD,Dec/RAD,Args.HalfFOVRad/RAD,Args.HalfFOVRad/RAD);            
    [OutRA, OutDec] = sky_square_corners(RA/RAD, Dec/RAD, 2.*Args.HalfFOVRad/RAD, Args.Roll/RAD);
    OutRA(OutRA<0)  = OutRA(OutRA<0) + 2*pi;    % need to have only positive RA
    Corners(:,1)    = OutRA .* RAD; Corners(:,2) = OutDec .*RAD;   
    % 
    if Args.Plot
        figure; clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(RA/RAD,Dec/RAD,'.','Color','red');
        plotm(Corners(:,2)/RAD,Corners(:,1)/RAD,'.','Color',Args.Color);
    end
end
%
function [RA_C, Dec_C] = sky_square_corners(RA, Dec, Side, Rot)
    % Calculate the coordinates of the four corners of a sky square
    % centered at RA, Dec with the side size Side and clockwise rotation angle Rot 
    % (NB: all the input and output values are in RAD!)
    
    % half-diagonal angular distance and azimuths
    HalfDiagonal = sqrt(2) * Side / 2;
    Az = Rot + [pi/4, 3*pi/4, 5*pi/4, 7*pi/4];
    % project the HalfDiagonal size at these azimuths 
    [Dec_C, RA_C] = arrayfun(@(az) reckon(Dec, RA, HalfDiagonal, az, 'radians'), Az);    
end
