function [FOV, Tiles] = getFOVcorners(RA, Dec, Args)
    % get coordinates of the ULTRASAT FOV corners given its center and roll angle
    %
    % Input : - RA [deg]
    %         - Dec [deg]
    %       * ...,key,val,... 
    %       'Roll'        - roll angle [deg]
    %       'HalfFOVSize' - angular size of half FOV (takes the gap into account) [deg]
    %       'TileSide'    - angular size of the tile side [deg]
    % Output : - a struct of FOV corner coordinates [deg]
    %          - (optional) a struct array of Tile corner coordinates [deg]
    % Author: A.M. Krassilchtchikov (2024 Nov)
    % Example: RA = 215; Dec = 60; Roll = 0; [FOV, Tiles] = ultrasat.tools.getFOVcorners(RA, Dec, 'Roll', Roll, 'Tiles',true)
    arguments
        RA
        Dec      
        Args.Roll       = 0;      % roll angle [deg]     
        Args.Tiles      = false;  % calculate the tile corners 
        Args.HalfFOV    = 7.3557; % half-size of the square FOV w/account of the gap [deg] 
        Args.TileSide   = 7.1637; % tile side [deg]         
        Args.Plot       = false; 
        Args.Color      = 'blue'; 
    end
    %
    RAD = 180/pi;    
    HalfGap = Args.HalfFOV-Args.TileSide;
    [FOV.RA, FOV.Dec] = sky_square_corners(RA, Dec, 2.*Args.HalfFOV, Args.Roll);    
    %
    if Args.Tiles                
        for Itile = 1:4
            [Arclen, Az] = distance(Dec,RA,FOV.Dec(Itile),FOV.RA(Itile));  % from the FOV center to the new corner
            Dist2TileCenter = Arclen/2 + sqrt(2) * HalfGap / 2;            % from the FOV center to the center of the tile
            [Center.Dec, Center.RA] = reckon(Dec, RA, Dist2TileCenter, Az,'degrees'); % RA, Dec of the tile center            
            Center.RA(Center.RA<0)  = Center.RA(Center.RA<0) + 360;
            [Tiles(Itile).RA, Tiles(Itile).Dec] = sky_square_corners(Center.RA, Center.Dec, Args.TileSide, Args.Roll);
        end
    else
        Tiles = [];
    end
    % 
    if Args.Plot
        figure; clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(Dec/RAD,RA/RAD,'.','Color','red');
        plotm(FOV.Dec/RAD,FOV.RA/RAD,'.','Color',Args.Color);
    end
end
%
function [RA_C, Dec_C] = sky_square_corners(RA, Dec, Side, Rot)
    % Calculate the coordinates of the four corners of a sky square
    % centered at RA, Dec with the side size Side and clockwise rotation angle Rot 
    % (NB: all the input and output values are in degrees!)
    RAD = 180/pi;
    % half-diagonal angular distance and azimuths
    HalfDiagonal = sqrt(2) * Side / 2;
    Az = Rot + [pi/4, 3*pi/4, 5*pi/4, 7*pi/4].*RAD;
    % project the HalfDiagonal size at these azimuths 
    [Dec_C, RA_C] = arrayfun(@(az) reckon(Dec, RA, HalfDiagonal, az, 'degrees'), Az);    
    RA_C(RA_C<0) = RA_C(RA_C<0) + 360; % the RA range is [0, 360] 
end
