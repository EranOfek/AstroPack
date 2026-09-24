function [FOV, Tiles] = getFOVcorners(RA0, Dec0, Args)
    % get coordinates of the ULTRASAT FOV corners given its center and roll angle
    %
    % Input : - RA0  [deg] of the FOV center
    %         - Dec0 [deg] of the FOV center
    %       * ...,key,val,... 
    %       'Roll'        - roll angle [deg]
    % Output : - a struct of FOV corner coordinates [deg]
    %          - (optional) a struct array of Tile corner coordinates [deg]
    % Author: A.M. Krassilchtchikov (2025 Jan)
    % Example: FOV  = ultrasat.tools.getFOVcorners(200.1,-50.)
    %         [FOV2, Tiles2] = ultrasat.tools.getFOVcorners(200.1,-50.,'Tiles',1,'Roll',90)
    %         [FOV3, Tiles3] = ultrasat.tools.getFOVcorners(30,60,'Plot',1,'Tiles',1,'Roll',30)
    %           
    arguments
        RA0
        Dec0      
        Args.Roll       = 0;      % roll angle [deg]     
        Args.Tiles      = false;  % calculate the tile corners 
        Args.Plot       = false; 
        Args.Color      = 'blue'; 
    end
    
    Tiles = [];
    
    RAD = 180/pi;
    
    TileSize  = 4738;  % number of pixels in a tile
    GapSize   = 253;   % number of "pixels" in the gap
    PixSizeDeg= 0.001512; % pixel size [deg]
    Npix      = 2*TileSize+GapSize; % total number of pixels in the FOV (including the gap)
    CRPIX1    = (Npix+1)/2;
    CRPIX2    = (Npix+1)/2;    
        
    SimWCS = AstroWCS();
    SimWCS.ProjType  = 'TAN';
    SimWCS.ProjClass = 'ZENITHAL';
    SimWCS.CooName   = {'RA'  'DEC'};
    SimWCS.CTYPE     = {'RA---TAN','DEC---TAN'};
    SimWCS.CUNIT     = {'deg', 'deg'};
    SimWCS.CD(1,1)   = PixSizeDeg;
    SimWCS.CD(2,2)   = PixSizeDeg;
    SimWCS.CRVAL(1)  = RA0;
    SimWCS.CRVAL(2)  = Dec0;
    SimWCS.CRPIX(1)  = CRPIX1;
    SimWCS.CRPIX(2)  = CRPIX2;
    SimWCS.populate_projMeta;
    
    Alpha_rad = Args.Roll/RAD;
    RotMatrix = [cos(Alpha_rad), -sin(Alpha_rad);
                 sin(Alpha_rad),  cos(Alpha_rad)];
    SimWCS.CD = RotMatrix * SimWCS.CD;
    
    FOVX = [Npix, Npix,    1, 1];
    FOVY = [Npix,    1,    1, Npix];
    
    [FOV.RA, FOV.Dec] = SimWCS.xy2sky(FOVX,FOVY);
    
    if Args.Tiles
        T1X = [TileSize, 1, 1, TileSize];
        T1Y = [TileSize+GapSize, TileSize+GapSize, Npix, Npix];
        [Tiles(1).RA, Tiles(1).Dec] = SimWCS.xy2sky(T1X,T1Y);
    
        T2X = [TileSize+GapSize, Npix, Npix, TileSize+GapSize];
        T2Y = [TileSize+GapSize, TileSize+GapSize, Npix, Npix];
        [Tiles(2).RA, Tiles(2).Dec] = SimWCS.xy2sky(T2X,T2Y);
        
        T3X = [TileSize+GapSize, Npix, Npix, TileSize+GapSize];
        T3Y = [TileSize, TileSize, 1, 1];
        [Tiles(3).RA, Tiles(3).Dec] = SimWCS.xy2sky(T3X,T3Y);
        
        T4X = [TileSize, 1, 1, TileSize];
        T4Y = [TileSize, TileSize, 1, 1];
        [Tiles(4).RA, Tiles(4).Dec] = SimWCS.xy2sky(T4X,T4Y);
    end
        
    if Args.Plot
        figure(1); clf
         plot(FOV.RA, FOV.Dec,'*','Color',Args.Color)
        hold on
        plot(RA0, Dec0,'+','LineWidth',3,'Color','black')
        if Args.Tiles
            plot([Tiles.RA], [Tiles.Dec],'o')
        end        
                
        figure(2); clf
        axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
        plotm(Dec0/RAD,RA0/RAD,'.','Color','black');
        plotm(FOV.Dec/RAD,FOV.RA/RAD,'.','Color',Args.Color);        
    end
end





%%%%% the old function is not precise, keeping here just for the case 

function [FOV, Tiles] = getFOVcorners_old(RA, Dec, Args)
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
%         axesm('MapProjection', 'aitoff', 'AngleUnits', 'radians', 'LabelUnits', 'radians', 'Grid', 'on');
%         plotm(Dec/RAD,RA/RAD,'.','Color','red');
%         plotm(FOV.Dec/RAD,FOV.RA/RAD,'.','Color',Args.Color);
        
        plot(FOV.RA, FOV.Dec,'*')
        hold on
        plot([Tiles.RA], [Tiles.Dec],'o')
        plot(RA, Dec,'+','LineWidth',3,'Color','black')
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
