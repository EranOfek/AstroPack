function skyEllipses(RA, Dec, SemiMajor, Excentr, Angle, Args)
    % draw ellipses of given size and rotation on the current flat sky plot 
    % Input: - RA  
    %        - Dec 
    %        - SemiMajor - length of the semi-major axis
    %        - Excentr   - excentricity
    %        - Angle     - rotation angle (measured clockwise from due north 
    %        * ...,key,val,...
    %        'AngleUnits' - 'degrees' or 'radians'
    %        'NumPoints' - number of dots in each circle
    %        'Color' - color of the plotted circles
    %        'PlotOnMap' - whether to plot with plotm instead of plot
    % Output: - ellipses drawn on the current figure
    % Author: A.M. Krassilchtchikov (Mar 2024)
    % Example: plot.skyEllipses(256, 14, 10, 0.5);
    %          plot.skyEllipses(180, -1, 7, 0.9, 45, 'Color', 'red');
    %          plot.skyEllipses(2, -1, 0.1, 0.9, 'Color', 'blue','AngleUnits','radians');
    arguments
        RA
        Dec
        SemiMajor
        Excentr
        Angle           = 0; % the angle is measured clockwise from due north
        Args.AngleUnits = 'degrees'
        Args.NumPoints  = 300
        Args.Color      = 'black'
        Args.PlotOnMap = false
    end
    
    RAD = 180/pi;
    
    hold on
    
    N = numel(RA);
    
    if numel(SemiMajor) == N
        Semi = SemiMajor;
        Exc  = Excentr;
        Ang  = Angle;
    else
        Semi = repmat(SemiMajor(1),1,N);
        Exc  = repmat(Excentr(1),1,N);
        Ang  = repmat(Angle(1),1,N);
    end
    
    for i = 1:N            
        [Lat,Lon] = ellipse1(Dec(i), RA(i), [Semi(i) Exc(i)], Ang(i),[],[],Args.AngleUnits,Args.NumPoints); 
        if strcmpi(Args.AngleUnits,'degrees')            
            Lon(Lon<0) = Lon(Lon<0)+360;            
            if Args.PlotOnMap
                plotm(Lat/RAD,Lon/RAD,'.','Color',Args.Color)
            else
                plot(Lon,Lat,'.','Color',Args.Color)
            end
        else            
            Lon(Lon<0) = Lon(Lon<0)+2*pi; 
            if Args.PlotOnMap
                plotm(Lat,Lon,'.','Color',Args.Color)
            else                
                plot(Lon*RAD,Lat*RAD,'.','Color',Args.Color)
            end
        end           
    end    
end