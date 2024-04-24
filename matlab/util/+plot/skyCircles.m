function skyCircles(RA, Dec, Args)
    % draw circles of given angular radii on the current flat sky plot 
    % Input: - RA [deg]
    %        - Dec [deg]
    %        * ...,key,val,...
    %        'Rad' - radius in [deg] (may be a vector)
    %        'NumPoints' - number of dots in each circle
    %        'Color' - color of the plotted circles
    %        'PlotOnMap' - whether to plot with plotm instead of plot
    % Output: - circles drawn on the current figure
    % Author: A.M. Krassilchtchikov (Mar 2024)
    % Example: plot.skyCircles(256, 14, 'Rad', 10);
    arguments
        RA
        Dec
        Args.Rad       = 7    % [deg] ULTRASAT good PSF FOV = 7 deg
        Args.NumPoints = 300
        Args.Color     = 'black'
        Args.PlotOnMap = false
    end
    
    RAD = 180/pi;
    
    hold on
    
    N = numel(RA);
    
    if numel(Args.Rad) == N
        Rad = Args.Rad;
    else
        Rad = repmat(Args.Rad(1),1,N);
    end
    
    for i = 1:N    
        [Lon,Lat]=celestial.coo.celestial_circ(RA(i)/RAD,Dec(i)/RAD,Rad(i)/RAD,Args.NumPoints);
        Lon(Lon<0) = Lon(Lon<0)+2*pi;
        if Args.PlotOnMap
            plotm(Lat,Lon,'.','Color',Args.Color)
        else
            plot(Lon*RAD,Lat*RAD,'.','Color',Args.Color)
        end
    end

%     hold off
    
end