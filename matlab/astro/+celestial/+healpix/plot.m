function [Result] = plot(Pix, Args)
    % make a flat plot of a vector of healpix pixels
    %     Optional detailed description
    % Input  : - a vector of pixel    %          - 
    %          * ...,key,val,... 
    %        'PixType' - 'nested' or 'ring'
    %        'Uniq'    - 'uniq' or 'ipix' indexes
    %        'Nside'   - needed for 'ipix'
    %        'PlotOnMap' - plot in 3d or flat
    %        'Color'     - color of the circles
    % Output : - a drawing of healpix "circles" (at maximal radius)
    % Author : A.M. Krassilchtchikov (2025 Sep) 
    % Example: Pix = [181313 181316 133256 133257 133251 133249 133248 181312];
    %          celestial.healpix.plot(Pix,'Nside',128);
    arguments
        Pix        
        Args.PixType           = 'nested';
        Args.Uniq              = false;
        Args.Nside             = [];   
        Args.PlotOnMap         = true;
        Args.Color             = 'black';
    end
    %
    RAD = 180/pi;
    
%     figure(1); 
    figure;clf; hold on
    
    if Args.PlotOnMap
        axesm('aitoff', 'Frame', 'on', 'Grid', 'on');
    end
    
    for i=1:numel(Pix)
        if Args.Uniq
            [Nside, Ipix] = celestial.healpix.pix2uniqueId(Args.Nside, Pix(i));
        else
            Ipix  = Pix(i);
            Nside = Args.Nside;
        end
        [RA, Dec] = celestial.healpix.pix2ang(Nside, Ipix,'CooUnits','deg','Type',Args.PixType);
        MaxRad    = celestial.healpix.pixRadius(Nside);
        
        plot.skyCircles(RA, Dec, 'Rad', MaxRad*RAD,'PlotOnMap',Args.PlotOnMap,'Color',Args.Color);        
    end
end
