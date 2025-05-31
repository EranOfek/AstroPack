function [Result] = polygon_boolean_operations(P0, P1, Args)
    % One line description
    %     Optional detailed description
    % Input  : - P0 - a polygon: Nx2 array of [RA, Dec] in degrees 
    %          - P1 - a polygon or a cell array of polygons
    %          * ...,key,val,... 
    %          'Resolution' - desired raster resolution [arcsec]
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: 

    arguments
        P0
        P1
        Args.Resolution = 5; % [arcsec]                
    end

    if iscell(P1)
        Np = numel(P1); % number of polygons in the cell array
    else
        Np = 1;
    end
    
    for Ip = 1:Np
        if iscell(P1)
            P2 = P1{Ip};
        else
            P2 = P1;
        end
        
        
    end
end
