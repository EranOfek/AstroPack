function [Grid, GroupNum] = ditherGrid(Grid0, Args)
    % make a dithered grid of sky points from a set of point centers
    %     Optional detailed description
    % Input  : - a table of dither centers (with Grid0.RA, Grid0.Dec, Grid0.id columns)
    %          * ...,key,val,... 
    % Output : - a grid of dithered points 
    %          - a vector of point group numbers
    % Author : A.M. Krassilchtchikov (2025 Feb) 
    % Example: [Grid, GroupNum] = ultrasat.tools.ditherGrid(Grid0,'Leg',3)
    arguments
        Grid0
        Args.Leg   = 2;       % [deg]        
        Args.Ngrid = 4;       % number of grid points per 1 center
        Args.Pattern = '2x2'; % dither pattern
    end
    %  
    N0 = size(Grid0,1);        % number of centers
    Ng = Args.Ngrid * N0;      % number of dithered points
    
    GroupNum = zeros(1, Ng);
    Grid.RA  = zeros(1, Ng);
    Grid.Dec = zeros(1, Ng);
    Grid.id  = repmat("",1, Ng);
    
    Ang = [45., 135., 225., 315.];
    
    for i = 1:N0        
        RA0  = Grid0.RA(i);   % dither center
        Dec0 = Grid0.Dec(i);      
        
        if strcmpi(Args.Pattern,'2x2')
            for j = 1:Args.Ngrid  % Args.Ngrid points per 1 center
                ind = (i-1)*Args.Ngrid+j;
                Grid.id(ind) = Grid0.id(i)+0.1*j;
                [Grid.Dec(ind), Grid.RA(ind)] = reckon(Dec0,RA0,Args.Leg,Ang(j));
                if Grid.RA(ind) < 0
                    Grid.RA(ind) = Grid.RA(ind) + 360.;
                end
            end
        else
            error('Unknown dither pattern');
        end
        
        GroupNum((i-1)*Args.Ngrid+1:(i-1)*Args.Ngrid+4) = i;        
    end
end
