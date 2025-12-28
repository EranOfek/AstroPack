function [Result] = plotFOVfromQueryTable(T, Args)
    % Plot FOVs of telescope (sub)images from tabulated DB query results
    %     Optional detailed description
    % Input  : - the input table (DB query results)   
    %          * ...,key,val,... 
    % Output : - 
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: L = 4:10; telescope.obs.plotFOVfromQueryTable(T, 'Lines', L);
    %
    arguments
        T
        Args.Lines = 1:1;
    end
    %
    figure; clf; hold on    
    for Ilin = Args.Lines
        P = [T.ra1(Ilin), T.dec1(Ilin); T.ra2(Ilin), T.dec2(Ilin); ...
             T.ra3(Ilin), T.dec3(Ilin); T.ra4(Ilin), T.dec4(Ilin)];
        plot.plot_polygon(P, 'Color', 'blue')
    end
end
