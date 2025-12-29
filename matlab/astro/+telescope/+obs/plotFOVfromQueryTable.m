function [Result] = plotFOVfromQueryTable(T, Args)
    % Plot FOVs of telescope (sub)images from tabulated DB query results
    %     Optional detailed description
    % Input  : - the input table (DB query results)   
    %          * ...,key,val,...
    %         'Lines' - a vector of table line numbers 
    %         'Color' - color of the plot lines
    % Output : - a flat plot of (sub)image borders
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: T = DB.query("select * from N3_visit_images where mountnum = 2 and camnum = 1");
    %          L = 4:10; telescope.obs.plotFOVfromQueryTable(T, 'Lines', L);
    %
    arguments
        T
        Args.Lines = 1:1;
        Args.Color = 'blue';
    end
    %
    figure; hold on    
    for Ilin = Args.Lines
        P = [T.ra1(Ilin), T.dec1(Ilin); T.ra2(Ilin), T.dec2(Ilin); ...
             T.ra3(Ilin), T.dec3(Ilin); T.ra4(Ilin), T.dec4(Ilin)];
        plot.plot_polygon(P, 'Color', Args.Color)
    end
end
