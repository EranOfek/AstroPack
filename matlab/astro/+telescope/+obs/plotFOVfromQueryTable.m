function [Result] = plotFOVfromQueryTable(T, Args)
    % Plot FOVs of telescope (sub)images from tabulated DB query results
    %     Optional detailed description
    % Input  : - the input table (DB query results)   
    %          * ...,key,val,...
    %         'Lines' - a vector of table line numbers 
    %         'Color' - color of the plot lines
    %         'CooNaming' - DB style (all flat) or AstroCatalog style (RA, Dec)
    % Output : - a flat plot of (sub)image borders
    % Author : A.M. Krassilchtchikov (2025 Dec) 
    % Example: T = DB.query("select * from N3_visit_images where mountnum = 2 and camnum = 1");
    %          L = 4:10; telescope.obs.plotFOVfromQueryTable(T, 'Lines', L);
    %
    arguments
        T
        Args.Lines = 1:1;
        Args.Color = 'blue';
        Args.CooNaming = 'DB';
    end
    %    
    figure; hold on    
    for Ilin = Args.Lines
        if strcmpi(Args.CooNaming,'db')
            P = [T.ra1(Ilin), T.dec1(Ilin); T.ra2(Ilin), T.dec2(Ilin); ...
                T.ra3(Ilin), T.dec3(Ilin); T.ra4(Ilin), T.dec4(Ilin)];
        else
            P = [T.RA1(Ilin), T.Dec1(Ilin); T.RA2(Ilin), T.Dec2(Ilin); ...
                T.RA3(Ilin), T.Dec3(Ilin); T.RA4(Ilin), T.Dec4(Ilin)];
        end
        plot.plot_polygon(P, 'Color', Args.Color)
    end
end
