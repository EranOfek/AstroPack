function [Xlin, Ylin, Zgr] = ungridded_image(X, Y, Z, Args)
    % color plot an image of Z(X,Y) when X and Y do not form a regular grid
    % Input: - X (a vector of the first variable)
    %        - Y (a vector of the second variable)
    %        - Z (a vector of the function)
    %         * ...,key,val,... 
    %        'NGridX' -- number of points in a regular grid in X
    %        'NGridY' -- number of points in a regular grid in Y
    % Output: - a color plot of Z(X,Y) as an image
    % Author: A.M. Krassilchtchikov (Jan 2024)
    % Example: X = rand(100,1); Y = rand(100,1); Z = X.*(1-Y);
    %          plot.ungridded_image(X,Y,Z)
    arguments
        X
        Y
        Z
        Args.NGridX = 1000;
        Args.NGridY = 1000;
    end

    Xlin = linspace(min(X),max(X),Args.NGridX);
    Ylin = linspace(min(Y),max(Y),Args.NGridY);
    [Xgr, Ygr] = meshgrid(Xlin, Ylin);
    Zgr  = griddata(X, Y, Z, Xgr, Ygr);
    imagesc(Xlin, Ylin, Zgr); colorbar
    set(gca,'YDir','normal');
       
end
