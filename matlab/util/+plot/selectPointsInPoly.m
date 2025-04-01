function [Flag,Poly] = selectPointsInPoly(Hg, Poly, Args)
    % Given a plot with data points, interatively plot  a polygon and choose points inside polygon.
    % Input  : - Axes handle. Default is gca.
    %          - Two column matrix of polygon verteces: [X, Y].
    %            If empty, then choose points interactively using mouse.
    %            Press enter to stop selection.
    %            Default is [].
    %          * ...,key,val,...
    %            'Plot' - Plot polygon in plot. Default is true.
    %            'Data' - [X, Y] data. If empty will get from plot.
    %                   Default is [].
    % Output : - Vector of flags indicating, for each point, if inside
    %            polygon.
    %          - A two column matrix of polygon [X,Y] verteces.
    % Author : Eran Ofek (2025 Apr) 
    % Example: FlagInPoly=plot.selectPointsInPoly

    arguments
        Hg             = [];
        Poly           = [];
        Args.Plot      = true;
        Args.Data      = [];
    end

    if isempty(Hg)
        Hg = gca;
    end
    % change axes focus
    axes(Hg);

    % get data points
    if isempty(Args.Data)
        X = Hg.Children.XData;
        Y = Hg.Children.YData;
    else
        X = Args.Data(:,1);
        Y = Args.Data(:,2);
    end

    if isempty(Poly)
        fprintf('Select polygon verteces using mouse - enter to continue\n');
        [Xp, Yp] = ginput();
    else
        Xp = Poly(:,1);
        Yp = Poly(:,2);
    end

    if Args.Plot
        hold on;
        plot(Xp, Yp);
        hold off;
    end

    % choose points inside polygon
    [Flag] = inpolygon(X, Y, Xp, Yp);
    Poly   = [Xp(:),Yp(:)];


end
