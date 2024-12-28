function [PosX, PosY] = xy2axesPos(Ha, X, Y, XLimYLim)
    % Given axis handle and X/Y positions, return the position in the axes coordinates.
    %   Taking into account linear and log scales, but ignoring axes flips.
    %   The returned coordinates are between 0 and 1 within the axes.
    % Input  : - Axis handle.
    %          - X positions.
    %          - Y positions.
    %          - optinal [Xmin Xmax Ymin Ymax]. Default is to take from
    %            axis (note may be change with zoom).
    % Output : - X position within axis.
    %          - Y position within axis.
    % Author : Eran Ofek (2024 Jan) 
    % Example: [PosX, PosY] = plot.xy2axesPos(Ha, X, Y)

    arguments
        Ha
        X
        Y
        XLimYLim = [];
    end

    if isempty(XLimYLim)
        XLim = Ha.XLim;
        YLim = Ha.YLim;
    else
        XLim = XLimYLim(1:2);
        YLim = XLimYLim(3:4);
    end
    
    switch lower(Ha.XScale)
        case 'linear'
            PosX = (X - XLim(1))./(XLim(2)-XLim(1));
        case 'log'
            PosX = (log10(X) - log10(XLim(1)))./(log10(XLim(2))-log10(XLim(1)));
    end

    
    switch lower(Ha.YScale)
        case 'linear'
            PosY = (Y - YLim(1))./(YLim(2)-YLim(1));
        case 'log'
            PosY = (log10(Y) - log10(YLim(1)))./(log10(YLim(2))-log10(YLim(1)));
    end

end
