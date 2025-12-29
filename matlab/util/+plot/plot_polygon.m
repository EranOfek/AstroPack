function plot_polygon(P, Args)
    % plot a closed polygon
    %     Optional detailed description
    % Input  : - 2D matrix in the form X, Y
    %          * ...,key,val,... 
    %          'Color' - color
    %          'Symbol'- plotting symbol
    % Output : - a graph
    % Author : A.M. Krasilchtchikov (2025 Dec) 
    % Example: plot.plot_polygon(P, 'Color', 'blue')
    arguments
        P
        Args.Color             = 'black';
        Args.Symbol            = '-o';
    end
    X = P(:,1); Y = P(:,2);
    X(end+1) = X(1); Y(end+1) = Y(1);
    plot(X, Y, Args.Symbol, 'Color', Args.Color);    
end
