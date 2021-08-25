function [Dist, Theta, X, Y] = distBetweenPoints(varargin)
    % Interactively calculate distances between points on 2D plot
    % Input  : - Optional number of mouse keys to strike.
    %            If not provided, then will stop when "return" key is
    %            pressed.
    % Output : - Distance
    %          - Theta radians (defined as atan2(DY, DX))
    %          - X
    %          - Y
    % Author : Eran Ofek (Aug 2021)
    % Example: plot(1,1); [Dist, Theta, X, Y] = plot.distBetweenPoints;
    
    [X, Y] = ginput(varargin{:});
    Dist = sqrt(diff(X).^2 + diff(Y).^2);  
    Theta = atan2(diff(Y), diff(X));
end