function [BestXY, BestRadius] = boundingCircle(X, Y)
% fit the smallest-radius bounding circle to set of X, Y points
% Input  : - An array containing X coordinates.
%          - An array containing Y coordinates (corresponding to the X
%            coordunates).
% Output : - A two element vector of best circle position [X,Y].
%          - The minimum radius around the best center than encompass all
%            the data points.
% Author : Eran Ofek (Apr 2021)
% Example: X = rand(10,1); Y = rand(10,1);
%          [BestXY, BestRadius] = tools.math.geometry.boundingCircle(X,Y);
%          plot(X,Y,'+'); hold on; plot.plot_ellipse(BestXY, [BestRadius, BestRadius],[],0);

arguments
    X
    Y
end

% RangeX = range(X);
% RangeY = range(Y);
% MidX   = mean(RangeX);
% MidY   = mean(RangeY);

MidX = median(X,'all','omitnan');
MidY = median(Y,'all','omitnan');

Options = optimset('MaxFunEvals',1000, 'TolX',min(range(X),range(Y))./1000);

[BestXY, BestRadius] = fminsearch(@radiusForCenter,[MidX,MidY], Options);

    function Radius = radiusForCenter(CenterXY)
        % maximum radius between center and data points
        Radius2 = (CenterXY(1) - X(:)).^2 + (CenterXY(2) - Y(:)).^2;
        Radius  = sqrt(max(Radius2));
    end
end