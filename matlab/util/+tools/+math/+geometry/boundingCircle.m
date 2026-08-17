function [BestXY, BestRadius] = boundingCircle(X, Y, Args)
    % fit the smallest-radius bounding circle to set of X, Y points
    %   See also: celestial.coo.boundingCircle
    % Input  : - An array containing X coordinates.
    %          - An array containing Y coordinates (corresponding to the X
    %            coordunates).
    %          * ...,key,val,...
    %            'UseMex' - A logical indicating if to use the fast MEX version:
    %                   tools.math.geometry.mex.smallestRadiusBoundingCircle
    %                   An exact minimal-enclosing-circle solver (vs. the
    %                   fminsearch approximation used when false) - see
    %                   issue #1197.
    %                   Default is true.
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
        Args.UseMex   = true;
    end

    % Filter out non-finite points so the mex and matlab paths handle
    % invalid input identically: the mex kernel bails out to an all-NaN
    % result on any non-finite input, while the matlab/fminsearch path
    % silently tolerates it (issue #1197).
    FlagFinite = isfinite(X) & isfinite(Y);
    X = X(FlagFinite);
    Y = Y(FlagFinite);
    if isempty(X)
        error('tools:math:geometry:boundingCircle:noValidPoints', 'No finite input points - can not compute a bounding circle');
    end

    if Args.UseMex
        [Xc, Yc, BestRadius] = tools.math.geometry.mex.smallestRadiusBoundingCircle(X,Y);
        BestXY = [Xc, Yc];
    else
        % RangeX = range(X);
        % RangeY = range(Y);
        % MidX   = mean(RangeX);
        % MidY   = mean(RangeY);
        
        MidX = median(X,'all','omitnan');
        MidY = median(Y,'all','omitnan');
        
        Options = optimset('MaxFunEvals',1000, 'TolX',min(range(X),range(Y))./1000);
        
        [BestXY, BestRadius] = fminsearch(@radiusForCenter,[MidX,MidY], Options);            
    
    end

    % Aux function:
    function Radius = radiusForCenter(CenterXY)
        % maximum radius between center and data points
        Radius2 = (CenterXY(1) - X(:)).^2 + (CenterXY(2) - Y(:)).^2;
        Radius  = sqrt(max(Radius2));
    end
end