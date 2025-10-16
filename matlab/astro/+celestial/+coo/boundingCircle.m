function [BestCoo, BestRadius] = boundingCircle(X, Y, Z, Args)
    % fit the smallest-radius bounding circle to set of X, Y points
    % Input  : - If 3-rd input is [], then this is longitude [rad].
    %            Elese cosine direction X.
    %          - If 3-rd input is [], then this is longitlatitudeude [rad].
    %            Elese cosine direction Y.
    %          - Coside direction Z. Default is [].
    %          * ...,key,val,...
    %            'UseMex' - A logical indicating if to use the fast MEX
    %                   function: celestial.coo.mex.boundingCircleSpherical_mex
    %                   About x40 faster.
    %                   Default is false (in this case input must be
    %                   lon/lat).
    % Output : - A two element vector of best circle position [X,Y] in radians.
    %          - The minimum radius around the best center than encompass all
    %            the data points [radians].
    % Author : Eran Ofek (Apr 2021)
    % Example: X = rand(10,1)+1; Y = rand(10,1);
    %          [BestCoo, BestRadius] = celestial.coo.boundingCircle(X,Y);
    %          axesm('aitoff', 'Frame', 'on', 'Grid', 'on');
    %          plotm(Y.*RAD,X.*RAD,'+'); hold on;
    %          [Lat,Lon]=reckon(BestCoo(2).*RAD, BestCoo(1).*RAD, BestRadius.*RAD, (0:1:360));
    %          plotm(Lat,Lon,'k-')
    
    arguments
        X
        Y
        Z           = [];
        Args.UseMex = false;
    end
    
    if Args.UseMex
        % call fast MEX function
        % this function ix x60 faster
        % see: celestial.coo.mex.boundingCircleSpherical_mex
        if isempty(Z)
            [LonC, LatC, BestRadius] = celestial.coo.mex.boundingCircleSpherical_mex(X,Y);
        else
            [LonC, LatC, BestRadius] = celestial.coo.mex.boundingCircleSpherical_mex(X,Y,Z);
        end
        BestCoo = [LonC, LatC];
    else
        if isempty(Z)
            [CD1, CD2, CD3] = celestial.coo.coo2cosined(X, Y);
        else
            CD1 = X;
            CD2 = Y;
            CD3 = Z;
        end
            
        MidCD1   = median(CD1,'all','omitnan');
        MidCD2   = median(CD2,'all','omitnan');
        MidCD3   = median(CD3,'all','omitnan');
        
        % RangeCD1 = range(CD1);
        % RangeCD2 = range(CD2);
        % RangeCD3 = range(CD3);
        % MidCD1   = mean(RangeCD1);
        % MidCD2   = mean(RangeCD2);
        % MidCD3   = mean(RangeCD3);
        
        [MidLon, MidLat] = celestial.coo.cosined2coo(MidCD1, MidCD2, MidCD3);
        
        Options = optimset('MaxFunEvals',1000, 'TolX',1e-5);
        
        [BestCoo, BestRadius] = fminsearch(@radiusForCenter,[MidLon, MidLat], Options);
    end

    function Radius = radiusForCenter(Center)
        % maximum radius between center and data points
        Radius = max(celestial.coo.sphere_dist_fast(Center(1), Center(2), X(:), Y(:)));
    end
end