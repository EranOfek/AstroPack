function [BestCoo, BestRadius] = boundingCircle(varargin)
% fit the smallest-radius bounding circle to set of X, Y points
% Input  : - An array containing X coordinates [radians].
%          - An array containing Y coordinates (corresponding to the X
%            coordunates) [radians].
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

if nargin==2
    % Input is Lon, Lat
    Lon = varargin{1};
    Lat = varargin{2};
    [CD1, CD2, CD3] = celestial.coo.coo2cosined(Lon, Lat);
elseif nargin==3
    % Input is cosine directions
    CD1 = varargin{1};
    CD2 = varargin{2};
    CD3 = varargin{3};
    [Lon, Lat] = celestial.coo.cosined2coo(CD1, CD2, CD3);
else
    error('Must provide either Lon,Lat, or CD1,CD2,CD3');
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

    function Radius = radiusForCenter(Center)
        % maximum radius between center and data points
        Radius = max(celestial.coo.sphere_dist_fast(Center(1), Center(2), Lon(:), Lat(:)));
    end
end